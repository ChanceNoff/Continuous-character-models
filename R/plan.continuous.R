plan.continuous <- drake_plan(
  tree = read.tree("data/Russula_mega_ML.tre"),
  mytree = ape::plot.phylo(tree, type="fan", cex=0.2),
  taxon = tree$tip.label,
  substr(taxon, 14, nchar(taxon)), ## removes everything from the 14th character to the end to the string, this worked for lots of the names but not all
  str_detect(taxon, "Russula"), ## finds all the strings containing Russula
  
  ##From brian 
  taxa_cultured = taxon[!grepl("uncultured", taxon)], ## grepl gives you back a logical output e.g. true or false; the ! reverses the comand
  Russula_only = taxa_cultured[grepl("Russula", taxa_cultured)], ## keeps only tips with Russula
  Russula_no_sp = Russula_only[!grepl("Russula_sp", Russula_only)], ## removes Russula_sp
  Russula_no_family = Russula_no_sp[!grepl("Russulaceae", Russula_no_sp)], ## removes Russulaceae
  strsplit(taxa_cultured, "_"), ## split taxa on underscore
  
  
  
  Russula_final_tips = ReturnGenusSpecies(Russula_no_family),
  #continuous.data <- read.csv(file="____PATH_TO_DATA_OR_SOME_OTHER_WAY_OF_GETTING_TRAITS____", stringsAsFactors=FALSE) #death to factors.
  
  Russula_subset = Russula_final_tips[1:30],
  
 # tk = which(tree$tip.label%in%Russula_lat=='TRUE'),
  
  #ContinuousData = GetLatLongData(Russula_subset), #function not working
  
  Russula_lat1 = read.csv("data/Russula_lat1.csv",header = FALSE, row.name = 1),
  #Russula_lat3 = ReplaceSpacesInRownames(Russula_lat1),
  Russula_lat4 = as.matrix(Russula_lat1),
  Clean = CleanData(CleanPhyTipNames(tree), Russula_lat4),
  
  Viz = VisualizeData(Clean$phy, Clean$data),
 
  cleantree = Clean$phy,
  cleand = Clean$data,
  ##cleanertree = ape::drop.tip(Clean$phy, VECTOR OF TIPS TO DROP), ## code to drop tips
  #cleandat2 = as.data.frame(Clean$data),
  #cleandat = get(data(Clean$data)),
  BM1 = geiger::fitContinuous(cleantree, cleand, model="BM", ncores = 1), #not working get $operator is invalid for atomic vectors
  print(paste("The rate of evolution is", BM1$opt$sigsq, "in units of", "latitude")), #what are the units? is this the correct rate
 
 OU1 = fitContinuous(cleantree, cleand, model="OU", ncores = 1),
 #par(mfcol(c(1,2))), # this doesent work and I get a Server error like unexpected plot index??
 plot(tree, show.tip.label=FALSE),
 ou.tree = rescale(tree, model="OU", OU1$opt$alpha),
 plot(ou.tree),

 # define set of models to compare
 models=c("BM", "OU"),
 summaries=c("diffusion", "Ornstein-Uhlenbeck"),
 
 ## ESTIMATING measurement error ##
 aic.se=numeric(length(models)),
 lnl.se=numeric(length(models)),
 for(m in 1:length(models)){
   cat("\n\n\n\n\t*** ", paste(toupper(summaries[m]),": fitting ", sep=""), models[m],
       " with SE *** \n", sep="")
   tmp=fitContinuous(cleantree,cleand, SE=NA, model=models[m],
                     bounds=list(SE=c(0,0.5)), ncores=1)
   print(tmp)
   aic.se[m]=tmp$opt$aicc
   lnl.se[m]=tmp$opt$lnL
 },
 
 ## OU is the better model with a higher log-liklihood
 q = list(rbind(c(-.5, .5), c(.5, -.5))),
 one.discrete.char = sim.char(cleantree, q, nsim = 1, model = "discrete", root = 1),
 reconstruction.info = ace(one.discrete.char, cleantree, type="discrete", method="ML", CI=TRUE),
 best.states = colnames(reconstruction.info$lik.anc)[apply(reconstruction.info$lik.anc, 1, which.max)],
 
 labeled.tree = tree$node.label,
 cleandfram = data.frame(cleantree$tip.label , one.discrete.char, cleand),
 nodeBased.OUMV = OUwie(labeled.tree, cleandfram[,3], model="OUMV", simmap.tree=FALSE, diagn=FALSE),
 print(nodeBased.OUMV),
 
)
 


