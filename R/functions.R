doubling <- function(x) {
  return(x*2)
}

is_even <- function(phy) {
  number_of_tips <- ape::Ntip(phy)
  even <- FALSE
  if(number_of_tips%%2==0) {
    even <- TRUE
  }
  return(even)
}

plot_tree <- function(phy, file) {
  pdf(file=file, width = 20, height = 20)
  plot(phy)
  plot(phy, type='fan')
  plot(phy, type = 'fan', show.tip.label = FALSE, edge.width = .1)
  dev.off()
}

CleanPhyTipNames <- function(phy) {
  phy$tip.label <- ReturnGenusSpecies(phy$tip.label)
  return(phy)
}

CleanData <- function(phy, data) {
  treedata(phy, data, sort = TRUE, warnings = TRUE)
  #treedata() in Geiger is probably my favorite function in R.
}

VisualizeData <- function(phy, data) {
  #Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
  
  # Now write the code to use VisualizeData() to actually look at your data
  plot(phy ,no.margin=TRUE,edge.width=2)
}

# BM1 <- function(phy, data){
#   geiger::fitContinuous(tree, cleaned.continuous, model="BM")
#   print(paste("The rate of evolution is", _____, "in units of", _______))
# }

GetTreeWithNameProcessing <- function(treefile) {
  raw <- readLines(treefile)
  #raw <- gsub(" CSM", "", raw)
  raw <- gsub(" ", "_", raw)
  phy <- ape::read.tree(text=raw)
}

ReplaceSpacesInRownames <- function(x) {
  rownames(x) <- gsub(" ", "_", rownames(x))
  return(x)
}

GetSingleGenusSpecies <- function(x) {
  return(paste(strsplit(x, " |_")[[1]][1:2], collapse=" "))
}

GetAllGenusSpecies <- function(x) {
  sapply(x, GetSingleGenusSpecies)
}

ReturnGenusSpecies <- function(x)
{ result <-rep(NA, length(x))
for (i in seq_along(x)) {
  split_result <- strsplit(x[i], "_")[[1]]
  result[i] <- paste0(split_result[2:length(split_result)], collapse =  "_")
}
return(result)
}  

GetLatLongData <- function(x) { 
  result <-rep(NA, length(x))
  for (i in seq_along(x)) {
    print(paste("Now doing ", x[i], " which is species number ", i))
    mycoinfo <- mycoportal(taxon = x[i], browserName = "chrome")
    try(result[i] <- mycoinfo@records$Locality[1:1])
  } 
  return(result)
}

#lat<-matrix(nrow=length(Russula_final_tips), ncol=1)
#rownames(lat) <- unique(elevation$gen2)
#colnames(lat) <- c('Latitude')


