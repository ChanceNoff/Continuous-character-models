plan <- drake_plan(
  taxon_names = read.csv(file=file_in("data/taxa.csv"), stringsAsFactors = FALSE), ## imports taxon file
  tree.id = rotl::tnrs_match_names(taxon_names$Taxon[1])$ott_id, #tnrs_match_names finds a match in open tree for the name in row one of our taxa.csv file
  tree = rotl::tol_subtree(ott_id=tree.id, label_format = 'name'), #gets subtree and pick out the node
  tree_resolution = (ape::Nnode(tree)/(Ntip(tree) - 1)),
  tree_print = plot_tree(tree, file=file_out("results/Russulaceae.pdf")),
  mytree = ape::plot.phylo(tree, type="fan", cex=0.2),
  tree_info = print(paste("The tree has ", ape::Ntip(tree), " terminals and ",
                          Nnode(tree), " internal nodes out of ",ape::Ntip(tree)-2,
                          " possible, which means it is ",
                          round(100*(ape::Nnode(tree)-1)/(ape::Ntip(tree)-3), 2),
                          "% resolved", sep="")),
  
  # Open Tree can also return the original studies with the source trees.
  amanita.trees = studies_find_trees(property="ot:ottTaxonName", value="Amanita", detailed=FALSE),
  amanita.studies.ids = unlist(amanita.trees$study_ids),
  
  # Let's get info on the first study
  amanita.study1.metadata = rotl::get_study_meta(amanita.studies.ids[1]),
  print(rotl::get_publication(amanita.study1.metadata)),
  
  # And let's get the tree from this study
  amanita.study1.tree1 = get_study(amanita.studies.ids[1])[[1]],
  
  # And plot it
  # ape::plot.phylo(amanita.study1.tree1, type="fan", cex=0.2)
  
  #HW
  # Get data from an external source
  # Load the data in
  # Plot the data, summarize the data, etc. to make sure there are no weird values.
  soils_dat = read.csv(file=file_in("data/Soil.csv"), header = TRUE, stringsAsFactors = FALSE),
  soil_long = gather(soils_dat, site, N_content, AR, RB, WB, GB),
  Nitrogen = soil_long$N_content,
  Nitrogen2 = as.numeric(as.character(Nitrogen)),
  ploted_N = boxplot(Nitrogen2) ## I know this is a bad plot, but I was struggling with the code
  #as.numeric(as.character(soil_long$N_content))
  
  #phy = ape::rcoal(20+round(stats::runif(1,1,20))),
  #even = is_even(phy),
  #plotted = plot_tree(phy, file_out("results/tree.pdf")),
  #save_even = save(phy, even, file=file_out("results/even.rda"))
)
