## Download data for Amanita muscaria 
russula.rec <- mycoportal(taxon = 'Russula_marangani')
## Plot species distribution 
plot_distmap(x = russula.rec , mapdatabase= 'state', interactive = FALSE) 
## Plot records heatmap for states of USA 
plot_datamap(x = russula.rec, mapdatabase = 'state', index = 'rec')


x <- 'Russula_marangani'
russula.rec <- mycoportal(taxon = x) #pulls data from mycoportal for Russula marangani
R_maragani_latlong <- russula.rec@records$Locality[1:1] #prints out al lat and long data for the first coloum and first row
R_maragani_lat <- strsplit(R_maragani_latlong, " ")[1] #returns only lat data
str(R_maragani_lat)


Russula_subset <- Russula_final_tips[1:2]
  latsTest <- numeric(length(Russula_subset))
 for (i in 1:length(unique(Russula_subset))) {
    species <- unique(Russula_subset)[i]
    mycoinfo <- mycoportal(taxon = species, browserName = "chrome", wait = 2)
    latsTest[i] <- mycoinfo@records$Locality[1:1]
  }  ### OMG this worked 
  
  make_url <- function(genus, species) {
    return(paste0("https://mycoportal.org/portal/collections/listtabledisplay.php?starr={%22db%22:%22all%22,%22taxa%22:%22", genus, "%20", species, "%22,%22usethes%22:true,%22taxontype%22:%221%22}&jsoncollstarr={%22db%22:%22all%22}"))
  } ## code from biran to pull data directly from website, this  just gives me the url containing the table



# 