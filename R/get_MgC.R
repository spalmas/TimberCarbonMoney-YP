#' Function to estimate Above Ground Biomass of the trees from Chave Paper
#'
#' Gets the AGB for a table of trees
#'
#' @param forest: table of trees with columns of DBH and HEIGHT 
#'
#' @references  Chave 2009, 2014; Zanne 2009 
#' 
#' @return AGB in Mg C
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizerINFyS()
#' forest$MgC <- get.MgC(forest) 
get.MgC <- function(forest = NULL){
  E.nohbec <- 0.306909   #from Nohbec (longitude = -88.178; latitude = 19.142)
  WD.mexico <- 0.6 #Mean WD just in case species not present
  #wd.list <- species$wd[match(forest$SPECIES.CODE, species$SPECIES.CODE)]   #
  forest <- forest %>% 
    mutate(agb = get.agb(DBH, E = E.nohbec, WD = WD.mexico),
         bgb = 0.489*(agb^0.89),
         totalbiomass = agb + bgb,
         MgC = totalbiomass*0.47   #should it be 0.5?
  )
  return(forest$MgC)
}

