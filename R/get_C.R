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
#' forest <- forest.randomizer()
#' forest$AGB <- get.agb(forest) 

get.C <- function(forest = NULL){
  
  #standard carbon fraction of Biomass
  C.fraction <- 0.5
  
  #wood density, Using Zanne 2009
  wd.list <- species$wd[match(forest$SPECIES.CODE, species$SPECIES.CODE)]
  
  #Chave et al. 2014. Equation 4
  C.list <- C.fraction * 0.0673 * (wd.list*forest$DBH^2*forest$HEIGHT)^0.976 / 1000
 
  return(C.list)
}

