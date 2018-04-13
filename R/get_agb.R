#' Function to estimate Above Ground Biomass of the trees from Chave Paper
#'
#' Gets the AGB for a table of trees
#'
#' @param DBH: 
#' @param E: 
#' @param WD: 
#'
#' @references  Chave 2009, 2014; Zanne 2009 
#' 
#' @return AGB
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer()
#' E.nohbec <- 0.306909   #from Nohbec (longitude = -88.178; latitude = 19.142)
#' WD.mexico <- 0.6 #Mean WD just in case species not present
#' forest$AGB <- get.agb(forest$DBH, E = E.nohbec, WD = WD.mexico) 
get.agb <-function(DBH, E = 0.306909, WD = 0.6){
  #AGB calculation using Chave et al., 2014 Eqn 7
  agbiomass<-exp(-1.803-0.976*E + 0.976*log(WD) + 2.673*log(DBH) - 0.0299*(log(DBH)^2)) 
  return (agbiomass)
}
