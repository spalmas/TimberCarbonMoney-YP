#' Function to estimate Above Ground Biomass of the trees from Chave Paper
#'
#' Gets the BA for a table of trees
#'
#' @param forest
#' @param area
#'
#' @return BA in m2
#'
#' @examples
#' forest <- forest.randomizerINFyS()
#' get.ba(forest, area = 1600)
get.ba <-function(forest, area){
  #AGB calculation using Chave et al., 2014 Eqn 7
  CF <- 10000/area
  BA <- sum(pi*forest$DBH^2/10000) * CF
  return (BA)
}
