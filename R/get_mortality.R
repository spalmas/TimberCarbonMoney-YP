#' NATURAL MORTALITY 
#'
#' Returns a TRUE/FALSE list of killed trees in the forest based on probabilities. Smaller trees have a higher probability of mortality
#'
#' @param forest The table of trees in the forest
#'
#' @references
#' 
#' @return an array of boolean values: TRUE: the tree if it is killed by natural causes
#'
#' @examples
#' source('startup.R')
#' forest <- stand.randomizer()
#' get.mortality(forest)
#' 
get.mortality <- function(forest){
  #cretes an empty array of FALSE with the length of the stand
  #natural.dead <- rep(x = c(FALSE), times = nrow(forest))
  #trees smaller than 10cm have a 3% of mortality probability
  #natural.dead [forest$DBH < 10] <- sample(x = c(TRUE, FALSE), prob = c(0.03, 0.97), replace = TRUE, size = sum(forest$DBH < 10))
  #trees larger than 10cm have a 1% of mortality probability
  #natural.dead [forest$DBH >= 10] <- sample(x = c(TRUE, FALSE), prob = c(0.01, 0.99), replace = TRUE, size = sum(forest$DBH >= 10))
  natural.dead <- sample(x = c(TRUE, FALSE), prob = c(0.03, 0.97), replace = TRUE, size = nrow(forest))
  
  return(natural.dead)
}