#' Directional mortality 
#'
#' The function finds those trees that are killed by felling of nearby trees
#' It finds the trees close to the harvested trees.
#' Only kills small trees under (20 cm) and those less than half of tree height distance
#'
#' @param forest The table of trees in the forest
#' @param harvested table of harvested trees
#' @param dir.felling bool. If directional felling is applied
#'
#' @references
#' Ninguna por ahora
#' 
#' @return an array of boolean values: TRUE: the tree is inside the rectangle and FALSE if not.
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 2)
#' harvested.list <- get.harvest(forest = forest, intensity = 'All', ACA. = 0)
#' harvested <- forest[harvested.list,]
#' forest <- forest[!harvested.list,]
#' killed.directional <- get.mortality.DF(forest = forest, harvested = harvested)
#' killed.directional
#' killed.trees <- forestkilled.directional,]
get.mortality.DF <- function(forest, harvested, dir.felling = FALSE){
  vecinity <- rep(x = c(FALSE), times = nrow(forest)) #empty vector
  
  if(!dir.felling){
    return(vecinity)
  } else if (nrow(harvested) != 0){    #If there are trees harvested
    for (i in 1:nrow(harvested)){    #For each harvested tree
      vecinity.i <- rep(x = c(FALSE), times = nrow(forest))
      
      #See if each tree in the stand is close to the harvested tree
      vecinity.i <- (forest$COORD.X - harvested$COORD.X[i])**2 + 
        (forest$COORD.Y-harvested$COORD.Y[i])**2 < (harvested$HEIGHT[i])**2
      
      #Adding those in the vecinity to those estimated before
      vecinity <- vecinity | vecinity.i 
    }
    
    #only killing small trees and those in the same ACA
    vecinity <- vecinity & (forest$DBH < 20) & (forest$ACA == unique(harvested$ACA))
    
    #There is a 50/50 change of mortality in those small trees
    vecinity[vecinity] <- sample(x = c(TRUE,FALSE), 
                                 prob = c(0.5, 0.5), 
                                 replace = TRUE,
                                 size = sum(vecinity))
  }
  #remove trees that were inside rectangles
  return(vecinity)
}