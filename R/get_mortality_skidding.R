#' Skidding mortality 
#'
#' The functions finds which trees are inside a rectangle from the position of the harvested tree
#' to the trail. It creates a rectangle 2m wide from the trail to the harvested tree and it returns
#' an array of boolean values: TRUE: the tree is inside the rectangle and FALSE if not. 
#' The winching distance avoids nmortality within that distance from the harvested tree.
#'
#' @param stand The table of trees in the stand
#' @param w.dist The winching distance that was input in the simulation
#' @param harvested table of harvested trees
#'
#' @references
#' Ninguna por ahora
#' 
#' @return an array of boolean values: TRUE: the tree is inside the rectangle and FALSE if not.
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' intensity <- 'Normal'
#' rotation <- 20
#' y <- 20
#' w.dist <- 5
#' harvested <- get.harvest(stand, intensity)
#' inside.small <- get.mortality.skidding(stand = stand, w.dist = w.dist, harvested = harvested)
#' inside.small
#' killed.trees <- stand[inside.small,]
get.mortality.skidding <- function(forest, w.dist, harvested, skidder = 'Skidder'){
  #creates an empty array of FALSE with the length of the stand
  inside <- rep(x = c(FALSE), times = nrow(forest))
  inside.small <- inside
  
    if (skidder == 'MAT'){half.width.skidder <- 1} else {half.width.skidder <- 2} #Skidder width = 4, MAT width = 2
  half.width.skidder <- 2 #half with of skidder or MAT.
  
  
  if (nrow(harvested) != 0){    #If there are trees harvested
    for (i in 1:nrow(harvested)){    #For each harvested tree
      #Creates a buffer rectangle from the tree to the road.
      rectangle <- matrix(c(harvested$COORD.X[i]+half.width.skidder, harvested$COORD.Y[i] - w.dist,
                            harvested$COORD.X[i]-half.width.skidder, 0,
                            harvested$COORD.X[i]+half.width.skidder, 0,
                            harvested$COORD.X[i]-half.width.skidder, harvested$COORD.Y[i] - w.dist),
                          ncol = 2, byrow = TRUE)
      #Check if some trees are inside the rectangle and add them to the total list of trees that will be killed
      #in.out from mgcv package
      inside <- inside | in.out(bnd = rectangle, x = matrix(data = c(forest$COORD.X, forest$COORD.Y), ncol = 2))
    }
  
  #only cutting those that are inside the rctantgle and are small trees  
  inside.small <- inside & (forest$DBH < 20)
    
  }

  #remove trees that were inside rectangles
  return(inside.small)
}