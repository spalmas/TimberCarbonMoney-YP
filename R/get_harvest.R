#' HARVEST FUNCTION
#'
#' Returns a list of the harvested trees depending on the chosen intensity, minimum harvest diameters and ACA
#'
#' @param stand The table of trees in the stand
#' @param intensity Logging intensity. Cubic meters per hectare inside ACA
#' @param ACA. Current ACA for logging
#' @param coord.y.accessed Limit of accessed Y coordinate
#'
#' @references Ellis et al. RIL-C Mexico
#' 
#' @return TRUE/FALSE array of harvested trees
#'
#' @examples
#' forest <- forest.randomizer(ROTATIONYEARS = 1)
#' harvested.list <- get.harvest(forest = forest, intensity = 5, ACA. = 0)
#' forest[harvested.list,]
get.harvest <- function(forest, intensity = 5, ACA., coord.y.accessed = 70){

  #adding a column with a harvestable list depending on minimum diameters and ACA
  forest <-  forest %>% mutate(
    REACHABLE = COORD.Y <= coord.y.accessed,
    HARVESTABLE = ((DBH > 55 & SPECIES.CODE == 'SWMA') | (DBH > 35 & SPECIES.CODE != 'SWMA')) & (ACA == ACA.) & (REACHABLE),
    VOLUME = get.volume(forest = forest)
  )
  
  
  #TRUE/FALSE which of the harvestable trees are the n.harvestable biggest trees
  #biggest.harvested <- which(forest$HARVESTABLE)[forest[forest$HARVESTABLE,]$VOLUME %>% order(decreasing = TRUE)]
  
  
  
  #TRUE/FALSE. Getting bool list of the trees to be harvested
  harvested <- rownames(forest) %in% which(forest$HARVESTABLE)[biggest.harvested]
  
  return(harvested)
}