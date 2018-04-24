#' HARVEST FUNCTION
#'
#' Returns a list of the harvested trees depending on the chosen intensity, minimum harvest diameters and ACA.
#' There is a limit of 7m3 per hectare
#'
#' @param stand The table of trees in the stand
#' @param VOLUME.HA.intensity Logging intensity. Cubic meters per hectare inside ACA. Default to 7
#' @param ACA. Current ACA for logging
#' @param coord.y.accessed Limit of accessed Y coordinate
#' @param area Area of the current ACA. Default to 1600 for INFyS
#'
#' @references Ellis et al. RIL-C Mexico
#' 
#' @return TRUE/FALSE array of harvested trees
#'
#' @examples
#' forest <- forest.randomizerINFyS(ROTATIONYEARS = 1)
#' harvested <- get.harvest(forest = forest, ACA. = 1)
#' forest[harvested,]
get.harvest <- function(forest, VOLUME.HA.intensity = 7, ACA. = 1, coord.y.accessed = 70, area = 1600){

  CF <- 10000/area 
  
  #adding a column with a harvestable list depending on minimum diameters and ACA
  forest <-  forest %>% mutate(
    REACHABLE = COORD.Y <= coord.y.accessed,
    HARVESTABLE = ((DBH > 55 & SPECIES.CODE == 'SWMA') | (DBH > 35 & SPECIES.CODE != 'SWMA')) & (ACA == ACA.) & (REACHABLE),
    VOLUME.HA = CF * get.volume(forest = forest) 
  )
  
  #TRUE/FALSE which of the harvestable trees are the n.harvestable biggest trees
  biggest.harvested <- which(forest$HARVESTABLE)[forest[forest$HARVESTABLE,]$VOLUME.HA %>% order(decreasing = TRUE)]
  
  #Only selecting the biggest trees up to 7 m3 of volume cumulative. 
  biggest.harvested <- biggest.harvested[cumsum(forest$VOLUME.HA[biggest.harvested]) < VOLUME.HA.intensity ]
  
  #TRUE/FALSE. Getting bool list of the trees to be harvested
  harvested <- rownames(forest) %in% biggest.harvested

  return(harvested)
}