0#' FOREST RANDOMIZER
#'
#' Creates a simulated forest based on the initial conditions of the INFyS data
#' It uses INFyS data to randomize the tree abundances.
#'
#' @param ROTATIONYEARS how many cutting areas are in the forest. There will be this number of 1600 m2 plots
#' @param seed for randomized plot selection
#'
#' @references
#' 
#' @return dataframe with ACA, SPECIES.CODE, DBH, HEIGHT, UNDER.BOSQUETE, COORD.X, COORD.Y, CARBON
#'
#' @examples
#' forest <- forest.randomizerINFyS()
#' head(forest)
forest.randomizerINFyS <- function(ROTATIONYEARS = 1, seed = NA){
  
  if (!is.na(seed)){
    set.seed(seed)
  }
  
  #Sequence from 1 to X (the number of plots of INFyS)
  unique.sitio.seq <- INFyS2009$UNIQUE.SITIO %>% unique %>% seq_along
  
  #randomizing the selected plots 
  unique.sitio.random <- unique.sitio.seq %>% sample(size = ROTATIONYEARS)

  forest <- INFyS2009 %>% 
    filter(UNIQUE.SITIO %in% c(unique.sitio.random)) %>% 
    rename(ACA = UNIQUE.SITIO) %>% 
    mutate(ACA = ACA %>% as.factor %>% as.numeric) %>% 
    select(ACA, SPECIES.CODE, DBH, HEIGHT, COORD.X, COORD.Y)
  
  #Estimate tree carbon
  forest$MgC <- get.MgC(forest)
  forest$UNDER.BOSQUETE <- FALSE

  return(forest)
}
