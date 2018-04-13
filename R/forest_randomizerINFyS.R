0#' FOREST RANDOMIZER
#'
#' Creates a simulated forest based on the initial conditions of the INFyS data
#' It uses INFyS data to randomize the tree abundances.
#'
#' @param ROTATIONYEARS how many cutting areas are in the forest. There will be this number of 1600 m2 plots
#'
#' @references
#' 
#' @return dataframe with ACA, SPECIES.CODE, DBH, HEIGHT, UNDER.BOSQUETE, COORD.X, COORD.Y, CARBON
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizerINFyS()
#' head(forest)
forest.randomizerINFyS <- function(ROTATIONYEARS = 1){
  
  #Sequence from 1 to X (the number of plots of INFyS)
  unique.sitio.seq <- INFyS2009$UNIQUE.SITIO %>% unique %>% seq_along
  
  #randomizing the selected plots 
  unique.sitio.random <- unique.sitio.seq %>% sample(size = ROTATIONYEARS)
  
  forest <- INFyS2009 %>% 
    filter(UNIQUE.SITIO %in% c(unique.sitio.random)) %>% 
    mutate(UNIQUE.SITIO %>% as.factor %>% as.numeric) %>% 
    rename(ACA = UNIQUE.SITIO) %>% 
    select(ACA, SPECIES.CODE, DBH, HEIGHT, COORD.X, COORD.Y)

  #Estimate tree carbon
  forest$MgC <- get.MgC(forest)
  forest$UNDER.BOSQUETE <- FALSE

  return(forest)
}
