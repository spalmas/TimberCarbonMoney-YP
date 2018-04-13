#' Enrichment process
#'
#' It receives the list of harvested trees and randomizes the number and sizes of trees planted inside that bosquete\
#' It only plants Swietenia macrophylla seedlings (SM). These are small in size but relatively homogeneous.
#'
#' @param harvested Table of harvested trees
#' @param ACA. Number of recently harvested ACA.
#' @param area Area of plot
#'
#' @references
#' Navarro-Martinez, el al. 2017

#' @return a table of seedlings inside the given ACA ready to add to the forest.
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 10)
#' forest$HARVESTED <- get.harvest(forest = forest, intensity = 'All', ACA. = 0)
#' harvested <- forest[forest$HARVESTED,]
#' get.enrichment(harvested = harvested, ACA  = 1)
get.enrichment <- function(harvested, ACA., area){
  #If the ejidos enrich their bosquetes the number of bosquet. 
  #One bosquete will be created for each 3 trees harvested

  CF <- 10000/area
  
  #Estimating the number of new seedlings
  n.seedlings <- harvested %>% nrow() %>% prod(., CF, 1/3) %>%     #one bosquete at for each 3 harvested trees by hectare
    rlnorm(n = ., mean = 0.1, sd = 0.039) %>%  #randomized area (ha), n is decimal, but it rounds as floor
    log() %>% 
    sum() %>%   #adding the area of all randomized bosquetes
    #prod(., rnorm(n = 1, mean = 2000/CF, sd = 600), .7 ) %>%    #a mean of 2000 seedlings per hectare and reducing to 70% because not all area of bosquete is used
    prod(., 2000, .7 , 1/CF) %>%    #2000 seedlings, Only 70% of bosquete is planted, only 1/CF is modeled
    round()   #integer number of new seedlings.
  
  #Something to avoid errors. It is horrible I know
  if(n.seedlings<0 | is.na(n.seedlings| !is.numeric(n.seedlings) | is.null(n.seedlings))){n.seedlings <- 0}
  
  #Creating a table with n.seedlings mahoganies
  enrich.table <- tibble('SPECIES.CODE' = rep(x = 'SWMA', times = n.seedlings)) 
  
  #adding new variables. Should be the same as forest randomizer
  enrich.table <- enrich.table %>% 
    mutate(
      ACA = ACA.,
      DBH = log(rlnorm(n = nrow(enrich.table), mean = 1, sd = .15)),
      HEIGHT = log(rlnorm(n = nrow(enrich.table) , mean = 1, sd = .15)),   #should change to allometric equations
      D.DBH = NA,#no initial growth
      UNDER.BOSQUETE = TRUE,
      COORD.X =  runif(n = nrow(enrich.table), min = 0, max = 99),
      COORD.Y =  runif(n = nrow(enrich.table), min = 0, max = 99),
      HARVESTED = FALSE
    )
  
  enrich.table$MgC = get.MgC(forest = enrich.table)
  
  #retunning a list of enrichment plants
  return(enrich.table)
  
}
