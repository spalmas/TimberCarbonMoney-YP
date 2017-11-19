#' Main simulator process for TCM paper
#'
#' Original simulator_full used for dissertation chapter
#' 
#'
#' @param forest.tab Forest list of trees
#' @param scenario text name of scenario
#' @param sy total simulation years
#' @param it number of iterations of the complete scenarios.
#' @param area Area of plot to be simulated in ha.
#' @param dir.felling TRUE/FALSE if directional felling is employed
#' @param enrich.bosquete The ejido performs enrichment bosquete planting (TRUE or FALSE)
#' @param intensity Logging intensity. Percentage of trees harvested from that ACA
#' @param skidder type of skidder used. 'Skidder' (Treefarmer) or 'MAT' (Modifed Agricultural Tractor)
#' @param rotation rotation period of forest. Also the number of ACA areas of one hectare
#' @param w.dist set to 25m
#'
#' @references Ninguna por ahora
#' 
#' @return a table with yearly results of harvested and biomass values
#' 
#' @examples
#' source('startup.R')
#' forest.tab <- forest.randomizer(ROTATIONYEARS = rotation)
#' table.results <- simulator(sy = 50, 
#'     it = 3,
#'     intensity = 'Highest',
#'     enrich.bosquete = FALSE, 
#'     w.dist = 0, 
#'     dir.felling  = FALSE, 
#'     forest.tab  = forest.tab,
#'     area = 1500)
#' View(table.results)
#'
simulator <- function(forest.tab,
                      scenario = 'A',
                      sy, 
                      it,
                      area = 1,   #
                      dir.felling = TRUE, 
                      enrich.bosquete = FALSE, 
                      intensity,
                      skidder = c('Skidder'),
                      rotation = 25, 
                      w.dist = 0){ 
  
  #Correction Factor to hectare
  CF = 1/area 
  
  ### TABLE TO STORE VALUES AND REPORT -----------
  table.results <- matrix(nrow = sy*it) %>% as_tibble() %>%
    mutate(SCENARIO = scenario,
           IT = rep(1:it, times = sy) %>% sort,   #column of iteration
           YEAR = rep(0:(sy-1), times = it),  #column of years
           N.HARVESTED = NA,
           VOL.HARVESTED = NA
    )
  
  
  #LOOP OF SIMULATION YEARS AND ITERATIONS-----------
  for (i in 1:it){   #For each iteration
    #i=1
    #forest.tab <- forest.randomizer(ROTATIONYEARS = rotation)  #Randomizes every year
    forest <- forest.tab  #return to initial forest
    
    #Random
    forest$ACA <- sample(forest$ACA, replace = TRUE)
    
    #AGB0 is the mean of the ACA AGB. Not by hectare
    forest.parameters <- forest %>% 
      group_by(ACA) %>% 
      summarise(BA = sum(pi * (DBH^2/40000), na.rm = TRUE),
                AGB = sum(AGB, na.rm = TRUE)) %>% 
      summarise(BA = mean(BA),
                AGB = mean(AGB))
    
    BA0 <- forest.parameters$BA[1]
    AGB0 <- forest.parameters$AGB[1]
    
    ACA <- 0 #reset ACA
    
    for (y in 0:(sy-1)){   #For each simulation year
      #y <- 0
      #row number based on the repetition and simulation year for table of results
      row.num <- (y + 1) + (i - 1) * sy
      
      #Get ACA from the year and rotation years
      if (ACA == rotation){ACA <- 0}
      
      ######## NATURAL MORTALITY
      natural.dead <- forest %>% mortality.calc()    #T/F list if they died of natural causes
      forest <- forest[!natural.dead,]          #removing dead from forest

      ####### GROWTH FUNCTION
      forest <- forest %>% get.diameter.growth()   #randomized diameter growth
      
      ####### REGENERATION
      regen.table <- forest %>% get.regeneration (area = area)   #Regeration process
      forest <- forest %>% bind_rows(regen.table)  #adding the new trees to the forest

      ####### DEFAULT VALUES FOR SOME VARIABLES. WILL NOT CHANGE IF THIS IS NOT A HARVESTING YEAR IN THE ROTATION.
      #emissions.harvest <- NA; emissions.skidding <- NA; emissions.directional <- NA
      
      ####### HARVESTING TREES. Since harvesting trees already occurs only at one plot,
      #harvested values are already in one hectare scale because it only cuts one ACA
      harvested.bool <- get.harvest(forest = forest, intensity = intensity, ACA. = ACA) #harvesting the forest and store harvested trees
      harvested <- forest[harvested.bool,]   #Getting a harvested tree list
      harvested$VOLUME <- (harvested %>% get.volume())     #Get total volume harvested. Not by hectare. Transformed below
      #harvested$PRICE <- harvested %>% get.price()       #Assigning price to each tree
      
      #Adding result numbers to table of results.
      #table.results$INCOME[row.num] <- sum(harvested$PRICE, na.rm = TRUE)  #All income from trees harvested in that year
      
      ####### DO SKIDDING MORTALITY
      #Should be in hectare basis because harvested only comes from one ACA
      skidding.dead.bool <- skidding.mortality(forest = forest, w.dist = w.dist, harvested = harvested)  ##kills inside area and small trees (< 20cm DBH)
      
      ####### HAVE DIRECTIONAL MORTALITY OF dir.felling is FALSE
      directional.dead.bool <- directional.mortality(forest = forest, harvested = harvested, dir.felling = dir.felling)    #directinonal felling mortality
      
      #Removing all killed trees from the forest
      forest <- forest[!(harvested.bool | skidding.dead.bool | directional.dead.bool),]
      
      # Final emissions
      #emissions.operations <- forest[directional.dead.bool | skidding.dead.bool | harvested.bool,]$AGB %>% sum(na.rm = TRUE) # directional felling mortality emissions
      
      ####### DO ENRICHMENT PLANTING
      if (enrich.bosquete){
        enrichment.table <- do.enrichment(harvested = harvested, ACA. = ACA, area)
        forest <- forest %>% bind_rows(enrichment.table)  #adding the new trees to the stand
      }
      
      ####### BIOMASS PER HECTARE (AT THE END OF MORTALITY, RECRUITMENT AND HARVEST)
      forest$AGB <- get.agb(forest = forest) #a new estiamate of biomass. Not by hectare
      
      ####### computing final forest values. Unit: area. These are NOT corrected to HA
      forest.parameters <- forest %>% group_by(ACA) %>% 
        summarise(BA = sum(pi * (DBH^2/40000), na.rm = TRUE),
                  AGB = sum(AGB, na.rm = TRUE)) %>% 
        summarise(BA = mean(BA),
                  AGB = mean(AGB))
      
      ####### Storing stand results. They should by hectare units
      table.results[row.num, 'N.HARVESTED'] <- nrow(harvested) * CF  #Number of extracted trees
      table.results[row.num, 'VOL.HARVESTED'] <- sum(harvested$VOLUME, na.rm = TRUE) * CF  #All volume extacted in that year
      table.results[row.num, 'BA0'] <- BA0 * CF  # by hectare
      table.results[row.num, 'AGB0'] <- AGB0 * CF #by hectare
      table.results[row.num, 'BA1'] <- forest.parameters$BA[1] * CF  #Mean of ACA values by hectare
      table.results[row.num, 'AGB1'] <- forest.parameters$AGB[1] * CF #by hectare
      table.results[row.num, 'EMISSIONS'] <- emissions.operations * CF  #Estimate biomass from the stand harvest by hectare
      #table.results$N.TREES.DEAD[row.num] <- CF * n.trees.dead/rotation  #Total number of deaths in year by hectare

      ####### UPDATING VALUES
      BA0 <- forest.parameters$BA[1]
      AGB0 <- forest.parameters$AGB[1]
      ACA <- ACA + 1 
    }  
  }
  
  #Estimate the total Emissions per hectare for each year
  table.results <- table.results %>% mutate(
    #EMISSIONSperm3 = EMISSIONS / VOL.HARVESTED,
    D.BA = BA1-BA0, #Estimate agb BALANCE
    D.AGB = AGB1 - AGB0 #Estimate agb BALANCE
  )
  
  #Return table of results
  return(table.results)
} 