#' Smalian's volume
#'
#' \code{get.volume} estimates volume for each tree in the given stand using Smalian's
#'
#' @param forest table of trees with the columns: SPECIES.CODE, DBH, HEIGHT
#' 
#' @references 
#' Fonseca, M.A., 2005. The measurement of roundwood: methodologies and conversion ratios. CABI.
#' 
#' @return A list of volumes for each tree. This can be added as another column in the table of trees.
#' 
#' @examples
#' forest <- forest.randomizer(ROTATIONYEARS = 1)
#' get.volume(forest)
get.volume <- function(forest){
  
  #dataframe of species code names and its prices. In a good simulation model,
  #this should be allowed to be input by the user. Maybe later
  #Smalian's=((pi()*R1^2+pi()*R2^2)/2)*H1
  
  #Adding a column for VOL.FORMULA to the forest table
  forest <- inner_join(forest, y = species[,c('SPECIES.CODE', 'VOL.FORMULA')], by = 'SPECIES.CODE')
  
  #The equations below were used for the Dissertation. They come from Alder Report
  #This equation is in form b0 + b1*DHB^2*H
  parameters <- data.frame(VOL.FORMULA = c('SWMA', 'LYLA', 'MEBR', 'MAZA', 'POUN', 'DURAS', 'BLANDAS', 'BE', 'CO', 'XXXX'),
                       B0 = c(0.01711, 0.00842, 0.00842, 0.00842, 0.00842, 0.00842, 0.01247, 0.03139, 0.07055, 0.00842),
                       B1 = c(0.000041591, 0.000050894, 0.000050894, 0.000050894, 0.000050894, 0.000050894,0.000047554, 0.000038954,0.000047705,0.000050894))

  #Calculating volume and returning list
  return(parameters$B0[ match(forest$VOL.FORMULA, parameters$VOL.FORMULA ) ] +
    parameters$B1[ match(forest$VOL.FORMULA, parameters$VOL.FORMULA ) ] * forest$DBH^2 * forest$HEIGHT)

}

