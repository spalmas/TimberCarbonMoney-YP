# Function to estimate height from a list of diameters
# It randomizes the height instead of having a closed solution
get.height <- function(DBH){
  #Height allometric equation from ?
  HEIGHT <- exp(0.93687 + 0.55204*log(DBH)) + rnorm(n = length(DBH), mean = 0, sd = 0.5)
  HEIGHT[HEIGHT < 0.5] <- 0.5
  return(HEIGHT)
}

