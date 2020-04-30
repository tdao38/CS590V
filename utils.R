#Function to check if packages have been installed. If not, install and load them
packages <- function(x){
  for (i in x) {
    if (!require(i, character.only = TRUE )) {
      install.packages(i, dependencies = TRUE )
      require( i , character.only = TRUE )
    }
  }
}
