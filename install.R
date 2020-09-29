requirements = c( "DBI",
                    "RPostgres",
                    "dplyr",
                    "DT",
                    "shiny",
                    "shinyjs",
                    "shinythemes",
                    "tidyr",
                    "lubridate",
                    "purrr",
                    "stringr",
                    "plotly",
                    "shinyFiles","knitr",
                    "kableExtra",
                    "brew",
                    "readr",
                    "rmarkdown",
                    "pander",
                    "dplR");
                    
# Via https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
check_or_install <- function(x){
  for( i in x ){
    print(paste('Checking:', i))
    
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      print(paste('Installing:', i))
      #  If package was not able to be loaded then re-install
      # install.packages( i , dependencies = TRUE ) # dependencies = TRUE does not work in workspace
      install.packages( i )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

#  Then try/install packages...
check_or_install(requirements)