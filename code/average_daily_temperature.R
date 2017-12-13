library(tidyverse)

average_daily_temperature <- function(city) {
    dictionary <- read_table("http://academic.udayton.edu/kissock/http/Weather/citywbanwmo.txt", 
                             col_names = FALSE, 
                             skip = 3)
    names(dictionary) <- c("City", "Filename", "OldCode", "NewCode")
    
    fname <- dictionary$Filename[dictionary$City == toupper(city)]
    
    flink <- paste0("http://academic.udayton.edu/kissock/http/Weather/gsod95-current/", fname, ".txt")
    mytbl <- read_table(flink, col_names = FALSE)
    names(mytbl) <- c("Month", "Day", "Year", "Temperature")
    
    return(mytbl)
}

average_daily_temperature("Seattle")
average_daily_temperature("Des Moines")
