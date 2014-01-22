# function to read monitor data and return a data frame
getmonitor <- function(directory, mon){
    mon.name <- paste0(sprintf('%03d', mon), '.csv')
    mon.path <- file.path(directory, mon.name)
    read.csv(mon.path)
}
