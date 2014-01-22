complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # function to read monitor data and return a data frame
    getmonitor <- function(directory, mon){
        mon.name <- paste0(sprintf('%03d', mon), '.csv')
        mon.path <- file.path(directory, mon.name)
        read.csv(mon.path)
    }
    
    # initialize empty data frame that will be returned
    result <- data.frame(id=rep(NA, length(id)),
                         nobs=rep(NA, length(id)))
    
    # loop through id values to populate result
    for (i in 1:length(id)){
        dd <- getmonitor(directory, id[i])
        result[i,] <- c(id[i], sum(complete.cases(dd)))
    }
    
    return(result)
}


