pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    
    # return mean and number of obs for poecific pollutant and mon
    # as a 2 vector c(mean, nobs)
    getMonitorMean <- function(directory, pollutant, mon){
        mon.name <- paste0(sprintf('%03d', mon), '.csv')
        mon.path <- file.path(directory, mon.name)
        dd <- read.csv(mon.path)
        mean.poll <- mean(dd[, pollutant], na.rm=TRUE)
        nobs.poll <- sum(!is.na(dd[, pollutant]))
        return(c(mean.poll, nobs.poll))
    }
    
    # start empty data frame to store means and nobs: length(id)x2
    result.df <- data.frame(mean=rep(NA, length(id)),
                            nobs=rep(NA, length(id)))
    
    # loop over id entries
    for (i in 1:length(id)){
        result.df[i, ] <- getMonitorMean(directory, pollutant, id[i])
    }
    
    # return weighted mean of all means
    sum(result.df$mean * result.df$nobs, na.rm=T)/sum(result.df$nobs)
}