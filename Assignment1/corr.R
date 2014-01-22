corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    # import complete() and getmonitor() functions
    source('getmonitorids.R')
    source('complete.R')
    source('getmonitor.R')
    
    # get list of all possible ids
    mon.list <- getmonitorids(directory)
    
    # get list of monitor ids where complete cases >= threshold
    mon.complete <- complete(directory, id=mon.list)
    elig.ids <- mon.complete$id[which(mon.complete$nobs >= threshold)]
     
    # start numeric vector of correlations
    result <- numeric(0)
    
    # loop over eligible monitor ids to complete result
    for (mon in elig.ids){
        dd <- getmonitor(directory, mon)
        corr.t <- cor(dd$sulfate, dd$nitrate, use='pairwise.complete.obs')
        result <- append(result, corr.t)
    }

    return(result)
}



