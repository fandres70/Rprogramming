# function to get list integer of all monitor ids in directory
getmonitorids <- function(directory){
    mon.list <- list.files(directory)
    for (i in 1:length(mon.list)){
        mon.list[i] <- strsplit(mon.list[i], split='.', fixed=TRUE)[[1]][1]
    }
    as.integer(mon.list)    
}
