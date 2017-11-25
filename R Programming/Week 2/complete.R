complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers to be used
    
    ## Return a data frame of the form:
    ## id  nobs
    ## 1   117
    ## 2   1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the number 
    ## of complete cases
    
    complete <- data.frame("id"=id,"nobs"=rep(0,length(id)))
    
    for (file in list.files(directory)) {
        data <- read.csv(paste(directory,file,sep="/"))
        good <- data$ID[complete.cases(data)]
        for (i in id) {
            complete$nobs[id==i] <- complete$nobs[id==i] + sum(good[good==i])/i
        }
    }
    
    complete
}