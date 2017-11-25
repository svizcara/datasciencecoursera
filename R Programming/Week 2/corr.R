corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all variables)
    ## required to compute the correlation between nitrate and sulfate;
    ## the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    c <- complete(directory)
    corvector <- numeric(0)
    dataFrame <- data.frame()
    
    for (file in list.files(directory)) {
        data <- read.csv(paste(directory,file,sep="/"))
        goodData <- data[complete.cases(data),]
        dataFrame <- rbind(dataFrame,goodData)
    }
    
    for (i in c$id[c$nobs > threshold] ) {
        tempData <- dataFrame[dataFrame$ID==i,]
        corvector <- c(corvector,cor(tempData$sulfate,tempData$nitrate))
    }
    corvector
}