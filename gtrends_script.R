##These functions are useful for getting data from Google Trends.

##There are a couple of issues getting lots of data from Google Trends. If you get data from more than 5 years at a time it only provides monthly data instead of weekly data.  You can run multiple queires to get around this, but for some reason the data doesn't line up perfectly.  This script gets around that by running multiple overlapping queries then using the least squares method on the overlap to find a reasonably good compromise to the descrepancies, before reiterating this process to the beginning of the specified time frame.
##This script is specifically for getting weekly data, but could be fairly easily adapted to get daily values instead.
##The optimise() function used here is probably a bit overkill for the least squares method.  Finding the root of the derivative would probably make more sense to make this script a bit more efficient.

###load packages
library(gtrendsR)
library(lubridate)
library(stats)
library(tidyr)
library(DataCombine)

#This function replaces "<1" with "0" then coerces the values to Date or Integer before finally pivoting the data.
CleanReshape <- function(variable.df) {
  variable.df <- FindReplace(variable.df, "hits", data.frame(from = "<1", to = "0"), from = "from", to = "to", exact = TRUE)      #replaces <1 with 0
  variable.df$hits <- as.integer(variable.df$hits)                                                                  #coerces values to integers
  wide <- spread(variable.df[,1:3], keyword, hits)                                                                  #pivots the data
  return(wide)          
}


##A function that knits together weekly Google Trends data from multiple, overlapping 5-year periods.
get_weekly_gtrends <- function(keyword,geo,start,end) {
  ###Initialize variables
  global.start <- as.Date(start)
  local.end <- as.Date(end)
  local.start <- max(c(local.end %m-% years(5),global.start))
  time <- paste(local.start,local.end)

  #Make first query and initialize rolling dataframe
  rolling.df <- gtrends(keyword = keyword, geo = geo, time = time, onlyInterest = TRUE)$interest_over_time[,1:2]
  names(rolling.df)[names(rolling.df) == 'hits'] <- 'hits.x'                    #Rename hits column preemptively
  
  repeat{  #Begin loop
    local.end <- local.end %m-% years(4)
    local.start <- max(c(local.start %m-% years(4),global.start))
    time <- paste(local.start,local.end)                                        #Back up by 4 years (1 year overlap with previous query)
    df2 <- gtrends(keyword = keyword, geo = geo, time = time, onlyInterest = TRUE)$interest_over_time[,1:2]
    #FIXME: Replace every "<1" with "0" and coerce "hits" column to type integer
    rolling.df <- merge(x = rolling.df[,1:2], y = df2, by = "date", all = TRUE) #Performs outer join on data frames.
    
    union <- rolling.df[complete.cases(rolling.df),]
    scale <- function(x){
      sum((union$hits.x - x * union$hits)^2)                                    #Sum of squares
    }
    scalar <- stats::optimize(scale,c(0,2),maximum = FALSE)$minimum             #Least squares optimization
    rolling.df[complete.cases(rolling.df),]$hits.x <- round(rowMeans(data.frame(union$hits.x,scalar*union$hits))) #Averages and rounds the overlapping values
    rolling.df$hits.x <- ifelse(is.na(rolling.df$hits.x), round(scalar*rolling.df$hits), rolling.df$hits.x)
    
    if(local.start == global.start){                                            #Check to see if we've reached the start date before looping
      break
    }
  }
  
  names(rolling.df)[names(rolling.df) == 'hits.x'] <- keyword                   #Rename the hits column
  rolling.df[,2]<- 100*rolling.df[,2]/max(rolling.df[,2])
  return(rolling.df[,1:2])
}

#Example:
#concerts <- get_weekly_gtrends("concerts","US","2004-01-01","2022-01-01")
