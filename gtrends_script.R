###load packages
library(gtrendsR)
library(lubridate)
library(stats)
library(tidyr)
library(DataCombine)

#Predictive R Squared function
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

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
