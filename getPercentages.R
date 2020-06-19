library(dplyr)
library(data.table)
library(tidyverse)
getPercentages <- function(){
  
  data <- read.csv('brainobrainResults.csv')
  
  centreNames <- unique(data$Centre.Name)
  
  newdata <- data.frame("Result"=character(),"Number"=character(),"Percentage"=numeric())
  
  for (i in centreNames){
    
    newdata <- newdata %>% add_row(Result = i, Number = "***********", Percentage = 11111111)
    
    dataTemp <- filter(data,data$Centre.Name==i)
    dataTemp <- as.data.table(dataTemp)
    resultTemp <- dataTemp[ , .N, by="Result"]
    resultTemp <- resultTemp %>% mutate(Per = round(N /sum(resultTemp$N)*100,digits = 2))
    colnames(resultTemp) <- c("Result","Number","Percentage")
    newdata <- rbind(newdata,resultTemp)
    
    
  }
  
  write_csv(newdata,paste(getwd(),'/FinalPercentages.csv',sep = ""))
  
}