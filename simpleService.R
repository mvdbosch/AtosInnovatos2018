##########################
# File: 099BMASimpleService.R
#
# Date: 11-Aug-2017
# Author: Marcel van den Bosch <marcel.vandenbosch@atos.net>
# 
# Description: This demonstrator script uses jug to expose functionality as a REST API
##########################

##source('InitEnvironment.R')
##source('functions.R')


##data <- read.csv(paste(data_dir,'cell_measurements.csv',sep = '/'),stringsAsFactors = FALSE);
##dataLocations <- read.csv(paste(data_dir,'cell_locations.csv',sep = '/'));

##dataCombined <- combineDataset(data,dataLocations);



library(jsonlite)

testHelloWorld <- function()
{
  return("Hello World!");
}

detectCapacityIssues <- function()
{
  
  return(toJSON(detectCellsWithProblems(dataCombined)))  
}

getShoppingList <- function()
{
  
  HW_Targets <- detectCellsWithProblems(dataCombined);
  HW_Targets <- HW_Targets[HW_Targets$TYPE=='HW_FEATURES',];
  
  
  shoppinglist <- rbindlist(lapply(seq(1:nrow(HW_Targets)),function(x) { 
    findNearestCells(dataLocations,HW_Targets[x,]$trueServingCoverageArea_latitude,
                     HW_Targets[x,]$trueServingCoverageArea_longitude)
  }))
  
  shoppinglist$SKU_ARTICLE <- 'LB_SW_FEATURE';
  shoppinglist$ECI <- 2**8 * shoppinglist$enbId_rncId + shoppinglist$Cell_ID;
  
  return(toJSON(subset(shoppinglist,select=c('enbId_rncId','Cell_ID','ECI','SKU_ARTICLE'))))
  

}


library(jug)

startMyService <- function()
{
  return(jug() %>%
    get("/detectCapacityIssues", decorate(detectCapacityIssues,content_type = "application/json")) %>%
    get("/getShoppingList", decorate(getShoppingList,content_type = "application/json"))  %>%
    get("/test", decorate(testHelloWorld)) %>%
    simple_error_handler() %>%
    serve_it(host = "0.0.0.0", port = 8080, daemonized = TRUE))
  
}


