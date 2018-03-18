# Load required libraries (please install them with install.packages()  if missing )
library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(DT)
library(ggplot2)
library(GGally)
library(dplyr)
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)


# Define variables for having the datasat globally
assign("data", NULL, envir = .GlobalEnv);
assign("dataLocations", NULL, envir = .GlobalEnv);
assign("dataCombined", NULL, envir = .GlobalEnv);
assign("FlagDataLoaded", FALSE, envir = .GlobalEnv);
assign("serviceInstance", FALSE, envir = .GlobalEnv);



getAnalysisMenu <- function(pDataLoad = FALSE, pDataClean= FALSE)
{
  if (pDataLoad == FALSE)
  {
    result <- 
      sidebarMenu(
      #menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Loading Dataset", tabName = "DataLoad", icon = icon("database"))
    )
  } else {
  result <- 
    sidebarMenu(
      menuItem("Dataset", tabName = "Dataset", icon = icon("database")),
      menuItem("Explore", tabName = "DataExplore", icon = icon("wpexplorer")),
      menuItem("Data Matching/Joining", tabName = "DataClean", icon = icon("database")),
      menuItem("Cell Location Data", tabName = "CellLocations", icon = icon("globe")),
      menuItem("Capacity Analysis", tabName = "CapacityAnalysis", icon = icon("area-chart")),
      menuItem("Load Balancing Candidates", tabName = "LoadBalance", icon = icon("balance-scale")),
      menuItem("API / Micro-service", tabName = "API", icon = icon("plug"))
    )
  }
  
  
  return(result)
}

server <- function(input, output,session) {
  
  ## Menu rendering logic
  if (FlagDataLoaded == TRUE)
  {
      output$menu <- renderMenu({getAnalysisMenu(pDataLoad=TRUE)})
  } else {
    output$menu <- renderMenu({getAnalysisMenu(pDataLoad=FALSE)})
  }
  
  # Function that dynamically updates our dropdownlists
  
  observe({
    
    if (input$tab == 'DataExplore')
    {
    
    ListOfVars <- names(data)
    ListOfVars <- ListOfVars[!ListOfVars %in% c('Datetime','Operator','Cell_ID')];
    
    ListOfCellsForAnom <- sort(unique(data$Cell_ID))
    
    
    
    
    #updateSliderInput(session, "intNoCorrVars",max = ncol(data));
    
      if (length(input$selectVarNameForHist) >0 && !input$selectVarNameForHist == '')
      {
        cat("Running for: '",input$selectVarNameForHist,"'")
        output$HistPlot <- renderPlot({ hist(data[,input$selectVarNameForHist],xlab=input$selectVarNameForHist,
                                             main=paste('Histogram of',input$selectVarNameForHist)) })
        
        output$BoxPlot <- renderPlot({ boxplot(data[,input$selectVarNameForHist],xlab=input$selectVarNameForHist,
                                             horizontal = TRUE) })
        
        output$TSPlot <- renderPlot({ 
          plot(data[,input$selectVarNameForHist] ~ data$Datetime,type='h',col = "cornflowerblue",
                                           ylab=input$selectVarNameForHist,xlab='Datetime')
          })
        
        
        if (length(input$selectCellForAnom) >0 && !input$selectCellForAnom == '')
        {
          output$AnomalyPlot <- renderPlot({ 
              AnomalyDetectionVec(data[data$Cell_ID == input$selectCellForAnom,]$Max_RRC_UE_fill_ratio, max_anoms=0.1, period=24*3, direction='pos', alpha=0.025,only_last=FALSE, plot=TRUE)
            
          })
          
        } else {
          updateSelectInput(session, "selectCellForAnom", choices = ListOfCellsForAnom);  
        }
        
        output$Out_summary <- renderPrint({ summary(data)  })
      } else {
        updateSelectInput(session, "selectVarNameForHist", choices = ListOfVars);  
      }
    }
    
    if (input$tab == 'CapacityAnalysis')
    {
    
      output$dtCellsWithProblems <- renderDataTable({ detectCellsWithProblems(dataCombined,pCutOffLic = input$cutoffLic, pCutOffHW = input$cutoffHW) });
      
      output$CellIssueLocPlot = renderPlot({
        cat("Render locations plot");
        plotProblemsOnMap(detectCellsWithProblems(dataCombined,pCutOffLic = input$cutoffLic, pCutOffHW = input$cutoffHW))
      })
      
    }
    
    if (input$tab == 'LoadBalance')
    {
      HW_Targets <- detectCellsWithProblems(dataCombined);
      HW_Targets <- HW_Targets[HW_Targets$TYPE=='HW_FEATURES',];
      
      shoppinglist <- rbindlist(lapply(seq(1:nrow(HW_Targets)),function(x) { 
        findNearestCells(dataLocations,HW_Targets[x,]$trueServingCoverageArea_latitude,
                         HW_Targets[x,]$trueServingCoverageArea_longitude)
        }))
      
      shoppinglist$SKU_ARTICLE <- 'LB_SW_FEATURE';
      shoppinglist$ECI <- 2**8 * shoppinglist$enbId_rncId + shoppinglist$Cell_ID;
      
      output$dtShoppingList <- renderDataTable({ subset(shoppinglist,select=c('enbId_rncId','Cell_ID','ECI','SKU_ARTICLE')) })
      
      list1 <- data.frame(trueServingCoverageArea_latitude=HW_Targets$trueServingCoverageArea_latitude,
                          trueServingCoverageArea_longitude=HW_Targets$trueServingCoverageArea_longitude,
                          TYPE='HW_ISSUE');
      
      list2 <- data.frame(trueServingCoverageArea_latitude=shoppinglist$trueServingCoverageArea_latitude,
                          trueServingCoverageArea_longitude=shoppinglist$trueServingCoverageArea_longitude,
                          TYPE='LB_CANDIDATE');
      
      list_combined <- rbind(list1,list2);
      
      output$LoadBalanceLocPlot <- renderPlot({
        cat("Render locations plot");
        plotLBOnMap(list_combined)
      })
      
    }
    
  })
  
  observeEvent(input$btnRenderScatter, {
    output$CapacityScatterPlot <- renderPlot({ plotScatterMatrix(sample_n(data,size=input$maxSamples)) });
  })
  
  observeEvent(input$btnStartService, {
    serviceInstance <<- startMyService();
  })
  
  observeEvent(input$btnStopService, {
    stop_daemon(serviceInstance)
  })
  
  observeEvent(input$btnLoadHTTPS, {
    
    
    
    data <<- read.csv(url(input$dataHttpsURL),stringsAsFactor = FALSE);
    dataLocations <<- read.csv(paste(data_dir,'cell_locations.csv',sep = '/'),stringsAsFactor = FALSE);
    
    data$Datetime <<- as.POSIXct(data$Datetime)
    
    dataCombined <<- combineDataset(data,dataLocations)
    
    FlagDataLoaded <<- TRUE;
    output$menu <- renderMenu({getAnalysisMenu(pDataLoad=TRUE)})
    
    cat("HTTPS load done!")
    
  })
  
  
  observeEvent(input$btnLoadLocal, {
    
    
    
    data <<- read.csv(paste(data_dir,'cell_measurements.csv',sep = '/'),stringsAsFactor = FALSE);
    dataLocations <<- read.csv(paste(data_dir,'cell_locations.csv',sep = '/'),stringsAsFactor = FALSE);
    
    data$Datetime <<- as.POSIXct(data$Datetime)
    
    dataCombined <<- combineDataset(data,dataLocations)
    
    FlagDataLoaded <<- TRUE;
    output$menu <- renderMenu({getAnalysisMenu(pDataLoad=TRUE)})
    
    cat("Local load click!")
 
  })
  
  observe({
  
    # Logic for rendering the data table output
    if (input$tab == 'Dataset')
    {
        output$dtViewDataset = renderDataTable({
        cat("Render data table");
        datatable(data, options = list(scrollX = TRUE,pageLength = 10))
        
      })
      
        output$dtViewDatasetLoc = renderDataTable({
          cat("Render data table");
          datatable(dataLocations, options = list(scrollX = TRUE,pageLength = 10))
        })
        
    }
    
    # Logic for rendering the data table output
    if (input$tab == 'DataClean')
    {
      output$dtViewMeasureKey = renderDataTable({
        cat("Render data table");
        datatable(subset(data,select=c('Datetime','Cell_ID','Max_RRC_UE_fill_ratio')), options = list(scrollX = TRUE,pageLength = 10))
        
      })
      
      output$dtViewLocationKey = renderDataTable({
        cat("Render data table");
        datatable(subset(dataLocations,select=c('enbId_rncId','cellId', 'trueServingCoverageArea_latitude','trueServingCoverageArea_longitude')), options = list(scrollX = TRUE,pageLength = 10))
      })
      
      output$dtCombined = renderDataTable({
        cat("Render data table");
        out <- subset(dataCombined,select=c('Datetime',"enbId_rncId","cellId",'Cell_ID',"trueServingCoverageArea_latitude","trueServingCoverageArea_longitude","Max_RRC_UE_fill_ratio","Max_RRC_users_HW_fill_ratio","Avg_UE_with_Data_in_Buffer_UL","Avg_UE_per_TTI_UL_fill_ratio","Avg_PRB_usage_per_TTI_DL","Avg_PRB_usage_per_TTI_UL"));
        colnames(out) <- c('Datetime','enbId_rncId' ,'cellId' ,'ECI','Latitude','Longitude','Max_RRC_UE_fill_ratio','Max_RRC_users_HW_fill_ratio','Avg_UE_with_Data_in_Buffer_UL','Avg_UE_per_TTI_UL_fill_ratio','Avg_PRB_usage_per_TTI_DL','Avg_PRB_usage_per_TTI_UL');
        datatable(out, options = list(scrollX = TRUE,pageLength = 10))
      })
      
    }
    
    
    # Logic for rendering the data table output
    if (input$tab == 'CellLocations')
    {
      output$CellLocationsPlot = renderPlot({
        cat("Render locations plot");
        plotLocationMap(pDataWithLocs = dataLocations)
      })
    }
        
  
  })
  
}
