#data <- read.csv(paste(data_dir,'cell_measurements.csv',sep = '/'),stringsAsFactors = FALSE);
#dataLocations <- read.csv(paste(data_dir,'cell_locations.csv',sep = '/'));



plotLocationMap <- function(pDataWithLocs, city="Amsterdam",pColor="blue")
{
  
  library(ggmap)
  
  if (!file.exists(paste(data_dir,'CityMap.RData',sep='/')))
  {
  CenterOfMap <- geocode(city)
  CityMap <- get_map(c(lon = CenterOfMap$lon, lat = CenterOfMap$lat),zoom = 15, maptype = "terrain", source = "google")
  save(CityMap,file=paste(data_dir,'CityMap.RData',sep='/'))
  } else {
    load(paste(data_dir,'CityMap.RData',sep='/'))
  }
  
  ggmap(CityMap) +
    geom_point(data = pDataWithLocs, aes(x = trueServingCoverageArea_longitude, y = trueServingCoverageArea_latitude), color = pColor, size = 2)
  
}

plotScatterMatrix <- function(data) {
  
 # plotmatrix(with(data, data.frame(Max_RRC_UE_fill_ratio,Max_RRC_users_HW_fill_ratio, Avg_UE_with_Data_in_Buffer_UL, Avg_UE_per_TTI_UL_fill_ratio,Avg_PRB_usage_per_TTI_DL)))
  ggpairs(data[,4:8], 
          lower = list(continuous = wrap("points", alpha = 0.3,    size=0.1), 
                       combo = wrap("dot", alpha = 0.4,            size=0.2) ),
          upper = "blank") + theme_grey(base_size = 9)

}

combineDataset <- function(data,dataLocations)
{
  new_DataLoc <- dataLocations;
  new_DataLoc$ECI <- 2**8 * new_DataLoc$enbId_rncId + new_DataLoc$cellId;
  
  dataCombined <- inner_join(data,new_DataLoc,by=c('Cell_ID' = 'ECI'))
  return(dataCombined)
}

detectCellsWithProblems <- function(dataCombined,pCutOffLic = 0.8, pCutOffHW =85)
{
  cat("Running with: ",pCutOffLic, " and ",pCutOffHW)
  cells.issues.licenses <- dataCombined[dataCombined$Max_RRC_UE_fill_ratio >= pCutOffLic,];
  cells.issues.licenses <- data.frame(cells.issues.licenses %>% group_by(Cell_ID, trueServingCoverageArea_latitude, trueServingCoverageArea_longitude) %>% summarise(event_counts=n()));
  cells.issues.licenses$TYPE <- 'CONNECTED_USER_LICENSE';
  cells.issues.licenses <- cells.issues.licenses[cells.issues.licenses$event_counts >= 2,];
  
  cells.issues.hw <- dataCombined[dataCombined$Avg_PRB_usage_per_TTI_DL >= pCutOffHW | dataCombined$Avg_PRB_usage_per_TTI_UL >= pCutOffHW,];
  cells.issues.hw <- data.frame(cells.issues.hw %>% group_by(Cell_ID, trueServingCoverageArea_latitude, trueServingCoverageArea_longitude) %>% summarise(event_counts=n()));
  cells.issues.hw$TYPE <- 'HW_FEATURES';
  cells.issues.hw <- cells.issues.hw[cells.issues.hw$event_counts >= 5,];
  
  issues.all <- rbind(cells.issues.licenses,cells.issues.hw)
  
  return(issues.all)
  
}

plotProblemsOnMap <- function(pIssues, city="Amsterdam")
{
  
  library(ggmap)
  
  if (!file.exists(paste(data_dir,'CityMap.RData',sep='/')))
  {
    CenterOfMap <- geocode(city)
    CityMap <- get_map(c(lon = CenterOfMap$lon, lat = CenterOfMap$lat),zoom = 12, maptype = "terrain", source = "google")
    save(CityMap,file=paste(data_dir,'CityMap.RData',sep='/'))
  } else {
    load(paste(data_dir,'CityMap.RData',sep='/'))
  }
  
  ggmap(CityMap) +
    geom_point(data = pIssues, aes(x = trueServingCoverageArea_longitude, y = trueServingCoverageArea_latitude, color = TYPE),size = 5) +
  scale_color_manual(values=c("red", "purple"))
  
}

plotLBOnMap <- function(pLB, city="Amsterdam")
{
  library(ggmap)
  
  if (!file.exists(paste(data_dir,'CityMap.RData',sep='/')))
  {
    CenterOfMap <- geocode(city)
    CityMap <- get_map(c(lon = CenterOfMap$lon, lat = CenterOfMap$lat),zoom = 12, maptype = "terrain", source = "google")
    save(CityMap,file=paste(data_dir,'CityMap.RData',sep='/'))
  } else {
    load(paste(data_dir,'CityMap.RData',sep='/'))
  }
  
  ggmap(CityMap) +
    geom_point(data = pLB, aes(x = trueServingCoverageArea_longitude, y = trueServingCoverageArea_latitude, color = TYPE),size = 3) +
    scale_color_manual(values=c("red", "green"))
  
}


findNearestCells <- function(dataLocations,pLat, pLong, noOfCells = 3)
{
  library(RANN)
  
  #point <- data.frame(qq[1,]$trueServingCoverageArea_latitude,qq[1,]$trueServingCoverageArea_longitude)
  point <- data.frame(trueServingCoverageArea_latitude=pLat,trueServingCoverageArea_longitude=pLong,stringsAsFactors = FALSE)
  allLocs <- subset(dataLocations,select=c('trueServingCoverageArea_latitude','trueServingCoverageArea_longitude'))
  allLocs <- allLocs[!(allLocs$trueServingCoverageArea_latitude == pLat & allLocs$trueServingCoverageArea_longitude == pLong),];
  list_of_near_cells <- nn2(data = allLocs, query = point, k = noOfCells)$nn.idx;
  
  res <- data.frame(enbId_rncId=dataLocations[list_of_near_cells,]$enbId_rncId, Cell_ID=dataLocations[list_of_near_cells,]$cellId,trueServingCoverageArea_latitude=dataLocations[list_of_near_cells,]$trueServingCoverageArea_latitude,
                    trueServingCoverageArea_longitude=dataLocations[list_of_near_cells,]$trueServingCoverageArea_longitude,stringsAsFactors = FALSE);
  return(res)
}
