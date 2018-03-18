# Random Cell dataset generator

library(rJava)
library(xlsx)
library(data.table)
library(sn)
library(ggplot2)

#setwd('/home/marcel/projects/Demo_CellAnalytics')

src_dir <- 'src_data';
data_dir <-'dataset'

timeline <- seq(ISOdatetime(2016,6,1,0,0,0,tz="UTC"), ISOdatetime(2016,6,15,0,0,0,tz="UTC"), by=(60*60))

cellid_location <- read.xlsx(paste(src_dir,'cells_ams.xlsx',sep='/'),sheetIndex = 1);
cellid_location <- subset(cellid_location,select=setdiff(names(cellid_location),c('country','region','city','street')))

cellid_location$ID <- seq(1:nrow(cellid_location));

## Dataset with random measurements
enodeb_id <- unique(2**8 * cellid_location$enbId_rncId + cellid_location$cellId);

# cells that we want to show a problem
suspect_ids <- sample(enodeb_id,ceiling(length(enodeb_id)*0.015),replace=FALSE)

problem_class <- rbinom(length(suspect_ids), 1, 0.5)

suspect_ids.lic <- suspect_ids[problem_class]
suspect_ids.hw <- suspect_ids[!problem_class]

# Make a randomized timeline, where we want the cells to have issues. For each cell
# we will later choose 10 moments of failures from this list
suspect_time <- sample(timeline,ceiling(length(suspect_ids)*7),replace=FALSE)

fields <- c('Datetime','Operator','Cell_ID','Max_RRC_UE_fill_ratio','Max_RRC_users_HW_fill_ratio',
'Avg_UE_with_Data_in_Buffer_UL','Avg_UE_per_TTI_UL_fill_ratio','Avg_PRB_usage_per_TTI_DL','Avg_PRB_usage_per_TTI_UL');

new_dataset <- data.frame(matrix(nrow=length(enodeb_id),ncol=length(fields)),stringsAsFactors = FALSE);
colnames(new_dataset) <- fields;
new_dataset$Cell_ID <- enodeb_id;


out <- NULL;

# Iterate through the timeline and generate a measurement for each enodeB
generateRandom <- function(x)
{
  new_tick <- new_dataset;
  
  new_tick$Datetime <- as.POSIXct(x, origin="1970-01-01 0:0:0",tz="UTC");
  new_tick$Operator <- 'MegaCom Ltd';
  
  bad_tick <- new_tick[new_tick$Cell_ID %in% suspect_ids & new_tick$Datetime %in% suspect_time,];
  good_tick <- new_tick[!(new_tick$Cell_ID %in% suspect_ids & new_tick$Datetime %in% suspect_time),];
  
  good_tick$Max_RRC_UE_fill_ratio <- pmax(pmin(rsn(nrow(good_tick),xi=0,omega=0.15,alpha=4),1),0.001);
  good_tick$Max_RRC_users_HW_fill_ratio <- pmax(pmin(rsn(nrow(good_tick),xi=0.03,omega=0.15,alpha=4),1.2),0.001);
  
  good_tick$Avg_UE_with_Data_in_Buffer_UL <- pmax(rnorm(nrow(good_tick),2.5,1),0.001);
  good_tick$Avg_UE_per_TTI_UL_fill_ratio <- pmax(pmin(rnorm(nrow(good_tick),0.5,0.1),1),0.001);
  good_tick$Avg_PRB_usage_per_TTI_DL<- pmax(pmin(rnorm(nrow(good_tick),15,20),100),0.001);
  good_tick$Avg_PRB_usage_per_TTI_UL<- pmax(pmin(rnorm(nrow(good_tick),10,25),100),0.001);
  
  
  bad_tick[bad_tick$Cell_ID %in% suspect_ids.lic,]$Max_RRC_UE_fill_ratio <- pmax(pmin(rsn(nrow(bad_tick[bad_tick$Cell_ID %in% suspect_ids.lic,]),xi=0,omega=0.5,alpha=4),1),0.001);
  bad_tick[bad_tick$Cell_ID %in% suspect_ids.lic,]$Max_RRC_users_HW_fill_ratio <- pmax(pmin(rsn(nrow(bad_tick[bad_tick$Cell_ID %in% suspect_ids.lic,]),xi=0,omega=0.175,alpha=4)+0.3*rpois(nrow(bad_tick[bad_tick$Cell_ID %in% suspect_ids.lic,]),lambda=0.7),1.2),0.001);
  
  bad_tick[!bad_tick$Cell_ID %in% suspect_ids.lic,]$Max_RRC_UE_fill_ratio <- pmax(pmin(rsn(nrow(bad_tick[!bad_tick$Cell_ID %in% suspect_ids.lic,]),xi=0,omega=0.15,alpha=4),1),0.001);
  bad_tick[!bad_tick$Cell_ID %in% suspect_ids.lic,]$Max_RRC_users_HW_fill_ratio <- pmax(pmin(rsn(nrow(bad_tick[!bad_tick$Cell_ID %in% suspect_ids.lic,]),xi=0.03,omega=0.15,alpha=4),1.2),0.001);
  
  bad_tick$Avg_UE_with_Data_in_Buffer_UL <- pmax(pmin(rnorm(nrow(bad_tick),5.5,1.5)*1+rpois(nrow(bad_tick),lambda=0.8),12.5),0.001);
  bad_tick$Avg_UE_per_TTI_UL_fill_ratio <- pmax(pmin(rnorm(nrow(bad_tick),0.8,0.2),1),0.001);
  
  bad_tick[bad_tick$Cell_ID %in% suspect_ids.hw,]$Avg_PRB_usage_per_TTI_DL <- pmax(pmin(rnorm(nrow(bad_tick[bad_tick$Cell_ID %in% suspect_ids.hw,]),80,20),100),0.001);
  bad_tick[bad_tick$Cell_ID %in% suspect_ids.hw,]$Avg_PRB_usage_per_TTI_UL <- pmax(pmin(rnorm(nrow(bad_tick[bad_tick$Cell_ID %in% suspect_ids.hw,]),70,20),100),0.001);
  
  bad_tick[!bad_tick$Cell_ID %in% suspect_ids.hw,]$Avg_PRB_usage_per_TTI_DL<- pmax(pmin(rnorm(nrow(bad_tick[!bad_tick$Cell_ID %in% suspect_ids.hw,]),15,20),100),0.001);
  bad_tick[!bad_tick$Cell_ID %in% suspect_ids.hw,]$Avg_PRB_usage_per_TTI_UL<- pmax(pmin(rnorm(nrow(bad_tick[!bad_tick$Cell_ID %in% suspect_ids.hw,]),10,25),100),0.001);
  
  final_tick <- rbind(good_tick,bad_tick)

  return(final_tick)
}

out <- rbindlist(lapply(lapply(timeline,generateRandom),function(x) { data.frame(x) }))

# Save to file
write.csv(out,file=paste(data_dir,'cell_measurements.csv',sep='/'),row.names=F)
write.csv(cellid_location,file=paste(data_dir,'cell_locations.csv',sep='/'),row.names=F)


# Sanity checking
bad_data <- out[out$Cell_ID %in% suspect_ids[1],]
good_data <- out[out$Cell_ID == setdiff(enodeb_id,suspect_ids)[1],]

plot(good_data$Max_RRC_UE_fill_ratio ~ good_data$Datetime,type='l',ylim=c(0,1))
plot(bad_data$Max_RRC_UE_fill_ratio ~ bad_data$Datetime,type='l',ylim=c(0,1))
hist(out$Max_RRC_UE_fill_ratio)
hist(bad_data$Max_RRC_UE_fill_ratio)

plot(good_data$Max_RRC_users_HW_fill_ratio ~ good_data$Datetime,type='l',ylim=c(0,1))
plot(bad_data$Max_RRC_users_HW_fill_ratio ~ bad_data$Datetime,type='l',ylim=c(0,1))
hist(out$Max_RRC_users_HW_fill_ratio)
hist(bad_data$Max_RRC_users_HW_fill_ratio)

plot(good_data$Avg_UE_with_Data_in_Buffer_UL ~ good_data$Datetime,type='l',ylim=c(0,12.5))
plot(bad_data$Avg_UE_with_Data_in_Buffer_UL ~ bad_data$Datetime,type='l',ylim=c(0,12.5))


labels <- out$Cell_ID %in% suspect_ids & out$Datetime %in% suspect_time;

ggplot(data=out, aes(x=Max_RRC_users_HW_fill_ratio, y=Max_RRC_UE_fill_ratio,color=labels)) + geom_point(size=2, shape=23);

### Optional visualizations

library(ggmap)
CenterOfMap <- geocode("Amsterdam")
Amsterdam <- get_map(c(lon = CenterOfMap$lon, lat = CenterOfMap$lat),zoom = 15, maptype = "terrain", source = "google")
ggmap(Amsterdam) +
  geom_point(data = cellid_location, aes(x = trueServingCoverageArea_longitude, y = trueServingCoverageArea_latitude), color = 'red', size = 1)



map.ams <- get_map("Amsterdam, Netherlands");
ggmap(map.ams)

get_map("Amsterdam", zoom = 12) %>% ggmap() +
  geom_point(data = df.tokyo_locations, aes(x = lon, y = lat), color = 'red', size = 3)
