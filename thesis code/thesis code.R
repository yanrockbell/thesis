# packages
library(data.table)
library(tidyverse)
library(SparseM)
library(quantreg)
library(ggplot2)
library(rworldmap)
library(dplyr)
library(ggmap) 
library(mapdata)
library(maps)
library(gridExtra)

ENSO <- read.csv("D:/CVEN/Data/ensoau.csv",header = TRUE, sep = ",")
colnames(ENSO)[1] <- "Year"
head(ENSO)

SOI_NEUTRAL <- subset(ENSO, MNINO >= -7 & MNINO <=7)
SOI_NEUTRAL$MNINO <- NULL
SOI_ELNINO <- subset(ENSO, MNINO < -7)
SOI_ELNINO$MNINO <- NULL
SOI_LANINA <- subset(ENSO, MNINO > 7)
SOI_LANINA$MNINO <- NULL

write.csv(SOI_NEUTRAL, file = "D:\\CVEN\\Thesis\\SOI_NEUTRAL.csv", row.names = FALSE)
write.csv(SOI_ELNINO, file = "D:\\CVEN\\Thesis\\SOI_ELNINO.csv", row.names = FALSE)
write.csv(SOI_LANINA, file = "D:\\CVEN\\Thesis\\SOI_LANINA.csv", row.names = FALSE)

SOI_ELNINO <- read.csv("D:\\CVEN\\Thesis\\SOI_ELNINO.csv", header = TRUE, sep = ",")
SOI_LANINA <- read.csv("D:\\CVEN\\Thesis\\SOI_LANINA.csv", header = TRUE, sep = ",")
station_temperature <- read.csv("D:\\CVEN\\Thesis\\Dailytemperature\\station_details_update.txt", sep = ",")
station_rainfall <- read.csv("D:\\CVEN\\Thesis\\Dailyrainfall\\DC02D_StnDet_999999998749126.txt", sep = ",")
station_temperature <- subset(station_temperature, station_temperature$Lat >= -45)
station_temperature <- subset(station_temperature, station_temperature$Lat <= -11)
station_temperature <- subset(station_temperature, station_temperature$Long >= 113)
station_temperature <- subset(station_temperature, station_temperature$Long <= 155)
station_temperature <- subset(station_temperature, select = c("Station", "Name"))
station_rainfall <- subset(station_rainfall, station_rainfall$Latitude.to.4.decimal.places.in.decimal.degrees >= -45)
station_rainfall <- subset(station_rainfall, station_rainfall$Latitude.to.4.decimal.places.in.decimal.degrees <= -11)
station_rainfall <- subset(station_rainfall, station_rainfall$Longitude.to.4.decimal.places.in.decimal.degrees >= 113)
station_rainfall <- subset(station_rainfall, station_rainfall$Longitude.to.4.decimal.places.in.decimal.degrees <= 155)
station_rainfall <- subset(station_rainfall, select = c("Bureau.of.Meteorology.Station.Number", "Station.Name"))
head(station_rainfall)
colnames(station_rainfall)[1] <- "Station"
colnames(station_rainfall)[2] <- "Name"
station_list <- merge(station_rainfall, station_temperature, by = c("Station", "Name"), all = FALSE)
head(station_list)
write.csv(station_list, file = "D:\\CVEN\\Thesis\\stationlist.csv", row.names = FALSE)

station_list <- read.csv("D:\\CVEN\\Thesis\\stationlist.csv", sep = ',', header = TRUE)
station_list$Station <- sprintf("%06d", station_list$Station)

new_station_list <- matrix(c(0), ncol = 1)
colnames(new_station_list) <- "station"

for (i in station_list$Station){
  path_to_temp <- paste0("D:\\CVEN\\Thesis\\Dailytemperature\\txt\\", i, ".txt")
  temperature <- read.csv(path_to_temp, header = TRUE, sep = ",")
  colnames(temperature)[6] <- "maxtemperature"
  colnames(temperature)[7] <- "quality"
  temperature <- subset(temperature, select = c("Year", "Month", "Day",
                                                "maxtemperature",
                                                "quality"))
  temperature <- subset(temperature, quality == "Y")
  temperature$quality <- NULL
  temperature <- na.omit(temperature)
  path_to_rain <- paste0("D:\\CVEN\\Thesis\\Dailyrainfall\\DC02D_Data_", i, "_999999998749126.txt")
  #list.files(path = "D:\\CVEN\\Thesis\\Dailyrainfall", pattern = paste(i), full.names = F)
  rainfall <- read.csv(path_to_rain, header = TRUE, sep = ",")
  colnames(rainfall)[6] <- "precipitation"
  colnames(rainfall)[7] <- "quality"
  rainfall <- subset(rainfall, select = c("Year", "Month", "Day", "precipitation", "quality"))
  rainfall <- subset(rainfall, quality == "Y")
  rainfall$quality <- NULL
  dataset <- merge.data.frame(temperature, rainfall, by = c("Year", "Month", "Day"), all = FALSE)
  dataset <- na.omit(dataset)
  if (nrow(dataset) != 0){
    if (mean(dataset$maxtemperature) > 25){
      dataset <- subset.data.frame(dataset, maxtemperature > 25)
      dataset <- subset.data.frame(dataset, precipitation != 0)
      dataset_elnino <- merge.data.frame(SOI_ELNINO, dataset, by = c("Year", "Month", "Day"), all = FALSE)
      dataset_lanina <- merge.data.frame(SOI_LANINA, dataset, by = c("Year", "Month", "Day"), all = FALSE)
      if (nrow(dataset_elnino) > 500 & nrow(dataset_lanina) > 500) {
        write.table(dataset_lanina, file = file.path('D:\\CVEN\\Thesis Data\\1\\', paste0(i, '_Lanina','.csv')),
                    row.names = FALSE, sep = ',')
        write.table(dataset_elnino, file = file.path('D:\\CVEN\\Thesis Data\\1\\', paste0(i, '_Elnino','.csv')),
                    row.names = FALSE, sep = ',')
        new_station_list <- rbind(new_station_list, i)
      }
    } else {
      dataset <- subset.data.frame(dataset, maxtemperature < 25)
      dataset <- subset.data.frame(dataset, maxtemperature > 5)
      dataset <- subset.data.frame(dataset, precipitation != 0)
      dataset_elnino <- merge.data.frame(SOI_ELNINO, dataset, by = c("Year", "Month", "Day"), all = FALSE)
      dataset_lanina <- merge.data.frame(SOI_LANINA, dataset, by = c("Year", "Month", "Day"), all = FALSE)
      if (nrow(dataset_elnino) > 500 & nrow(dataset_lanina) > 500) {
        write.table(dataset_lanina, file = file.path('D:\\CVEN\\Thesis Data\\1\\', paste0(i, '_Lanina','.csv')),
                    row.names = FALSE, sep = ',')
        write.table(dataset_elnino, file = file.path('D:\\CVEN\\Thesis Data\\1\\', paste0(i, '_Elnino','.csv')),
                    row.names = FALSE, sep = ',')
        new_station_list <- rbind(new_station_list, i)
      }
    }
  }}
new_station_list <- new_station_list[-1,]
write.table(new_station_list, file = "D:\\CVEN\\Thesis Data\\1\\station_list.csv", sep = ',', 
            row.names = FALSE)

new_station_list <- read.csv("D:\\CVEN\\Thesis Data\\1\\station_list.csv", header = TRUE, sep = ',')
colnames(new_station_list) <- "station"
new_station_list$station <- sprintf("%06d", new_station_list$station)

result_50 <- matrix(c(0), ncol = 3)
colnames(result_50) <- c("station","El Nino 50%", "La Nina 50%")
result_99 <- matrix(c(0), ncol = 3)
colnames(result_99) <- c("station","El Nino 99%", "La Nina 99%")

for (i in new_station_list$station){
  path_to_lanina <- paste0("D:\\CVEN\\Thesis Data\\1\\", i, "_Lanina.csv")
  lanina <- read.csv(path_to_lanina, header = TRUE, sep = ',')
  corrupt_lanina_rainfall <- runif(length(lanina$precipitation), min = 0, max = 0.01)
  lanina$precipitation <- lanina$precipitation + corrupt_lanina_rainfall
  if (nrow(lanina) %% 2 == 0){
    lanina <- lanina[-nrow(lanina),]
  }
  path_to_elnino <- paste0("D:\\CVEN\\Thesis Data\\1\\", i, "_Elnino.csv")
  elnino <- read.csv(path_to_elnino, header = TRUE, sep = ',')
  corrupt_elnino_rainfall <- runif(length(elnino$precipitation), min = 0, max = 0.01)
  elnino$precipitation <- elnino$precipitation + corrupt_elnino_rainfall
  if (nrow(elnino) %% 2 == 0){
    elnino <- elnino[-nrow(elnino),]
  }
  war = tryCatch({
    Qreg50_ElNino = rq(log10(elnino$precipitation) ~ elnino$maxtemperature, tau = 0.50, model = TRUE, method = 'fn')
    Qreg50_LaNina = rq(log10(lanina$precipitation) ~ lanina$maxtemperature, tau = 0.50, model = TRUE, method = 'fn')
    result_50 <- rbind(result_50, list(i, summary(Qreg50_ElNino)$coef[2],
                                       summary(Qreg50_LaNina)$coef[2]))}, 
    warning = function(w){
      print(paste(i,w))})
}
result_50 <- result_50[-1,]

for (i in new_station_list$station){
  path_to_lanina <- paste0("D:\\CVEN\\Thesis Data\\1\\", i, "_Lanina.csv")
  lanina <- read.csv(path_to_lanina, header = TRUE, sep = ',')
  corrupt_lanina_rainfall <- runif(length(lanina$precipitation), min = 0, max = 0.01)
  lanina$precipitation <- lanina$precipitation + corrupt_lanina_rainfall
  if (nrow(lanina) %% 2 == 0){
    lanina <- lanina[-nrow(lanina),]
  }
  path_to_elnino <- paste0("D:\\CVEN\\Thesis Data\\1\\", i, "_Elnino.csv")
  elnino <- read.csv(path_to_elnino, header = TRUE, sep = ',')
  corrupt_elnino_rainfall <- runif(length(elnino$precipitation), min = 0, max = 0.01)
  elnino$precipitation <- elnino$precipitation + corrupt_elnino_rainfall
  if (nrow(elnino) %% 2 == 0){
    elnino <- elnino[-nrow(elnino),]
  }
  war = tryCatch({
    Qreg99_ElNino = rq(log10(elnino$precipitation) ~ elnino$maxtemperature, tau = 0.99, model = TRUE, method = 'fn')
    Qreg99_LaNina = rq(log10(lanina$precipitation) ~ lanina$maxtemperature, tau = 0.99, model = TRUE, method = 'fn')
    result_99 <- rbind(result_99, list(i, summary(Qreg99_ElNino)$coef[2],
                                       summary(Qreg99_LaNina)$coef[2]))}, 
    warning = function(w){
      print(paste(i,w))})
}
result_99 <- result_99[-1,]


write.csv(result_50, file = "D:\\CVEN\\Thesis Data\\1\\result_50_1.csv", row.names = FALSE)
write.csv(result_99, file = "D:\\CVEN\\Thesis Data\\1\\result_99_1.csv", row.names = FALSE)

result_50 <- read.csv("D:\\CVEN\\Thesis Data\\1\\result_50_1.csv", header = TRUE, sep = ',')
result_99 <- read.csv("D:\\CVEN\\Thesis Data\\1\\result_99_1.csv", header = TRUE, sep = ',')
t.test(result_50$El.Nino.50., result_50$La.Nina.50., paired = TRUE)
sum(result_50$El.Nino.50. < result_50$La.Nina.50.)/nrow(result_50)
sum(result_50$El.Nino.50. >= result_50$La.Nina.50.)/nrow(result_50)
t.test(result_99$El.Nino.99., result_99$La.Nina.99., paired = TRUE)
sum(result_99$El.Nino.99. < result_99$La.Nina.99.)/nrow(result_99)
sum(result_99$El.Nino.99. >= result_99$La.Nina.99.)/nrow(result_99)

result <- merge.data.frame(result_50, result_99, by = 'station')
write.csv(result, file = "D:\\CVEN\\Thesis Data\\1\\result_1.csv", row.names = FALSE)
t.test(result$El.Nino.50., result$La.Nina.50., paired = TRUE)
t.test(result$El.Nino.99., result$La.Nina.99., paired = TRUE)
head(result)
sum(result$El.Nino.50. < result$La.Nina.50.)/nrow(result)
sum(result$El.Nino.50. >= result$La.Nina.50.)/nrow(result)
sum(result$El.Nino.99. < result$La.Nina.99.)/nrow(result)
sum(result$El.Nino.99. >= result$La.Nina.99.)/nrow(result)



########partial correlation
for (i in new_station_list$station){
  path_to_temp <- paste0("D:\\CVEN\\Thesis\\Dailytemperature\\txt\\", i, ".txt")
  temperature <- read.csv(path_to_temp, header = TRUE, sep = ",")
  colnames(temperature)[6] <- "maxtemperature"
  colnames(temperature)[7] <- "quality"
  temperature <- subset(temperature, select = c("Year", "Month", "Day",
                                                "maxtemperature",
                                                "quality"))
  temperature <- subset(temperature, quality == "Y")
  temperature$quality <- NULL
  temperature <- na.omit(temperature)
  path_to_rain <- paste0("D:\\CVEN\\Thesis\\Dailyrainfall\\DC02D_Data_", i, "_999999998749126.txt")
  #list.files(path = "D:\\CVEN\\Thesis\\Dailyrainfall", pattern = paste(i), full.names = F)
  rainfall <- read.csv(path_to_rain, header = TRUE, sep = ",")
  colnames(rainfall)[6] <- "precipitation"
  colnames(rainfall)[7] <- "quality"
  rainfall <- subset(rainfall, select = c("Year", "Month", "Day", "precipitation", "quality"))
  rainfall <- subset(rainfall, quality == "Y")
  rainfall$quality <- NULL
  dataset <- merge.data.frame(temperature, rainfall, by = c("Year", "Month", "Day"), all = FALSE)
  dataset <- na.omit(dataset)
  dataset <- merge.data.frame(dataset, ENSO, by = c("Year", "Month", "Day"), all = FALSE)
  dataset <- subset(dataset, dataset$precipitation != 0)
  dataset$rank_T <- rank(dataset$maxtemperature, na.last = NA, ties.method = 'last')
  dataset$rank_SOI <- rank(dataset$MNINO, na.last = NA, ties.method = 'last')
  dataset$rank_R <- rank(dataset$precipitation, na.last = NA, ties.method = 'last')
  write.table(dataset, file = file.path('D:\\CVEN\\Thesis Data\\1\\RANK\\', paste0(i,'.csv')),
              sep=',', row.names = FALSE)
}

partial_1 <- matrix(c(0), ncol = 6)
colnames(partial_1) <- c("station","estimate", "p.value","statistics", "n", "gp")
partial_2 <- matrix(c(0), ncol = 6)
colnames(partial_2) <- c("station","estimate", "p.value","statistics", "n", "gp")

for (i in new_station_list$station) {
  path_to_file <- paste0("D:\\CVEN\\Thesis Data\\1\\RANK\\",i,".csv")
  file <- read.csv(path_to_file, header = TRUE, sep = ",")
  pcor <- pcor.test(file$rank_R, file$rank_T, file$rank_SOI, method = c("spearman"))
  partial_1 <- rbind.data.frame(partial_1, c(i, pcor$estimate, pcor$p.value, pcor$statistic, pcor$n, pcor$gp))
}
partial_1 <- partial_1[-1,]

for (i in new_station_list$station) {
  path_to_file <- paste0("D:\\CVEN\\Thesis Data\\1\\RANK\\",i,".csv")
  file <- read.csv(path_to_file, header = TRUE, sep = ",")
  pcor <- pcor.test(file$rank_R, file$rank_SOI, file$rank_T, method = c("spearman"))
  partial_2 <- rbind.data.frame(partial_2, c(i, pcor$estimate, pcor$p.value, pcor$statistic, pcor$n, pcor$gp))
}
partial_2 <- partial_2[-1,]
write.table(partial_1, file = "D:\\CVEN\\Thesis Data\\1\\RANK\\partial_1.csv", sep=',', row.names = FALSE)
write.table(partial_2, file = "D:\\CVEN\\Thesis Data\\1\\RANK\\partial_2.csv", sep=',', row.names = FALSE)

partial_1 <- read.csv("D:\\CVEN\\Thesis Data\\1\\RANK\\partial_1.csv", sep=',', header = TRUE)
partial_2 <- read.csv("D:\\CVEN\\Thesis Data\\1\\RANK\\partial_2.csv", sep=',', header = TRUE)
graph_partial_1 <- merge.data.frame(location, partial_1, by = "station")
graph_partial_2 <- merge.data.frame(location, partial_2, by = "station")

location_insignificant_1 <- subset(graph_partial_1, p.value > 0.05)
location_significant_1 <- subset(graph_partial_1, p.value <= 0.05)
location_insignificant_2 <- subset(graph_partial_2, p.value > 0.05)
location_significant_2 <- subset(graph_partial_2, p.value <= 0.05)

australia_map <- ggplot() + geom_polygon(data = australia, aes(x=long, y = lat, group = group),
                                         fill = "white", color = "black") + coord_fixed(1.23)
print(australia_map)

pdf('D:\\CVEN\\Thesis Data\\1\\partial_1.pdf', width = 5, height = 5)
map_rank_1 <- australia_map + geom_point(aes(x = long, y = lat), data = location_insignificant_1, shape = 1, color = '#999999') +
  geom_point(aes(x = long, y = lat, color = estimate), data = location_significant_1, shape = 19, show.legend = TRUE) +
  scale_color_gradient2(low="blue", high="red", mid = "white", midpoint = 0, limits=c(-0.5, 0.5), breaks=seq(-0.5,0.5,by=0.2),
                        name = NULL) + labs(x = "Longitude", y = "Latitude")
print(map_rank_1)
dev.off()

pdf('D:\\CVEN\\Thesis Data\\1\\partial_2.pdf', width = 5, height = 5)
map_rank_2 <- australia_map + geom_point(aes(x = long, y = lat), data = location_insignificant_2, shape = 1, color = '#999999') +
  geom_point(aes(x = long, y = lat, color = estimate), data = location_significant_2, shape = 19, show.legend = TRUE) +
  scale_color_gradient2(low="blue", high="red", mid = "white", midpoint = 0, limits=c(-0.5, 0.5), breaks=seq(-0.5,0.5,by=0.2),
                        name = NULL) + labs(x = "Longitude", y = "Latitude")
print(map_rank_2)
dev.off()









#######seasonal impact
station_list <- read.csv("D:\\CVEN\\Thesis Data\\1\\station_list.csv", header = TRUE, sep = ',')
colnames(station_list) <- "station"
station_list$station <- sprintf("%06d", station_list$station)

list_summer_elnino <- matrix(c(0), ncol = 1)
colnames(list_summer_elnino) <- "station"
list_winter_elnino <- matrix(c(0), ncol = 1)
colnames(list_winter_elnino) <- "station"
list_summer_lanina <- matrix(c(0), ncol = 1)
colnames(list_summer_lanina) <- "station"
list_winter_lanina <- matrix(c(0), ncol = 1)
colnames(list_winter_lanina) <- "station"

#150 in thesis data\\1\\season, 100 in thesis data\\season
for (i in station_list$station){
  path_to_elnino <- paste0("D:\\CVEN\\Thesis Data\\1\\", i,"_Elnino.csv")
  elnino <- read.csv(path_to_elnino, header = TRUE, sep = ',')
  path_to_lanina <- paste0("D:\\CVEN\\Thesis Data\\1\\", i,"_Lanina.csv")
  lanina <- read.csv(path_to_lanina, header = TRUE, sep = ',')
  elnino_summer <- subset.data.frame(elnino, Month == '2' | Month == '1' | Month == '12')
  if (nrow(elnino_summer) >= 100){
    path <- paste0("D:\\CVEN\\Thesis Data\\season\\", i, "_elnino_summer.csv")
    write.table(elnino_summer, file = path, sep = ',', row.names = FALSE)
    list_summer_elnino <- rbind.data.frame(list_summer_elnino, i)
  }
  lanina_summer <- subset.data.frame(lanina, Month == '2' | Month == '1' | Month == '12')
  if (nrow(lanina_summer) >= 100){
    path <- paste0("D:\\CVEN\\Thesis Data\\season\\", i, "_lanina_summer.csv")
    write.table(lanina_summer, file = path, sep = ',', row.names = FALSE)
    list_summer_lanina <- rbind.data.frame(list_summer_lanina, i)
  }
  elnino_winter <- subset.data.frame(elnino, Month >= 6 & Month <= 8)
  if (nrow(elnino_winter) >= 100){
    path <- paste0("D:\\CVEN\\Thesis Data\\season\\", i, "_elnino_winter.csv")
    write.table(elnino_winter, file = path, sep = ',', row.names = FALSE)
    list_winter_elnino <- rbind.data.frame(list_winter_elnino, i)
  }
  lanina_winter <- subset.data.frame(lanina, Month >= 6 & Month <= 8)
  if (nrow(lanina_winter) >= 100){
    path <- paste0("D:\\CVEN\\Thesis Data\\season\\", i, "_lanina_winter.csv")
    write.table(lanina_winter, file = path, sep = ',', row.names = FALSE)
    list_winter_lanina <- rbind.data.frame(list_winter_lanina, i)
  }
}
list_summer_elnino <- list_summer_elnino[-1,]
list_summer_lanina <- list_summer_lanina[-1,]
list_winter_elnino <- list_winter_elnino[-1,]
list_winter_lanina <- list_winter_lanina[-1,]

write.table(list_summer_elnino, file = "D:\\CVEN\\Thesis Data\\season\\list_summer_elnino.csv", sep = ',', row.names = FALSE)
write.table(list_summer_lanina, file = "D:\\CVEN\\Thesis Data\\season\\list_summer_lanina.csv", sep = ',', row.names = FALSE)
write.table(list_winter_elnino, file = "D:\\CVEN\\Thesis Data\\season\\list_winter_elnino.csv", sep = ',', row.names = FALSE)
write.table(list_winter_lanina, file = "D:\\CVEN\\Thesis Data\\season\\list_winter_lanina.csv", sep = ',', row.names = FALSE)






result_elninosummer_50 <- matrix(c(0), ncol = 2)
colnames(result_elninosummer_50) <- c("station", "elnino50summer")
result_elninosummer_99 <- matrix(c(0), ncol = 2)
colnames(result_elninosummer_99) <- c("station", "elnino99summer")

for (i in list_summer_elnino){
  path <- paste0("D:\\CVEN\\Thesis Data\\season\\", i,"_elnino_summer.csv")
  data <- read.csv(path, header = TRUE, sep = ',')
  corrupt_data_rainfall <- runif(length(data$precipitation), min = 0, max = 0.05)
  data$precipitation <- data$precipitation + corrupt_data_rainfall
  if (nrow(data) %% 2 == 0){
    data <- data[-nrow(data),]
  }
  war = tryCatch({
    Qreg50 = rq(log10(data$precipitation) ~ data$maxtemperature, tau = 0.50, model = TRUE, method = 'fn')
    result_elninosummer_50 <- rbind(result_elninosummer_50, list(i, summary(Qreg50)$coef[2]))
    Qreg99 = rq(log10(data$precipitation) ~ data$maxtemperature, tau = 0.99, model = TRUE, method = 'fn')
    result_elninosummer_99 <- rbind(result_elninosummer_99, list(i, summary(Qreg99)$coef[2]))
    }, 
    warning = function(w){
      print(paste(i,w))})
}
result_elninosummer_50 <- result_elninosummer_50[-1,]
result_elninosummer_99 <- result_elninosummer_99[-1,]

write.table(result_elninosummer_50, file = "D:\\CVEN\\Thesis Data\\season\\elninosummer50.csv", sep = ',', row.names = FALSE)
write.table(result_elninosummer_99, file = "D:\\CVEN\\Thesis Data\\season\\elninosummer99.csv", sep = ',', row.names = FALSE)



result_elninowinter_50 <- matrix(c(0), ncol = 2)
colnames(result_elninowinter_50) <- c("station", "elnino50winter")
result_elninowinter_99 <- matrix(c(0), ncol = 2)
colnames(result_elninowinter_99) <- c("station", "elnino99winter")

for (i in list_winter_elnino){
  path <- paste0("D:\\CVEN\\Thesis Data\\season\\", i,"_elnino_winter.csv")
  data <- read.csv(path, header = TRUE, sep = ',')
  corrupt_data_rainfall <- runif(length(data$precipitation), min = 0, max = 0.05)
  data$precipitation <- data$precipitation + corrupt_data_rainfall
  if (nrow(data) %% 2 == 0){
    data <- data[-nrow(data),]
  }
  war = tryCatch({
    Qreg50 = rq(log10(data$precipitation) ~ data$maxtemperature, tau = 0.50, model = TRUE, method = 'fn')
    result_elninowinter_50 <- rbind(result_elninowinter_50, list(i, summary(Qreg50)$coef[2]))
    Qreg99 = rq(log10(data$precipitation) ~ data$maxtemperature, tau = 0.99, model = TRUE, method = 'fn')
    result_elninowinter_99 <- rbind(result_elninowinter_99, list(i, summary(Qreg99)$coef[2]))
  }, 
  warning = function(w){
    print(paste(i,w))})
}
result_elninowinter_50 <- result_elninowinter_50[-1,]
result_elninowinter_99 <- result_elninowinter_99[-1,]

write.table(result_elninowinter_50, file = "D:\\CVEN\\Thesis Data\\season\\elninowinter50.csv", sep = ',', row.names = FALSE)
write.table(result_elninowinter_99, file = "D:\\CVEN\\Thesis Data\\season\\elninowinter99.csv", sep = ',', row.names = FALSE)




result_laninasummer_50 <- matrix(c(0), ncol = 2)
colnames(result_laninasummer_50) <- c("station", "lanina50summer")
result_laninasummer_99 <- matrix(c(0), ncol = 2)
colnames(result_laninasummer_99) <- c("station", "lanina99summer")

for (i in list_summer_lanina){
  path <- paste0("D:\\CVEN\\Thesis Data\\season\\", i,"_lanina_summer.csv")
  data <- read.csv(path, header = TRUE, sep = ',')
  corrupt_data_rainfall <- runif(length(data$precipitation), min = 0, max = 0.05)
  data$precipitation <- data$precipitation + corrupt_data_rainfall
  if (nrow(data) %% 2 == 0){
    data <- data[-nrow(data),]
  }
  war = tryCatch({
    Qreg50 = rq(log10(data$precipitation) ~ data$maxtemperature, tau = 0.50, model = TRUE, method = 'fn')
    result_laninasummer_50 <- rbind(result_laninasummer_50, list(i, summary(Qreg50)$coef[2]))
    Qreg99 = rq(log10(data$precipitation) ~ data$maxtemperature, tau = 0.99, model = TRUE, method = 'fn')
    result_laninasummer_99 <- rbind(result_laninasummer_99, list(i, summary(Qreg99)$coef[2]))
  }, 
  warning = function(w){
    print(paste(i,w))})
}
result_laninasummer_50 <- result_laninasummer_50[-1,]
result_laninasummer_99 <- result_laninasummer_99[-1,]

write.table(result_laninasummer_50, file = "D:\\CVEN\\Thesis Data\\season\\laninasummer50.csv", sep = ',', row.names = FALSE)
write.table(result_laninasummer_99, file = "D:\\CVEN\\Thesis Data\\season\\laninasummer99.csv", sep = ',', row.names = FALSE)



result_laninawinter_50 <- matrix(c(0), ncol = 2)
colnames(result_laninawinter_50) <- c("station", "lanina50winter")
result_laninawinter_99 <- matrix(c(0), ncol = 2)
colnames(result_laninawinter_99) <- c("station", "lanina99winter")

for (i in list_winter_lanina){
  path <- paste0("D:\\CVEN\\Thesis Data\\season\\", i,"_lanina_winter.csv")
  data <- read.csv(path, header = TRUE, sep = ',')
  corrupt_data_rainfall <- runif(length(data$precipitation), min = 0, max = 0.05)
  data$precipitation <- data$precipitation + corrupt_data_rainfall
  if (nrow(data) %% 2 == 0){
    data <- data[-nrow(data),]
  }
  war = tryCatch({
    Qreg50 = rq(log10(data$precipitation) ~ data$maxtemperature, tau = 0.50, model = TRUE, method = 'fn')
    result_laninawinter_50 <- rbind(result_laninawinter_50, list(i, summary(Qreg50)$coef[2]))
    Qreg99 = rq(log10(data$precipitation) ~ data$maxtemperature, tau = 0.99, model = TRUE, method = 'fn')
    result_laninawinter_99 <- rbind(result_laninawinter_99, list(i, summary(Qreg99)$coef[2]))
  }, 
  warning = function(w){
    print(paste(i,w))})
}
result_laninawinter_50 <- result_laninawinter_50[-1,]
result_laninawinter_99 <- result_laninawinter_99[-1,]

write.table(result_laninawinter_50, file = "D:\\CVEN\\Thesis Data\\season\\laninawinter50.csv", sep = ',', row.names = FALSE)
write.table(result_laninawinter_99, file = "D:\\CVEN\\Thesis Data\\season\\laninawinter99.csv", sep = ',', row.names = FALSE)










result_elninosummer_50 <- read.csv("D:\\CVEN\\Thesis Data\\season\\elninosummer50.csv", header = TRUE, sep = ',')
result_elninosummer_99 <- read.csv("D:\\CVEN\\Thesis Data\\season\\elninosummer99.csv", header = TRUE, sep = ',')
result_laninasummer_50 <- read.csv("D:\\CVEN\\Thesis Data\\season\\laninasummer50.csv", header = TRUE, sep = ',')
result_laninasummer_99 <- read.csv("D:\\CVEN\\Thesis Data\\season\\laninasummer99.csv", header = TRUE, sep = ',')
result_elninowinter_50 <- read.csv("D:\\CVEN\\Thesis Data\\season\\elninowinter50.csv", header = TRUE, sep = ',')
result_elninowinter_99 <- read.csv("D:\\CVEN\\Thesis Data\\season\\elninowinter99.csv", header = TRUE, sep = ',')
result_laninawinter_50 <- read.csv("D:\\CVEN\\Thesis Data\\season\\laninawinter50.csv", header = TRUE, sep = ',')
result_laninawinter_99 <- read.csv("D:\\CVEN\\Thesis Data\\season\\laninawinter99.csv", header = TRUE, sep = ',')


result_winter_50 <- merge.data.frame(result_elninowinter_50, result_laninawinter_50, by = "station")
result_winter_99 <- merge.data.frame(result_elninowinter_99, result_laninawinter_99, by = "station")
result_summer_50 <- merge.data.frame(result_elninosummer_50, result_laninasummer_50, by = "station")
result_summer_99 <- merge.data.frame(result_elninosummer_99, result_laninasummer_99, by = "station")

t.test(result_winter_50$elnino50winter, result_winter_50$lanina50winter, paired = TRUE)
t.test(result_winter_99$elnino99winter, result_winter_99$lanina99winter, paired = TRUE)
t.test(result_summer_50$elnino50summer, result_summer_50$lanina50summer, paired = TRUE)
t.test(result_summer_99$elnino99summer, result_summer_99$lanina99summer, paired = TRUE)

sum(result_winter_50$elnino50winter > result_winter_50$lanina50winter)/nrow(result_winter_50)
sum(result_winter_50$elnino50winter <= result_winter_50$lanina50winter)/nrow(result_winter_50)
sum(result_winter_99$elnino99winter > result_winter_99$lanina99winter)/nrow(result_winter_99)
sum(result_winter_99$elnino99winter <= result_winter_99$lanina99winter)/nrow(result_winter_99)
sum(result_summer_50$elnino50summer > result_summer_50$lanina50summer)/nrow(result_summer_50)
sum(result_summer_50$elnino50summer <= result_summer_50$lanina50summer)/nrow(result_summer_50)
sum(result_summer_99$elnino99summer > result_summer_99$lanina99summer)/nrow(result_summer_99)
sum(result_summer_99$elnino99summer <= result_summer_99$lanina99summer)/nrow(result_summer_99)


data_full <- merge.data.frame(result_summer_50, result_summer_99, by = "station")
data_full <- merge.data.frame(data_full, result_winter_50, by = "station")
data_full <- merge.data.frame(data_full, result_winter_99, by = "station")
sum(data_full$elnino50winter < data_full$lanina50winter) / nrow(data_full)
sum(data_full$elnino50winter >= data_full$lanina50winter) / nrow(data_full)
sum(data_full$elnino99winter < data_full$lanina99winter) / nrow(data_full)
sum(data_full$elnino99winter >= data_full$lanina99winter) / nrow(data_full)
sum(data_full$elnino50summer < data_full$lanina50summer) / nrow(data_full)
sum(data_full$elnino50summer >= data_full$lanina50summer) / nrow(data_full)
sum(data_full$elnino99summer < data_full$lanina99summer) / nrow(data_full)
sum(data_full$elnino99summer >= data_full$lanina99summer) / nrow(data_full)

t.test(data_full$elnino50winter, data_full$lanina50winter, paired = TRUE)
t.test(data_full$elnino99winter, data_full$lanina99winter, paired = TRUE)
t.test(data_full$elnino50summer, data_full$lanina50summer, paired = TRUE)
t.test(data_full$elnino99summer, data_full$lanina99summer, paired = TRUE)


coefficients_compare_1 <- ggplot(result_winter_50, aes(x = result_winter_50$elnino50winter, 
                                                       y = result_winter_50$lanina50winter))+
  geom_point(size = 2, shape = 16, color='black') +
  scale_x_continuous(limits = c(-0.3,0.1)) +
  scale_y_continuous(limits = c(-0.3,0.1)) +
  xlab("El Nino") + 
  ylab("La Nina") + 
  ggtitle("Winter 50%") +
  geom_abline(intercept = 0, slope = 1) + theme(axis.text=element_text(size=12), 
                                                 axis.title=element_text(size=14),
                                                 plot.title = element_text(size = 15))
pdf('D:\\CVEN\\Thesis Data\\1\\season_winter_50.pdf', width = 5, height = 5)
print(coefficients_compare_1)
dev.off()

coefficients_compare_2 <- ggplot(result_winter_99, aes(x = result_winter_99$elnino99winter, 
                                                       y = result_winter_99$lanina99winter))+
  geom_point(size = 2, shape = 16, color='black') +
  scale_x_continuous(limits = c(-0.3,0.1)) +
  scale_y_continuous(limits = c(-0.3,0.1)) +
  xlab("El Nino") + 
  ylab("La Nina") + 
  ggtitle("Winter 99%") +
  geom_abline(intercept = 0, slope = 1) + theme(axis.text=element_text(size=12), 
                                                axis.title=element_text(size=14),
                                                plot.title = element_text(size = 15))
pdf('D:\\CVEN\\Thesis Data\\1\\season_winter_99.pdf', width = 5, height = 5)
print(coefficients_compare_2)
dev.off()

coefficients_compare_3 <- ggplot(result_summer_50, aes(x = result_summer_50$elnino50summer, 
                                                       y = result_summer_50$lanina50summer))+
  geom_point(size = 2, shape = 16, color='black') +
  scale_x_continuous(limits = c(-0.3,0.1)) +
  scale_y_continuous(limits = c(-0.3,0.1)) +
  xlab("El Nino") + 
  ylab("La Nina") + 
  ggtitle("Summer 50%") +
  geom_abline(intercept = 0, slope = 1) + theme(axis.text=element_text(size=12), 
                                                axis.title=element_text(size=14),
                                                plot.title = element_text(size = 15))
pdf('D:\\CVEN\\Thesis Data\\1\\season_summer_50.pdf', width = 5, height = 5)
print(coefficients_compare_3)
dev.off()

coefficients_compare_4 <- ggplot(result_summer_99, aes(x = result_summer_99$elnino99summer, 
                                                       y = result_summer_99$lanina99summer))+
  geom_point(size = 2, shape = 16, color='black') +
  scale_x_continuous(limits = c(-0.3,0.1)) +
  scale_y_continuous(limits = c(-0.3,0.1)) +
  xlab("El Nino") + 
  ylab("La Nina") + 
  ggtitle("Summer 99%") +
  geom_abline(intercept = 0, slope = 1) + theme(axis.text=element_text(size=12), 
                                                axis.title=element_text(size=14),
                                                plot.title = element_text(size = 15))
pdf('D:\\CVEN\\Thesis Data\\1\\season_summer_99.pdf', width = 5, height = 5)
print(coefficients_compare_4)
dev.off()

pdf('D:\\CVEN\\Thesis Data\\1\\season_1.pdf', width = 10, height = 10)
grid.arrange(coefficients_compare_1, coefficients_compare_2, coefficients_compare_3, coefficients_compare_4, nrow=2)
dev.off()








coefficients_compare_1 <- ggplot(data_full, aes(x = data_full$elnino50winter, y = data_full$lanina50winter))+
  geom_point(size = 2, shape = 16, color='black') +
  scale_x_continuous(limits = c(-0.3,0.1)) +
  scale_y_continuous(limits = c(-0.3,0.1)) +
  xlab("El Nino") + 
  ylab("La Nina") + 
  ggtitle("Winter 50%") +
  geom_abline(intercept = 0, slope = 1)
print(coefficients_compare_1)

coefficients_compare_2 <- ggplot(data_full, aes(x = data_full$elnino99winter, y = data_full$lanina99winter))+
  geom_point(size = 2, shape = 16, color='black') +
  scale_x_continuous(limits = c(-0.3,0.1)) +
  scale_y_continuous(limits = c(-0.3,0.1)) +
  xlab("El Nino") + 
  ylab("La Nina") + 
  ggtitle("Winter 99%") +
  geom_abline(intercept = 0, slope = 1)
print(coefficients_compare_2)

coefficients_compare_3 <- ggplot(data_full, aes(x = data_full$elnino50summer, y = data_full$lanina50summer))+
  geom_point(size = 2, shape = 16, color='black') +
  scale_x_continuous(limits = c(-0.3,0.1)) +
  scale_y_continuous(limits = c(-0.3,0.1)) +
  xlab("El Nino") + 
  ylab("La Nina") + 
  ggtitle("Summer 50%") +
  geom_abline(intercept = 0, slope = 1)
print(coefficients_compare_3)

coefficients_compare_4 <- ggplot(data_full, aes(x = data_full$elnino99summer, y = data_full$lanina99summer))+
  geom_point(size = 2, shape = 16, color='black') +
  scale_x_continuous(limits = c(-0.3,0.1)) +
  scale_y_continuous(limits = c(-0.3,0.1)) +
  xlab("El Nino") + 
  ylab("La Nina") + 
  ggtitle("Summer 99%") +
  geom_abline(intercept = 0, slope = 1)
print(coefficients_compare_4)

pdf('D:\\CVEN\\Thesis Data\\1\\season_2.pdf', width = 10, height = 10)
grid.arrange(coefficients_compare_1, coefficients_compare_2, coefficients_compare_3, coefficients_compare_4, nrow=2)
dev.off()
