scaling <- read.csv("D:\\CVEN\\Thesis Data\\1\\result.csv", header = TRUE, sep = ',')
location <- read.csv("D:\\CVEN\\Thesis\\Dailyrainfall\\DC02D_StnDet_999999998749126.txt", sep = ',', header = TRUE)
location <- subset(location, select = c("Bureau.of.Meteorology.Station.Number",
                                        "Station.Name",
                                        "Latitude.to.4.decimal.places.in.decimal.degrees",
                                        "Longitude.to.4.decimal.places.in.decimal.degrees"))
colnames(location) <- c("station", "Name", "lat", "long")
head(location)
location <- subset(location, location$long > 110)
data_graph <- merge.data.frame(location, scaling, by = "station", all = FALSE)
head(data_graph)

data_elnino_50_positive <- subset.data.frame(data_graph, data_graph$El.Nino.50. >= 0)
data_elnino_50_negative <- subset.data.frame(data_graph, data_graph$El.Nino.50. < 0)
data_lanina_50_positive <- subset.data.frame(data_graph, data_graph$La.Nina.50. >= 0)
data_lanina_50_negative <- subset.data.frame(data_graph, data_graph$La.Nina.50. < 0)
data_elnino_99_positive <- subset.data.frame(data_graph, data_graph$El.Nino.99. >= 0)
data_elnino_99_negative <- subset.data.frame(data_graph, data_graph$El.Nino.99. < 0)
data_lanina_99_positive <- subset.data.frame(data_graph, data_graph$La.Nina.99. >= 0)
data_lanina_99_negative <- subset.data.frame(data_graph, data_graph$La.Nina.99. < 0)


australia <- map("world", xlim = c(112, 155), ylim = c(-45, -11))
australia_map <- ggplot() + geom_polygon(data = australia, aes(x=long, y = lat, group = group),
                                         fill = "white", color = "black") + coord_fixed(1.23)

data_graph$difference_50 <- data_graph$La.Nina.50. - data_graph$El.Nino.50.
data_graph$difference_99 <- data_graph$La.Nina.99. - data_graph$El.Nino.99.

data_difference_50_positive <- subset.data.frame(data_graph, difference_50 >= 0)
data_difference_50_negative <- subset.data.frame(data_graph, difference_50 < 0)
data_difference_99_positive <- subset.data.frame(data_graph, difference_99 >= 0)
data_difference_99_negative <- subset.data.frame(data_graph, difference_99 < 0)

map_difference_1 <- australia_map + geom_point(aes(x = long, y = lat, size = difference_50), alpha = 0.5,
                                               data = (data_difference_50_positive), show.legend = TRUE,
                                               color = 'red')
map_difference_1 <- map_difference_1 + geom_point(aes(x = long, y = lat, size = abs(difference_50)), alpha = 0.5,
                                                  data = (data_difference_50_negative), show.legend = TRUE,
                                                  color = 'blue') + scale_size_continuous(limits = c(0,0.15), 
                                                                                          range = c(1,6),
                                                                                          breaks=seq(0,0.15,by=0.05), 
                                                                                          name = NULL)+
  labs(title = NULL, x = "Longitude", y = "Latitude")  
#+ scale_size_continuous(range = c(1,5), breaks = 3)
pdf('D:\\CVEN\\Thesis Data\\1\\compare_50_size.pdf', width = 5, height = 5)
print(map_difference_1)
dev.off()


map_difference_2 <- australia_map + geom_point(aes(x = long, y = lat, size = difference_99), alpha = 0.5,
                                               data = (data_difference_99_positive), show.legend = TRUE,
                                               color = 'red')
map_difference_2 <- map_difference_2 + geom_point(aes(x = long, y = lat, size = abs(difference_99)), alpha = 0.5,
                                                  data = (data_difference_99_negative), show.legend = TRUE,
                                                  color = 'blue')+ scale_size_continuous(limits = c(0,0.15), 
                                                                                         range = c(1,6),
                                                                                         breaks=seq(0,0.15,by=0.05), 
                                                                                         name = NULL)+
  labs(title = NULL, x = "Longitude", y = "Latitude")  
print(map_difference_2)

pdf('D:\\CVEN\\Thesis Data\\1\\compare_99_size.pdf', width = 5, height = 5)
print(map_difference_2)
dev.off()

pdf('D:\\CVEN\\Thesis Data\\1\\compare.pdf', width = 13, height = 5)
grid.arrange(map_difference_1, map_difference_2, nrow = 1)
dev.off()

#range (-0.1,0.1) for difference
data_graph_01 <- data_graph
data_graph_01$difference_50[data_graph_01$difference_50 < -0.1] <- -0.1
data_graph_01$difference_50[data_graph_01$difference_50 > 0.1] <- 0.1
data_graph_01$difference_99[data_graph_01$difference_99 < -0.1] <- -0.1
data_graph_01$difference_99[data_graph_01$difference_99 > 0.1] <- 0.1

difference_50_color <- australia_map + geom_point(aes(x = long, y = lat, color = difference_50), data = data_graph_01,
                                                  show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.1, 0.1), breaks=seq(-0.1,0.1,by=0.05), name = NULL) +
  labs(title = NULL, x = "Longitude", y = "Latitude")
pdf('D:\\CVEN\\Thesis Data\\1\\compare_50_color_0.1.pdf', width = 5, height = 5)
print(difference_50_color)
dev.off()


difference_99_color <- australia_map + geom_point(aes(x = long, y = lat, color = difference_99), data = data_graph_01,
                                                  show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.1, 0.1), breaks=seq(-0.1,0.1,by=0.05), name = NULL) +
  labs(title = NULL, x = "Longitude", y = "Latitude")
pdf('D:\\CVEN\\Thesis Data\\1\\compare_99_color_0.1.pdf', width = 5, height = 5)
print(difference_99_color)
dev.off()

#range (-0.05,0.05) for difference
data_graph_005 <- data_graph
data_graph_005$difference_50[data_graph_005$difference_50 < -0.05] <- -0.05
data_graph_005$difference_50[data_graph_005$difference_50 > 0.05] <- 0.05
data_graph_005$difference_99[data_graph_005$difference_99 < -0.05] <- -0.05
data_graph_005$difference_99[data_graph_005$difference_99 > 0.05] <- 0.05

difference_50_color <- australia_map + geom_point(aes(x = long, y = lat, color = difference_50), data = data_graph_005,
                                                  show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.05, 0.05), breaks=seq(-0.05,0.05, by = 0.02), name = NULL) +
  labs(title = NULL, x = "Longitude", y = "Latitude")
pdf('D:\\CVEN\\Thesis Data\\1\\compare_50_color_0.05.pdf', width = 5, height = 5)
print(difference_50_color)
dev.off()

difference_99_color <- australia_map + geom_point(aes(x = long, y = lat, color = difference_99), data = data_graph_005,
                                                  show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.05, 0.05), breaks=seq(-0.05,0.05, by = 0.02), name = NULL) +
  labs(title = NULL, x = "Longitude", y = "Latitude")
pdf('D:\\CVEN\\Thesis Data\\1\\compare_99_color_0.05.pdf', width = 5, height = 5)
print(difference_50_color)
dev.off()





#range (-0.1,0.1) for elnino/lanina
data_01 <- data_graph
data_01$El.Nino.50.[data_01$El.Nino.50. <= -0.1] <- -0.1
data_01$El.Nino.50.[data_01$El.Nino.50. >= 0.1] <- 0.1
data_01$La.Nina.50.[data_01$La.Nina.50. <= -0.1] <- -0.1
data_01$La.Nina.50.[data_01$La.Nina.50. >= 0.1] <- 0.1
data_01$El.Nino.50.[data_01$El.Nino.50. <= -0.1] <- -0.1
data_01$El.Nino.50.[data_01$El.Nino.50. >= 0.1] <- 0.1
data_01$La.Nina.50.[data_01$La.Nina.50. <= -0.1] <- -0.1
data_01$La.Nina.50.[data_01$La.Nina.50. >= 0.1] <- 0.1


# range (-0.2,0.2)
data_02 <- data_graph
data_02$El.Nino.50.[data_02$El.Nino.50. <= -0.2] <- -0.2
data_02$El.Nino.50.[data_02$El.Nino.50. >= 0.2] <- 0.2
data_02$La.Nina.50.[data_02$La.Nina.50. <= -0.2] <- -0.2
data_02$La.Nina.50.[data_02$La.Nina.50. >= 0.2] <- 0.2
data_02$El.Nino.99.[data_02$El.Nino.99. <= -0.2] <- -0.2
data_02$El.Nino.99.[data_02$El.Nino.99. >= 0.2] <- 0.2
data_02$La.Nina.99.[data_02$La.Nina.99. <= -0.2] <- -0.2
data_02$La.Nina.99.[data_02$La.Nina.99. >= 0.2] <- 0.2

map50_elnino_01 <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.50.), data = data_01, show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.1, 0.1), breaks=seq(-0.1,0.1,by=0.05), name = NULL) +
  labs(title = "El Nino 50%", x = "Longitude", y = "Latitude")
print(map50_elnino_01)
australia_map <- ggplot() + geom_polygon(data = australia, aes(x=long, y = lat, group = group),
                                           +                                          fill = "white", color = "black") + coord_fixed(1.23)
map50_elnino_01 <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.50.), data = data_01, show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.1, 0.1), breaks=seq(-0.1,0.1,by=0.05), name = NULL) +
  labs(title = "El Nino 50%", x = "Longitude", y = "Latitude")
print(map50_elnino_01)
map50_lanina_01 <- australia_map + geom_point(aes(x = long, y = lat, color = La.Nina.50.), data = data_01, show.legend = TRUE) +
  +   scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.1, 0.1), breaks=seq(-0.1,0.1,by=0.05), name = NULL) +
  +   labs(title = "La Nina 50%", x = "Longitude", y = "Latitude")
print(map50_lanina_01)
map99_elnino_01 <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.99.), data = data_01, show.legend = TRUE) +
  +   scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.1, 0.1), breaks=seq(-0.1,0.1,by=0.05), name = NULL) +
  +   labs(title = "El Nino 99%", x = "Longitude", y = "Latitude")
print(map99_elnino_01)
map99_lanina_01 <- australia_map + geom_point(aes(x = long, y = lat, color = La.Nina.99.), data = data_01, show.legend = TRUE) +
  +   scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.1, 0.1), breaks=seq(-0.1,0.1,by=0.05), name = NULL) +
  +   labs(title = "La Nina 99%", x = "Longitude", y = "Latitude")
print(map99_lanina_01)
map50_elnino_02 <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.50.), data = data_02, show.legend = TRUE) +
  +   scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.2, 0.2), breaks=seq(-0.2,0.2,by=0.05), name = NULL) +
  +   labs(title = "El Nino 50%", x = "Longitude", y = "Latitude")
print(map50_elnino_02)
map50_lanina_02 <- australia_map + geom_point(aes(x = long, y = lat, color = La.Nina.50.), data = data_02, show.legend = TRUE) +
  +   scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.2, 0.2), breaks=seq(-0.2,0.2,by=0.05), name = NULL) +
  +   labs(title = "La Nina 50%", x = "Longitude", y = "Latitude")
print(map50_lanina_02)
map99_elnino_02 <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.99.), data = data_02, show.legend = TRUE) +
  +   scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.2, 0.2), breaks=seq(-0.2,0.2,by=0.05), name = NULL) +
  +   labs(title = "El Nino 99%", x = "Longitude", y = "Latitude")
print(map99_elnino_02)
map99_lanina_02 <- australia_map + geom_point(aes(x = long, y = lat, color = La.Nina.99.), data = data_02, show.legend = TRUE) +
  +   scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.2, 0.2), breaks=seq(-0.2,0.2,by=0.05), name = NULL) +
  +   labs(title = "La Nina 99%", x = "Longitude", y = "Latitude")
print(map99_lanina_02)
grid.arrange(map99_elnino_01, map99_lanina_01, map50_elnino_01, map50_lanina_01, nrow = 2)
grid.arrange(map99_elnino_02, map99_lanina_02, map50_elnino_02, map50_lanina_02, nrow = 2)
grid.arrange(map99_elnino_01, map99_lanina_01, map50_elnino_01, map50_lanina_01, nrow = 2)
grid.arrange(map99_elnino_01, map99_lanina_01, map50_elnino_01, map50_lanina_01, nrow = 2)
dev.off()


# 不对称
  map50_elnino <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.50.), data = data_elnino_50_negative, show.legend = TRUE)
map50_elnino <- map50_elnino + scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                                       +                                                      limits=c(-0.2, 0.1), breaks=seq(-0.2,0.1,by=0.05), name = NULL) +
  +   labs(title = "El Nino 50%", x = "Longitude", y = "Latitude")
map50_elnino <- map50_elnino +  geom_point(aes(x = long, y = lat, fill = El.Nino.50.), data = data_elnino_50_positive, shape =21,size=2,col = "white", show.legend = FALSE)
map50_elnino <- map50_elnino + scale_fill_gradient(low = "white", high = "red", limits=c(0, 0.1))
print(map50_elnino)
map50_lanina <- australia_map + geom_point(aes(x = long, y = lat, color = La.Nina.50.), data = data_lanina_50_negative, show.legend = TRUE)
map50_lanina <- map50_lanina +  geom_point(aes(x = long, y = lat, fill = La.Nina.50.), data = data_lanina_50_positive, shape =21,size=2,col = "white", show.legend = FALSE)
map50_lanina <- map50_lanina + scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                                       +                                                      limits=c(-0.2, 0.1), breaks=seq(-0.2,0.1,by=0.05), name = NULL) +
  +   labs(title = "El Nino 50%", x = "Longitude", y = "Latitude")
map50_lanina <- map50_lanina + scale_fill_gradient(low = "white", high = "red", limits=c(0, 0.1))
print(map50_lanina)
map99_elnino <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.99.), data = data_elnino_99_negative, show.legend = TRUE)
map99_elnino <- map99_elnino + scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                                       +                                                      limits=c(-0.2, 0.2), breaks=seq(-0.2,0.2,by=0.05), name = NULL) +
  +   labs(title = "El Nino 99%", x = "Longitude", y = "Latitude")
map99_elnino <- map99_elnino +  geom_point(aes(x = long, y = lat, fill = El.Nino.99.), data = data_elnino_99_positive, shape =21,size=2,col = "white", show.legend = FALSE)
map99_elnino <- map99_elnino + scale_fill_gradient(low = "white", high = "red", limits=c(0, 0.2))
print(map99_elnino)
map99_lanina <- australia_map + geom_point(aes(x = long, y = lat, color = La.Nina.99.), data = data_lanina_99_negative, show.legend = TRUE)
map99_lanina <- map99_lanina + scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                                       +                                                      limits=c(-0.2, 0.2), breaks=seq(-0.2,0.2,by=0.05), name = NULL) +
  +   labs(title = "El Nino 99%", x = "Longitude", y = "Latitude")
map99_lanina <- map99_lanina +  geom_point(aes(x = long, y = lat, fill = La.Nina.99.), data = data_lanina_99_positive, shape =21,size=2,col = "white", show.legend = FALSE)
map99_lanina <- map99_lanina + scale_fill_gradient(low = "white", high = "red", limits=c(0, 0.2))
print(map99_lanina)
pdf('D:\\CVEN\\Thesis Data\\1\\map_03.pdf', width = 10, height = 10)
grid.arrange(map99_elnino, map99_lanina, map50_elnino, map50_lanina, nrow = 2)
dev.off()
RStudioGD 
2 
pdf('D:\\CVEN\\Thesis Data\\1\\map_01_5.pdf', width = 5, height = 5)
grid.arrange(map99_elnino_01, map99_lanina_01, map50_elnino_01, map50_lanina_01, nrow = 2)
dev.off()
RStudioGD 
2 
pdf('D:\\CVEN\\Thesis Data\\1\\map_02_5.pdf', width = 5, height = 5)
grid.arrange(map99_elnino_02, map99_lanina_02, map50_elnino_02, map50_lanina_02, nrow = 2)
dev.off()
RStudioGD 
2 
pdf('D:\\CVEN\\Thesis Data\\1\\map_03_5.pdf', width = 5, height = 5)
grid.arrange(map99_elnino, map99_lanina, map50_elnino, map50_lanina, nrow = 2)
dev.off()


#range (-0.15,0.15)
data_015 <- as.data.frame(data_graph)
data_015$El.Nino.50.[data_015$El.Nino.50. <= -0.15] <- -0.15
data_015$El.Nino.50.[data_015$El.Nino.50. >= 0.15] <- 0.15
data_015$La.Nina.50.[data_015$La.Nina.50. <= -0.15] <- -0.15
data_015$La.Nina.50.[data_015$La.Nina.50. >= 0.15] <- 0.15
data_015$El.Nino.50.[data_015$El.Nino.50. <= -0.15] <- -0.15
data_015$El.Nino.50.[data_015$El.Nino.50. >= 0.15] <- 0.15
data_015$La.Nina.50.[data_015$La.Nina.50. <= -0.15] <- -0.15
data_015$La.Nina.50.[data_015$La.Nina.50. >= 0.15] <- 0.15
data_015$Name <- NULL
data_015 <- round(data_015, digits = 2)


map50_elnino_015 <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.50.), data = data_015, show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.15, 0.15), breaks=seq(-0.15,0.15,by=0.05), name = NULL) +
  labs(title = "El Nino 50%", x = "Longitude", y = "Latitude")
print(map50_elnino_015)

map50_lanina_015 <- australia_map + geom_point(aes(x = long, y = lat, color = La.Nina.50.), data = data_015, show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.15, 0.15), breaks=seq(-0.15,0.15,by=0.05), name = NULL) +
  labs(title = "La Nina 50%", x = "Longitude", y = "Latitude")
print(map50_lanina_015)
map99_elnino_015 <- australia_map + geom_point(aes(x = long, y = lat, color = El.Nino.99.), data = data_015, show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.15, 0.15), breaks=seq(-0.15,0.15,by=0.05), name = NULL) +
  labs(title = "El Nino 99%", x = "Longitude", y = "Latitude")
print(map99_elnino_015)
map99_lanina_015 <- australia_map + geom_point(aes(x = long, y = lat, color = La.Nina.99.), data = data_015, show.legend = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid = "white", midpoint = 0, limits=c(-0.15, 0.15), breaks=seq(-0.15,0.15,by=0.05), name = NULL) +
  labs(title = "La Nina 99%", x = "Longitude", y = "Latitude")
print(map99_lanina_015)

pdf('D:\\CVEN\\Thesis Data\\1\\map_04.pdf', width = 10, height = 10)
grid.arrange(map99_elnino_015, map99_lanina_015, map50_elnino_015, map50_lanina_015, nrow = 2)
dev.off()


#plotting
scaling <- read.csv("D:\\CVEN\\Thesis Data\\1\\result_1.csv", header = TRUE, sep = ',')

pdf('D:\\CVEN\\Thesis Data\\1\\scatterplot_50.pdf', width = 5, height = 5)
plot_50 <- ggplot(result, aes(x = result$El.Nino.50., y = result$La.Nina.50.), show.legend = FALSE) +
  geom_point(color = 'black') + geom_abline(intercept = 0, slope = 1) +
  labs(title = "50%", x = "El Nino", y = "La Nina") + scale_x_continuous(limits = c(-0.15, 0.1)) +
  scale_y_continuous(limits = c(-0.15, 0.1)) + theme(axis.text=element_text(size=12), 
                                                     axis.title=element_text(size=14),
                                                     plot.title = element_text(size = 15))
print(plot_50)
dev.off()

pdf('D:\\CVEN\\Thesis Data\\1\\scatterplot_99.pdf', width = 5, height = 5)
plot_99 <- ggplot(result, aes(x = result$El.Nino.99., y = result$La.Nina.99.), show.legend = FALSE) +
  geom_point(color = 'black') + geom_abline(intercept = 0, slope = 1) +
  labs(title = "99%", x = "El Nino", y = "La Nina") + scale_x_continuous(limits = c(-0.15, 0.1)) +
  scale_y_continuous(limits = c(-0.15, 0.1)) + theme(axis.text=element_text(size=12), 
                                                     axis.title=element_text(size=14),
                                                     plot.title = element_text(size = 15))
print(plot_99)
dev.off()
