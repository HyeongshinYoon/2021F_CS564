library(maps)
library(ggplot2)
library(dplyr)
library(ggmap)
library(doBy)
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################


register_google(key='AIzaSyAlPfbgZoK0_zn-LjGhItJtE_hCasBTWI8')

# Read Data
origin_data <- na.omit(read.csv('data.csv'))
read_data <- origin_data[c("longitude", "latitude", "taxamount", "calculatedfinishedsquarefeet")]

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

data <- read_data
data$longitude <- apply(data[1], 1, function(x) {x/1000000})
data$latitude <- apply(data[2], 1, function(x) {x/1000000})

center <- as.numeric(c(-118.515, 34.075))
map <- get_googlemap(center=center, language="ko-KR", maptype='roadmap',zoom=8)

# -119.4754 ~ -117.5566
lon_axis <- seq(from=-119.475, to=-117.555, by=0.02)
#  33.33913 ~  34.81960
lat_axis <- seq(from=33.115, to=35.035, by=0.02)

lat <- c(0)
lon <- c(0)
mean_tax <- c(0)
mean_size <- c(0)
freq <- c(0)

new_df <- data.frame(lon, lat, mean_tax, mean_size, freq)

for (i in 1:97){
  for (j in 1:97){
    tmp_lon = lon_axis[i]
    tmp_lat = lat_axis[j]
    tmp <- data %>% filter((tmp_lon-0.01<=longitude)&(longitude<tmp_lon+0.01)&
                              (tmp_lat-0.01<=latitude)&(latitude<tmp_lat+0.01))
    if (nrow(tmp) == 0){
      new_df <- rbind(new_df, c(lon_axis[i],lat_axis[j],0,0,0))
    }
    else{
      new_df <- rbind(new_df, c(lon_axis[i],lat_axis[j], min(mean(tmp$taxamount), 15000), min(mean(tmp$calculatedfinishedsquarefeet),3000), nrow(tmp)))
    }
  }
}

new_df <- new_df[2:nrow(new_df),1:ncol(new_df)]

data1 <- read_data
data1$lon <- apply(data1[c("longitude")], 1, function(x) {x/1000000})
data1$lat <- apply(data1[c("latitude")], 1, function(x) {x/1000000})

sort_tax <- orderBy(~taxamount,data1)[(nrow(data1)-299):nrow(data1),1:ncol(data1)]
sort_size <- orderBy(~calculatedfinishedsquarefeet,data1)[(nrow(data1)-299):nrow(data1),1:ncol(data1)]

heat_mean <- ggmap(map) + 
  geom_tile(data = new_df, aes(x = lon, y = lat, alpha = mean_tax), fill="blue")+
  labs(title='Mean tax amount of houses for sale by region')+
  xlab('longitude')+ylab('latitude')+
  scale_alpha(range = c(0, 0.8))+
  scale_x_continuous(limits=c(-119.48,-117.55), expand=c(0,0))+
  scale_y_continuous(limits=c(33.11,35.04), expand=c(0,0))+
  geom_point(data = sort_tax[c("lon","lat")],size=1,color='#FF0000',alpha = 0.2)
ggsave("heatmap_tax.png", width=13.5, height=15, units="cm")

heat_mean_size <- ggmap(map) + 
  geom_tile(data = new_df, aes(x = lon, y = lat, alpha = mean_size), fill="blue")+
  labs(title='Mean calculated finished square footage of houses for sale by region')+
  xlab('longitude')+ylab('latitude')+
  scale_alpha(range = c(0, 0.8))+
  scale_x_continuous(limits=c(-119.48,-117.55), expand=c(0,0))+
  scale_y_continuous(limits=c(33.11,35.04), expand=c(0,0))+
  geom_point(data = sort_size[c("lon","lat")],size=1,color='#FF0000',alpha = 0.2)
ggsave("heatmap_size.png", width=13.5, height=15, units="cm")

heat_freq <- ggmap(map) + 
  geom_tile(data = new_df, aes(x = lon, y = lat, alpha = freq), fill="blue")+
  labs(title='Frequency of houses for sale by region')+
  xlab('longitude')+ylab('latitude')+
  scale_alpha(range = c(0, 0.8))+
  scale_x_continuous(limits=c(-119.48,-117.55), expand=c(0,0))+
  scale_y_continuous(limits=c(33.11,35.04), expand=c(0,0))
ggsave("heatmap_freq.png", width=13.5, height=15, units="cm")


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

data2 <- read_data
data2$lon <- apply(data2[c("longitude")], 1, function(x) {round(x/20000)/50})
data2$lat <- apply(data2[c("latitude")], 1, function(x) {round(x/20000)/50})
data2$lon2 <- apply(data2[c("longitude")], 1, function(x) {x/1000000})
data2$lat2 <- apply(data2[c("latitude")], 1, function(x) {x/1000000})

new_df2 <- data2 %>% group_by(lon) %>%
  dplyr::summarise(freq = n(), mean_tax = mean(taxamount), mean_size = mean(calculatedfinishedsquarefeet))

p1_1 <- ggplot(new_df2, aes(x=lon, y=freq))+geom_bar(stat='identity')+
  xlim(-119.39, -117.64)+scale_y_continuous(position = "right")+
  xlab('longitude')+ylab('frequency')
ggsave("freq_lon.png", width=11.3, height=3.5, units="cm")
p1_2 <- ggplot(new_df2, aes(x=lon, y=mean_tax))+geom_bar(stat='identity')+
xlim(-119.39, -117.64)+scale_y_continuous(position = "right")+
  xlab('longitude')+ylab('mean tax')
ggsave("tax_lon.png", width=11.3, height=3.5, units="cm")
p1_3 <- ggplot(new_df2, aes(x=lon, y=mean_size))+geom_bar(stat='identity')+
  xlim(-119.39, -117.64)+scale_y_continuous(position = "right")+
  xlab('longitude')+ylab('mean size')
ggsave("size_lon.png", width=11.3, height=3.5, units="cm")



new_df3 <- data2 %>% group_by(lat) %>%
  dplyr::summarise(freq = n(), mean_tax = mean(taxamount), mean_size = mean(calculatedfinishedsquarefeet))
new_df3 <- rbind(new_df3, c(33.20, 0, 0, 0))
new_df3 <- rbind(new_df3, c(35.00, 0, 0, 0))

p2_1 <- ggplot(new_df3, aes(x=lat, y=freq))+geom_bar(stat='identity')+coord_flip()+
  theme(axis.text.x = element_text(angle=90))+
  xlim(33.33, 35.0)+xlab('latitude')+ylab('frequency')
ggsave("freq_lat.png", width=3.8, height=12.69, units="cm")
p2_2 <- ggplot(new_df3, aes(x=lat, y=mean_tax))+geom_bar(stat='identity')+coord_flip()+
  theme(axis.text.x = element_text(angle=90))+
  xlim(33.33, 35.0)+xlab('latitude')+ylab('mean tax')
ggsave("tax_lat.png", width=3.8, height=12.69, units="cm")
p2_3 <- ggplot(new_df3, aes(x=lat, y=mean_size))+geom_bar(stat='identity')+coord_flip()+
  theme(axis.text.x = element_text(angle=90))+
  xlim(33.33, 35.0)+xlab('latitude')+ylab('mean size')
ggsave("size_lat.png", width=3.8, height=12.69, units="cm")
