library(dplyr)
library(ggplot2)
library(Rmisc)
library(rpart)
library(rpart)
library(vcd)
library(psych)
library(caret)
library(vcdExtra)
library(coefplot)
library(plot3D)
library(data.table)
library(doBy)
library(ggpmisc)
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

# Read Data
origin_data <- na.omit(read.csv('data.csv'))
origin_data <- origin_data[c("bathroomcnt","bedroomcnt","calculatedfinishedsquarefeet","lotsizesquarefeet", "poolcnt",
                             "unitcnt","taxamount","yearbuilt","propertylandusedesc")]

filtered_data <- origin_data %>% filter((bathroomcnt > 0) & (bedroomcnt > 0) & (calculatedfinishedsquarefeet > 0) & (lotsizesquarefeet > 0) & (unitcnt > 0) &
                                          (taxamount > 0) & (yearbuilt > 0))

alpha = 0.025
sort_data <- orderBy(~taxamount, filtered_data)
read_data <- sort_data[round(nrow(sort_data)*alpha):round(nrow(sort_data)*(1-alpha)), 1:ncol(sort_data)]
sort_data <- orderBy(~calculatedfinishedsquarefeet, read_data)
read_data <- sort_data[round(nrow(sort_data)*alpha):round(nrow(sort_data)*(1-alpha)), 1:ncol(sort_data)]
sort_data <- orderBy(~lotsizesquarefeet, read_data)
read_data <- sort_data[round(nrow(sort_data)*alpha):round(nrow(sort_data)*(1-alpha)), 1:ncol(sort_data)]
read_data <- read_data %>% filter(unitcnt < 7)


data1 <- read_data[c("bathroomcnt","bedroomcnt","calculatedfinishedsquarefeet","lotsizesquarefeet", "poolcnt",
                     "unitcnt","yearbuilt","taxamount")]

# Normalization
scale_model <- preProcess(data1, method = "range")
data <- predict(scale_model, data1)
data <- as.data.frame(data)

data_group <- data
data_group <- data_group %>% mutate(bathroomcnt2 = cut(bathroomcnt, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf), right = F,
                                                       labels = c("01","02","03","04","05","06","07","08","09","10")))
data_group <- data_group %>% mutate(bedroomcnt2 = cut(bedroomcnt, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf), right = F,
                                                      labels = c("01","02","03","04","05","06","07","08","09","10")))
data_group <- data_group %>% mutate(calculatedfinishedsquarefeet2 = cut(calculatedfinishedsquarefeet, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf), right = F,
                                                                        labels = c("01","02","03","04","05","06","07","08","09","10")))
data_group <- data_group %>% mutate(lotsizesquarefeet2 = cut(lotsizesquarefeet, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf), right = F,
                                                             labels = c("01","02","03","04","05","06","07","08","09","10")))
data_group <- data_group %>% mutate(poolcnt2 = as.character(poolcnt))
data_group <- data_group %>% mutate(unitcnt2 = as.character(unitcnt))
data_group <- data_group %>% mutate(yearbuilt2 = cut(yearbuilt, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf), right = F,
                                                     labels = c("01","02","03","04","05","06","07","08","09","10")))

aov.out <- aov(taxamount ~ bathroomcnt2, data=data_group)
print((summary(aov.out)))
aov.out <- aov(taxamount ~ bedroomcnt2, data=data_group)
print((summary(aov.out)))
aov.out <- aov(taxamount ~ calculatedfinishedsquarefeet2, data=data_group)
print((summary(aov.out)))
aov.out <- aov(taxamount ~ lotsizesquarefeet2, data=data_group)
print((summary(aov.out)))
aov.out <- aov(taxamount ~ poolcnt2, data=data_group)
print((summary(aov.out)))
aov.out <- aov(taxamount ~ unitcnt2, data=data_group)
print((summary(aov.out)))
aov.out <- aov(taxamount ~ yearbuilt2, data=data_group)
print((summary(aov.out)))

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################

# Read Data
data2 <- read_data[c("bathroomcnt","bedroomcnt","calculatedfinishedsquarefeet","lotsizesquarefeet", "poolcnt",
                     "unitcnt","taxamount","yearbuilt","propertylandusedesc")]

# Make New Column
data2$proportion_squarefeet <- apply(data2[c("calculatedfinishedsquarefeet","lotsizesquarefeet")],1,function(x){x[1]/x[2]})
data2$poocnt_bool <- apply(data2[c("poolcnt")],1,function(x) {if (x==1){return('True')}; return ('False')})

# Draw Trend Line Plot
p1 <- ggplot(data=data2, aes(x=bathroomcnt, y=taxamount))+
  stat_smooth(color="#FF6666")+
  labs(title="Tax Amount VS Bathroom Count",
       x="Bathroom Count", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p1.png", width=20, height=15, units="cm")

p2 <- ggplot(data=data2, aes(x=bedroomcnt, y=taxamount))+
  stat_smooth(color="#FF6666")+
  labs(title="Tax Amount VS Bedroom Count",
       x="Bedroom Count", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p2.png", width=20, height=15, units="cm")

p3 <- ggplot(data=data2, aes(x=calculatedfinishedsquarefeet, y=taxamount))+
  stat_smooth(color="#FF6666")+
  labs(title="Tax Amount VS Finished Square Footage",
       x="Finished Square Footage(ft2)", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p3.png", width=20, height=15, units="cm")

p4_1 <- ggplot(data=data2, aes(x=lotsizesquarefeet, y=taxamount))+
  stat_smooth(color="#FF6666")+
  labs(title="Tax Amount VS Lot Size",
       x="Lot Size(ft2)", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p4_1.png", width=10, height=15, units="cm")

p4_2 <- ggplot(data=data2 %>%filter(lotsizesquarefeet<20000), aes(x=lotsizesquarefeet, y=taxamount))+
  stat_smooth(color="#FF6666")+
  labs(title="Tax Amount VS Lot Size",
       x="Lot Size(ft2)", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p4_2.png", width=10, height=15, units="cm")

p4_3 <- ggplot()+
  labs(title="Tax Amount VS Lot Size")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p4_3.png", width=20, height=15, units="cm")

p5 <- ggplot(data=data2, aes(x=poocnt_bool, y=taxamount))+
  geom_boxplot()+
  labs(title="Tax Amount VS Including a Swimming Pool",
       x="Including a Swimming Pool", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p5.png", width=20, height=15, units="cm")

unit_data <- data2 %>% filter(unitcnt < 7)
unit_data$unit_cnt <- apply(unit_data[c('unitcnt')], 1, function(x) {as.character(x)})
p6 <- ggplot(data=unit_data, aes(x=unit_cnt, y=taxamount))+
  geom_boxplot()+
  labs(title="Tax Amount VS Unit Count",
       x="Unit Count", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p6.png", width=20, height=15, units="cm")

p7 <- ggplot(data=data2, aes(x=yearbuilt, y=taxamount))+
  stat_smooth(color="#FF6666")+
  labs(title="Tax Amount VS Year Built",
       x="Year Built", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p7.png", width=20, height=15, units="cm")

p8 <- ggplot(data=data2, aes(x=proportion_squarefeet, y=taxamount))+
  stat_smooth(color="#FF6666")+
  labs(title="Tax Amount VS Finished Square Footage to Lot Size Ratio",
       x="Finished Square Footage to Lot Size Ratio", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p8.png", width=20, height=15, units="cm")

p13 <- ggplot(data=data2, aes(x=reorder(propertylandusedesc,taxamount), y=taxamount))+
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),width = .75, linetype = "dashed")+
  theme(axis.text.x = element_text(hjust=1,vjust=1,angle=45))+
  labs(title="Tax Amount VS Property Land Use Type",
       x="Property Land Use Type", y="Tax Amount($)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p13.png", width=20, height=15, units="cm")

p13_2 <- ggplot(data=data2, aes(x=reorder(propertylandusedesc,lotsizesquarefeet), y=lotsizesquarefeet))+
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),width = .75, linetype = "dashed")+
  theme(axis.text.x = element_text(hjust=1,vjust=1,angle=45))+
  labs(title="Lot Size VS Property Land Use Type",
       x="Property Land Use Type", y="Lot Size(ft2)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p13_2.png", width=20, height=15, units="cm")

p13_3 <- ggplot(data=data2, aes(x=reorder(propertylandusedesc,proportion_squarefeet), y=proportion_squarefeet))+
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),width = .75, linetype = "dashed")+
  theme(axis.text.x = element_text(hjust=1,vjust=1,angle=45))+
  labs(title="Finished Square Footage to Lot Size Ratio VS Property Land Use Type",
       x="Property Land Use Type", y="Finished Square Footage to Lot Size Ratio")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("p13_3.png", width=20, height=15, units="cm")
