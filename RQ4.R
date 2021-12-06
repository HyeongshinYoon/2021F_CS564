library(maps)
library(ggplot2)
library(dplyr)
library(usmap)
library("data.table")
library(reshape2)
library(ggplot2)
library(scales)
library(purrr)

# filepath = "C:/Users/seoyun/Documents/2021F_CS564/data_sampled_1.csv"
filepath = "C:/Users/seoyun/Documents/2021F_CS564/data.csv"
df <- read.csv(filepath, header = TRUE)
head(df)

# Year Built VS ("bathroomcnt", "bedroomcnt", "calculatedfinishedsquarefeet", "lotsizesquarefeet", "poolcnt", "propertylandusetypeid", "roomcnt", "unitcnt")
property_list <- c("bathroomcnt", "bedroomcnt", "calculatedfinishedsquarefeet", "lotsizesquarefeet", "poolcnt", "propertylandusetypeid", "roomcnt", "unitcnt")

map_year <- function(x) {
  as.integer((x%/%10)*10)
}

df.year = df
df.year$yearbuilt <- as.integer(df.year$yearbuilt)
head(df.year)
typeof(df.year$yearbuilt)
df.year$yearbuilt <- map_int(df.year$yearbuilt, map_year)
head(df.year)

theme_update(plot.title = element_text(hjust = 0.5))

#################### group by ten years
# Chi-square Test of Independence
#   - Use to test the statistical independence between two or more categorical variables
## 1) Relationship between year build and bathroomcnt
property = "bathroom"
df.yearbath <- df.year[c("yearbuilt", "bathroomcnt")]
min(df.yearbath$bathroomcnt)
df.yearbath <- df.yearbath %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(bathroomcnt))
min(df.yearbath$bathroomcnt)
head(df.yearbath, 3)
df.yearbath.meanbath <- df.yearbath %>%
  group_by(yearbuilt) %>%
  summarize(meanbathroomcnt = mean(bathroomcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearbath.meanbath, 3)
## check if mean bathroom count differ in yearbuilt
chisq.test(df.yearbath.meanbath)
# p-value = 0.9981 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "bathroomcnt"

### 1-1) Graph
np.yearbathroomcnt <- ggplot(data=df.yearbath.meanbath, aes(x=yearbuilt, y=meanbathroomcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Bathroom Count') + 
  ggtitle("Year Built VS Mean Bathroom Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
(np.yearbathroomcnt)
ggsave(sprintf("RQ4_10yr_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 2) Relationship between year build and bedroomcnt
property = "bedroom"
df.yearbed <- df.year[c("yearbuilt", "bedroomcnt")]
min(df.yearbed$bedroomcnt)
df.yearbed <- df.yearbed %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(bedroomcnt))
min(df.yearbed$bedroomcnt)
head(df.yearbed, 3)
df.yearbed.meanbed <- df.yearbed %>%
  group_by(yearbuilt) %>%
  summarize(meanbedroomcnt = mean(bedroomcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearbed.meanbed, 3)
## check if mean bedroom count differ in yearbuilt
chisq.test(df.yearbed.meanbed)
# p-value = 0.9575 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "bedroomcnt"

### 2-1) Graph
np.yearbedroomcnt <- ggplot(data=df.yearbed.meanbed, aes(x=yearbuilt, y=meanbedroomcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Bedroom Count') + 
  ggtitle("Year Built VS Mean Bedroom Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearbedroomcnt)
ggsave(sprintf("RQ4_10yr_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 3) Relationship between year build and calculatedfinishedsquarefeet
property = "finished_square_footage"
df.yearfsquarefeet <- df.year[c("yearbuilt", "calculatedfinishedsquarefeet")]
names(df.yearfsquarefeet)[2] <- "fsquarefeet"
min(df.yearfsquarefeet$fsquarefeet)
df.yearfsquarefeet <- df.yearfsquarefeet %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(fsquarefeet))
min(df.yearfsquarefeet$fsquarefeet)
head(df.yearfsquarefeet, 3)
df.yearfsquarefeet.mean <- df.yearfsquarefeet %>%
  group_by(yearbuilt) %>%
  summarize(meanfsf = mean(fsquarefeet, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearfsquarefeet.mean, 3)

## check if mean squarefeet differ in yearbuilt
chisq.test(df.yearfsquarefeet.mean)
# p-value = p-value < 2.2e-16 : we reject the null hypothesis of independence.
# There is a strong relationship between the "yearbuilt" and "squarefeet"

### 3-1) Graph
np.yearfsquarefeet <- ggplot(data=df.yearfsquarefeet.mean, aes(x=yearbuilt, y=meanfsf)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Finished Square Footage(ft2)') + 
  ggtitle("Year Built VS Mean Finished Square Footage(ft2)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearfsquarefeet)
ggsave(sprintf("RQ4_10yr_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 4) Relationship between year build and lotsizesquarefeet
property = "lot_size"
df.yearlsquarefeet <- df.year[c("yearbuilt", "lotsizesquarefeet")]
names(df.yearlsquarefeet)[2] <- "lsquarefeet"
min(df.yearlsquarefeet$lsquarefeet)
df.yearlsquarefeet <- df.yearlsquarefeet %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(lsquarefeet))
min(df.yearlsquarefeet$lsquarefeet)
head(df.yearlsquarefeet, 3)
df.yearlsquarefeet.mean <- df.yearlsquarefeet %>%
  group_by(yearbuilt) %>%
  summarize(meanlsf = mean(lsquarefeet, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearlsquarefeet.mean, 3)
## check if mean lotsizesquarefeet differ in yearbuilt
chisq.test(df.yearlsquarefeet.mean)
# p-value = p-value < 2.2e-16 : we reject the null hypothesis of independence.
# There is a strong relationship between the "yearbuilt" and "lotsizesquarefeet"

### 4-1) Graph
np.yearlsquarefeet <- ggplot(data=df.yearlsquarefeet.mean, aes(x=yearbuilt, y=meanlsf)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Lot Size(ft2)') + 
  ggtitle("Year Built VS Mean Lot Size(ft2)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearlsquarefeet)
ggsave(sprintf("RQ4_10yr_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 5) Relationship between year build and poolcnt
property = "pool"
df.yearpoolcnt <- df.year[c("yearbuilt", "poolcnt")]
min(df.yearpoolcnt$poolcnt)
df.yearpoolcnt <- df.yearpoolcnt %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(poolcnt))
min(df.yearpoolcnt$poolcnt)
head(df.yearpoolcnt, 3)
df.yearpoolcnt.meanpool <- df.yearpoolcnt %>%
  group_by(yearbuilt) %>%
  summarize(meanpoolcnt = mean(poolcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearpoolcnt.meanpool, 3)
## check if mean pool count differ in yearbuilt
chisq.test(df.yearpoolcnt.meanpool)
# p-value = 1 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "poolcnt"

### 5-1) Graph
np.yearpoolcnt <- ggplot(data=df.yearpoolcnt.meanpool, aes(x=yearbuilt, y=meanpoolcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Pool Count') + 
  ggtitle("Year Built VS Mean Pool Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearpoolcnt)
ggsave(sprintf("RQ4_10yr_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 7) Relationship between year build and roomcnt
property = "room"
df.yearroomcnt <- df.year[c("yearbuilt", "roomcnt")]
min(df.yearroomcnt$roomcnt)
df.yearroomcnt <- df.yearroomcnt %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(roomcnt))
min(df.yearroomcnt$roomcnt)
head(df.yearroomcnt, 3)
df.yearroomcnt.meanroomcnt <- df.yearroomcnt %>%
  group_by(yearbuilt) %>%
  summarize(meanroomcnt = mean(roomcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearroomcnt.meanroomcnt, 3)
## check if mean bedroom count differ in yearbuilt
chisq.test(df.yearroomcnt.meanroomcnt)
# p-value = 1 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "roomcnt"

### 6-1) Graph
np.yearroomcnt <- ggplot(data=df.yearroomcnt.meanroomcnt, aes(x=yearbuilt, y=meanroomcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Room Count') + 
  ggtitle("Year Built VS Mean Room Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearroomcnt)
ggsave(sprintf("RQ4_10yr_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 8) Relationship between year build and unitcnt
property = "unit"
df.yearunitcnt <- df.year[c("yearbuilt", "unitcnt")]
min(df.yearunitcnt$unitcnt)
df.yearunitcnt <- df.yearunitcnt %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(unitcnt))
min(df.yearunitcnt$unitcnt)
head(df.yearunitcnt, 3)
df.yearunitcnt.meanunitcnt <- df.yearunitcnt %>%
  group_by(yearbuilt) %>%
  summarize(meanunitcnt = mean(unitcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearunitcnt.meanunitcnt, 3)
## check if mean unitcnt count differ in yearbuilt
chisq.test(df.yearunitcnt.meanunitcnt)
# p-value = 0.9729 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "unitcnt"

### 8-1) Graph
np.yearunitcnt <- ggplot(data=df.yearunitcnt.meanunitcnt, aes(x=yearbuilt, y=meanunitcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Unit Count') + 
  ggtitle("Year Built VS Mean Unit Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearunitcnt)
ggsave(sprintf("RQ4_10yr_%s.png", property), dpi = 100, width=20, height=15, units="cm")

### => "calculatedfinishedsquarefeet", "lotsizesquarefeet" has a strong relationship with "yearbuilt"

## 9) Relationship between year build and calculatedfinishedsquarefeet/lotsizesquarefeet
property = "finished_lot_ratio"
df.yearflsquarefeet <- df.year[c("yearbuilt", "calculatedfinishedsquarefeet", "lotsizesquarefeet")]
min(df.yearflsquarefeet$calculatedfinishedsquarefeet)
min(df.yearflsquarefeet$lotsizesquarefeet)
df.yearflsquarefeet <- df.yearflsquarefeet %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(calculatedfinishedsquarefeet)) %>%
  subset(!is.na(lotsizesquarefeet))
min(df.yearflsquarefeet$calculatedfinishedsquarefeet)
min(df.yearflsquarefeet$lotsizesquarefeet)
head(df.yearflsquarefeet, 3)
df.yearflsquarefeet <- mutate(df.yearflsquarefeet, flsquarefeet = calculatedfinishedsquarefeet/lotsizesquarefeet)
df.yearflsquarefeet <- df.yearflsquarefeet %>%
  subset(!is.na(flsquarefeet))
head(df.yearflsquarefeet, 3)
max(df.yearflsquarefeet$flsquarefeet)
df.yearflsquarefeet.mean <- df.yearflsquarefeet %>%
  group_by(yearbuilt) %>%
  summarize(meanflsf = mean(flsquarefeet, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearflsquarefeet.mean, 3)

## check if mean squarefeet differ in yearbuilt
chisq.test(df.yearflsquarefeet.mean) # p-value = 1 

### 9-1) Graph
np.yearflsquarefeet <- ggplot(data=df.yearflsquarefeet.mean, aes(x=yearbuilt, y=meanflsf)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Finished Square Footage(ft2) to Lot Size(ft2) Ratio') + 
  ggtitle("Year Built VS Mean Finished Square Footage(ft2) to Lot Size(ft2) Ratio") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearflsquarefeet)
ggsave(sprintf("RQ4_10yr_%s.png", property), dpi = 100, width=20, height=15, units="cm")


# Regression analysis: Univariate Logistic Regression
## Is yearbuilt significant associated with the calculatedfinishedsquarefeet?
head(df.yearfsquarefeet)
df.yearfsquarefeet.scale <- mutate(df.yearfsquarefeet, scaledf = rescale(fsquarefeet, to=c(0, 1)))
head(df.yearfsquarefeet.scale)
yearfsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearfsquarefeet.scale)
summary(yearfsquarefeet.scale.out)
### p-value 2.06e-13 < 0.05
###   yearbuilt is significantly associated with the calculatedfinishedsquarefeet
### logit(P) = -13.596030 + 0.004385*yearbuilt
### OR=exp(0.004385)=1.004395
###   The odds ratio is 1.004395. The odds of calculatedfinishedsquarefeet in recent years are 1.004395 times higher.

## Is yearbuilt significant associated with the lotsizesquarefeet?
head(df.yearlsquarefeet)
df.yearlsquarefeet.scale <- mutate(df.yearlsquarefeet, scaledf = rescale(lsquarefeet, to=c(0, 1)))
head(df.yearlsquarefeet.scale)
df.yearlsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearlsquarefeet.scale)
summary(df.yearlsquarefeet.scale.out)
### p-value<2e-16
###   yearbuilt is significantly associated with the lotsizesquarefeet
### logit(P) = -62.685131 + 0.028900*yearbuilt
### OR=exp(0.028900)=1.029322
###   The odds ratio is 1.029322 The odds of lotsizesquarefeet in recent years are 1.029322 times higher.

## Is yearbuilt significant associated with the calculatedfinishedsquarefeet/lotsizesquarefeet?
head(df.yearflsquarefeet)
df.yearflsquarefeet.scale <- mutate(df.yearflsquarefeet, scaledf = rescale(flsquarefeet, to=c(0, 1)))
head(df.yearflsquarefeet.scale)
df.yearflsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearflsquarefeet.scale)
summary(df.yearflsquarefeet.scale.out)
### p-value 0.0203 < 0.5
###  yearbuilt is significantly associated with the calculatedfinishedsquarefeet/lotsizesquarefeet
### logit(P) = -2.3929100 + -0.0011341*yearbuilt
### OR=exp(-0.0011341)=0.9988665
###   The odds ratio is 0.9988665 The odds of calculatedfinishedsquarefeet/lotsizesquarefeet in recent years are 0.9988665 times higher.



#################### group by one year
# Year Built VS ("bathroomcnt", "bedroomcnt", "calculatedfinishedsquarefeet", "lotsizesquarefeet", "poolcnt", "propertylandusetypeid", "roomcnt", "unitcnt")
# Chi-square Test of Independence
#   - Use to test the statistical independence between two or more categorical variables
## 1) Relationship between year build and bathroomcnt
property = "bathroom"
df.yearbath <- df[c("yearbuilt", "bathroomcnt")]
min(df.yearbath$bathroomcnt)
df.yearbath <- df.yearbath %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(bathroomcnt))
min(df.yearbath$bathroomcnt)
head(df.yearbath, 3)
df.yearbath.meanbath <- df.yearbath %>%
  group_by(yearbuilt) %>%
  summarize(meanbathroomcnt = mean(bathroomcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearbath.meanbath, 3)
min(df.yearbath$bathroomcnt)
## check if mean bathroom count differ in yearbuilt
chisq.test(df.yearbath.meanbath)
# p-value = 1 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "bathroomcnt"

### 1-1) Graph
np.yearbathroomcnt <- ggplot(data=df.yearbath.meanbath, aes(x=yearbuilt, y=meanbathroomcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Bathroom Count') + 
  ggtitle("Year Built VS Mean Bathroom Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearbathroomcnt)
ggsave(sprintf("RQ4_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 2) Relationship between year build and bedroomcnt
property = "bedroom"
df.yearbed <- df[c("yearbuilt", "bedroomcnt")]
min(df.yearbed$bedroomcnt)
df.yearbed <- df.yearbed %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(bedroomcnt))
min(df.yearbed$bedroomcnt)
head(df.yearbed, 3)
df.yearbed.meanbed <- df.yearbed %>%
  group_by(yearbuilt) %>%
  summarize(meanbedroomcnt = mean(bedroomcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearbed.meanbed, 3)
min(df.yearbed.meanbed$meanbedroomcnt)
## check if mean bedroom count differ in yearbuilt
chisq.test(df.yearbed.meanbed)
# p-value = 1 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "bedroomcnt"

### 2-1) Graph
np.yearbedroomcnt <- ggplot(data=df.yearbed.meanbed, aes(x=yearbuilt, y=meanbedroomcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Bedroom Count') + 
  ggtitle("Year Built VS Mean Bedroom Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearbedroomcnt)
ggsave(sprintf("RQ4_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 3) Relationship between year build and calculatedfinishedsquarefeet
property = "finished_square_footage"
df.yearfsquarefeet <- df[c("yearbuilt", "calculatedfinishedsquarefeet")]
names(df.yearfsquarefeet)[2] <- "fsquarefeet"
min(df.yearfsquarefeet$fsquarefeet)
df.yearfsquarefeet <- df.yearfsquarefeet %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(fsquarefeet))
min(df.yearfsquarefeet$fsquarefeet)
head(df.yearfsquarefeet, 3)
df.yearfsquarefeet.mean <- df.yearfsquarefeet %>%
  group_by(yearbuilt) %>%
  summarize(meanfsf = mean(fsquarefeet, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearfsquarefeet.mean, 3)
min(df.yearfsquarefeet.mean$meanfsf)
## check if mean squarefeet differ in yearbuilt
chisq.test(df.yearfsquarefeet.mean)
# p-value = p-value < 2.2e-16 : we reject the null hypothesis of independence.
# There is a strong relationship between the "yearbuilt" and "squarefeet"

### 3-1) Graph
np.yearfsquarefeet <- ggplot(data=df.yearfsquarefeet.mean, aes(x=yearbuilt, y=meanfsf)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Finished Square Footage(ft2)') + 
  ggtitle("Year Built VS Mean Finished Square Footage(ft2)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearfsquarefeet)
ggsave(sprintf("RQ4_%s.png", property), dpi = 100, width=20, height=15, units="cm")


## 4) Relationship between year build and lotsizesquarefeet
property = "lot_size"
df.yearlsquarefeet <- df[c("yearbuilt", "lotsizesquarefeet")]
names(df.yearlsquarefeet)[2] <- "lsquarefeet"
min(df.yearlsquarefeet$lsquarefeet)
df.yearlsquarefeet <- df.yearlsquarefeet %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(lsquarefeet))
min(df.yearlsquarefeet$lsquarefeet)
head(df.yearlsquarefeet, 3)
df.yearlsquarefeet.mean <- df.yearlsquarefeet %>%
  group_by(yearbuilt) %>%
  summarize(meanlsf = mean(lsquarefeet, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearlsquarefeet.mean, 3)
min(df.yearlsquarefeet.mean$meanlsf)
## check if mean lotsizesquarefeet differ in yearbuilt
chisq.test(df.yearlsquarefeet.mean)
# p-value = p-value < 2.2e-16 : we reject the null hypothesis of independence.
# There is a strong relationship between the "yearbuilt" and "lotsizesquarefeet"

### 4-1) Graph
np.yearlsquarefeet <- ggplot(data=df.yearlsquarefeet.mean, aes(x=yearbuilt, y=meanlsf)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Lot Size(ft2)') + 
  ggtitle("Year Built VS Mean Lot Size(ft2)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearlsquarefeet)
ggsave(sprintf("RQ4_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 5) Relationship between year build and poolcnt
property = "pool"
df.yearpoolcnt <- df[c("yearbuilt", "poolcnt")]
head(df.yearpoolcnt, 3)
df.yearpoolcnt.meanpool <- df.yearpoolcnt %>%
  group_by(yearbuilt) %>%
  summarize(meanpoolcnt = mean(poolcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearpoolcnt.meanpool, 3)
## check if mean pool count differ in yearbuilt
chisq.test(df.yearpoolcnt.meanpool)
# p-value = 1 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "poolcnt"

### 5-1) Graph
np.yearpoolcnt <- ggplot(data=df.yearpoolcnt.meanpool, aes(x=yearbuilt, y=meanpoolcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Pool Count') + 
  ggtitle("Year Built VS Mean Pool Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearpoolcnt)
ggsave(sprintf("RQ4_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 7) Relationship between year build and roomcnt
property = "room"
df.yearroomcnt <- df[c("yearbuilt", "roomcnt")]
head(df.yearroomcnt, 3)
df.yearroomcnt.meanroomcnt <- df.yearroomcnt %>%
  group_by(yearbuilt) %>%
  summarize(meanroomcnt = mean(roomcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearroomcnt.meanroomcnt, 3)
## check if mean bedroom count differ in yearbuilt
chisq.test(df.yearroomcnt.meanroomcnt)
# p-value = 1 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "roomcnt"

### 6-1) Graph
np.yearroomcnt <- ggplot(data=df.yearroomcnt.meanroomcnt, aes(x=yearbuilt, y=meanroomcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Room Count') + 
  ggtitle("Year Built VS Mean Room Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearroomcnt)
ggsave(sprintf("RQ4_%s.png", property), dpi = 100, width=20, height=15, units="cm")

## 8) Relationship between year build and unitcnt
property = "unit"
df.yearunitcnt <- df[c("yearbuilt", "unitcnt")]
head(df.yearunitcnt, 3)
df.yearunitcnt.meanunitcnt <- df.yearunitcnt %>%
  group_by(yearbuilt) %>%
  summarize(meanunitcnt = mean(unitcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearunitcnt.meanunitcnt, 3)
## check if mean unitcnt count differ in yearbuilt
chisq.test(df.yearunitcnt.meanunitcnt)
# p-value = 1 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "unitcnt"

### 8-1) Graph
np.yearunitcnt <- ggplot(data=df.yearunitcnt.meanunitcnt, aes(x=yearbuilt, y=meanunitcnt)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Unit Count') + 
  ggtitle("Year Built VS Mean Unit Count") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearunitcnt)
ggsave(sprintf("RQ4_%s.png", property), dpi = 100, width=20, height=15, units="cm")

### => "calculatedfinishedsquarefeet", "lotsizesquarefeet" has a strong relationship with "yearbuilt"

## 9) Relationship between year build and calculatedfinishedsquarefeet/lotsizesquarefeet
property = "finished_lot_ratio"
df.yearflsquarefeet <- df[c("yearbuilt", "calculatedfinishedsquarefeet", "lotsizesquarefeet")]
head(df.yearflsquarefeet, 3)
df.yearflsquarefeet <- mutate(df.yearflsquarefeet, flsquarefeet = calculatedfinishedsquarefeet/lotsizesquarefeet)
df.yearflsquarefeet <- df.yearflsquarefeet %>%
  subset(!is.na(yearbuilt)) %>%
  subset(!is.na(flsquarefeet))
head(df.yearflsquarefeet, 3)
max(df.yearflsquarefeet$flsquarefeet)
df.yearflsquarefeet.mean <- df.yearflsquarefeet %>%
  group_by(yearbuilt) %>%
  summarize(meanflsf = mean(flsquarefeet, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearflsquarefeet.mean, 3)

## check if mean squarefeet differ in yearbuilt
chisq.test(df.yearflsquarefeet.mean) # p-value = 1 

### 9-1) Graph
np.yearflsquarefeet <- ggplot(data=df.yearflsquarefeet.mean, aes(x=yearbuilt, y=meanflsf)) +
  stat_smooth(color = "#FF6666") +
  xlab('Year Built') + 
  ylab('Mean Finished Square Footage(ft2) to Lot Size(ft2) Ratio') + 
  ggtitle("Year Built VS Mean Finished Square Footage(ft2) to Lot Size(ft2) Ratio") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
(np.yearflsquarefeet)
ggsave(sprintf("RQ4_%s.png", property), dpi = 100, width=20, height=15, units="cm")


# Regression analysis: Univariate Logistic Regression
## Is yearbuilt significant associated with the calculatedfinishedsquarefeet?
head(df.yearfsquarefeet)
df.yearfsquarefeet.scale <- mutate(df.yearfsquarefeet, scaledf = rescale(fsquarefeet, to=c(0, 1)))
head(df.yearfsquarefeet.scale)
yearfsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearfsquarefeet.scale)
summary(yearfsquarefeet.scale.out)
### p-value 4.78e-14 < 0.05
###   yearbuilt is significantly associated with the calculatedfinishedsquarefeet
### logit(P) = -13.856330 + 0.004508*yearbuilt
### OR=exp(0.004508)=1.004518
###   The odds ratio is 1.004518. The odds of calculatedfinishedsquarefeet in recent years are 1.004518 times higher.

## Is yearbuilt significant associated with the lotsizesquarefeet?
head(df.yearlsquarefeet)
df.yearlsquarefeet.scale <- mutate(df.yearlsquarefeet, scaledf = rescale(lsquarefeet, to=c(0, 1)))
head(df.yearlsquarefeet.scale)
df.yearlsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearlsquarefeet.scale)
summary(df.yearlsquarefeet.scale.out)
### p-value <2e-16
###   yearbuilt is significantly associated with the lotsizesquarefeet
### logit(P) = -63.138127 + 0.029066*yearbuilt
### OR=exp(0.029066)=1.029493
###   The odds ratio is 1.029493. The odds of lotsizesquarefeet in recent years are 1.029493 times higher.

## Is yearbuilt significant associated with the calculatedfinishedsquarefeet/lotsizesquarefeet?
head(df.yearflsquarefeet)
df.yearflsquarefeet.scale <- mutate(df.yearflsquarefeet, scaledf = rescale(flsquarefeet, to=c(0, 1)))
head(df.yearflsquarefeet.scale)
df.yearflsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearflsquarefeet.scale)
summary(df.yearflsquarefeet.scale.out)
### p-value 0.03260 < 0.5
###  yearbuilt is significantly associated with the calculatedfinishedsquarefeet/lotsizesquarefeet
### logit(P) = -2.5609773 + -0.0010457*yearbuilt
### OR=exp(-0.0010457)=0.9989548
###   The odds ratio is 0.9989548. The odds of calculatedfinishedsquarefeet/lotsizesquarefeet in recent years are 0.9989548 times higher.



## RQ1: Map
for (yr in seq(1900, 2020, 20)) {
  yr
  df_years <- df %>%
    filter(yearbuilt < yr)
  print(nrow(df_years))
  data <- map_data('county') %>%
    filter(region == 'california')
  head(data)
  ggplot(data, mapping = aes(x = long, y = lat)) +
    geom_polygon(data, mapping = aes(group = group, fill = subregion, alpha = 0.3)) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45, xlim = c(-119.5, -117.5), ylim = c(33, 35)) +
    geom_point(data = df_years, aes(x = longitude / 1000000 + 0.03, y = latitude / 1000000 + 0.02), alpha = 0.5, size = 0.02, color = as.factor(df_years$propertylandusetypeid))
  ggsave(sprintf("plot_%d.png", yr), dpi = 100)
}


##### Unused

### 3-1) Graph
# lp <- ggplot(data=df.yearsquarefeet.mean, aes(x=yearbuilt, y=meansf)) +
#   geom_line(col=1) + geom_point(col=4) +
#   xlab('Year Built') +
#   ylab('Mean squarefeet') +
# theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# lp + stat_smooth(formula = y~x, color = "#FC4E07")
# 
# p <- ggplot(data=df.yearsquarefeet.mean, aes(x=yearbuilt, y=meansf)) +
#   geom_point(col=4) + 
#   xlab('Year Built') + 
#   ylab('Mean squarefeet') + 
# theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# p + stat_smooth(formula = y~x, color = "#FC4E07")



# tail(df.yearfinishedsquarefeet)
# df.yearfinishedsquarefeet$calculatedfinishedsquarefeet <- as.factor(df.yearfinishedsquarefeet$calculatedfinishedsquarefeet)
# ggplot(df.yearfinishedsquarefeet, aes(x=yearbuilt, y = calculatedfinishedsquarefeet)) +
#   geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
# 
# ggplot(df.yearfinishedsquarefeet.mean, aes(x=yearbuilt, y = meansf)) +
#   geom_bar(stat="identity") + 
# theme_bw() + theme(plot.title = element_text(hjust = 0.5))

## TODO: 6) Relationship between year build and propertylandusetypeid
# df.yearbed <- df[c("yearbuilt", "propertylandusetypeid")]
# head(df.yearbed, 3)
# df.yearbed.meanbed <- df.yearbed %>%
#   group_by(yearbuilt) %>%
#   summarize(meanbedroomcnt = mean(calculatedfinishedsquarefeet, na.rm=TRUE)) %>%
#   subset(!is.na(yearbuilt))
# head(df.yearbed.meanbed, 3)
# ## check if mean bedroom count differ in yearbuilt
# chisq.test(df.yearbed.meanbed)
# # p-value = 1 : we accept the null hypothesis of independence.
# # There isn't a strong relationship between the "yearbuilt" and "calculatedfinishedsquarefeet"


# ## 10) Relationship between year build and lotsizesquarefeet/calculatedfinishedsquarefeet
# df.yearlfsquarefeet <- df[c("yearbuilt", "lotsizesquarefeet", "calculatedfinishedsquarefeet")]
# head(df.yearlfsquarefeet, 3)
# df.yearlfsquarefeet <- mutate(df.yearlfsquarefeet, lfsquarefeet = lotsizesquarefeet/calculatedfinishedsquarefeet)
# df.yearlfsquarefeet <- df.yearlfsquarefeet %>%
#   subset(!is.na(yearbuilt)) %>%
#   subset(!is.na(lfsquarefeet))
# max(df.yearlfsquarefeet$lfsquarefeet)
# head(df.yearlfsquarefeet, 3)
# df.yearlfsquarefeet.mean <- df.yearlfsquarefeet %>%
#   group_by(yearbuilt) %>%
#   summarize(meanlfsf = mean(lfsquarefeet, na.rm=TRUE))
# head(df.yearlfsquarefeet.mean, 3)
# 
# ## check if mean squarefeet differ in yearbuilt
# chisq.test(df.yearlfsquarefeet.mean)
# 
# ### 10-1) Graph
# np <- ggplot(data=df.yearlfsquarefeet.mean, aes(x=yearbuilt, y=meanlfsf)) +
#   stat_smooth(color = "#FF6666") +
#   xlab('Year Built') + 
#   ylab('Mean lotsizesquarefeet/calculatedfinishedsquarefeet') + 
#   ggtitle("Year Built VS Mean lotsizesquarefeet/finishedsquarefeet") +
# theme_bw() + theme(plot.title = element_text(hjust = 0.5))
# (np)



# df.yearbath.groupcnt <- df.yearbath %>%
#                           group_by(yearbuilt, bathroomcnt) %>%
#                           summarise(n=n())
# head(df.yearbath.groupcnt, 3)
# dt.yearbath <- dcast(df.yearbath.groupcnt, yearbuilt~bathroomcnt)
# head(dt.yearbath)
# setDT(dt.yearbath)
# class(dt.yearbath)
# head(dt.yearbath)
#         
