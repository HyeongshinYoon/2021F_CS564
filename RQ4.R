library(maps)
library(ggplot2)
library(dplyr)
library(usmap)
library("data.table")
library(reshape2)
library(ggplot2)
library(scales)

filepath = "C:/Users/seoyun/Documents/2021F_CS564/data_sampled_1.csv"
df <- read.csv(filepath, header = TRUE)
head(df)

# year built vs ("bathroomcnt", "bedroomcnt", "calculatedfinishedsquarefeet", "lotsizesquarefeet", "poolcnt", "propertylandusetypeid", "roomcnt", "unitcnt")
property_list <- c("bathroomcnt", "bedroomcnt", "calculatedfinishedsquarefeet", "lotsizesquarefeet", "poolcnt", "propertylandusetypeid", "roomcnt", "unitcnt")

# year built vs ("bathroomcnt", "bedroomcnt", "calculatedfinishedsquarefeet", "lotsizesquarefeet", "poolcnt", "propertylandusetypeid", "roomcnt", "unitcnt")
# Chi-square Test of Independence
#   - Use to test the statistical independence between two or more categorical variables
## 1) Relationship between year build and bathroomcnt
df.yearbath <- df[c("yearbuilt", "bathroomcnt")]
head(df.yearbath, 3)
df.yearbath.meanbath <- df.yearbath %>%
                          group_by(yearbuilt) %>%
                          summarize(meanbathroomcnt = mean(bathroomcnt, na.rm=TRUE)) %>%
                          subset(!is.na(yearbuilt))
head(df.yearbath.meanbath, 3)
## check if mean bathroom count differ in yearbuilt
chisq.test(df.yearbath.meanbath)
  # p-value = 1 : we accept the null hypothesis of independence.
  # There isn't a strong relationship between the "yearbuilt" and "bathroomcnt"

### 1-1) Graph
np.yearbathroomcnt <- ggplot(data=df.yearbath.meanbath, aes(x=yearbuilt, y=meanbathroomcnt)) +
  stat_smooth(color = "#FC4E07") +
  xlab('Built year of building') + 
  ylab('Mean bathroom count') + 
  ggtitle("Relation between built year of the building and mean bathroom count") +
  theme_bw()
(np.yearbathroomcnt)

## 2) Relationship between year build and bedroomcnt
df.yearbed <- df[c("yearbuilt", "bedroomcnt")]
head(df.yearbed, 3)
df.yearbed.meanbed <- df.yearbed %>%
  group_by(yearbuilt) %>%
  summarize(meanbedroomcnt = mean(bedroomcnt, na.rm=TRUE)) %>%
  subset(!is.na(yearbuilt))
head(df.yearbed.meanbed, 3)
## check if mean bedroom count differ in yearbuilt
chisq.test(df.yearbed.meanbed)
# p-value = 1 : we accept the null hypothesis of independence.
# There isn't a strong relationship between the "yearbuilt" and "bedroomcnt"

### 2-1) Graph
np.yearbedroomcnt <- ggplot(data=df.yearbed.meanbed, aes(x=yearbuilt, y=meanbedroomcnt)) +
  stat_smooth(color = "#FC4E07") +
  xlab('Built year of building') + 
  ylab('Mean bedroom count') + 
  ggtitle("Relation between built year of the building and mean bedroom count") +
  theme_bw()
(np.yearbedroomcnt)

## 3) Relationship between year build and calculatedfinishedsquarefeet
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

## check if mean squarefeet differ in yearbuilt
chisq.test(df.yearfsquarefeet.mean)
# p-value = p-value < 2.2e-16 : we reject the null hypothesis of independence.
# There is a strong relationship between the "yearbuilt" and "squarefeet"

### 3-1) Graph
np.yearfsquarefeet <- ggplot(data=df.yearfsquarefeet.mean, aes(x=yearbuilt, y=meanfsf)) +
  stat_smooth(color = "#FC4E07") +
  xlab('Built year of building') + 
  ylab('Mean finished squarefeet') + 
  ggtitle("Relation between built year of the building and mean finished squarefeet") +
  theme_bw()
(np.yearfsquarefeet)


## 4) Relationship between year build and lotsizesquarefeet
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
## check if mean lotsizesquarefeet differ in yearbuilt
chisq.test(df.yearlsquarefeet.mean)
# p-value = p-value < 2.2e-16 : we reject the null hypothesis of independence.
# There is a strong relationship between the "yearbuilt" and "lotsizesquarefeet"

### 4-1) Graph
np.yearlsquarefeet <- ggplot(data=df.yearlsquarefeet.mean, aes(x=yearbuilt, y=meanlsf)) +
  stat_smooth(color = "#FC4E07") +
  xlab('Built year of building') + 
  ylab('Mean lotsize squarefeet') + 
  ggtitle("Relation between built year of the building and mean lotsize squarefeet") +
  theme_bw()
(np.yearlsquarefeet)

## 5) Relationship between year build and poolcnt
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
  stat_smooth(color = "#FC4E07") +
  xlab('Built year of building') + 
  ylab('Mean pool count') + 
  ggtitle("Relation between built year of the building and mean pool count") +
  theme_bw()
(np.yearpoolcnt)

## 7) Relationship between year build and roomcnt
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
  stat_smooth(color = "#FC4E07") +
  xlab('Built year of building') + 
  ylab('Mean room count') + 
  ggtitle("Relation between built year of the building and mean room count") +
  theme_bw()
(np.yearroomcnt)

## 8) Relationship between year build and unitcnt
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
  stat_smooth(color = "#FC4E07") +
  xlab('Built year of building') + 
  ylab('Mean unit count') + 
  ggtitle("Relation between built year of the building and mean unit count") +
  theme_bw()
(np.yearunitcnt)

### => "calculatedfinishedsquarefeet", "lotsizesquarefeet" has a strong relationship with "yearbuilt"

## 9) Relationship between year build and calculatedfinishedsquarefeet/lotsizesquarefeet
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
  stat_smooth(color = "#FC4E07") +
  xlab('Built year of building') + 
  ylab('Mean calculatedfinishedsquarefeet/lotsizesquarefeet') + 
  ggtitle("Relation between built year of the building and mean finishedsquarefeet/lotsizesquarefeet") +
  theme_bw()
(np.yearflsquarefeet)


# Regression analysis: Univariate Logistic Regression
## Is yearbuilt significant associated with the calculatedfinishedsquarefeet?
head(df.yearfsquarefeet)
df.yearfsquarefeet.scale <- mutate(df.yearfsquarefeet, scaledf = rescale(fsquarefeet, to=c(0, 1)))
head(df.yearfsquarefeet.scale)
yearfsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearfsquarefeet.scale)
summary(yearfsquarefeet.scale.out)
### p-value 0.000207 < 0.05
###   yearbuilt is significantly associated with the calculatedfinishedsquarefeet
### logit(P) = -12.808570 + 0.005053*yearbuilt
### OR=exp(0.005053)=1.005066
###   The odds ratio is 1.005066. The odds of calculatedfinishedsquarefeet in recent years are 1.005066 times higher.

## Is yearbuilt significant associated with the lotsizesquarefeet?
head(df.yearlsquarefeet)
df.yearlsquarefeet.scale <- mutate(df.yearlsquarefeet, scaledf = rescale(lsquarefeet, to=c(0, 1)))
head(df.yearlsquarefeet.scale)
df.yearlsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearlsquarefeet.scale)
summary(df.yearlsquarefeet.scale.out)
### p-value 2.03e-14
###   yearbuilt is significantly associated with the lotsizesquarefeet
### logit(P) = -61.065989 + 0.028554*yearbuilt
### OR=exp(0.028554)=1.028966
###   The odds ratio is 1.028966 The odds of lotsizesquarefeet in recent years are 1.028966 times higher.

## Is yearbuilt significant associated with the calculatedfinishedsquarefeet/lotsizesquarefeet?
head(df.yearflsquarefeet)
df.yearflsquarefeet.scale <- mutate(df.yearflsquarefeet, scaledf = rescale(flsquarefeet, to=c(0, 1)))
head(df.yearflsquarefeet.scale)
df.yearflsquarefeet.scale.out <- glm(scaledf~yearbuilt, family=binomial(logit), data=df.yearflsquarefeet.scale)
summary(df.yearflsquarefeet.scale.out)
### p-value 0.288 < 0.5
###  yearbuilt is significantly associated with the calculatedfinishedsquarefeet/lotsizesquarefeet
### logit(P) = -0.116462 + -0.001131*yearbuilt
### OR=exp(-0.001131)=0.9988696
###   The odds ratio is 0.9988696 The odds of calculatedfinishedsquarefeet/lotsizesquarefeet in recent years are 0.9988696 times higher.

                          

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
#   xlab('Built year of building') +
#   ylab('Mean squarefeet') +
#   theme_bw()
# lp + stat_smooth(formula = y~x, color = "#FC4E07")
# 
# p <- ggplot(data=df.yearsquarefeet.mean, aes(x=yearbuilt, y=meansf)) +
#   geom_point(col=4) + 
#   xlab('Built year of building') + 
#   ylab('Mean squarefeet') + 
#   theme_bw()
# p + stat_smooth(formula = y~x, color = "#FC4E07")



# tail(df.yearfinishedsquarefeet)
# df.yearfinishedsquarefeet$calculatedfinishedsquarefeet <- as.factor(df.yearfinishedsquarefeet$calculatedfinishedsquarefeet)
# ggplot(df.yearfinishedsquarefeet, aes(x=yearbuilt, y = calculatedfinishedsquarefeet)) +
#   geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)
# 
# ggplot(df.yearfinishedsquarefeet.mean, aes(x=yearbuilt, y = meansf)) +
#   geom_bar(stat="identity") + 
#   theme_bw()

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
#   stat_smooth(color = "#FC4E07") +
#   xlab('Built year of building') + 
#   ylab('Mean lotsizesquarefeet/calculatedfinishedsquarefeet') + 
#   ggtitle("Relation between built year of the building and mean lotsizesquarefeet/finishedsquarefeet") +
#   theme_bw()
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
