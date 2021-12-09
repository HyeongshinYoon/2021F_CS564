library(dplyr)

df1 <- read.csv("team-project/properties_2016.csv" , head=TRUE)
df2 <- read.csv("team-project/train_2016_v2.csv" , head=TRUE)
r2016 <- merge(df1,df2,by=c('parcelid'),all.x=T)

df3 <- read.csv("team-project/properties_2017.csv" , head=TRUE)
df4 <- read.csv("team-project/train_2017.csv" , head=TRUE)
r2017 <- merge(df3,df4,by=c('parcelid'),all.x=T)

filter_col <- c('parcelid',
                'bathroomcnt',
                'bedroomcnt',
                'calculatedfinishedsquarefeet',
                'latitude',
                'longitude',
                'lotsizesquarefeet',
                'poolcnt',
                'propertylandusetypeid',
                'roomcnt',
                'unitcnt',
                'yearbuilt',
                'taxamount')

filter_r2016 <- r2016[, filter_col]
filter_r2017 <- r2017[, filter_col]
filter_r2016$poolcnt[is.na(filter_r2016$poolcnt)] <- 0
filter_r2017$poolcnt[is.na(filter_r2017$poolcnt)] <- 0

mergedata <- rbind(filter_r2016, filter_r2017)

mergedata$na_count <- apply(is.na(mergedata), 1, sum)
mergedata_result = mergedata[mergedata$na_count < 2,]
mergedata_result_v1 <- mergedata_result[, 2:13]
mergedata_result_sample <- sample_frac(mergedata_result_v1, 0.2)

write.csv(mergedata_result_sample,"team-project/mergedata_result_sample_v1.csv" , all(T), row.names = F)
