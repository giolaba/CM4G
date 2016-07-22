getwd()
#setwd('C:\\Users\\id022621\\Documents\\work\\4G_capacity')
getwd()

library(dplyr)
library(tidyr)

df <- read.csv("all_20160711_20160712.csv", header =T)

names(df) <- c("ShortName", "TimeStamp", "DLVolumeMBytes", "ULVolumeMBytes", "MaxDLActiveUsers", "AvgDLActiveUsers")

df$ShortName <- as.character(df$ShortName)

df <- tbl_df(df)

sample <- df 
#head(df, 2000)

sample <-sample %>%
            mutate(Date = substr(TimeStamp,1,8),
                   Hour = substr(TimeStamp,9,10))

sample

##sample$Date <- substr(sample$TimeStamp,1,8)
##sample$Hour <- substr(sample$TimeStamp,9,10)

#head(sample)

##strsplit(as.character(sample$ShortName),"_")


sample <- sample %>%
                separate(col=ShortName, into=c("Location", "RAT", "BaseStation", "FrequencySector"), sep = "_")%>%
                separate(col=FrequencySector, into = c("Frequency", "Sector"),sep="F")%>%
                unite(col="LocBSSec",Location,BaseStation,Sector,sep="_")%>%
                select(-RAT,-Frequency)

by_date_hour_locbssec <-
        sample %>%
        group_by(Date,Hour,LocBSSec)%>%
        summarise(DLVolumeMBytes=sum(DLVolumeMBytes, na.rm = TRUE),
                  ULVolumeMBytes=sum(ULVolumeMBytes, na.rm = TRUE),
                  MaxDLActiveUsers=sum(MaxDLActiveUsers, na.rm = TRUE),
                  AvgDLActiveUsers=sum(AvgDLActiveUsers, na.rm = TRUE)
                  )

by_date_hour_locbssec
##check
sum(by_date_hour_locbssec$DLVolumeMBytes+by_date_hour_locbssec$ULVolumeMBytes, na.rm = TRUE)

#sample$Location <- lapply(strsplit(as.character(sample$ShortName),"_"),"[", 1)
#sample$BaseStation <- lapply(strsplit(as.character(sample$ShortName),"_"),"[", 3)
#sample$Sector <- substr(lapply(strsplit(as.character(sample$ShortName),"_"),"[", 4),4,5)

#head(sample[c(1,9,10,11,2,7,8)],200)

#sample$LocBSSec <- paste(sample$Location, sample$BaseStation, sample$Sector, sep="_")

#sample$Location <- NULL
#sample$BaseStation <- NULL
#sample$Sector <- NULL

#head(sample)

#sample <-aggregate(cbind(DLVolumeMBytes, ULVolumeMBytes, MaxDLActiveUsers, AvgDLActiveUsers)~Date+Hour+LocBSSec, data=sample, sum, na.rm=TRUE)
##aggregate(MaxDLActiveUsers~Date+Hour+LocBSSec, data=sample, max, na.rm=TRUE)
##aggregate(AvgDLActiveUsers~Date+Hour+LocBSSec, data=sample, mean, na.rm=TRUE)

##merge(x=merge(x=aggregate(cbind(DLVolumeMBytes, ULVolumeMBytes)~Date+Hour+LocBSSec, data=sample, sum, na.rm=TRUE),
##      y=aggregate(MaxDLActiveUsers~Date+Hour+LocBSSec, data=sample, max, na.rm=TRUE)),
##      y=aggregate(AvgDLActiveUsers~Date+Hour+LocBSSec, data=sample, mean, na.rm=TRUE))

#head(sample)

#sum(sample$DLVolumeMBytes)

daily_bh <- by_date_hour_locbssec%>%
                        group_by(Date,LocBSSec)%>%
                        summarise(BHDLVolumeMBytes=max(DLVolumeMBytes, na.rm = TRUE),
                                  DailyDLVolumeMBytes=sum(DLVolumeMBytes, na.rm = TRUE)
                        )

daily_bh <- daily_bh %>%
                inner_join(by_date_hour_locbssec, 
                           by = c("Date" = "Date", "BHDLVolumeMBytes"="DLVolumeMBytes", "LocBSSec"="LocBSSec"))%>%
                select(LocBSSec, Date, Hour, MaxDLActiveUsers, AvgDLActiveUsers, BHDLVolumeMBytes, DailyDLVolumeMBytes)

daily_bh <- daily_bh %>% 
                mutate(BHRatio = BHDLVolumeMBytes/DailyDLVolumeMBytes)

#BHVolyume <- aggregate(DLVolumeMBytes~Date+LocBSSec, data=sample, max, na.rm=TRUE)
#DailyVolyume <- aggregate(DLVolumeMBytes~Date+LocBSSec, data=sample, sum, na.rm=TRUE)

#DailyBH <- merge(BHVolyume,DailyVolyume, by=c("Date", "LocBSSec"))

#names(DailyBH) <- c("Date", "LocBSSec", "BHDLVolumeMBytes", "DailyDLVolumeMBytes")

#head(DailyBH)

#DailyBH$BHRatio <- DailyBH$BHDLVolumeMBytes/DailyBH$DailyDLVolumeMBytes

##DailyBH[DailyBH$BHRatio=="NaN",]

#dim(aggregate(BHDLVolumeMBytes~LocBSSec, data=DailyBH, max, na.rm=TRUE))


##head(aggregate(BHDLVolumeMBytes~LocBSSec, data=DailyBH, FUN=quantile, probs=c(16/30), type=1, na.rm=TRUE))
##ByLocBSSec <- group_by(DailyBH, LocBSSec)

##BH <- summarise(ByLocBSSec,
##                MaxBHDLVolumeMBytes = max(BHDLVolumeMBytes, na.rm = TRUE),
##                MinBHDLVolumeMBytes = min(BHDLVolumeMBytes, na.rm = TRUE),
##                test = slice_(BHDLVolumeMBytes,1))

##BH1<- DailyBH %>%
##  group_by(LocBSSec) %>%
##  distinct(BHDLVolumeMBytes)%>% 
##  arrange(desc(BHDLVolumeMBytes)) %>% 
##  slice(1)%>%
##  select(BHDLVolumeMBytes)

##BH2<- DailyBH %>%
##  group_by(LocBSSec) %>%
##  distinct(BHDLVolumeMBytes)%>% 
##  arrange(desc(BHDLVolumeMBytes)) %>% 
##  slice(2) %>%
##  select(BHDLVolumeMBytes)

##names(BH1) <- c("LocBSSec","BH1DLVolumeMBytes")
##names(BH2) <- c("LocBSSec","BH2DLVolumeMBytes")

##head(merge(BH1,BH2))

by_locbssec <- daily_bh %>%
                arrange(desc(BHDLVolumeMBytes))%>%
                group_by(LocBSSec)

#$ByLocBSSec <- group_by(arrange(DailyBH, desc(BHDLVolumeMBytes)), LocBSSec)

daily_bh

by_locbssec %>%
        summarise(AverageHourlyDLVolumeforMonth = sum(DailyDLVolumeMBytes/(30/24), na.rm=TRUE),
                  AverageOfAllBH = mean(BHDLVolumeMBytes, na.rm = TRUE),
                  max = max(BHDLVolumeMBytes, na.rm = TRUE),
                  max1 = nth(BHDLVolumeMBytes,1),
                  max2 = nth(BHDLVolumeMBytes,2),
                  max3 = nth(BHDLVolumeMBytes,3),
                  max4 = nth(BHDLVolumeMBytes,4),
                  max5 = nth(BHDLVolumeMBytes,5),
                  max6 = nth(BHDLVolumeMBytes,6),
                  max7 = nth(BHDLVolumeMBytes,7),
                  max8 = nth(BHDLVolumeMBytes,8))

#summarise(ByLocBSSec,
#          AverageHourlyForMonth = sum(DailyDLVolumeMBytes/(30/24), na.rm=TRUE),
#          AverageOfAllBH = mean(BHDLVolumeMBytes, na.rm = TRUE),
#          max = max(BHDLVolumeMBytes, na.rm = TRUE),
#          test1 = nth(BHDLVolumeMBytes,1),
#          test2 = nth(BHDLVolumeMBytes,2))
