getwd()
setwd('C:\\Users\\Giorgi\\Documents\\4G_capacity\\20160714\\input_data')
getwd()

library(dplyr)
library(tidyr)

############# PARAMETERS ###########################

NumberOfDays=30 ### Number of days in sample data

#####################################################


df <- read.csv("all_20160613_20160712.csv", header =T) ### import sample data into R dataframe

names(df) <- c("ShortName", "TimeStamp", "DLVolumeMBytes", 
               "ULVolumeMBytes", "MaxDLActiveUsers", "AvgDLActiveUsers") ### renaming columns 

df$ShortName <- as.character(df$ShortName) ### changing df$ShortName from factor to char

df <- tbl_df(df) ### The S3 class tbl_df wraps a local data frame. The main advantage to using a tbl_df over a regular data frame is the printing

TotalDLVolumeMBytes <- sum(df$DLVolumeMBytes, na.rm = TRUE)
TotalVolumeMBytes <- sum(df$DLVolumeMBytes+df$ULVolumeMBytes, na.rm = TRUE)

sample <- df
#head(df, 2000)  ### use smallerdataset durring development 

sample <-sample %>%
  mutate(Date = substr(TimeStamp,1,8),
         Hour = substr(TimeStamp,9,10))  ### Splitting TimeStamp into Date and Hour columns 

#sample

sample <- sample %>%
  separate(col=ShortName, into=c("Location", "RAT", "BaseStation", "FrequencySector"), sep = "_")%>%   ### splitting Shortname into Location, RAT, BaseStation and FrequencySector columns
  separate(col=FrequencySector, into = c("Frequency", "Sector"),sep="F")%>% ### splitting FrequencySector into Frequency and Sector columns
  unite(col="LocBSSec",Location,BaseStation,Sector,sep="_")%>%  ### merging Location, BaseStation and Sector into LocBSSec column 
  select(-TimeStamp,-RAT,-Frequency) ### droping TimeStamp, RAT and Frequency columns

sample


by_date_hour_locbssec <-
  sample %>%
  group_by(Date,Hour,LocBSSec)%>%
  summarise(DLVolumeMBytes=sum(DLVolumeMBytes, na.rm = TRUE),
            ULVolumeMBytes=sum(ULVolumeMBytes, na.rm = TRUE),
            MaxDLActiveUsers=sum(MaxDLActiveUsers, na.rm = TRUE),
            AvgDLActiveUsers=sum(AvgDLActiveUsers, na.rm = TRUE)
  )  ### grouping the sample table by Date,Hour and LocBSSec and adding DLVolumeMBytes, ULVolumeMBytes, MaxDLActiveUsers and DLActiveUsers columns

by_date_hour_locbssec

################# checks ##################

round(TotalDLVolumeMBytes)  == round(sum(by_date_hour_locbssec$DLVolumeMBytes, na.rm = TRUE))
round(TotalVolumeMBytes) == round(sum(by_date_hour_locbssec$DLVolumeMBytes+by_date_hour_locbssec$ULVolumeMBytes, na.rm = TRUE))

###########################################

daily_bh <- by_date_hour_locbssec%>%
  group_by(Date,LocBSSec)%>%
  summarise(BHDLVolumeMBytes=max(DLVolumeMBytes, na.rm = TRUE),
            DailyDLVolumeMBytes=sum(DLVolumeMBytes, na.rm = TRUE)
  ) ### finding Busy Hour Download Volume and Daily Download Volume for LocBSSec

daily_bh <- daily_bh %>%
  inner_join(by_date_hour_locbssec, 
             by = c("Date" = "Date", "BHDLVolumeMBytes"="DLVolumeMBytes", "LocBSSec"="LocBSSec"))%>% ### joining daily_bh and by_date_hour_locbssec tables
  select(LocBSSec, Date, Hour, MaxDLActiveUsers, AvgDLActiveUsers, BHDLVolumeMBytes, DailyDLVolumeMBytes) ### adding MaxDLActiveUsers and AvgDLActiveUsers columns

daily_bh <- daily_bh %>% 
  mutate(BHRatio = BHDLVolumeMBytes/DailyDLVolumeMBytes)  ### adding Busy Hour Ration 

daily_bh

by_locbssec <- daily_bh %>%
  arrange(desc(BHDLVolumeMBytes))%>%
  group_by(LocBSSec)

#$ByLocBSSec <- group_by(arrange(DailyBH, desc(BHDLVolumeMBytes)), LocBSSec)

by_locbssec

by_locbssec %>%
  summarise(AverageHourlyDLVolumeforMonth = sum(DailyDLVolumeMBytes/NumberOfDays/24, na.rm=TRUE),
            AverageOfAllBH = mean(BHDLVolumeMBytes, na.rm = TRUE),
            max = max(BHDLVolumeMBytes, na.rm = TRUE),
            max1 = nth(BHDLVolumeMBytes,1),
            max2 = nth(BHDLVolumeMBytes,2),
            max3 = nth(BHDLVolumeMBytes,3),
            max4 = nth(BHDLVolumeMBytes,4),
            max5 = nth(BHDLVolumeMBytes,5),
            max6 = nth(BHDLVolumeMBytes,6),
            max7 = nth(BHDLVolumeMBytes,7),
            max8 = nth(BHDLVolumeMBytes,8),
            AverageOfTop3BH=mean(c(max1,max2,max3)),
            RatioOfAverageOfTop3BH=AverageOfTop3BH/AverageHourlyDLVolumeforMonth,
            AverageOfTop8BH=mean(c(max1,max2,max3,max4,max5,max6,max7,max8)),
            RatioOfAverageOfTop8BH=AverageOfTop8BH/AverageHourlyDLVolumeforMonth
  ) %>%
  select(LocBSSec,AverageHourlyDLVolumeforMonth, AverageOfTop3BH, RatioOfAverageOfTop3BH, AverageOfTop8BH ,RatioOfAverageOfTop8BH)


max_1_table <- by_locbssec %>%
  summarise(AverageHourlyDLVolumeforMonth = sum(DailyDLVolumeMBytes/NumberOfDays/24, na.rm=TRUE),
            AverageOfAllBH = mean(BHDLVolumeMBytes, na.rm = TRUE),
            max = max(BHDLVolumeMBytes, na.rm = TRUE)
  ) 

max_1_table


daily_bh

max_1_table %>% inner_join(daily_bh, 
                           by = c("max" = "BHDLVolumeMBytes", "LocBSSec"="LocBSSec"))

%>%
  select(LocBSSec, AverageHourlyDLVolumeforMonth, AverageOfAllBH, Max1=max, 
         MaxDLActiveUsers1=MaxDLActiveUsers, AvgDLActiveUsers1=AvgDLActiveUsers)


#summarise(ByLocBSSec,
#          AverageHourlyForMonth = sum(DailyDLVolumeMBytes/(30/24), na.rm=TRUE),
#          AverageOfAllBH = mean(BHDLVolumeMBytes, na.rm = TRUE),
#          max = max(BHDLVolumeMBytes, na.rm = TRUE),
#          test1 = nth(BHDLVolumeMBytes,1),
#          test2 = nth(BHDLVolumeMBytes,2))
