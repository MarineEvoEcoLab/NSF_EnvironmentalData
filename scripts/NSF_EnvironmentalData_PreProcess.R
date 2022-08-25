library(dataRetrieval) # https://pubs.er.usgs.gov/publication/tm4A10
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

setwd("D:/Puritz_Lab/NSF/NSF_EnvironmentalData/")

buzzards_datapoints <- read_excel("D:/Puritz_Lab/NSF/NSF_EnvironmentalData/data/bbcdata1992to2020-ver07May2021.xlsx",sheet = 2)
buzzardsbay_positions <- read_excel("D:/Puritz_Lab/NSF/NSF_EnvironmentalData/data/bbcdata1992to2020-ver07May2021.xlsx",sheet = 6,col_names = T,skip=1)

buz <- buzzards_datapoints %>% 
  # Removing records w/ QC codes
  filter(
    !(TEMP_QC %in% qc_exclude),
    !(SAL_QC %in% qc_exclude),
    !(DO_QC %in% qc_exclude),
    !(PH_QC %in% qc_exclude),
    !(is.na(STN_ID))) %>% 
  # GET Positions of Stations
  left_join(.,buzzardsbay_positions,by="STN_ID") %>% 
  
  mutate(
    time = format(as.POSIXct(TIME, format='%Y-%m-%d %H:%M:%S'),format='%H:%M:%S'),
    time_stamp = as.POSIXct(paste(SAMP_DATE,time,sep=" "),format = '%Y-%m-%d %H:%M:%S'),
    coalition="buzzards") 


buz_curated <- buz %>%
  select(stn_id=STN_ID,location=WQI_Area,town=Town,DO=DO_MGL,PH=PH,SAL=SAL_FIELD,depth=TOTDEP_M,lat=LATITUDE,long=LONGITUDE,coalition,time_stamp)


ches <- read.table("D:/Puritz_Lab/NSF/NSF_EnvironmentalData/data/waterquality_do_temp_sal_ph_chesapeakbay_Jan1990_Aug2022.txt",sep="\t",header=T,
                   nrow=length(count.fields("D:/Puritz_Lab/NSF/NSF_EnvironmentalData/data/waterquality_do_temp_sal_ph_chesapeakbay_Jan1990_Aug2022.txt")) - 1, na.strings = "NA", fill = TRUE) %>% 
  left_join(.,read.delim("D:/Puritz_Lab/NSF/NSF_EnvironmentalData/data/WaterQualityStationHUC8_ChesapeakebayDataHub.txt",sep="\t",header=TRUE),by="Station") %>% filter(Layer == "S ") %>%
  
  mutate(row=row_number(),
         time_stamp=as.POSIXct(paste(SampleDate,SampleTime,sep=" "),format="%m/%d/%Y %H:%M:%S"),
         coalition="eyesonthebay") %>%
  
  pivot_wider(.,names_from = Parameter, values_from = MeasureValue) 


ches_curated <- ches %>% 
  select(stn_id=Station,location=StationDescription,town=CountyCity,DO,PH,SAL=SALINITY,depth=TotalDepth,lat=Latitude.x,long=Longitude.x,coalition,time_stamp)


rbind(buz_curated,ches_curated)

