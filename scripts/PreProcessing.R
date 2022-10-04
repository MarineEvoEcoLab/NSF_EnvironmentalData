library(dataRetrieval) # https://pubs.er.usgs.gov/publication/tm4A10
library(dplyr)
library(tidyr)
library(readxl)
# set your working directory to data within the home dir.
setwd("/home/gbarrett/NSF/NSF_EnvironmentalData/data") # Personal path to dir. 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Water Quality Portal

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#setwd("D:/Puritz_Lab/NSF/NSF_EnvironmentalData/data/")
#setwd("/home/gbarrett/NSF/NSF_EnvironmentalData/data")
WQP_files <- list.files(pattern = 'WQP') # $ indicates end of string


subsets <- list() # stache curated datasets
variables <- c("Dissolved oxygen (DO)", "Oxygen", "Temperature, water", 'Salinity', "pH") # Measurments We Want

for (WQP in c(1:length(WQP_files))) {
  
  df <- read.csv(WQP_files[WQP],na.strings=c("","NA")) %>% 
    filter(CharacteristicName %in% variables) %>% 
    mutate(time_stamp=as.POSIXct(ActivityStartDate,format="%Y-%M-%d"))
  
  sites <- df %>% 
    distinct(MonitoringLocationIdentifier,.keep_all = TRUE) %>%
    group_by(MonitoringLocationIdentifier) %>%
    dplyr::mutate(end_date=max(time_stamp)) %>% 
    #dplyr::filter(end_date >= "2015-01-01") %>% 
    dplyr::select(site=MonitoringLocationIdentifier,end_date)
  
  siteLoadedList <- lapply(sites$site,function(i){whatWQPdata(siteid=i)}) # One line information pertaining to sites activity, location, ect.
  sitemeta <- do.call(rbind,siteLoadedList) # combine into one df
  
  # Place Curated datasets within subsets list
  
  subsets[[WQP]] <- df %>% 
    dplyr::group_by(MonitoringLocationIdentifier) %>%
    dplyr::mutate(row=dplyr::row_number(),
                  result = as.numeric(ResultMeasureValue),
                  datatype = gsub("([A-Za-z]+).*", "\\1",CharacteristicName),
                  
                  time_stamp=as.POSIXct(ActivityStartDate,format="%Y-%M-%d")) %>%
    
    # Remove unwanted units 
    filter(!(ResultMeasure.MeasureUnitCode == "ppth" || ResultMeasure.MeasureUnitCode == "deg F" || is.na(ResultMeasure.MeasureUnitCode) == TRUE || ResultMeasure.MeasureUnitCode == "%" || ResultMeasure.MeasureUnitCode == "ppm")) %>%
    
    # Must subset or else pivot wider does not fully combine Results into one row but multiple in different columns
    select(MonitoringLocationIdentifier,datatype,time_stamp,result) %>%
    
    pivot_wider(.,names_from="datatype",values_from="result", values_fn = ~mean(.x, na.rm = TRUE)) %>% 
    
    # Get meta information
    left_join(.,sitemeta,by="MonitoringLocationIdentifier") %>%
    
    # Final DataFrame Structure
    summarise(
      stn_id=MonitoringLocationIdentifier,
      location=MonitoringLocationName,
      town=CountyName,
      TEMP=as.numeric(Temperature),
      DO=as.numeric(Dissolved),
      PH=as.numeric(pH),
      SAL=as.numeric(Salinity),
      depth=NA,
      lat=lat,
      long=lon,
      coalition="WQP",
      time_stamp=time_stamp)
  
}

combine_cur_WQP <- do.call(rbind,subsets)

#object_size(combine_WQP)

#rlimit_all()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Buzzards Bay

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

buzzards_datapoints <- read.csv("bbcdata1992to2020-ver07May2021.csv")
buzzardsbay_positions <- read_excel("bbcdata1992to2020-ver07May2021.xlsx",sheet = 6,col_names = T,skip=1)

qc_exclude <- c(2,4,6,7,8,9)

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
  select(stn_id=STN_ID,
         location=WQI_Area,
         town=Town,
         TEMP=TEMP_C,
         DO=DO_MGL,
         PH=PH,
         SAL=SAL_FIELD,
         depth=TOTDEP_M,
         lat=LATITUDE,
         long=LONGITUDE,
         coalition,
         time_stamp)

write.table(x=buz_curated,file="buzzards.tsv",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Eyes on The Bay

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ches <- read.table("waterquality_do_temp_sal_ph_chesapeakbay_Jan1990_Aug2022.txt",sep="\t",header=T,
                   nrow=length(count.fields("waterquality_do_temp_sal_ph_chesapeakbay_Jan1990_Aug2022.txt")) - 1, na.strings = "NA", fill = TRUE) %>% 
  left_join(.,read.delim("WaterQualityStationHUC8_ChesapeakebayDataHub.txt",sep="\t",header=TRUE),by="Station") %>% filter(Layer == "S ") %>%
  
  mutate(row=row_number(),
         time_stamp=as.POSIXct(paste(SampleDate,SampleTime,sep=" "),format="%m/%d/%Y %H:%M:%S"),
         coalition="eyesonthebay") %>%
  
  pivot_wider(.,names_from = Parameter, values_from = MeasureValue) 


ches_curated <- ches %>% 
  select(stn_id=Station,
         location=StationDescription,
         town=CountyCity,
         TEMP=WTEMP,
         DO,
         PH,
         SAL=SALINITY,
         depth=TotalDepth,
         lat=Latitude.x,
         long=Longitude.x,
         coalition,
         time_stamp)

write.table(x=ches_curated, file="eyesonthebay.tsv",sep="\t",row.names = FALSE,col.names = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# RI DOH

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ri_parametercodes <- read_xlsx("RIDOH_Supporting_Files.xlsx",sheet=2) %>% 
  mutate(ParameterName = gsub("\\ -.*","",ParameterName)) 

ri_sites <- read_xlsx("RIDOH_Supporting_Files.xlsx",sheet=5)

ridoh_curated <- read.delim("RIDOH_1988_2020.csv",sep = ',') %>% # description - code; removing description
  
  mutate(parameter_code = gsub(".* - ","",Parameter..),
         date = mdy(Date.of.Sample),
         time = hms(Time), # Some strings are missing time, only evaluated based date and avg across time for a given day
         # time_stamp = mdy_hms(paste0(Date.of.Sample," ",Time)),
         measured_value = as.numeric(Concentration),
         id = row_number()) %>% 
  
  filter(parameter_code %in% c("00300","00400","00480","00011"), # dissolved oxygen = 00300, pH = 00400, salinity = 00480, temperature = 00011
         !(Qualifier.Code %in% c("N","U","I","V"))) %>%
  
  group_by(date, WW.ID, parameter_code) %>% # pooled different depths and samples from the same day
  
  summarise(avg_measuredvalue = mean(Concentration,na.rm=TRUE)) %>% # Took the average across these avg. depths and samples taken that day
  
  pivot_wider(names_from=parameter_code, values_from = avg_measuredvalue, names_prefix = "env_") %>% 
  
  left_join(.,ri_sites, by=c("WW.ID"="WW_Station")) %>%
  
  ungroup() %>%
  
  summarise(
    stn_id=WW.ID,
    location=Site_DESCR,
    town=Town,
    TEMP=env_00011,
    DO=env_00300,
    PH=env_00400,
    SAL=env_00480,
    depth=NA,
    lat=LAT_DD,
    long=LON_DD,
    coalition="ridoh",
    time_stamp=date)

write.table(x=ridoh_curated,file="ridoh.tsv",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Combined DataFrame

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


EnvData <- rbind(combine_cur_WQP,ches_curated,buz_curated,ridoh_curated)

write.table(x = EnvData, file="NSF_EnvironmentalData.tsv",quote = FALSE,sep = "\t",row.names = FALSE,col.names = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Water Discharge Permitting

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
states <- c('MA','RI','CT','NY','NJ','MD','DE','VA','PA')

epa <- read.delim("EF_NPDES.csv",sep=",") %>% 
  filter(STATE_CODE %in% states && grepl('WASTEWATER TREATMENT FACILITY',PRIMARY_NAME))














































