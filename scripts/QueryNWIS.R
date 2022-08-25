library(dataRetrieval) # https://pubs.er.usgs.gov/publication/tm4A10
library(dplyr)

# Sensor Codes
params_cds <- c("00003", "00010", "00400","00480", "00300", "90860")

params_cds_key <- readNWISpCode(c("00010","00004","00480", "00003","00400", "00300", "99975", "99976", "99979", "99980","01335"))

# WHAT NWIS data is available
# HUC-8

HUC8_codes <- c("01090002", "02030203", "01100003", "01080205", "01100004", "01100005", "01100006", "02030101", "02040204", "02030102", "01090004", "02080101", "02040207", "02040205", "02040202", "02040206", "02080101", "02060001", "02080111", "02080110", "02060005", "02060002", "02060003", "02060004", "02060006", "02070011", "02080104", "02080102", "02080108", "02080206", "02080107", "02070011", "02070010")

NWIS_list <- list()
i <- 1
for (HUC8 in HUC8_codes) {
  
  whatNWISdf <- whatNWISdata(huc = HUC8, service=c("uv","dv"), statCd="00003",
                             parameterCd=params_cds) %>% 
    select(site_no,station_nm,data_type_cd,begin_date,end_date,count_nu,parm_cd,dec_lat_va,dec_long_va) %>% 
    mutate(begin_date=as.POSIXct(begin_date,format="%Y-%M-%d"),
           end_date=as.POSIXct(end_date,format="%Y-%M-%d")) %>% 
    # Get Human Readable format for sensor codes used in leaflet vis.
    left_join(.,params_cds_key,by=c("parm_cd"="parameter_cd"))  %>% 
    # Convert to wide format
    group_by(site_no, .drop=FALSE) %>% 
    mutate(sensor = paste(srsname,collapse=", ")) %>% 
    distinct(site_no,.keep_all=TRUE)
  
  NWIS_list[[i]] <- whatNWISdf 
  
  i <- i + 1
}

NWIS_availdata <- do.call(rbind, NWIS_list)

NWIS_availdata_filtered <- NWIS_availdata %>% filter(sensor == "Temperature, water, Temperature, water, Oxygen, pH" || sensor == "Temperature, water, Oxygen, pH")

NWISuv_list <- list()
i <- 1
for (site in NWIS_availdata_filtered$site_no){

  NWISuv <- readNWISuv(site, parameterCd=params_cds,startDate = "1992-01-01",endDate = "2020-01-01")
  
  NWISuv_list[[i]] <- NWISuv
  
  i <- i + 1
}

do.call(rbind,NWISuv_list)



# What WQP data is available
statecodes <- c("US:44", "US:25","US:09","US:34","US:10","US:24")

WQP_list <- list()
i <- 1
for (statecode in statecodes) {
  
  state <- gsub('US:','',statecode)
  
  whatWQPdf <- whatWQPdata(statecode = statecode, siteType = 'Estuary', parameterCd=params_cds)
  
  WQP_list[[i]] <- whatWQPdf
  
  i <- i + 1
  
}

WQP_availdata <- do.call(rbind, WQP_list)

WQP_fulldata <- readWQPdata(siteNumbers = WQP_availdata$MonitoringLocationIdentifier, parameterCd=params_cds)


























