# What WQP data is available

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# NWIS data

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Sensor Codes
# pH, salinity, dissolved oxygen
params_cds <- c("00003", "00010", "00400","00480", "00300")
params_cds_key <- readNWISpCode(c("00003", "00010", "00400","00480", "00300", "90860"))

parm_cd_codes <- c("00300","00400","00010","00480")

pcode <- readNWISpCode("all")
pcode[grep("",
           pcode$parameter_nm,
           ignore.case=TRUE),]


# WHAT NWIS data is available
# HUC-8

HUC8_codes <- c("01090002", "02030203", "01100003", "01080205", "01100004", "01100005", "01100006", "02030101", "02040204", "02030102", "01090004", "02080101", "02040207", "02040205", "02040202", "02040206", "02080101", "02060001", "02080111", "02080110", "02060005", "02060002", "02060003", "02060004", "02060006", "02070011", "02080104", "02080102", "02080108", "02080206", "02080107", "02070011", "02070010")
NWIS_list <- list()
i <- 1
for (HUC8 in HUC8_codes) {
  
  whatNWISdf <- whatNWISdata(huc = HUC8, service=c("dv"),
                             parameterCd=parm_cd_codes) %>% group_by(site_no) %>% 
    #select(site_no,station_nm,data_type_cd,begin_date,end_date,count_nu,parm_cd,dec_lat_va,dec_long_va) %>% 
    distinct(site_no,parm_cd,.keep_all = TRUE) %>%
    mutate(
      
      params = paste(parm_cd, collapse = ", "), 
      times = length(parm_cd),
      begin_date=as.Date(begin_date,format="%Y-%M-%d"),
      end_date=as.Date(end_date,format="%Y-%M-%d"),
      loc_web_ds = as.character(loc_web_ds)) %>% 
    # Get Human Readable format for sensor codes used in leaflet vis.
    filter(times >= 3, end_date >= "2019-01-01")
  # Convert to wide format
  
  NWIS_list[[i]] <- whatNWISdf 
  
  i <- i + 1
}

NWIS_availdata <- do.call(rbind, NWIS_list) %>% distinct(site_no,.keep_all=TRUE)


NWISdv_list <- list()
i <- 1
for (site in NWIS_availdata$site_no) {
  #set_config(verbose())
  #set_config(progress())
  
  
  NWISdv <- readNWISdv(site, parameterCd=params_cds,startDate = "1992-01-01",endDate = "2020-01-01")
  
  # Not all dataframes have the data despite whatNWISdata saying otherwhise
  if (ncol(NWISdv) == 9){
    NWISdv_list[[i]] <- NWISdv
  }
  
  print(i)    
  i <- i + 1
}

NWIS <- do.call(rbind,NWISdv_list) %>% 
  left_join(.,NWIS_availdata,by="site_no") %>%
  #filter(across(ends_with("_"))) %>%
  summarise(
    stn_id=site_no,
    location=station_nm,
    town=NA,
    TEMP=X_00010_00003,
    DO=X_00300_00003,
    PH=X_00400_00003,
    SAL=NA,
    depth=NA,
    lat=dec_lat_va,
    long=dec_long_va,
    coalition="NWIS",
    time_stamp=as.POSIXct(Date,format="%Y-%M-%d"))




statecodes <- c("US:44", "US:25","US:09","US:34","US:10","US:24")

WQP_list <- list()
i <- 1
for (statecode in statecodes) {
  
  state <- gsub('US:','',statecode)
  
  whatWQPdf <- whatWQPdata(statecode = statecode)
  
  WQP_list[[i]] <- whatWQPdf
  
  i <- i + 1
  
}

WQP_availdata <- do.call(rbind, WQP_list)

WQP_fulldata <- readWQPdata(siteNumbers = WQP_availdata$MonitoringLocationIdentifier, parameterCd=params_cds)


WQP <- WQP_fulldata %>% 
  mutate(row=row_number(),
         datatype = sub(",.*",'',CharacteristicName),
         time_stamp=as.POSIXct(ActivityStartDate,format="%Y-%M-%d")) %>%
  pivot_wider(.,names_from="datatype",values_from="ResultMeasureValue") %>% 
  group_by(MonitoringLocationIdentifier,time_stamp) %>%
  fill(Temperature, Oxygen, pH, .direction = "updown") %>%
  distinct() %>%
  ungroup() %>%
  left_join(.,whatWQPdf,by="MonitoringLocationIdentifier") %>%
  summarise(
    stn_id=MonitoringLocationIdentifier,
    location=MonitoringLocationName,
    town=CountyName,
    TEMP=as.numeric(Temperature),
    DO=as.numeric(Oxygen),
    PH=as.numeric(pH),
    SAL=NA,
    depth=NA,
    lat=lat,
    long=lon,
    coalition="WQP",
    time_stamp=as.POSIXct(ActivityStartDate,format="%Y-%M-%d")
  ) %>% distinct(stn_id,time_stamp,.keep_all =TRUE)
