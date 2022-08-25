EnvTimeSeriesPoints <- function(data=data, x = time_stamp, y = env, color = site) {

  lims <- as.POSIXct(strptime(c("1992-01-01 24:00","2020-01-01 24:00"), format = "%Y-%m-%d %H:%M"))
  
  ggplot() + geom_point(data=data,aes(x = {{x}}, y = {{y}}, color={{color}}),size = 1.2) + 
    
    scale_x_datetime(limits = lims,
                     date_labels = "%d/%m/%Y",
                     expand = c(0,0),
                     date_breaks = "6 months") + 
    
    scale_fill_viridis(name="",discrete=T) +
    
    theme_dark() + 
    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(x="")
  
}
