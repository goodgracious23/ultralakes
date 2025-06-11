# Package ID: knb-lter-ntl.18.35 Cataloging System:https://pasta.edirepository.org.
# Data set title: Minocqua Dam Daily Meteorological Data at North Temperate Lakes LTER 1978 - current.
# Data set creator:  Timothy Kratz - University of Wisconsin-Madison 
# Data set creator:   National Weather Service - NOAA 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/18/35/34ec9386a894ba332529349d24154333" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year4",     
                 "sampledate",     
                 "min_air_temp",     
                 "max_air_temp",     
                 "precip",     
                 "snow",     
                 "snow_depth",     
                 "data_status"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$min_air_temp)=="factor") dt1$min_air_temp <-as.numeric(levels(dt1$min_air_temp))[as.integer(dt1$min_air_temp) ]               
if (class(dt1$min_air_temp)=="character") dt1$min_air_temp <-as.numeric(dt1$min_air_temp)
if (class(dt1$max_air_temp)=="factor") dt1$max_air_temp <-as.numeric(levels(dt1$max_air_temp))[as.integer(dt1$max_air_temp) ]               
if (class(dt1$max_air_temp)=="character") dt1$max_air_temp <-as.numeric(dt1$max_air_temp)
if (class(dt1$precip)=="factor") dt1$precip <-as.numeric(levels(dt1$precip))[as.integer(dt1$precip) ]               
if (class(dt1$precip)=="character") dt1$precip <-as.numeric(dt1$precip)
if (class(dt1$snow)=="factor") dt1$snow <-as.numeric(levels(dt1$snow))[as.integer(dt1$snow) ]               
if (class(dt1$snow)=="character") dt1$snow <-as.numeric(dt1$snow)
if (class(dt1$snow_depth)=="factor") dt1$snow_depth <-as.numeric(levels(dt1$snow_depth))[as.integer(dt1$snow_depth) ]               
if (class(dt1$snow_depth)=="character") dt1$snow_depth <-as.numeric(dt1$snow_depth)
if (class(dt1$data_status)!="factor") dt1$data_status<- as.factor(dt1$data_status)


precip = dt1 %>% 
  mutate(doy = yday(sampledate)) %>%
  #limit to summer time (May 1 - Sept 15)
  filter(doy > 120,
         doy < 250) %>%
  group_by(year4) %>%
  summarize(summer_precip = sum(precip, na.rm = TRUE)) %>%
  ungroup() 

longterm_precip = mean(precip$summer_precip, na.rm = TRUE)


precip_anomaly = precip %>%
  mutate(ppt_anomaly = summer_precip - longterm_precip) %>%
  filter(year4 >= 2004) %>%
  #2024 data from cocorahs because the minocqua dam data set is not up to date with the 2024 data
  add_row(year4 = 2024, summer_precip = 431.29,
          ppt_anomaly = 431.29 - longterm_precip)

ggplot(precip_anomaly, aes(x = year4, y = ppt_anomaly)) + 
  geom_point(color = "#006699", size = 2) + 
  geom_line(color = "#006699", linewidth = 1) +
  theme_minimal() +
  xlab("") + ylab("Summer Precip Anomaly (mm)") +
  # ggtitle("Summer Precipitation Anomaly from 1978-2024") +
  geom_hline(yintercept = 0)



# Package ID: knb-lter-ntl.32.31 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Ice Duration - Trout Lake Area 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/32/31/22dfd43d5e56e7d828c13317c7c7a9d1" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year",     
                 "lastopen",     
                 "ice_on",     
                 "lastice",     
                 "ice_off",     
                 "duration",     
                 "comments"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year)=="factor") dt1$year <-as.numeric(levels(dt1$year))[as.integer(dt1$year) ]               
if (class(dt1$year)=="character") dt1$year <-as.numeric(dt1$year)                                   
# attempting to convert dt1$lastopen dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1lastopen<-as.Date(dt1$lastopen,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$lastopen != "",]) == length(tmp1lastopen[!is.na(tmp1lastopen)])){dt1$lastopen <- tmp1lastopen } else {print("Date conversion failed for dt1$lastopen. Please inspect the data and do the date conversion yourself.")}                                                                    

# attempting to convert dt1$ice_on dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1ice_on<-as.Date(dt1$ice_on,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$ice_on != "",]) == length(tmp1ice_on[!is.na(tmp1ice_on)])){dt1$ice_on <- tmp1ice_on } else {print("Date conversion failed for dt1$ice_on. Please inspect the data and do the date conversion yourself.")}                                                                    

# attempting to convert dt1$lastice dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1lastice<-as.Date(dt1$lastice,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$lastice != "",]) == length(tmp1lastice[!is.na(tmp1lastice)])){dt1$lastice <- tmp1lastice } else {print("Date conversion failed for dt1$lastice. Please inspect the data and do the date conversion yourself.")}                                                                    

# attempting to convert dt1$ice_off dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1ice_off<-as.Date(dt1$ice_off,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$ice_off != "",]) == length(tmp1ice_off[!is.na(tmp1ice_off)])){dt1$ice_off <- tmp1ice_off } else {print("Date conversion failed for dt1$ice_off. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$duration)=="factor") dt1$duration <-as.numeric(levels(dt1$duration))[as.integer(dt1$duration) ]               
if (class(dt1$duration)=="character") dt1$duration <-as.numeric(dt1$duration)
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)


ice = dt1 %>% 
  filter(year >= 2004,
         !lakeid == "AL",
         !lakeid == "BM") %>%
  mutate(ice_on_doy = yday(ice_on),
         ice_off_doy = yday(ice_off))

avg_ice = ice %>%
  group_by(year) %>%
  summarize(avg_duration = mean(duration, na.rm = TRUE),
            avg_iceon = mean(ice_on_doy, na.rm = TRUE),
            avg_iceoff = mean(ice_off_doy, na.rm = TRUE)) %>%
  ungroup()

ggplot(avg_ice, aes(x = year, y = avg_duration)) +
  geom_point() + geom_line() +
  theme_minimal() + xlab("") + ylab("Days of Ice Cover")

ggplot(avg_ice, aes(x = year, y = avg_iceoff)) +
  geom_point() + geom_line() +
  theme_minimal() + xlab("") + ylab("Ice Off Date")



