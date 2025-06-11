# Package ID: knb-lter-ntl.1.63 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current.
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


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/63/0ff1fd13116d6097376e3745194cdc5f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "ph",     
                 "phair",     
                 "alk",     
                 "dic",     
                 "tic",     
                 "doc",     
                 "toc",     
                 "no3no2",     
                 "no2",     
                 "nh4",     
                 "totnf",     
                 "totnuf",     
                 "totpf",     
                 "totpuf",     
                 "drsif",     
                 "brsif",     
                 "brsiuf",     
                 "tpm",     
                 "totnuf_wslh",     
                 "no3no2_wslh",     
                 "nh4_wslh",     
                 "kjdl_n_wslh",     
                 "totpuf_wslh",     
                 "drp_wslh",     
                 "drsif_wslh",     
                 "flagdepth",     
                 "flagph",     
                 "flagphair",     
                 "flagalk",     
                 "flagdic",     
                 "flagtic",     
                 "flagdoc",     
                 "flagtoc",     
                 "flagno3no2",     
                 "flagno2",     
                 "flagnh4",     
                 "flagtotnf",     
                 "flagtotnuf",     
                 "flagtotpf",     
                 "flagtotpuf",     
                 "flagdrsif",     
                 "flagbrsif",     
                 "flagbrsiuf",     
                 "flagtpm",     
                 "flagtotnuf_wslh",     
                 "flagno3no2_wslh",     
                 "flagnh4_wslh",     
                 "flagkjdl_n_wslh",     
                 "flagtotpuf_wslh",     
                 "flagdrp_wslh",     
                 "flagdrsif_wslh"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$ph)=="factor") dt1$ph <-as.numeric(levels(dt1$ph))[as.integer(dt1$ph) ]               
if (class(dt1$ph)=="character") dt1$ph <-as.numeric(dt1$ph)
if (class(dt1$phair)=="factor") dt1$phair <-as.numeric(levels(dt1$phair))[as.integer(dt1$phair) ]               
if (class(dt1$phair)=="character") dt1$phair <-as.numeric(dt1$phair)
if (class(dt1$alk)=="factor") dt1$alk <-as.numeric(levels(dt1$alk))[as.integer(dt1$alk) ]               
if (class(dt1$alk)=="character") dt1$alk <-as.numeric(dt1$alk)
if (class(dt1$dic)=="factor") dt1$dic <-as.numeric(levels(dt1$dic))[as.integer(dt1$dic) ]               
if (class(dt1$dic)=="character") dt1$dic <-as.numeric(dt1$dic)
if (class(dt1$tic)=="factor") dt1$tic <-as.numeric(levels(dt1$tic))[as.integer(dt1$tic) ]               
if (class(dt1$tic)=="character") dt1$tic <-as.numeric(dt1$tic)
if (class(dt1$doc)=="factor") dt1$doc <-as.numeric(levels(dt1$doc))[as.integer(dt1$doc) ]               
if (class(dt1$doc)=="character") dt1$doc <-as.numeric(dt1$doc)
if (class(dt1$toc)=="factor") dt1$toc <-as.numeric(levels(dt1$toc))[as.integer(dt1$toc) ]               
if (class(dt1$toc)=="character") dt1$toc <-as.numeric(dt1$toc)
if (class(dt1$no3no2)=="factor") dt1$no3no2 <-as.numeric(levels(dt1$no3no2))[as.integer(dt1$no3no2) ]               
if (class(dt1$no3no2)=="character") dt1$no3no2 <-as.numeric(dt1$no3no2)
if (class(dt1$no2)=="factor") dt1$no2 <-as.numeric(levels(dt1$no2))[as.integer(dt1$no2) ]               
if (class(dt1$no2)=="character") dt1$no2 <-as.numeric(dt1$no2)
if (class(dt1$nh4)=="factor") dt1$nh4 <-as.numeric(levels(dt1$nh4))[as.integer(dt1$nh4) ]               
if (class(dt1$nh4)=="character") dt1$nh4 <-as.numeric(dt1$nh4)
if (class(dt1$totnf)=="factor") dt1$totnf <-as.numeric(levels(dt1$totnf))[as.integer(dt1$totnf) ]               
if (class(dt1$totnf)=="character") dt1$totnf <-as.numeric(dt1$totnf)
if (class(dt1$totnuf)=="factor") dt1$totnuf <-as.numeric(levels(dt1$totnuf))[as.integer(dt1$totnuf) ]               
if (class(dt1$totnuf)=="character") dt1$totnuf <-as.numeric(dt1$totnuf)
if (class(dt1$totpf)=="factor") dt1$totpf <-as.numeric(levels(dt1$totpf))[as.integer(dt1$totpf) ]               
if (class(dt1$totpf)=="character") dt1$totpf <-as.numeric(dt1$totpf)
if (class(dt1$totpuf)=="factor") dt1$totpuf <-as.numeric(levels(dt1$totpuf))[as.integer(dt1$totpuf) ]               
if (class(dt1$totpuf)=="character") dt1$totpuf <-as.numeric(dt1$totpuf)
if (class(dt1$drsif)=="factor") dt1$drsif <-as.numeric(levels(dt1$drsif))[as.integer(dt1$drsif) ]               
if (class(dt1$drsif)=="character") dt1$drsif <-as.numeric(dt1$drsif)
if (class(dt1$brsif)=="factor") dt1$brsif <-as.numeric(levels(dt1$brsif))[as.integer(dt1$brsif) ]               
if (class(dt1$brsif)=="character") dt1$brsif <-as.numeric(dt1$brsif)
if (class(dt1$brsiuf)=="factor") dt1$brsiuf <-as.numeric(levels(dt1$brsiuf))[as.integer(dt1$brsiuf) ]               
if (class(dt1$brsiuf)=="character") dt1$brsiuf <-as.numeric(dt1$brsiuf)
if (class(dt1$tpm)=="factor") dt1$tpm <-as.numeric(levels(dt1$tpm))[as.integer(dt1$tpm) ]               
if (class(dt1$tpm)=="character") dt1$tpm <-as.numeric(dt1$tpm)
if (class(dt1$totnuf_wslh)=="factor") dt1$totnuf_wslh <-as.numeric(levels(dt1$totnuf_wslh))[as.integer(dt1$totnuf_wslh) ]               
if (class(dt1$totnuf_wslh)=="character") dt1$totnuf_wslh <-as.numeric(dt1$totnuf_wslh)
if (class(dt1$no3no2_wslh)=="factor") dt1$no3no2_wslh <-as.numeric(levels(dt1$no3no2_wslh))[as.integer(dt1$no3no2_wslh) ]               
if (class(dt1$no3no2_wslh)=="character") dt1$no3no2_wslh <-as.numeric(dt1$no3no2_wslh)
if (class(dt1$nh4_wslh)=="factor") dt1$nh4_wslh <-as.numeric(levels(dt1$nh4_wslh))[as.integer(dt1$nh4_wslh) ]               
if (class(dt1$nh4_wslh)=="character") dt1$nh4_wslh <-as.numeric(dt1$nh4_wslh)
if (class(dt1$kjdl_n_wslh)=="factor") dt1$kjdl_n_wslh <-as.numeric(levels(dt1$kjdl_n_wslh))[as.integer(dt1$kjdl_n_wslh) ]               
if (class(dt1$kjdl_n_wslh)=="character") dt1$kjdl_n_wslh <-as.numeric(dt1$kjdl_n_wslh)
if (class(dt1$totpuf_wslh)=="factor") dt1$totpuf_wslh <-as.numeric(levels(dt1$totpuf_wslh))[as.integer(dt1$totpuf_wslh) ]               
if (class(dt1$totpuf_wslh)=="character") dt1$totpuf_wslh <-as.numeric(dt1$totpuf_wslh)
if (class(dt1$drp_wslh)=="factor") dt1$drp_wslh <-as.numeric(levels(dt1$drp_wslh))[as.integer(dt1$drp_wslh) ]               
if (class(dt1$drp_wslh)=="character") dt1$drp_wslh <-as.numeric(dt1$drp_wslh)
if (class(dt1$drsif_wslh)=="factor") dt1$drsif_wslh <-as.numeric(levels(dt1$drsif_wslh))[as.integer(dt1$drsif_wslh) ]               
if (class(dt1$drsif_wslh)=="character") dt1$drsif_wslh <-as.numeric(dt1$drsif_wslh)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagph)!="factor") dt1$flagph<- as.factor(dt1$flagph)
if (class(dt1$flagphair)!="factor") dt1$flagphair<- as.factor(dt1$flagphair)
if (class(dt1$flagalk)!="factor") dt1$flagalk<- as.factor(dt1$flagalk)
if (class(dt1$flagdic)!="factor") dt1$flagdic<- as.factor(dt1$flagdic)
if (class(dt1$flagtic)!="factor") dt1$flagtic<- as.factor(dt1$flagtic)
if (class(dt1$flagdoc)!="factor") dt1$flagdoc<- as.factor(dt1$flagdoc)
if (class(dt1$flagtoc)!="factor") dt1$flagtoc<- as.factor(dt1$flagtoc)
if (class(dt1$flagno3no2)!="factor") dt1$flagno3no2<- as.factor(dt1$flagno3no2)
if (class(dt1$flagno2)!="factor") dt1$flagno2<- as.factor(dt1$flagno2)
if (class(dt1$flagnh4)!="factor") dt1$flagnh4<- as.factor(dt1$flagnh4)
if (class(dt1$flagtotnf)!="factor") dt1$flagtotnf<- as.factor(dt1$flagtotnf)
if (class(dt1$flagtotnuf)!="factor") dt1$flagtotnuf<- as.factor(dt1$flagtotnuf)
if (class(dt1$flagtotpf)!="factor") dt1$flagtotpf<- as.factor(dt1$flagtotpf)
if (class(dt1$flagtotpuf)!="factor") dt1$flagtotpuf<- as.factor(dt1$flagtotpuf)
if (class(dt1$flagdrsif)!="factor") dt1$flagdrsif<- as.factor(dt1$flagdrsif)
if (class(dt1$flagbrsif)!="factor") dt1$flagbrsif<- as.factor(dt1$flagbrsif)
if (class(dt1$flagbrsiuf)!="factor") dt1$flagbrsiuf<- as.factor(dt1$flagbrsiuf)
if (class(dt1$flagtpm)!="factor") dt1$flagtpm<- as.factor(dt1$flagtpm)
if (class(dt1$flagtotnuf_wslh)!="factor") dt1$flagtotnuf_wslh<- as.factor(dt1$flagtotnuf_wslh)
if (class(dt1$flagno3no2_wslh)!="factor") dt1$flagno3no2_wslh<- as.factor(dt1$flagno3no2_wslh)
if (class(dt1$flagnh4_wslh)!="factor") dt1$flagnh4_wslh<- as.factor(dt1$flagnh4_wslh)
if (class(dt1$flagkjdl_n_wslh)!="factor") dt1$flagkjdl_n_wslh<- as.factor(dt1$flagkjdl_n_wslh)
if (class(dt1$flagtotpuf_wslh)!="factor") dt1$flagtotpuf_wslh<- as.factor(dt1$flagtotpuf_wslh)
if (class(dt1$flagdrp_wslh)!="factor") dt1$flagdrp_wslh<- as.factor(dt1$flagdrp_wslh)
if (class(dt1$flagdrsif_wslh)!="factor") dt1$flagdrsif_wslh<- as.factor(dt1$flagdrsif_wslh)

chem = dt1 %>%
  filter(year4 >= 2004,
         lakeid == "TR" | 
         lakeid == "SP" |
         lakeid == "CR" |
         lakeid == "CB" |
         lakeid == "TB",
         daynum >= 100,
         daynum <= 300)

chem_sum = chem %>%
  group_by(year4, lakeid) %>%
  summarize(doc = mean(doc, na.rm = TRUE)) %>%
  ungroup()

ggplot(chem_sum, aes(x = year4, y = doc, color = lakeid)) +
  geom_point() +
  scale_y_continuous(transform = "log10")


# Package ID: knb-lter-ntl.350.8 Cataloging System:https://pasta.edirepository.org.
# Data set title: Cascade Project at North Temperate Lakes LTER Core Data Carbon 1984 - 2023.
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Jim Kitchell - University of Wisconsin 
# Data set creator:  Jonathan Cole - Cary Institute of Ecosystem Studies 
# Data set creator:  Michael Pace - University of Virginia 
# Contact:  Stephen Carpenter -  University of Wisconsin  - steve.carpenter@wisc.edu
# Contact:  Michael Pace -  University of Virginia  - pacem@virginia.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/350/8/4130f043c28c43abe2c74d194e301e5d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "lakename",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "depth_id",     
                 "tpc",     
                 "tpn",     
                 "DIC_mg",     
                 "DIC_uM",     
                 "air_pco2",     
                 "water_pco2",     
                 "doc",     
                 "absorbance"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$lakename)!="factor") dt1$lakename<- as.factor(dt1$lakename)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$depth)!="factor") dt1$depth<- as.factor(dt1$depth)
if (class(dt1$depth_id)!="factor") dt1$depth_id<- as.factor(dt1$depth_id)
if (class(dt1$tpc)=="factor") dt1$tpc <-as.numeric(levels(dt1$tpc))[as.integer(dt1$tpc) ]               
if (class(dt1$tpc)=="character") dt1$tpc <-as.numeric(dt1$tpc)
if (class(dt1$tpn)=="factor") dt1$tpn <-as.numeric(levels(dt1$tpn))[as.integer(dt1$tpn) ]               
if (class(dt1$tpn)=="character") dt1$tpn <-as.numeric(dt1$tpn)
if (class(dt1$DIC_mg)=="factor") dt1$DIC_mg <-as.numeric(levels(dt1$DIC_mg))[as.integer(dt1$DIC_mg) ]               
if (class(dt1$DIC_mg)=="character") dt1$DIC_mg <-as.numeric(dt1$DIC_mg)
if (class(dt1$DIC_uM)=="factor") dt1$DIC_uM <-as.numeric(levels(dt1$DIC_uM))[as.integer(dt1$DIC_uM) ]               
if (class(dt1$DIC_uM)=="character") dt1$DIC_uM <-as.numeric(dt1$DIC_uM)
if (class(dt1$air_pco2)=="factor") dt1$air_pco2 <-as.numeric(levels(dt1$air_pco2))[as.integer(dt1$air_pco2) ]               
if (class(dt1$air_pco2)=="character") dt1$air_pco2 <-as.numeric(dt1$air_pco2)
if (class(dt1$water_pco2)=="factor") dt1$water_pco2 <-as.numeric(levels(dt1$water_pco2))[as.integer(dt1$water_pco2) ]               
if (class(dt1$water_pco2)=="character") dt1$water_pco2 <-as.numeric(dt1$water_pco2)
if (class(dt1$doc)=="factor") dt1$doc <-as.numeric(levels(dt1$doc))[as.integer(dt1$doc) ]               
if (class(dt1$doc)=="character") dt1$doc <-as.numeric(dt1$doc)
if (class(dt1$absorbance)=="factor") dt1$absorbance <-as.numeric(levels(dt1$absorbance))[as.integer(dt1$absorbance) ]               
if (class(dt1$absorbance)=="character") dt1$absorbance <-as.numeric(dt1$absorbance)

# Convert Missing Values to NA for non-dates

dt1$tpc <- ifelse((trimws(as.character(dt1$tpc))==trimws("NA")),NA,dt1$tpc)               
suppressWarnings(dt1$tpc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tpc))==as.character(as.numeric("NA"))),NA,dt1$tpc))
dt1$tpn <- ifelse((trimws(as.character(dt1$tpn))==trimws("NA")),NA,dt1$tpn)               
suppressWarnings(dt1$tpn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$tpn))==as.character(as.numeric("NA"))),NA,dt1$tpn))
dt1$DIC_mg <- ifelse((trimws(as.character(dt1$DIC_mg))==trimws("NA")),NA,dt1$DIC_mg)               
suppressWarnings(dt1$DIC_mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC_mg))==as.character(as.numeric("NA"))),NA,dt1$DIC_mg))
dt1$DIC_uM <- ifelse((trimws(as.character(dt1$DIC_uM))==trimws("NA")),NA,dt1$DIC_uM)               
suppressWarnings(dt1$DIC_uM <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC_uM))==as.character(as.numeric("NA"))),NA,dt1$DIC_uM))
dt1$air_pco2 <- ifelse((trimws(as.character(dt1$air_pco2))==trimws("NA")),NA,dt1$air_pco2)               
suppressWarnings(dt1$air_pco2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$air_pco2))==as.character(as.numeric("NA"))),NA,dt1$air_pco2))
dt1$water_pco2 <- ifelse((trimws(as.character(dt1$water_pco2))==trimws("NA")),NA,dt1$water_pco2)               
suppressWarnings(dt1$water_pco2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$water_pco2))==as.character(as.numeric("NA"))),NA,dt1$water_pco2))
dt1$doc <- ifelse((trimws(as.character(dt1$doc))==trimws("NA")),NA,dt1$doc)               
suppressWarnings(dt1$doc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$doc))==as.character(as.numeric("NA"))),NA,dt1$doc))
dt1$absorbance <- ifelse((trimws(as.character(dt1$absorbance))==trimws("NA")),NA,dt1$absorbance)               
suppressWarnings(dt1$absorbance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$absorbance))==as.character(as.numeric("NA"))),NA,dt1$absorbance))


cascade = dt1 %>%
  filter(year4 >= 2004,
         !is.na(doc),
         #filter to upper mixed layer depth
         depth_id == "-1" | depth_id == "1") %>%
  group_by(year4, lakeid) %>%
  summarize(doc = mean(doc, na.rm = TRUE)) %>%
  ungroup() %>%
  #get rid of lakes we're not studying
  filter(!lakeid == "E",
         !lakeid == "W", 
         !lakeid == "Ward")

combo_doc = rbind(cascade, chem_sum) %>%
  mutate(lakeid = case_when(lakeid == "L" ~ "Paul",
                            lakeid == "R" ~ "Peter",
                            lakeid == "T" ~ "Tuesday",
                            lakeid == "M" ~ "Crampton",
                            lakeid == "CB" ~ "CrystalBog",
                            lakeid == "TB" ~ "TroutBog",
                            lakeid == "TR" ~ "Trout",
                            lakeid == "SP" ~ "Sparkling",
                            lakeid == "CR" ~ "Crystal")) %>%
  #Adding some NAs to break up the lines for plotting
  add_row(lakeid = "Tuesday", year4 = 2020, doc = NA) %>%
  add_row(lakeid = "Crampton", year4 = 2015, doc = NA) %>%
  #Adding in the NEON data from June each year, fuck NEON data formatting
  add_row(lakeid = "Crampton", year4 = 2015, doc = 4.43) %>%
  add_row(lakeid = "Crampton", year4 = 2017, doc = 4.36) %>%
  add_row(lakeid = "Crampton", year4 = 2018, doc = 3.98) %>%
  add_row(lakeid = "Crampton", year4 = 2019, doc = 4.42) %>%
  add_row(lakeid = "Crampton", year4 = 2020, doc = 5.00) %>%
  add_row(lakeid = "Crampton", year4 = 2021, doc = 4.10) %>%
  add_row(lakeid = "Crampton", year4 = 2022, doc = 3.81) %>%
  add_row(lakeid = "Crampton", year4 = 2023, doc = 4.34) %>%
  add_row(lakeid = "LittleRock", year4 = 2017, doc = 4.34) %>%
  add_row(lakeid = "LittleRock", year4 = 2018, doc = 3.30) %>%
  add_row(lakeid = "LittleRock", year4 = 2019, doc = 3.80) %>%
  add_row(lakeid = "LittleRock", year4 = 2020, doc = 4.57) %>%
  add_row(lakeid = "LittleRock", year4 = 2021, doc = 3.36) %>%
  add_row(lakeid = "LittleRock", year4 = 2022, doc = 3.21) %>%
  add_row(lakeid = "LittleRock", year4 = 2023, doc = 3.84) %>%
  mutate(lakeid = factor(lakeid, levels = c("TroutBog",
                                            "CrystalBog",
                                            "Trout",
                                            "Sparkling", 
                                            "Crystal",
                                            "Crampton", 
                                            "LittleRock",
                                            "Peter",
                                            "Paul",
                                            "Tuesday")))


ggplot(combo_doc, aes(x = year4, y = doc, color = lakeid)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_y_continuous(transform = "log10") +
  theme_minimal() + xlab("") + 
  theme(legend.title = element_blank()) +
  ylab("Dissolved Organic Carbon (mg/L)") +
  scale_color_manual(values = c("#006699", "#3399cc", 
                                "#669900", "#99cc33", "#ccee66",
                                "#990066", "#cc3399", 
                                "#ff6600", "#ff9900", "#ffcc00"))
  
# ANOMALY
sum_doc = combo_doc %>%
  group_by(lakeid) %>%
  summarize(mean_doc = mean(doc, na.rm = TRUE)) %>%
  ungroup()

anomaly_doc = left_join(combo_doc, sum_doc, by = c("lakeid")) %>%
  mutate(doc_anomaly = doc - mean_doc)

ggplot(anomaly_doc, aes(x = year4, y = doc_anomaly, color = lakeid)) +
  geom_line(linewidth = 0.75) + geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  theme_minimal() + xlab("") + 
  theme(legend.title = element_blank()) +
  ylab("DOC Anomaly (mg/L)") +
  scale_color_manual(values = c("#006699", "#3399cc", 
                                "#669900", "#99cc33", "#ccee66",
                                "#990066", "#cc3399", 
                                "#ff6600", "#ff9900", "#ffcc00"))



