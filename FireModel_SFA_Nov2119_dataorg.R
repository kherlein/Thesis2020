#ImportData
gis.data <- read.csv(file="GISData_Nov29.csv", header=TRUE, sep=",")
colnames(gis.data)[colnames(gis.data)=="CAPTURE_DA"] <- "weather_date"

all.weather <- read.csv(file="20174stations.csv")

status.data <- read.csv(file="firestatus_norepeats_nov29.csv", header=TRUE, sep=",")
colnames(status.data)[colnames(status.data)=="Date"] <- "weather_date"

air <- read.csv(file="Aircraft_HWF_2017.csv", header=TRUE, sep=",")
colnames(air)[colnames(air)=="DATE_WORKED"] <- "weather_date"
colnames(air)[colnames(air)=="FIRE_NUMBER"] <- "FireID"

equip <- read.csv(file="Equiprment_HWF_2017.csv", header=TRUE, sep=",")
colnames(equip)[colnames(equip)=="DATE_WORKED"] <- "weather_date"
colnames(equip)[colnames(equip)=="FIRE_NUMBER"] <- "FireID"

crew <- read.csv(file="Personnel_HWF_2017.csv", header=TRUE, sep=",")
colnames(crew)[colnames(crew)=="DATE_WORKED"] <- "weather_date"
colnames(crew)[colnames(crew)=="FIRE_NUMBER"] <- "FireID"

require(dplyr)
require(lubridate)
require(tidyr)

weather <- all.weather %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(FireID, weather_date) %>% 
  summarize(RH_max = max(relative_humidity), temp_max = max(dry_bulb_temperature),
            wind_max = max(wind_gust_kmh, na.rm = TRUE ), wind_speed_max=max(wind_speed_kmh),
            wind_speed = mean(wind_speed_kmh), agric_wind_max = max(wind_agric),agric_wind_avg = mean(wind_agric),
            dewpoint_max = max(dew_point), rain_total = sum(rain_mm)) %>%
  as.data.frame()

rh.max <- all.weather %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(FireID, weather_date) %>% 
  summarize(rh_max = max(relative_humidity), rain_total = sum(rain_mm) ) %>%
  as.data.frame()
require( "plm" )
rh.panel <- pdata.frame(rh.max, c("FireID", "weather_date"))
rh.lag <- rh.panel %>% 
  mutate(rh_lag=lag(rh.panel$rh_max, k=1, shift="time"))  %>% 
  mutate(rain_lag=lag(rh.panel$rain_total, k=1, shift="time"))  %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d")) %>% 
  as.data.frame()

gis.data.date <- gis.data %>%
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d"))


air.date.sum <- air %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d")) %>% 
  group_by(FireID, weather_date) %>% 
  summarize(airtotal = sum(TOTAL_AIRCRAFT_PER_DAY, na.rm = TRUE) ) %>%
   # spread(key= AIRCRAFT_CLASS,
   #       value=TOTAL_AIRCRAFT_PER_DAY) %>%
  # group_by(FireID, weather_date) %>% 
  as.data.frame()

unique(equip$EQUIPMENT_TYPE)

equip.date <- equip %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d")) %>% 
  group_by(FireID, weather_date) %>% 
  filter(EQUIPMENT_TYPE=="Truck -Water Tank Truck") %>% 
  select(c(FireID, weather_date, TOTAL_EQUIPMENT_PER_DAY)) %>% 
  as.data.frame()
colnames(equip.date)[colnames(equip.date)=="TOTAL_EQUIPMENT_PER_DAY"] <- "equiptotal"



unique(crew$POSITION)
unique(crew.date$POSITION)
crew.date.sum <- crew %>% 
  mutate(weather_date=as.Date(weather_date, format = "%Y-%m-%d")) %>% 
  group_by(FireID, weather_date) %>% 
  filter(FUNCTION=="Operations") %>% 
  summarize(crewtotal = sum(TOTAL_PERSONNEL_PER_DAY, na.rm = TRUE) ) %>%
  as.data.frame()




#checking that date conversion is correct
is.Date(gis.data.date$weather_date)
is.Date(weather$weather_date)

join.gis.weather <-  left_join(gis.data.date, weather, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.rh <- left_join(join.gis.weather, rh.lag, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.crew <- left_join(join.rh, crew.date.sum, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.air <- left_join(join.crew, air.date.sum, by = c("FireID" = "FireID", "weather_date" = "weather_date"))
join.final <- left_join(join.air, equip.date, by = c("FireID" = "FireID", "weather_date" = "weather_date")) %>% 
  mutate(crewtotal=ifelse(is.na(crewtotal),0,crewtotal)) %>% 
  mutate(equiptotal=ifelse(is.na(equiptotal),0,equiptotal)) %>%   mutate(airtotal=ifelse(is.na(airtotal),0,airtotal)) %>% 

  mutate(relativedays= FireDayActual/nogrowthday)

#changins 0's to 1's so that row doesn't get dropped from SFA
#Adding dummy variables for inputs
join.final <- join.final %>% 
  mutate(Daily_Held_noneg = ifelse(Daily_Held_Perim <= 0,1,Daily_Held_Perim)) %>% 
  mutate(air_d = ifelse(airtotal == 0,1,0)) %>% 
  mutate(crew_d = ifelse(crewtotal == 0,1,0)) %>% 
  mutate(equip_d = ifelse(equiptotal == 0,1,0)) %>% 
  mutate(crew2 = crewtotal^2) %>% 
  mutate(air2 = airtotal^2) %>% 
  mutate(lnair = log(pmax(airtotal, air_d))) %>% 
  mutate(lncrew = log(pmax( crewtotal, crew_d))) %>% 
  mutate(lnair2 = (0.5*log(pmax(airtotal, air_d))^2)) %>% 
  mutate(lncrew2 = (0.5*log(pmax( crewtotal, crew_d))^2)) %>% 
  mutate(aircrew = (log(pmax(airtotal, air_d)))*(log(pmax( crewtotal, crew_d))))

#Changin RH value for HWF252 day 1 because missing value was generated (from AB ag)
join.final[5, 31] = 79


#summary(join.final)
#Continue to Frontier_R_Code for analysis code
  
  
  