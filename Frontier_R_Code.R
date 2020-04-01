
require(frontier)
require(plm)


################################################
################################################
#          Dropping SWF fire due to no crew data
join.final.noSWF <- join.final %>% filter(FireID != "SWF107")


sumstat <- join.final.noSWF %>%
  select(
    `Fire Age` = FireDayActual,
    `Held Perimeter` = Held_Perimeter, `Fire Size (ha)` = Area_ha,
    `Crew` = crewtotal, `Air` = airtotal,
    `RH` = rh_max,`Max Temperature` = temp_max,`Max Wind Speed` = agric_wind_max,
    `% Held River` = perc_river, `Relative Age` = relativedays ) %>%
summarise_each(funs(mean, sd, min, max)) %>%
  # Move summary stats to columns
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  # Set order of summary statistics 
  select(variable, mean, sd, min, max) %>%
  # Round all numeric variables to one decimal point
  mutate_each(funs(round(., 1)), -variable)

sumstat
write.table(sumstat, file = "sumstats.xls", sep = ",", quote = FALSE, row.names = F)


################################################
################################################
# add data set with information about its panel structure
library( "plm" )
firePanel <- pdata.frame( join.final.noSWF, c( "FireID", "FireDayActual" ) )

require(ExPanDaR)
ExPanD(df = firePanel, cs_id = "FireID", ts_id = "FireDayActual")

###############################################
###############################################
############ Create Correlation Plot
# install.packages("corrplot")
library("corrplot")
library("corrgram")
my.num.data <- join.final[, sapply(join.final, is.numeric)] %>%
  select(Daily_Held_noneg, crew_d, air_d,
         lnair, lncrew, rh_lag,  rh_max, agric_wind_max, agric_wind_avg, wind_speed, wind_speed_max,
         perc_river, relativedays, Timber, FireDayActual, rain_total.y, rain_lag, dewpoint_max, temp_max)
corrplot(corrgram(my.num.data), method = "ellipse")


# Cobb-Douglas production frontier
# cobbDouglas <- sfa( log( Daily_Held_noneg ) ~ log( crewtotal ) + log( airtotal ) + log( equiptotal ),
#                     data = join.final )


# via Katuwal Following Battese (1997) we create dummy variable for zero-resources such
# that crew_d ¼ 1 if crew ¼ 0 and crew_d ¼ 0 if crew>0. Log of the resources variables
# are created using: ln(crew) = ln(Max(crew, crew_d)).
cobbDouglasFULL <- sfa(log( Daily_Held_noneg ) ~ crew_d + air_d +lncrew + lnair
                       + rh_lag + temp_max + agric_wind_max +  perc_river +
                  Timber , data = join.final.noSWF )
summary(cobbDouglasFULL) #remember! dummy vars have opposite interpretation, 1 = not used
fitted( cobbDouglasFULL )

cobb.crew <- sfa(log( Daily_Held_noneg ) ~ crew_d + lncrew + 
                         + rh_lag + temp_max + agric_wind_max +  perc_river +
                         Timber , data = join.final.noSWF )
summary(cobb.crew) #remember! dummy vars have opposite interpretation, 1 = not used
fitted( cobb.crew )

cobb.air <- sfa(log( Daily_Held_noneg ) ~  air_d + lnair + rh_lag + temp_max + agric_wind_max +  perc_river +
                   Timber , data = join.final.noSWF )
summary(cobb.air) #remember! dummy vars have opposite interpretation, 1 = not used
fitted( cobb.air )


# translogFULL <- frontierQuad(log( Daily_Held_noneg ) ~ crew_d + air_d  
#                 + lnair + lncrew + lnair2 + lncrew2 + aircrew
#                 + rh_lag + temp_max + agric_wind_max +  perc_river +
#                   Timber + relativedays, data = join.final.noSWF)
# summary(translogFULL)

#Test to see which is better functional form
#Sandeep's notes: (need to somehow call parameters from model output) 
#test LL2 = 0, accum test LK2 = 0, accum, test LL*LK=0, accum
#Sandeep: Test for returns to scale:
#test LL + LK = 1, test LL2 + LK2 + 2*LL*LK = 0, accum
#Should I restrict model to impose CRS or DRTS?

lrtest(cobbDouglasFULL,translogFULL)
AIC(cobbDouglasFULL,translogFULL)


# Error Components Frontier (Battese & Coelli 1992)
# with time-invariant efficiencies
fireTimeInv <- sfa(log( Daily_Held_noneg ) ~ crew_d + air_d + log(pmax(crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
                    data = firePanel )
summary( fireTimeInv )
efficiencies( fireTimeInv )


# Error Components Frontier (Battese & Coelli 1992)
# with time-variant efficiencies
fireTimeVar <- sfa( log(Daily_Held_noneg) ~ crew_d + air_d + log(pmax(crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
                    data = firePanel, timeEffect = TRUE)
summary(fireTimeVar)
efficiencies(fireTimeVar)

# Technical Efficiency Effects Frontier (Battese & Coelli 1995)
# (efficiency effects model with intercept)
fireZ <- sfa(log( Daily_Held_noneg ) ~ crew_d + air_d 
             + lnair + lncrew
             | temp_max + agric_wind_max + rain_total.y + dewpoint_max, data = firePanel )
summary(fireZ)
efficiencies(fireZ)
fitted( fireZ )

sfacrew <- sfa(log( Daily_Held_noneg ) ~ crew_d + lncrew
             | temp_max + agric_wind_max + rain_total.y + dewpoint_max, data = firePanel )
summary(sfacrew)
efficiencies(sfacrew)
fitted(sfacrew)

sfaair<- sfa(log( Daily_Held_noneg ) ~ air_d + lnair 
               | temp_max + agric_wind_max + rain_total.y + dewpoint_max, data = firePanel )
summary(sfaair)
efficiencies(sfaair)
fitted(sfaair)

#############################################################
###################  CHECKING FOR COLLINERAITY using VIF ####
justair <- lm(log( Daily_Held_noneg ) ~ air_d 
             + lnair + temp_max + agric_wind_max + rain_total.y + dewpoint_max, data = join.final.noSWF )
justcrew <- lm(log( Daily_Held_noneg ) ~ crew_d
               + lncrew + temp_max + agric_wind_max + rain_total.y + dewpoint_max, data = join.final.noSWF )
both <- lm(log( Daily_Held_noneg ) ~ crew_d + air_d 
              + lnair + lncrew + temp_max + agric_wind_max + rain_total.y + dewpoint_max, data = join.final.noSWF )
library("car")
vif(justair)
vif(justcrew)
vif(both)


# 
# 
# fireZ.trans <- frontierQuad(log( Daily_Held_noneg ) ~ crew_d + air_d  
#              + lnair + lncrew + lnair2 + lncrew2 + aircrew 
#              | rh_lag + agric_wind_max + temp_max + rain_total.y
#              + perc_river + relativedays , data = firePanel )
# summary(fireZ.trans)
# efficiencies(fireZ.trans)
# fitted( fireZ.trans )
# firePanel$fittedZ.trans <- fitted( fireZ.trans, asInData = TRUE )
# plot(firePanel$fittedZ.trans)


#rejecting the null means the larger/ non-nested model is a better fit
#rejecting the null means the inefficiency model is superior to cobb-doug
#lower information criteria indicates better model fit
lrtest(fireZ, fireZ.trans)
AIC(fireZ, fireZ.trans)

lrtest(fireZ.trans)
lrtest(fireZ)


# Technical Efficiency Effects Frontier (Battese & Coelli 1995)
# (efficiency effects model without intercept)
# Added max and dummy variables to match Battese 1997 specification for frequent 0 observations
fireZ2 <- sfa( log( Daily_Held_noneg ) ~ crew_d + air_d + log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d))|
                 rh_lag + temp_max + agric_wind_max +  perc_river + Timber + relativedays - 1, data = firePanel)
summary( fireZ2 )
efficiencies( fireZ2 )

# plot(log(Daily_Held_noneg) ~ crew_d + air_d + log(pmax( crewtotal, crew_d)) + log(pmax(airtotal, air_d)),
#      type = "p", col = "red", ylim = c(0,1), data = firePanel)
# x.seq <- seq(0, 1, by = 0.01)
# lines( log(Daily_Held_noneg) ~ x.seq, col = "blue", data=firePanel)





