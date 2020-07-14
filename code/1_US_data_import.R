# Data Import - Alaska

# Justin Priest & Ryan Whitmore
# July 2020
# justin.priest@alaska.gov
# Ryan.Whitmore@dfo-mpo.gc.ca

# Libraries
library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(ggsidekick)
library(patchwork)
library(extrafont)
# font_import()  #this only needs to be run once
loadfonts(device="win") #change for mac users

source(here::here("code/functions.R"))


#######################
###### LOAD DATA ###### 


##### SEAK Historical Harvest #####

harvest_historic <- read_csv(here::here("data/SEAK_Coho_HistoricalCommHarvest.csv")) %>%
  dplyr::select(Year:CohoTotalCommHarvest_Count)


wildproportion <- read_csv(here::here("data/SEAK_Coho_HistoricalCommHarvest.csv")) %>%
  dplyr::select(1:5) %>%
  filter(Year >= 1980) %>%
  mutate(wildpercent = 1 - (CohoHatcheryOriginHarvest_Count / CohoTotalCommHarvest_Count)) %>%
  dplyr::select(Year, wildpercent)






SEAK_smolt <- read_csv(here::here("data/SEAK_Coho_SmoltProduction_INPROGRESS.csv")) %>%
  dplyr::select(1:7)

SEAK_smolt %>% pivot_longer(-SmoltYear)






##### US Harvest Totals #####

chilkat_harvest <- read_csv(here::here("data/SEAK_Coho_ChilkatHarvest.csv")) %>%
  dplyr::select(1:4) 

chilkat_harvest









###### Troll CPUE ######


# Troll catches and CPUE for the Northern boundary area, districts 101/102 only, stat weeks 27-30 
troll_boundary <- read_csv(here::here("data/SEAK_Coho_TrollFPD_2000-2019.csv")) %>% 
  dplyr::select(Year:Chum) %>% 
  rename("Gear" = `Gear Code`,
         "SellDate" = `Sell Date`, 
         "StatWeek" = `Stat Week`, 
         "TrollArea" = `Troll Area`,
         "StatArea" = `Stat Area`,
         "DaysFished" = `FPD-Days Fished`,
         "HoursPerDay" = `Hours per Day`,
         "CohoCatch" = "Coho") %>% 
  filter(Gear == 15, Year < 2020) %>% # remove hand troll (power troll only)
  mutate(SellDate = as_date(as.POSIXct(SellDate, format = "%m/%d/%Y", tz = "US/Alaska")),
         District = as.factor(District),
         Effort_boatdays = DaysFished * HoursPerDay / 13,
         CohoCPUE = CohoCatch / Effort_boatdays) %>%
  dplyr::select(Year, SellDate, StatWeek, TrollArea, District, StatArea, CohoCatch, Effort_boatdays, CohoCPUE)





###### Tyee Test Fishery ######

tyeecpue <- read_csv(here::here("data/NBC_Coho_TyeeTestFisheryDaily_1956-2019.csv")) %>%
  pivot_longer(-DATE, "Year") %>%
  rename("CohoCPUE" = "value") %>%
  mutate(Date = as.Date(paste0(DATE, "-", Year), format = "%d-%B-%Y"),
         Std_date = as.Date(paste0(DATE, "-", 2020), format = "%d-%B-%Y"),
         Year = as.numeric(Year)) %>% 
  dplyr::select(Year, Date, Std_date, CohoCPUE) %>%
  arrange(Year) %>%
  filter(Year <= 2019)




###### Nass River FW Catch #####

NassFW <- read_csv(here::here("data/NBC_Coho_NassRiverFWCatch_2000-2019.csv")) %>%
  dplyr::select(Date:`2019`) %>%
  pivot_longer(-Date, "Year") %>%
  rename("CohoCumCatch" = "value") %>%
  mutate(Std_date = as.Date(Date, format = "%d-%B-%Y") + years(2000), #add 2000 yrs to acct for two digit year
         Year = as.numeric(Year),
         Date = ymd(paste0(Year, "-",month(Std_date), "-", day(Std_date))),
         week = statweek(Date)) %>%
  arrange(Date) %>% group_by(Year) %>%
  mutate(CohoCatchDaily = CohoCumCatch - lag(CohoCumCatch, 1),
         CohoCatchDaily = replace(CohoCatchDaily, Date=="2019-06-16", 30), # manually replace value
         CohoCatchDaily = replace_na(CohoCatchDaily, 0)) %>%
  dplyr::select(Year, Date, Std_date, week, CohoCatchDaily, CohoCumCatch) %>%
  ungroup()



  
