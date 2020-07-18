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


##### SEAK Harvest & Escapement #####

harvest_historic <- read_csv(here::here("data/SEAK_Coho_HistoricalCommHarvest.csv")) %>%
  dplyr::select(Year:CohoHatcheryOriginHarvest_Count) %>%
  rename("Hatchery" = "CohoHatcheryOriginHarvest_Count",
         "Totalcount" = "CohoTotalCommHarvest_Count") %>%
  mutate(Wild = Totalcount - Hatchery) %>%
  dplyr::select(Year, Wild, Hatchery)
harvest_historic

wildproportion <- read_csv(here::here("data/SEAK_Coho_HistoricalCommHarvest.csv")) %>%
  dplyr::select(Year:CohoHatcheryOriginHarvest_Count) %>%
  rename("Total" = "CohoTotalCommHarvest_Count",
         "Hatchery" = "CohoHatcheryOriginHarvest_Count") %>%
  mutate(Wild = Total - Hatchery) %>%
  filter(Year >= 1980) %>%
  mutate(wildpercent = Wild / Total) %>%
  dplyr::select(Year, Wild, Hatchery, wildpercent) 


trollharvest <- read_csv(here::here("data/SEAK_Coho_TrollHarvest_Wildvshatchery.csv")) %>% 
  dplyr::select(Year:`Other hatchery`) %>%
  mutate(Hatchery = `Alaska hatchery` + `Other hatchery`) %>%
  dplyr::select(-`Alaska hatchery`, -`Other hatchery`) %>%
  pivot_longer(cols = c(`Wild contribution`, "Hatchery"), names_to = "Source", values_to = "Harvest")


SEAK_escgoals <- read_csv(here::here("data/SEAK_Coho_EscGoals.csv")) %>% 
  dplyr::select(-CollectionType, -GoalType, - Comment)


SEAK_escape <- read_csv(here::here("data/SEAK_Coho_Escapement_1972-2019.csv")) %>%
  left_join(SEAK_escgoals, by = c("River" = "System"))


indic_harvest <- read_csv(here::here("data/SEAK_Coho_HarvestSourcesForIndicatorStocks1980-2019.csv")) %>%
  pivot_wider(names_from= Fishery_Type, values_from = Coho_Harvest_Count) %>%
  rename("AK_Gillnet" = `AK_Gillnet (Drift & Set)`) %>% 
  mutate(Other = AK_Gillnet + AK_Other + AK_Seine + AK_Sport + CAN_AllFisheries) %>%
  dplyr::select(-c(AK_Gillnet, AK_Other, AK_Seine, AK_Sport, CAN_AllFisheries)) 

indic_totalrun <- indic_harvest %>%
  left_join(SEAK_escape %>% 
              filter(River %in% c("Auke Creek", "Berners River", "Ford Arm Lake", "Hugh Smith Lake")) %>%
              dplyr::select(c(Year, River, Escapement_Count)),
            by = c("Year" = "Year", "River" = "River")) %>%
  pivot_longer(cols = c(AK_Troll, Other, Escapement_Count), names_to = "Fishery", values_to = "Count") %>%
  left_join(SEAK_escgoals, by = c("River" = "System")) %>%
  mutate(Fishery = factor(Fishery, levels = c("AK_Troll", "Other", "Escapement_Count")),
         Fishery = recode(Fishery, "AK_Troll" = "Alaska Troll"), 
         Fishery = recode(Fishery, "Escapement_Count" = "Escapement"))




chilkat_harvest <- read_csv(here::here("data/SEAK_Coho_ChilkatHarvest.csv")) %>%
  dplyr::select(Year:Coho_Harvest_Count) 


taku_harvest <- read_csv(here::here("data/SEAK_Coho_TakuHarvest.csv")) %>%
  dplyr::select(Year:Canadian) %>%
  mutate(Other = Seine + Gillnet + Sport + Inriver) %>%
  rename("Alaska Troll" = Troll) %>%
  dplyr::select(Year, `Alaska Troll`, Escapement, Other) %>%
  pivot_longer(cols = c(`Alaska Troll`, Other, Escapement), names_to = "Fishery", values_to = "Count") %>%
  mutate(Fishery = factor(Fishery, levels = c("Alaska Troll", "Other", "Escapement")))
taku_harvest

taku_harvest_can <- read_csv(here::here("data/SEAK_Coho_TakuHarvest.csv")) %>%
  dplyr::select(Year:Canadian) %>%
  mutate(`Other Alaska` = Seine + Gillnet + Sport + Inriver) %>%
  rename("Alaska Troll" = Troll,
         "Canada" = Canadian) %>%
  dplyr::select(Year, `Alaska Troll`, Escapement, `Other Alaska`, Canada) %>%
  pivot_longer(cols = c(`Alaska Troll`, `Other Alaska`, Canada, Escapement), 
               names_to = "Fishery", values_to = "Count") %>%
  mutate(Fishery = factor(Fishery, levels = c("Alaska Troll", "Canada", "Other Alaska", "Escapement")))


SEAK_sport <- read_csv(here::here("data/SEAK_Coho_SportHarvest.csv")) %>%
  dplyr::select(Year:Harvest_Count) 




##### Smolt Totals #####

SEAK_smolt <- read_csv(here::here("data/SEAK_Coho_SmoltProduction_INPROGRESS.csv")) %>%
  dplyr::select(1:7)

SEAK_smolt %>% pivot_longer(-SmoltYear)



SEAK_marsurv <- read_csv(here::here("data/SEAK_Coho_SmoltMarineSurvival_INPROGRESS.csv")) %>%
  pivot_longer(-ReturnYear, names_to = "River", values_to = "Survival") 



Auke_survival <- read_csv(here::here("data/SEAK_Coho_AukeCreekSurvival_1980-2018.csv")) %>%
  dplyr::select(SmoltYear:Survival_Jacks, -SmoltOutmigration)


##### Exploitation Rate #####
trollindex <- indic_totalrun %>%
  filter( !(River =="Berners River" & Year < 1989)) %>% # These years are incorrect, exclude
  dplyr::select(-EscapementGoal_Lower, -EscapementGoal_Upper) %>%
  group_by(Year, River) %>%
  mutate(freq = Count / sum(Count)) %>%
  ungroup() %>%
  filter(Fishery == "Alaska Troll") %>%
  dplyr::select(-Count) %>%
  rename("index" = "freq")

globalimpute(trollindex, Year_column="Year", outputprefix = "trollindex", 
             StreamName_column="River", Count_column = "index")
# this makes a dataframe called trollindex_imputed

trollindex <- trollindex_imputed %>%
  dplyr::select(-imputed) %>%
  pivot_wider(names_from = River, values_from = Count) %>%
  #calculate troll index from Leon's weighting (differnt weightings pre/post 1989)
  mutate(trollindex = ifelse(Year < 1989, (`Auke Creek` * 0.4) + (`Ford Arm Lake` * 0.2) + 
                               (`Hugh Smith Lake` * 0.4), 
                             (`Berners River` * 0.2) + (`Auke Creek` * 0.2) + (`Ford Arm Lake` * 0.2) + 
                               (`Hugh Smith Lake` * 0.4))) 
rm(trollindex_imputed)





##### Ricker Model #####
BernersRicker <- read_csv(here::here("data/SEAK_Coho_Berners_RickerHockeyFit.csv")) %>% 
  dplyr::select(S:hockey)

Berners_SR <- read_csv(here::here("data/SEAK_Coho_Berners_SpawnerRecruitActual.csv")) %>% 
  dplyr::select(Year:Recruits) %>%
  mutate(Decade = if_else(Year < 1990, "1980s", 
                          if_else(between(Year, 1990, 1999), "1990s",
                                  if_else(between(Year, 2000, 2010), "2000s","2010s")))) 



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








