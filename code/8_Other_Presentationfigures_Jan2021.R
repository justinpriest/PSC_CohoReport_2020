
####### Figures for Jan 11-13, 2021 PSC Meeting #####
# Libraries
library(correlation)
library(broom)
library(adfgcolors) # remotes::install_github("justinpriest/adfgcolors")
library(see)
library(mgcv)

### COPIED FROM RMD
source(here::here("code/1_US_data_import.R"))


##### DATA WEEKLY #####
tyee_weekly <- tyeecpue %>%
  mutate(week = statweek(Date)) %>%
  group_by(Year, week) %>%
  summarise(Tyee_cpue = mean(CohoCPUE)) %>% ungroup()

NassFW_weekly <- NassFW %>% 
  group_by(Year, week) %>% 
  summarise(Nass_coho = sum(CohoCatchDaily)) %>% ungroup()

troll_USboundary <- troll_boundary %>%
  filter(between(StatWeek, 27, 30),
         StatArea != 101-85, StatArea != 101-90, StatArea != 101-95) %>% # exclude terminal harvest areas
  group_by(Year, StatWeek) %>%
  summarise(UStrollCPUE = mean(CohoCPUE))
#101-40, 101-41, 101-45,

treepointharvest <- read_csv(here::here("data/SEAK_Coho_HarvestTreePoint_2000-2019.csv")) %>% 
  rename("Year" = `DOL Year`,
         "StatArea" = `Stat Area`,
         "GearCodeName" = `Gear Code and Name`,
         "StatWeek" = `Stat Week`,
         "TreePtHarv" = `Number Of Animals (sum)`) %>%
  mutate(Species = "coho salmon") %>%
  dplyr::select(Year, StatArea, GearCodeName, Species, StatWeek, TreePtHarv) %>%
  filter(between(StatWeek, 27, 29))




# Read in Hugh Smith data
# JTP: This ends up not helping at all. Consider removing 
hughsmith_esc <- read_csv(here::here("data/SEAK_Coho_HughSmithDailyEscapement_1982-2019.csv")) %>% 
  rename("Date" = `Obs Date`) %>% 
  mutate(Date = as_date(as.POSIXct(Date, format = "%m/%d/%Y", tz = "US/Alaska")),
         week = statweek(Date)) %>%
  group_by(Year, week) %>%
  summarise(HS_count = sum(Count))


# NBC Troll 
nbctroll <- read_csv(here::here("data/_PRIVATE_NBC_Coho_TrollHarvest2001-2019.csv")) %>%
  janitor::clean_names() %>%
  rename("Can_week" = "stat_week",
         "Year" = "year") %>%
  mutate(harvestdate = as_date(as.POSIXct(fishing_date, format = "%m/%d/%Y", tz = "America/Vancouver")),
         StatWeek = statweek(harvestdate),
         cohototal = coho_kept + coho_reld) %>% 
  filter(mgmt_area == 101 | mgmt_area == 1) %>%
  dplyr::select(Year, mgmt_area, harvestdate, StatWeek, subarea, 
                fe_effort, fe_hrs_fished, fe_set_no, cohototal) %>%
  mutate(fe_hrs_fished = replace_na(fe_hrs_fished, 10)) %>% # assume blanks are a standard 10-hr day
  group_by(Year, StatWeek) %>%
  summarise(NBCtrollcount = sum(cohototal),
            NBCtrolleffort = sum(fe_hrs_fished)) %>%
  mutate(NBCtrollCPUE = NBCtrollcount / NBCtrolleffort) %>%
  dplyr::select(Year, StatWeek, NBCtrollcount, NBCtrollCPUE)


# Put these indices  together  
indices <- tyee_weekly %>% 
  left_join(NassFW_weekly) %>% 
  left_join(troll_USboundary, by = c("Year" = "Year", "week" = "StatWeek")) %>%
  left_join(hughsmith_esc) %>% 
  left_join(nbctroll, by = c("Year" = "Year", "week" = "StatWeek")) %>%
  left_join(treepointharvest, by = c("Year" = "Year", "week" = "StatWeek"))




indices_2000 <- indices %>% 
  filter(Year >= 2000, between(week, 27, 29)) %>% 
  group_by(Year) %>% # Need this so that NAs appear when you lead (can't use next week's info this week!)
  mutate(Tyee_cpue_lead1 = lead(Tyee_cpue, 1),
         Nass_coho_lead1 = lead(Nass_coho, 1),
         NBCtrollCPUElead1 = lead(NBCtrollCPUE, 1)) %>% 
  ungroup() 



# This is the 1981-2019 data which will be used to show the relationship between CPUE early vs late
UStroll_cpue <- read_csv(here::here("data/SEAK_Coho_TrollFPD_1981-2019.csv"), 
                         guess_max = 84000) %>% #increased guess b/c of many blanks 
  rename("Gear" = `Gear Code`,
         "TripNum" = `Trip No`,
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
         Effort_boatdays = DaysFished * HoursPerDay / 13, # Effort is standardized to a 13 hour boat day
         CohoCPUE = CohoCatch / Effort_boatdays,
         TripNumber = paste0(Year, "-", TripNum)) %>% 
  dplyr::select(Year, TripNumber, SellDate, StatWeek, TrollArea, District, 
                StatArea, CohoCatch, Effort_boatdays, CohoCPUE)


############ END DATA IMPORT ################




# FIRST: Perform Gen Additive Model
# this assumes the the post week 37 blanks are true zeros
temptyee <- tyee_weekly %>% filter(Year >= 2000) %>% 
  mutate(Tyee_cpue = replace(Tyee_cpue, is.na(Tyee_cpue) & week >=37, 0)) # NAs at end of season threw off model

tyeegam <- mgcv::gam(Tyee_cpue ~ s(week) + s(Year, bs="re"), 
          data = temptyee)
#summary(tyeegam)
predtyee <- data.frame(Year = 2000, week = seq(25, 40))
predtyee$predcpue <- predict(tyeegam, newdata = predtyee)
predtyee


# Now same for Nass
nassgam <- mgcv::gam(Nass_coho ~ s(week) + s(Year, bs="re"), 
                     data = NassFW_weekly)
#summary(nassgam)
prednass <- data.frame(Year = 2000, week = seq(25, 38))
prednass$predcpue <- predict(nassgam, newdata = prednass)
prednass



  
## BACKGROUND PLOTS
tyeebgplot <- ggplot(tyee_weekly %>% filter(Year >= 2000), aes(x = week, y = Tyee_cpue, color = Year, group = Year)) +
  geom_line() +
  geom_line(data = predtyee, aes(x=week, y=predcpue), color = "orange", size = 1.5) +
  geom_vline(xintercept = 32, color = "red", size = 1.5) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(title = "Tyee CPUE by stat week", x = "Stat Week", y = "Coho CPUE") +
  theme_coho(base_family = "Arial", rotate_text = FALSE)

nassbgplot <- ggplot(NassFW_weekly, aes(x=week, y = Nass_coho, group = Year, color = Year)) + 
  geom_line() +
  geom_line(data = prednass, aes(x=week, y=predcpue), color = "orange", size = 1.5) +
  geom_vline(xintercept = 30, color = "red", size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Nass FW Catch by stat week", x = "Stat Week", y = "Coho Count") +
  theme_coho(base_family = "Arial", rotate_text = FALSE)

ustrollbgplot <- ggplot(troll_USboundary, 
       aes(x=StatWeek, y = UStrollCPUE, group = Year, color = Year)) +
  geom_point() + 
  geom_line() +
  labs(title = "US Boundary Area Troll CPUE by Stat Week", x = "Stat Week", y = "Troll Coho CPUE") +
  theme_coho(base_family = "Arial", rotate_text = FALSE)


nbctrollbgplot <- ggplot(nbctroll %>% filter(between(StatWeek, 27, 30)), 
       aes(x=StatWeek, y = NBCtrollCPUE, group = Year, color = Year)) +
  geom_point() + 
  geom_line() +
  labs(title = "NBC Troll CPUE by Stat Week", x = "Stat Week", y = "Troll Coho CPUE") +
  theme_coho(base_family = "Arial", rotate_text = FALSE)


# ggsave(tyeebgplot, filename = here::here("output/pres_tyeebg.png"), width = 6.5, height = 4, units = "in")
# ggsave(nassbgplot, filename = here::here("output/pres_nassbg.png"), width = 6.5, height = 4, units = "in")
# ggsave(ustrollbgplot, filename = here::here("output/pres_ustrollbg.png"), width = 6.5, height = 4, units = "in")
# ggsave(nbctrollbgplot, filename = here::here("output/pres_nbctrollbg.png"), width = 6.5, height = 4, units = "in")





# CORRELATION PLOTS

indices_2000 %>%
  dplyr::select(UStrollCPUE, NBCtrollCPUE, Tyee_cpue, Nass_coho, HS_count, TreePtHarv) %>%
  correlation() %>%
  summary(redundant = TRUE) %>% #redundant = TRUE
  plot(type = "tile", show_values = TRUE, digits = 2, show_p = TRUE) + 
  scale_fill_adfg(palette = "redblue", discrete = FALSE, limits = c(-1,1)) + 
  theme_coho(base_family = "Arial")


indices_2000 %>%
  dplyr::select(UStrollCPUE, NBCtrollCPUE, Tyee_cpue_lead1, Nass_coho_lead1) %>%
  correlation() %>% 
  summary(redundant = TRUE) %>% #redundant = TRUE
  plot(type = "tile", show_values = TRUE, digits = 2, show_p = TRUE) + 
  scale_fill_adfg(palette = "redblue", discrete = FALSE, limits = c(-1,1)) + 
  theme_coho(base_family = "Arial")








mod_tyee_linear <- lm(UStrollCPUE ~ Tyee_cpue, data = indices_2000)
summary(mod_tyee_linear)

tyeetroll <- indices_2000 %>%
  ggplot(aes(x = Tyee_cpue, y = UStrollCPUE, fill = Year)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(pch = 21, color="black", size = 4) +
  scale_fill_adfg("glacier", discrete = FALSE) +
  labs(x = "Tyee CPUE", y = "US Troll FPD CPUE", 
       title = "Tyee CPUE vs US Troll CPUE, Stat Weeks 27–29") +
  theme_coho(base_family = "Arial", rotate_text = FALSE)
tyeetroll


tyeetroll_alt <- indices_2000 %>%
  ggplot(aes(x = Tyee_cpue, y = UStrollCPUE, fill = week)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(pch = 21, color="black", size = 4) +
  scale_fill_adfg("sitkasunset", discrete = FALSE) +
  labs(x = "Tyee CPUE", y = "US Troll FPD CPUE", 
       title = "Tyee CPUE vs US Troll CPUE, Stat Weeks 27–29") +
  theme_coho(base_family = "Arial", rotate_text = FALSE)
tyeetroll_alt

#ggsave(tyeetroll, filename = here::here("output/pres_tyeemodel.png"), width = 5, height = 4, units = "in")
#ggsave(tyeetroll_alt, filename = here::here("output/pres_tyeemodel_alt.png"), width = 5, height = 4, units = "in")




# Not included but this shows us the mean cumulative CPUE by week
tyee_weekly %>% 
  filter(Year >= 2000) %>%
  group_by(Year) %>%
  mutate(cumsum = cumsum(Tyee_cpue)) %>% 
  #ungroup() %>%
  group_by(week) %>%
  summarise(meancumm = mean(cumsum, na.rm = TRUE)) %>%
  mutate(weeklymean = lead(meancumm, 1) - meancumm) 




# Tried a negative binomial model but doesn't really work well yet
tyeemod <- mgcv::gam(UStrollCPUE ~ Tyee_cpue + week + s(Year, bs="re"), family = "nb", link = "log", 
                     data = indices_2000) 
summary(tyeemod)

new.df <- crossing(week = c(27), Year = 2018, Tyee_cpue = seq(from=0, to = 200))
predtyeenb <- predict(tyeemod, newdata=new.df, se=T, type="link") # Std. error on log-scale
predtyeenbdf <- data.frame(Tyee_cpue = seq(from = 0, to = 0.3, length=nrow(new.df)), 
                           lwr = (predtyeenb$fit - 1.96*predtyeenb$se.fit),  # Back-transformed lower confidence limit, 
                           predval = (predtyeenb$fit), 
                           upr = (predtyeenb$fit + 1.96*predtyeenb$se.fit))  # Back-transformed upper confidence limit

ggplot(predtyeenbdf, aes(x=Tyee_cpue, y = predval)) + 
  geom_line() + 
  theme_minimal()







