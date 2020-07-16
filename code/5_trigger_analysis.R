# Boundary Area Closure Trigger Analysis

# Justin Priest & Ryan Whitmore
# July 2020
# justin.priest@alaska.gov
# Ryan.Whitmore@dfo-mpo.gc.ca

# Libraries
library("correlation")
library("lme4")
library("forecast")


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
  filter(StatArea != 101-85, StatArea != 101-90, StatArea != 101-95) %>% # exclude terminal harvest areas
  group_by(Year, StatWeek) %>%
  summarise(trollCPUE = mean(CohoCPUE))
#101-40, 101-41, 101-45,


# Read in Hugh Smith data
# JTP: This ends up not helping at all. Consider removing soon
hughsmith_esc <- read_csv(here::here("data/SEAK_Coho_HughSmithDailyEscapement_1982-2019.csv")) %>% 
  rename("Date" = `Obs Date`) %>% 
  mutate(Date = as_date(as.POSIXct(Date, format = "%m/%d/%Y", tz = "US/Alaska")),
         week = statweek(Date)) %>%
  group_by(Year, week) %>%
  summarise(HS_count = sum(Count))
  


# Put these indices  together  
indices <- tyee_weekly %>% 
  left_join(NassFW_weekly) %>% 
  left_join(troll_USboundary, by = c("Year" = "Year", "week" = "StatWeek")) %>%
  left_join(hughsmith_esc)
# Originally this had FPD data 1981-2019 but this was restricted. May see vestigial references to pre-2000 data


##### PRELIM PLOTTING & SUMMARIES #####

ggplot(tyeecpue, aes(x = Std_date, y = CohoCPUE, color = Year, group = Year)) +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0, 10))

ggplot(tyee_weekly, aes(x = week, y = Tyee_cpue, color = Year, group = Year)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 11))

ggplot(NassFW_weekly, aes(x=week, y = Nass_coho, group = Year, color = Year)) + 
  geom_line()

ggplot(troll_USboundary %>% filter(StatWeek <40, Year > 1999), 
       aes(x=StatWeek, y = trollCPUE, group = Year, color = Year)) +
  geom_point() + 
  geom_line()
  #geom_smooth(se=FALSE)



# Look at whether the effort, totals, or samples have changed over time
troll_boundary %>%
#  mutate(stddate = as_date(paste0("2020/", month(SellDate), "/", day(SellDate))),
#         preJuly25 = stddate <= as_date("2020-07-25")) %>% # This section can set up filter to exclude >7/25
  filter(StatWeek <= 29, # exclude week 30, based on treaty language
         StatArea != 101-85, StatArea != 101-90, StatArea != 101-95) %>%
  group_by(Year) %>%
  summarise(meanCPUE = mean(CohoCPUE),
            totalEffort = sum(Effort_boatdays),
            totalCaught = sum(CohoCatch),
            numsamples = length(Effort_boatdays))



##### CORRELATIONS ######

cor(indices$Nass_coho, indices$Tyee_cpue, use="complete.obs")

cor(indices$trollCPUE, indices$Tyee_cpue, use="complete.obs")
cor(indices$trollCPUE, lead(indices$Tyee_cpue, 1), use="complete.obs")
cor(indices$trollCPUE, lead(indices$Tyee_cpue, 2), use="complete.obs")

cor(indices$trollCPUE, indices$Nass_coho, use="complete.obs")
cor(indices$trollCPUE, lead(indices$Nass_coho, 1), use="complete.obs")
cor(indices$trollCPUE, lead(indices$Nass_coho, 2), use="complete.obs")
# The Tyee Test Fishery has a moderate correlation with the SEAK FPD data (Districts 101/102). 
# There is a difference between including pre-2000 data (worse correlation earlier).
# Using the post 2000 data, using a lag of 1 week is optimal


# The correlation btwn Tyee & SEAK troll changed pre- & post 2000. These data aren't included here
# but were found using FPD 1981-2000 and 2000-2019
indices %>% 
  filter(Year >= 2000) %>% 
  mutate(Tyee_cpue_lead1 = lead(Tyee_cpue, 1)) %>%
  dplyr::select(-Year, -week) %>%
  correlation()


indices %>% 
  filter(Year >= 2000, week <= 30) %>% 
  group_by(Year) %>% # Need this so that NAs appear when you lead (can't use next week's info this week!)
  mutate(Tyee_cpue_lead1 = lead(Tyee_cpue, 1),
         Nass_coho_lead1 = lead(Nass_coho, 1)) %>% 
  ungroup() %>%
  dplyr::select(-Year, -week) %>% 
  correlation()

# Correlation of rho=0.53 between troll CPUE & Nass FW (lead one week; i.e., next week's Nass catch)
# Correlation of rho=0.48 between troll CPUE & Tyee Test fishery (lead one week; i.e., next week's Tyee CPUE)


##############

indices_2000 <- indices %>% 
  filter(Year >= 2000, between(week, 27, 30)) %>% 
  group_by(Year) %>% # Need this so that NAs appear when you lead (can't use next week's info this week!)
  mutate(Tyee_cpue_lead1 = lead(Tyee_cpue, 1),
         Nass_coho_lead1 = lead(Nass_coho, 1)) %>% 
  ungroup() 
  #pivot_longer(-c(Year, week), "Fishery") 

tyeelead <- indices_2000 %>%
  ggplot(aes(x = trollCPUE, y = Tyee_cpue_lead1, color = Year)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(size = 2) +
  labs(x = "Troll FPD CPUE (Districts 101/102)", y = "Tyee CPUE, one week lead", 
       title = "Troll CPUE vs following week Tyee CPUE, Stat Weeks 27–30; rho=0.48") +
  theme_coho(base_family = "Arial", rotate_text = FALSE)
tyeelead
#ggsave(tyeelead, filename = here::here("output/TRIGGER_tyee_corrplot_lead.png"), width = 6.5, height = 4, units = "in")

nasslead <- indices_2000 %>%
  ggplot(aes(x = trollCPUE, y = Nass_coho_lead1, color = Year)) +
  geom_smooth(method = "lm", color = "black") + 
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Troll FPD CPUE (Districts 101/102)", y = "Nass Catch, one week lead", 
       title = "Troll CPUE vs following week Nass catch, Stat Weeks 27–30; rho=0.53") +
  theme_coho(base_family = "Arial", rotate_text = FALSE)
nasslead
#ggsave(nasslead, filename = here::here("output/TRIGGER_nass_corrplot_lead.png"), width = 6.5, height = 4, units = "in")






#############
# Simple modeling
indices_2000
summary(lm(trollCPUE ~ HS_count, data = indices_2000)) # Hugh Smith isn't a sig indicator of troll CPUE
summary(lm(trollCPUE ~ Nass_coho, data = indices_2000)) # 
summary(lm(trollCPUE ~ Tyee_cpue, data = indices_2000)) # 
summary(lm(trollCPUE ~ Nass_coho + Tyee_cpue, data = indices_2000)) # much collinearity, but improves fit
summary(lm(trollCPUE ~ Nass_coho_lead1, data = indices_2000)) # fairly good indicator
summary(lm(trollCPUE ~ Tyee_cpue_lead1, data = indices_2000)) # not as good as Nass lead1

summary(lm(trollCPUE ~ Nass_coho_lead1 + week, data = indices_2000)) 
plot(residuals(lm(trollCPUE ~ Nass_coho_lead1 + week, data = indices_2000)))
qqnorm(indices_2000$trollCPUE)
qqline(indices_2000$trollCPUE)
checkresiduals(lm(trollCPUE ~ Nass_coho_lead1 + week, data = indices_2000)) # from package "forecast"



# Autocorrelation
acf(indices_2000$trollCPUE, na.action = na.exclude, main = "Autocorr - Troll CPUE")
acf(indices_2000$Nass_coho, na.action = na.exclude)
acf(indices_2000$Tyee_cpue, na.action = na.exclude)

# Just to check. This includes all stat weeks but for a longer time frame
# 
acf((indices %>% filter(Year > 1990))$trollCPUE, na.action = na.exclude, main = "Autocorr - Troll CPUE")

pacf(indices_2000$trollCPUE, na.action = na.exclude)






