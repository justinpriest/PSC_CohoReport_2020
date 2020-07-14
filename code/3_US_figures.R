# Figures for US Part of Report

# Justin Priest
# July 2020
# justin.priest@alaska.gov


# Load cleaned up data and import US only dat
source(here::here("code/1_US_data_import.R"))


# Unfortunately the 1981-2019 file is confidential and cannot be shared
troll_cpue <- read_csv(here::here("data/SEAK_Coho_TrollFPD_1981-2019.csv"), 
                       guess_max = 84000) %>% #increased guess b/c of many blanks 
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
          Effort_boatdays = DaysFished * HoursPerDay / 13, # Effort is standardized to a 13 hour boat day
          CohoCPUE = CohoCatch / Effort_boatdays) %>% 
   dplyr::select(Year, SellDate, StatWeek, TrollArea, District, StatArea, CohoCatch, Effort_boatdays, CohoCPUE)

troll_cpue



# Figure 2 is comm harvest over time
# Figure 3 is CAN comm harvest over time
# Figure 4 is Sport harvest over time









##### KTN & HUGH SMITH INDICES ####

# Figure 7a
(Fig7a <- ggplot(data = ktn_index_hist,
                 aes(x=Year, y = surveycount)) + 
   geom_bar(stat="identity", fill="lightgray", color="black") +
   geom_hline(yintercept=c(4250, 8500)) +
   scale_y_continuous(breaks = seq(from = 0, to = 18000, by = 2000),
                      labels = scales::comma) +
   #scale_x_continuous(breaks = seq(from=1982, to=2019, by = 2)) + #rm this to make a whole plot
   expand_limits(x = c(1982, 2019), y = c(0, 18000)) +
   labs(y = "Spawners") +
   annotate("text", x= 2000, y=18000, label = "Ketchikan Index (Survey)", size = 4.5) +
   theme_coho(base_family = "Arial", rotate_text = TRUE) +
   theme(axis.text.x = element_blank(), axis.title.x = element_blank()))

# Figure 7b
(Fig7b <- ggplot(data = hs,
                 aes(x=Year, y = Escapement_Weir)) + 
    geom_bar(stat="identity", fill="lightgray", color="black") +
    geom_hline(yintercept=c(500, 1600)) +
    scale_y_continuous(breaks = seq(from = 0, to = 4000, by = 500),
                       labels = scales::comma) +
    scale_x_continuous(breaks = seq(from=1982, to=2019, by = 2)) +
    expand_limits(y = c(0, 4150)) +
    labs(x="Adult Return Year", y = "Spawners") +
    annotate("text", x= 2000, y=3750, label = 'atop("Hugh Smith Lake","(Weir/Mark-Recapture)")', parse=TRUE, size = 4.5) +
    theme_coho(base_family = "Arial", rotate_text = TRUE))

Fig7 <- Fig7a / Fig7b
ggsave(Fig7, filename = here::here("output/Fig7.png"), width = 6, height = 6, units = "in")







## Figure 18
## Troll and Wild abundance

troll_cpue %>% 
   filter(between(StatWeek, 28, 38)) %>% # only keep 28-38
   group_by(Year) %>%
   summarise(meanannualCPUE = mean(CohoCPUE, na.rm = TRUE)) %>%
   left_join(wildproportion) %>%
   mutate(annualwildCPUE = meanannualCPUE * wildpercent) %>%
   ggplot(aes(x=Year, y = annualwildCPUE)) +
   geom_line()




troll_cpue



