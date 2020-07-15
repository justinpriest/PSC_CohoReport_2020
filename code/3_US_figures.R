# Figures for US Part of Report

# Justin Priest
# July 2020
# justin.priest@alaska.gov


# Load cleaned up data and import US only data
source(here::here("code/1_US_data_import.R"))


# Unfortunately the 1981-2019 file is confidential and cannot be shared
# Read it in here to prevent sourcing issues in the main data import file
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


harvest_historic %>%
   filter(Year > 1900) %>%
   pivot_longer(-Year, names_to = "Source", values_to = "Count") %>%
   mutate(Count = Count / 1000000) %>%
   ggplot(aes(x = Year, y = Count, fill = Source)) + 
   geom_col(color = "black") + 
   scale_x_continuous(breaks = seq(from=1900, to=2020, by = 10)) +
   scale_y_continuous(breaks = seq(from=0, to=5.5, by = 0.5)) +
   scale_fill_manual(values = c("white", "gray")) + 
   labs(y = "Number of fish (Millions)") + 
   theme_coho(base_family = "Arial") + 
   theme(legend.position=c(.2,.85), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1.5,"line"))


# Figure 3 is CAN comm harvest over time
# Need data from Ryan


# Figure 4 is Sport harvest over time
SEAK_sport %>%
   ggplot(aes(x = Year, y = Harvest_Count, fill = Fishery)) +
   geom_col(color = "black") + 
   scale_x_continuous(breaks = seq(from=1977, to=2019, by = 2)) +
   scale_y_continuous(labels = comma) +
   scale_fill_manual(values = c("white", "gray")) + 
   labs(y = "Number of fish") + 
   theme_coho(base_family = "Arial") + 
   theme(legend.position=c(.2,.85), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line"))


# Figure 5 is escapement over time
(Fig5a <- create_figure5("Auke Creek", setbreaks = c(0, 500, 1000, 1500)) + 
   annotate("text", x = 2000, y = 1500, label = "Auke Creek (Weir)", size = 3.5)) 

(Fig5b <- create_figure5("Montana Creek", setbreaks = c(0, 500, 1000, 1500, 2000, 2500), minyear = 1981) + 
   annotate("text", x = 2000, y = 2550, label = "Montana Creek (Survey)", size = 3.5))

(Fig5c <- create_figure5("Peterson Creek", setbreaks = c(0, 200, 400, 600), minyear = 1981) + 
   annotate("text", x = 2000, y = 630, label = "Peterson Creek (Survey)", size = 3.5))

(Fig5d <- create_figure5("Berners River", setbreaks = c(0, 5000, 10000, 15000, 20000, 25000), minyear = 1982) + 
   annotate("text", x = 2000, y = 26000, label = "Berners River (Survey)", size = 3.5) + 
   labs(y = "Spawners"))

(Fig5e <- create_figure5("Chilkat River", setbreaks = c(0, 50000, 100000, 150000, 200000), minyear = 1987) + 
   annotate("text", x = 2000, y = 210000, label = "Chilkat River (Mark-recapture / Expanded Survey)", size = 3.5))

(Fig5f <- create_figure5("Taku River", setbreaks = c(0, 50000, 100000, 150000, 200000), minyear = 1987, blank_x = FALSE) + 
   annotate("text", x = 2000, y = 230000, label = "Taku River (Mark-recapture)", size = 3.5))

Fig5 <- Fig5a / Fig5b / Fig5c / Fig5d / Fig5e / Fig5f
ggsave(Fig5, filename = here::here("output/Fig5.png"), width = 6, height = 9, units = "in")


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



