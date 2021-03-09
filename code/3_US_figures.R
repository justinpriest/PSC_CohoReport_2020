# Figures for US Part of Report

# Justin Priest
# January 2021
# justin.priest@alaska.gov


# Load cleaned up data and import US only data
source(here::here("code/1_US_data_import.R"))



# Unfortunately the 1981-2019 file is confidential and cannot be shared
# Read it in here to prevent sourcing issues in the main data import file
troll_cpue <- read_csv(here::here("data/SEAK_Coho_TrollFPD_1981-2019.csv"), 
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

troll_cpue



#### FIGURE 1 - MAP - Not created here ####
# The basemap was created in the RProject "GIS_Shapefiles" using shapefiles from Canada GeoBase
# This was then finalized and touched up in PowerPoint for final, manual label placement



# Figure 2 is comm harvest over time

US_Fig2 <- harvest_historic %>%
   filter(Year > 1900) %>%
   pivot_longer(-Year, names_to = "Source", values_to = "Count") %>%
   mutate(Count = Count / 1000000) %>%
   ggplot(aes(x = Year, y = Count, fill = Source)) + 
   geom_col(color = "black") + 
   scale_x_continuous(breaks = seq(from=1900, to=2020, by = 10)) +
   scale_y_continuous(breaks = seq(from=0, to=5.5, by = 0.5)) +
   scale_fill_manual(values = c("white", "gray")) + 
   labs(y = "Number of fish (Millions)") + 
   theme_crisp(base_family = "Arial") + 
   theme(legend.position=c(.2,.85), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line"))
#ggsave(US_Fig2, filename = here::here("output/US_Fig2.png"), width = 6.5, height = 4, units = "in")


# Figure 3 is CAN comm harvest over time
# Need data from Ryan


###### Figure 4 ######
# Sport harvest over time
US_Fig4 <- SEAK_sport %>%
   ggplot(aes(x = Year, y = Harvest_Count, fill = Fishery)) +
   geom_col(color = "black") + 
   scale_x_continuous(breaks = seq(from=1977, to=2019, by = 2)) +
   scale_y_continuous(labels = comma) +
   scale_fill_manual(values = c("white", "gray")) + 
   labs(y = "Number of Coho Salmon") + 
   theme_crisp(base_family = "Arial") + 
   theme(legend.position=c(.2,.85), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line"))
#ggsave(US_Fig4, filename = here::here("output/US_Fig4.png"), width = 6.5, height = 4, units = "in")


###### Figure 5 ######
# Inside rivers escapement
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

(Fig5f <- create_figure5("Taku River", setbreaks = c(0, 50000, 100000, 150000, 200000), 
                         minyear = 1987, blank_x = FALSE) + 
   annotate("text", x = 2000, y = 230000, label = "Taku River (Mark-recapture)", size = 3.5))

US_Fig5 <- Fig5a / Fig5b / Fig5c / Fig5d / Fig5e / Fig5f # This is Patchwork notation to make a stacked figure
#ggsave(US_Fig5, filename = here::here("output/US_Fig5.png"), width = 6, height = 9, units = "in")
rm(Fig5a, Fig5b, Fig5c, Fig5d, Fig5e, Fig5f)


###### Figure 6 ######
# Sitka streams escapement
Fig6a <- create_figure5("Sitka Survey Index", setbreaks = seq(from=0, to=3000, by=500), minyear = 1982) +
   annotate("text", x = 2000, y = 3000, label = "Sitka Index (Survey)", size = 3.5) + 
   labs(y = "Spawners")

Fig6b <- create_figure5("Ford Arm Lake", setbreaks = seq(from=0, to=8000, by=1000), 
               minyear = 1982, blank_x = FALSE) +
   annotate("text", x = 2000, y = 8000, label = "Ford Arm Creek (Weir/Mark-recapture)", size = 3.5) + 
   labs(y = "Spawners")

US_Fig6 <- Fig6a / Fig6b
#ggsave(US_Fig6, filename = here::here("output/US_Fig6.png"), width = 6, height = 6, units = "in")
rm(Fig6a, Fig6b)


###### Figure 7 ######
# Ketchikan streams escapement
Fig7a <- create_figure5("Ketchikan Survey Index", setbreaks = seq(from=0, to=18000, by=2000), minyear = 1987) +
   annotate("text", x = 2000, y = 16000, label = "Ketchikan Index (Survey)", size = 3.5) + 
   labs(y = "Spawners")

Fig7b <- create_figure5("Hugh Smith Lake", setbreaks = seq(from=0, to=4000, by=500), 
                        minyear = 1982, blank_x = FALSE) +
   annotate("text", x = 2000, y = 4000, label = "Hugh Smith Lake (Weir/Mark-recapture)", size = 3.5) + 
   labs(y = "Spawners")

US_Fig7 <- Fig7a / Fig7b
#ggsave(US_Fig7, filename = here::here("output/US_Fig7.png"), width = 6, height = 6, units = "in")
rm(Fig7a, Fig7b)


###### Figure 8 ######
# Yakutat streams escapement
Fig8a <- create_figure5("Tawah Creek", setbreaks = seq(from=0, to=10000, by=2500), minyear = 1972) +
   annotate("text", x = 1995, y = 10000, label = "Tawah Creek", size = 3.5) + 
   scale_x_continuous(breaks = seq(from = 1972, to = 2019, by = 2)) 

Fig8b <- create_figure5("Situk River", setbreaks = seq(from=0, to=40000, by=10000), minyear = 1972) +
   annotate("text", x = 1995, y = 40000, label = "Situk River", size = 3.5) + 
   scale_x_continuous(breaks = seq(from = 1972, to = 2019, by = 2)) +
   labs(y = "Spawners")

Fig8c <- create_figure5("Tsiu River", setbreaks = seq(from=0, to=60000, by=20000), minyear = 1972, blank_x = FALSE) +
   annotate("text", x = 1995, y = 60000, label = "Tsiu River", size = 3.5) +
   scale_x_continuous(breaks = seq(from = 1972, to = 2019, by = 2)) 

US_Fig8 <- Fig8a / Fig8b / Fig8c
#ggsave(US_Fig8, filename = here::here("output/US_Fig8.png"), width = 6, height = 6, units = "in")
rm(Fig8a, Fig8b, Fig8c)


###### Figure 9 ######
# Indicator Stock Run Reconstruction
Fig9a <- create_harvestfig(river = "Auke Creek", blank_x = TRUE, setbreaks = seq(from=0, to=3000, by=500)) + 
   annotate("text", x = 2000, y = 3300, label = "Auke Creek", size = 3.5) + 
   labs(y = "") +
   theme(legend.position=c(.88,.82), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line")) 

# Changed to remove this. Dec 2020
#Fig9b <- create_harvestfig(river = "Berners River", blank_x = TRUE, setbreaks = seq(from=0, to=75000, by=10000)) + 
#   labs(y = "") +
#   annotate("text", x = 2000, y = 75000, label = "Berners River", size = 3.5) 

Fig9b <- create_harvestfig(river = "Ford Arm Lake", blank_x = TRUE, setbreaks = seq(from=0, to=16000, by=2000)) + 
   annotate("text", x = 2000, y = 17000, label = "Ford Arm Creek", size = 3.5) 

Fig9c <- create_harvestfig(river = "Hugh Smith Lake", blank_x = FALSE, setbreaks = seq(from=0, to=9000, by=1000)) + 
   labs(y = "") +
   annotate("text", x = 2000, y = 10000, label = "Hugh Smith Lake", size = 3.5) 

US_Fig9 <- Fig9a / Fig9b / Fig9c 
#ggsave(US_Fig9, filename = here::here("output/US_Fig9.png"), width = 6.5, height = 8, units = "in")
rm(Fig9a, Fig9b, Fig9c)



###### Figure 10 ######
# Northern Rivers Run Reconstruction

# Removed because this doesn't have Can harvest and is duplicated in other figure
# Fig10a <- taku_harvest %>% 
#    mutate(River = "Taku River") %>%
#    left_join(SEAK_escgoals, by = c("River" = "System")) %>%
#    create_harvestfig(river = "Taku River", setbreaks = seq(from=0, to=400000, by=100000), minyear = 1987) +
#    expand_limits(x=1987) +
#    labs(y = "") +
#    annotate("text", x = 2002, y = 400000, label = "Taku River", size = 3.5) 


Fig10a <- SEAK_escape %>% filter(River == "Chilkat River", Year >= 1987) %>% 
   dplyr::select(-c(Count_Type, Expansion, EscapementGoal_Lower, EscapementGoal_Upper)) %>%
   left_join(chilkat_harvest %>%
                pivot_wider(names_from= Fishery_Type, values_from = Coho_Harvest_Count) %>%
                mutate(Other = Seine + `Drift Gillnet` + `Sport (marine)` + `Sport (freshwater)` + Subsistence) %>%
                dplyr::select(Year, River, Troll, Other)) %>%
   rename("Escapement" = "Escapement_Count",
          "Alaska Troll" = "Troll") %>%
   pivot_longer(cols = c(Escapement, `Alaska Troll`, Other), names_to = "Fishery", values_to = "Count") %>%
   mutate(Fishery = factor(Fishery, levels = c("Alaska Troll", "Other", "Escapement"))) %>%
   left_join(SEAK_escgoals, by = c("River" = "System")) %>%
   create_harvestfig(river = "Chilkat River", setbreaks = seq(from=0, to=400000, by=100000)) +
   annotate("text", x = 2002, y = 400000, label = "Chilkat River", size = 3.5) +
   theme(legend.position=c(0.88, 0.82), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line")) 


Fig10b <- create_harvestfig(river = "Berners River", 
                  setbreaks = seq(from=0, to=60000, by=20000), blank_x = FALSE, minyear = 1987) +
   annotate("text", x = 2002, y = 75000, label = "Berners River", size = 3.5) 
US_Fig10 <- Fig10a / Fig10b 
#ggsave(US_Fig10, filename = here::here("output/US_Fig10.png"), width = 6.5, height = 6, units = "in")
rm(Fig10a, Fig10b)


###### Figure 11 ######
# Escapement Correlation

US_Fig11 <- SEAK_escape %>%
   filter(River %in% c("Berners River", "Chilkat River", "Hugh Smith Lake"), Year >= 1982) %>%
   dplyr::select(Year, River, Escapement_Count) %>%
   group_by(River) %>%
   mutate(Scale = scale(Escapement_Count)) %>%
   #mutate(Scale = (Escapement_Count - mean(Escapement_Count, na.rm = TRUE)) / sd(Escapement_Count, na.rm = TRUE))
   ggplot(aes(x = Year, y = Scale, color = River, linetype = River)) +
   geom_line(size = 1.25) +
   geom_point(size = 2) +
   scale_x_continuous(breaks = seq(from=1980, to= 2019, by =2)) +
   scale_color_manual(values = c("#6b6b6b", "#c77512", "black")) +
   scale_linetype_manual(values = c("solid", "solid", "solid")) + # Berners used to be dashed
   expand_limits(y = c(-1.7, 3.3)) +
   labs(y = expression("Scaled Escapement (X-"*mu*" / "*sigma*")")) +
   theme_crisp(base_family = "Arial") +
   theme(legend.position=c(.5,.06), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(2.5,"line"), legend.direction="horizontal") 
#ggsave(US_Fig11, filename = here::here("output/US_Fig11.png"), width = 6.5, height = 4, units = "in")



###### Figure 12 ######
# Taku River Harvest
US_Fig12 <- taku_harvest_can %>%
   mutate(River = "Taku River") %>%
   left_join(SEAK_escgoals, by = c("River" = "System")) %>%
   create_harvestfig(river = "Taku River", 
                     setbreaks = seq(from=0, to=350000, by=50000), blank_x = FALSE, minyear = 1987) +
   scale_x_continuous(breaks = seq(from=1987, to=2019, by = 2)) +
   scale_fill_manual(values = c("white", "black", "lightblue", "gray")) +
   annotate("text", x = 2002, y = 360000, label = "Taku River", size = 3.5) +
   annotate(geom = "curve", x = 1988, y = 175000, xend = 1987, yend = 75000, 
            curvature = 0.1, arrow = arrow(length = unit(2, "mm"))) +
   annotate(geom = "curve", x = 1990, y = 175000, xend = 1991, yend = 145000, 
            curvature = -0.1, arrow = arrow(length = unit(2, "mm"))) +
   annotate(geom = "text", x = 1988.5, y = 230000, 
            label = "No Alaska \n harvest \n estimates \n 1987–1991", hjust = "center") +
   theme(legend.position=c(.85,.85), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line")) 
#ggsave(US_Fig12, filename = here::here("output/US_Fig12.png"), width = 6.5, height = 4, units = "in")


###### Figure 13 ######
# Smolt Marine Survival



Fig13a <- SEAK_marsurv %>%
   filter(River == "Ford Arm Creek") %>%
   ggplot(aes(x=ReturnYear, y = Survival)) +
   geom_line(size = 1.25) +
   geom_point(size = 2) +
   scale_x_continuous(breaks = seq(from=1980, to= 2019, by =2)) +
   scale_y_continuous(breaks = seq(from=0, to = 35, by = 5)) +
   expand_limits(y = c(0,25)) +
   labs(x = "Return Year", y = "Survival (%)") +
   annotate("text", x = 2006, y = 24, label = "Outer Coastal System - Ford Arm Creek (Presmolts)", size = 4) +
   theme_crisp(base_family = "Arial") +
   theme(legend.position=c(.8,.85), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(2,"line"),
         axis.text.x = element_blank(), axis.title.x = element_blank())

Fig13b <- SEAK_marsurv %>%
   filter(River %in% c("Auke Creek", "Berners River", "Hugh Smith Lake")) %>%
   ggplot(aes(x=ReturnYear, y = Survival, color = River, linetype = River)) +
   geom_line(size = 1.25) +
   geom_point(size = 2) +
   scale_x_continuous(breaks = seq(from=1980, to= 2019, by =2)) +
   scale_y_continuous(breaks = seq(from=0, to = 35, by = 5)) +
   scale_color_manual(values = c( "#4fa3bd", "#6b6b6b","black")) +
   scale_linetype_manual(values = c("solid", "dashed", "solid")) +
   expand_limits(y = c(0,35)) +
   labs(x = "Return Year", y = "Survival (%)") +
   annotate("text", x = 2006, y = 35, label = "Inside Systems (Smolts)", size = 4) +
   theme_crisp(base_family = "Arial") +
   theme(legend.position=c(.5,.08), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(2.5,"line"), legend.direction="horizontal") 

US_Fig13 <-(Fig13a / Fig13b) + plot_layout(heights = c(1, 35/25)) # make diff heights so y-axis scales are similar
#ggsave(US_Fig13, filename = here::here("output/US_Fig13.png"), width = 6.5, height = 6, units = "in")



###### Figure 14 ######
# Auke Marine Survival
US_Fig14 <- Auke_survival %>%
   pivot_longer(cols = c("Survival_Adults", "Survival_Jacks"), 
                names_to = "AdultJack", values_to = "Survival") %>%
   mutate(AdultJack = replace(AdultJack, AdultJack == "Survival_Adults", "Adult Survival"),
          AdultJack = replace(AdultJack, AdultJack == "Survival_Jacks", "Jack Survival")) %>%
   ggplot(aes(x = SmoltYear, y = Survival, fill = AdultJack)) +
   geom_col(color = "black", width = 0.75) +
   scale_x_continuous(breaks = seq(from=1980, to= 2019, by =2)) +
   scale_y_continuous(breaks = seq(from=0, to = 45, by = 5)) +
   scale_fill_manual(values = c("#4fa3bd", "black")) +
   labs(x = "Smolt Outmigration Year", y = "Marine Survival (%)") +
   theme_crisp(base_family = "Arial") +
   theme(legend.position=c(.8,0.8), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line")) 
#ggsave(US_Fig14, filename = here::here("output/US_Fig14.png"), width = 6.5, height = 4, units = "in")

#cor(Auke_survival$Survival_Adults, Auke_survival$Survival_Jacks)
   
###### Figure 15 ######
# All-Gear & Troll Harvest Rate

US_Fig15 <- indic_totalrun %>%
   filter( !(River =="Berners River" & Year < 1989)) %>% # These years are incorrect, exclude
   dplyr::select(-EscapementGoal_Lower, -EscapementGoal_Upper) %>%
   group_by(Year, River) %>%
   #this is a hacky way to do it but oh well
   mutate(freq = Count / sum(Count),
          total = if_else(Fishery == "Escapement", 1-freq, 0),
          index = if_else(Fishery == "Escapement", total,
                          ifelse(Fishery == "Alaska Troll", freq, NA))) %>%
   filter(Fishery != "Other Harvest") %>%
   mutate(Fishery = recode(Fishery, "Escapement" = "All Gear Harvest",
                           "Alaska Troll" = "Troll Harvest"),
          Fishery = fct_relevel(Fishery, "All Gear Harvest", "Troll Harvest"),
          River = recode(River, "Ford Arm Lake" = "Ford Arm Creek")) %>%
   dplyr::select(-Count, -freq, -total) %>%
   ggplot(aes(x=Year, y = index, linetype = Fishery)) + 
   geom_line() +
   scale_x_continuous(breaks = seq(from=1980, to=2019, by = 3)) +
   scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
   expand_limits(y= c(0,1)) +
   labs(y = "Harvest Rate") + 
   theme_crisp(base_family = "Arial") + 
   theme(legend.position=c(.33,.9), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1.5,"line")) +
   facet_wrap(~River)
US_Fig15

#ggsave(US_Fig15, filename = here::here("output/US_Fig15.png"), width = 6.5, height = 6, units = "in")


#ggsave(US_Fig15, filename = here::here("output/US_Fig15_pres.png"), width = 10, height = 6, units = "in")




###### Figure 16 ######
# All Harvest Rate

US_Fig16 <- trollindex %>%
   filter(Year >= 1982) %>% # remove the blank, Auke-only years
   ggplot(aes(x=Year, y = trollindex)) +
   geom_line(size = 1.25) + 
   geom_smooth(method = "lm", se=FALSE, linetype = "dashed", color = "black") +
   scale_x_continuous(breaks = seq(from=1982, to=2019, by=3)) +
   scale_y_continuous(labels = scales::percent, breaks = seq(from=0, to=0.6, by=0.1)) +
   expand_limits(x = 1983, y = c(0, 0.6)) +
   labs(x = "", y = "Troll Harvest Rate Index") + 
   theme_crisp(base_family = "Arial")
US_Fig16

#ggsave(US_Fig16, filename = here::here("output/US_Fig16.png"), width = 6.5, height = 4, units = "in")

summary(lm(trollindex ~ Year, data = trollindex %>% filter(Year >= 1982)))


###### Figure 17 ######   NEW 16?
# Hatchery vs Wild

# All Gear, not just troll
Fig17a <- wildproportion_allgear %>%
   dplyr::select(-wildpercent, -Gear) %>%
   pivot_longer(-Year, names_to = "Source", values_to = "Count") %>%
   ggplot(aes(x=Year, y =Count, fill = Source)) +
   geom_col(color = "black", width = 0.7, size=0.5) + 
   scale_x_continuous(breaks = seq(from=1980, to=2019, by = 2)) +
   scale_y_continuous(labels = comma, breaks = seq(from=0, to=5000000, by = 1000000)) +
   scale_fill_manual(values = c("white", "gray")) + 
   labs(y = "Number of Coho Salmon") + 
   annotate("text", x = 2000, y = 5750000, label = "All Gear Harvest", size = 4, fontface = "bold") +
   theme_crisp(base_family = "Arial") +
   theme(legend.position=c(.85,.85), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line"),
         axis.text.x = element_blank(), axis.title.x = element_blank()) # remove x axis

# Troll only
Fig17b <- trollharvest %>%
   filter(Year >= 1980) %>%
   ggplot(aes(x=Year, y=Harvest, fill=Source)) +
   geom_col(color = "black", width = 0.7, size=0.5) + 
   scale_x_continuous(breaks = seq(from=1980, to=2019, by = 2)) +
   scale_y_continuous(labels = comma, breaks = seq(from=0, to=5000000, by = 1000000)) +
   scale_fill_manual(values = c("white", "gray"), guide = FALSE) + 
   labs(y = "Number of Coho Salmon") + 
   annotate("text", x = 2000, y = 3600000, label = "Troll Harvest", size = 4, fontface = "bold") +
   theme_crisp(base_family = "Arial") 

US_Fig17 <- Fig17a / Fig17b
#ggsave(US_Fig17, filename = here::here("output/US_Fig17.png"), width = 6.5, height = 6, units = "in")
rm(Fig17a, Fig17b)



###### Figure XX ######   NEW 17?
# Troll Exploitation Rate vs Wild

wildproportion_allgear %>%
   dplyr::select(Year, wildpercent) %>%
   left_join(trollindex %>% dplyr::select(Year, trollindex)) %>%
   rename("Troll Exploitation Rate Index" = "trollindex",
          "Wild proportion of total harvest" = "wildpercent") %>%
   pivot_longer(-Year, "Source") %>%
   ggplot(aes(x = Year, y = value, linetype = Source)) +
   geom_line() + 
   scale_x_continuous(breaks = seq(from = 1980, to = 2019, by = 2)) +
   labs(y= "Proportion") +
   theme_crisp(base_family = "Arial") +
   theme(legend.position=c(.65,.95), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1,"line")) 



###### Figure 18 ######
## Troll and Wild abundance

# Troll CPUE
Fig18a <- troll_cpue %>% 
   filter(between(StatWeek, 28, 38), Year >= 1982) %>% # only keep weeks 28-38
   group_by(Year) %>%
   summarise(meanannualCPUE = mean(CohoCPUE, na.rm = TRUE)) %>%
   left_join(wildproportion_allgear) %>%
   mutate(annualwildCPUE = meanannualCPUE * wildpercent) %>%
   ggplot(aes(x=Year, y = annualwildCPUE)) +
   geom_line(size = 1.125) +
   scale_x_continuous(breaks = seq(from=1982, to = 2019, by = 2)) +
   scale_y_continuous(breaks = seq(from=0, to = 90, by = 20)) +
   expand_limits(y=c(0, 90)) +
   labs(y = "Catch per Boat Day") +
   annotate("text", x = 2001, y = 90, label = "Mean Annual Power Troll Wild CPUE", size = 4, fontface = "bold") +   
   theme_crisp(base_family = "Arial") +
   theme(axis.text.x = element_blank(), axis.title.x = element_blank()) # remove x axis


# Wild Harvest by Fishery
Fig18b <- trollharvest %>%
   rename("Harvest: Troll Fishery" = "Harvest") %>%
   filter(Source == "Wild contribution", Year >= 1982) %>%
   left_join(harvest_historic) %>%
   rename("Harvest: All Gear" = "Wild") %>%
   dplyr::select(Year, `Harvest: Troll Fishery`, `Harvest: All Gear`) %>%
   left_join(wildabundance %>% dplyr::select(Year, EstTotalWildAbund)) %>%
   rename("Regional Wild Abundance" = "EstTotalWildAbund") %>%
   pivot_longer(-Year, names_to = "Fishery", values_to = "Wild_count") %>%
   mutate(Count_mil = Wild_count / 1000000, 
          Fishery = fct_relevel(Fishery, "Regional Wild Abundance", "Harvest: All Gear",
                                "Harvest: Troll Fishery")) %>%
   ggplot(aes(x=Year, y = Count_mil, linetype = Fishery, color = Fishery)) +
   geom_line(size = 1.125) +
   scale_x_continuous(breaks = seq(from=1982, to = 2019, by = 2)) +
   scale_y_continuous(breaks = seq(from=0, to = 7, by = 1)) +
   scale_color_manual(values = c("black", "darkgray", "black")) + 
   scale_linetype_manual(values = c("solid", "solid", "dashed")) +
   expand_limits(y=c(0, 7)) +
   labs(y = "Number of Coho Salmon (Millions)") + 
   annotate("text", x = 2001, y = 7, label = "Wild Abundance and Harvest", size = 4, fontface = "bold") +   
   theme_crisp(base_family = "Arial") +
   theme(legend.position=c(0.5, 0.05), legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(2.0,"line"), legend.direction="horizontal") 




US_Fig18 <- Fig18a / Fig18b
#ggsave(US_Fig18, filename = here::here("output/US_Fig18.png"), width = 6.5, height = 6, units = "in")
rm(Fig18a, Fig18b)

# Is wild abundance increasing? Yes, with caveats that come with using troll index. 
# The increase is driven mainly by increases in the index (increases in troll efficiency)
summary(lm(EstTotalWildAbund ~ Year, data = wildabundance))
summary(lm(trollindex ~ Year, data = wildabundance))
summary(lm(Wildharvest ~ Year, data = wildabundance))


###### Figure XX ######
## Berners Ricker

US_FigXX_rick <- ggplot() + 
   geom_line(data = BernersRicker, aes(x=S, y = ricker_fit), color = "black", size = 2) +
   geom_abline(intercept = 0, lty = 2) +
   geom_line(data = BernersRicker, aes(x=S, y = hockey), color = "darkgray", size = 2, lty = "longdash") +
   geom_point(data = Berners_SR, aes(x=Spawners, y = Recruits, fill=Decade), size = 4, shape = 21) +
   scale_y_continuous(labels = comma, breaks = seq(from=0, to=60000, by=10000)) + 
   scale_x_continuous(labels = comma, limits = c(0, 30000)) +
   scale_fill_manual(values = c("#55207d", "#5d7cba", "#0090fc", "#a5e6da")) +
   labs(x="Spawners", y ="Recruits") + 
   theme_crisp(rotate_text = FALSE, base_family = "Arial") +
   theme(legend.position=c(.9175,.7875), # c(.9175,.7875) only work with 6.5x4" and legend outlined
         legend.title = element_blank(), legend.text = element_text(size = 10),
         legend.key.size = unit(1.5,"line"),
         legend.background = element_rect(colour = 'darkgray', linetype='solid')) 
US_FigXX_rick
#ggsave(US_FigXX_rick, filename = here::here("output/US_FigXX_rick.png"), width = 6.5, height = 4, units = "in")



# Alternate Version w text labels of year
ggplot() + 
   geom_line(data = BernersRicker, aes(x=S, y = ricker_fit), color = "black", size = 2) + 
   geom_abline(intercept = 0, lty = 2) +
   geom_line(data = BernersRicker, aes(x=S, y = hockey), color = "darkgray", size = 2, lty = "longdash") +
   geom_point(data = Berners_SR, aes(x=Spawners, y = Recruits), color="#2a84c9", size = 3) +
   #geom_text(Berners_SR, mapping=aes(x=Spawners, y = Recruits, label = substr(Year, 3, 4)), vjust =1.5) +
   #ggrepel::geom_text_repel(Berners_SR, mapping=aes(x=Spawners, y = Recruits, label = Year)) +
   scale_y_continuous(labels = comma, breaks = seq(from=0, to=60000, by=10000)) + 
   scale_x_continuous(labels = comma, limits = c(0, 30000)) +
   labs(x="Spawners", y ="Recruits") + 
   theme_crisp(rotate_text = FALSE, base_family = "Arial")


###### Figure XX ######
## Smolt Production
US_FigXX_smolt <- SEAK_smolt %>%
   pivot_longer(cols = `Auke Creek`:`Ford Arm Creek`, names_to = "River", values_to = "Count") %>%
   ggplot(aes(x=SmoltYear, y = Count, color = River)) +
   geom_line(size = 1.25) +
   scale_x_continuous(breaks = seq(from = 1980, to = 2020, by = 3)) +
   scale_y_log10(labels = comma, breaks = c(0, 10000, 100000, 1000000)) + 
   scale_color_manual(values = c("#4fa3bd", "black", "#c77512", "#5d8c77", "#6b6b6b", "#66437d")) + 
   #geom_dl(aes(label=River),method="last.points") +
   geom_smooth(data = SEAK_smolt %>% dplyr::select(SmoltYear, `Ford Arm Creek`) %>%
               rename("Count" = `Ford Arm Creek`) %>%
               mutate(River = "Ford Arm Creek"),
               method = "lm", se = FALSE, linetype = 2) +
   geom_smooth(data = SEAK_smolt %>% dplyr::select(SmoltYear, `Chilkat River`) %>%
                     rename("Count" = `Chilkat River`) %>%
                     mutate(River = "Chilkat River"),
               method = "lm", se = FALSE, linetype = 2) +
   annotate("text", x = 2016, y = 12000, label= "Auke Creek") + 
   annotate("text", x = 2016, y = 26000, label = "Hugh Smith Lake") + 
   annotate("text", x = 2016, y = 80000, label= "Ford Arm Creek") + 
   annotate("text", x = 2011.5, y = 275000, label = "Berners River") + 
   annotate("text", x = 2014, y = 550000, label= "Chilkat River") + 
   annotate("text", x = 2016, y = 2500000, label = "Taku River") + 
   annotation_logticks() +
   labs(x = "", y = "Number of Smolt") + 
   theme_crisp(rotate_text = TRUE) +
   theme(legend.position="none") 
US_FigXX_smolt
# ggsave(US_FigXX_smolt, filename = here::here("output/US_FigXX_smolt.png"),
#        width = 6.5, height = 4, units = "in")





###### Figure XX3 ###### NEW FIGURE 11
## Change in total production ##
baseline_esc <- indic_totalrun %>%
   dplyr::select(Year, River, Fishery, Count) %>%
   group_by(River, Year) %>%
   summarise(totalcount = sum(Count)) %>%
   filter(between(Year, 1985, 2009)) %>%
   group_by(River) %>%
   summarise(meancount1990_2009 = round(mean(totalcount), 0)) 


rivernames <- data.frame(River = c("Auke Creek", "Berners River", "Ford Arm Creek", "Hugh Smith Lake"),
                         Year = c(2016, 2016, 2016, 2016), baselinediff = rep(0.9, 4))


totalchange <- indic_totalrun %>%
   dplyr::select(Year, River, Fishery, Count) %>%
   group_by(River, Year) %>%
   summarise(totalcount = sum(Count)) %>%
   filter(between(Year, 2010, 2019)) %>%
   left_join(baseline_esc, by =  c("River" = "River")) %>%
   mutate(baselinediff = (totalcount - meancount1990_2009) / meancount1990_2009,
          posneg = ifelse(baselinediff > 0, "positive", "negative"),
          Year = as.integer(Year),
          River = recode(River, "Ford Arm Lake" = "Ford Arm Creek"))

US_FigXX_relchange <- ggplot(totalchange, aes(x = Year, y = baselinediff, fill = posneg)) +
   geom_col(alpha = 0.75, color = "black") + 
   geom_hline(yintercept = 0) +
   scale_x_continuous(breaks = seq(from=2010, to=2019, by = 1)) +
   scale_y_continuous(labels = scales::percent) + 
   geom_text(data = rivernames, aes(x = Year,  y = baselinediff, label = River, fill = NA))+
   #scale_fill_manual(values = c("red4", "darkgreen")) +
   #scale_fill_manual(values = c("#8f3622", "#548768")) +
   #scale_fill_manual(values = c("#2e586b", "#0888c2")) +
   scale_fill_manual(values = c("gray90", "gray10")) +
   labs(y = "Relative change of total return compared to 1985–2009 average", title = "") +
   facet_wrap(~River, nrow = 4) +
   theme_crisp(rotate_text = FALSE) +
   theme(legend.position = "none",
         strip.text.x = element_blank(), panel.spacing = unit(1.25, "lines")) # remove facet title and add more spacing
US_FigXX_relchange

# ggsave(US_FigXX_relchange, filename = here::here("output/US_FigXX_relchange_V2.png"),
#         width = 6.5, height = 8, units = "in")

rm(baseline_esc, totalchange, rivernames)



