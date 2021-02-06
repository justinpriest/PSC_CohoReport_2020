

# Testing out color scheme
vals <- c("#fffdde", "#fff3b8", "#e3c56b", "#d69f47", "#d14141", 
          "#961439", "#800921", "#610552", "#59043b")

ggplot(data.frame(ht = rep(10, 9), name = as.factor(letters[1:9])), aes(x=name, y = ht, fill = name)) + 
  geom_col() +
  scale_fill_manual(values = vals)


# Temp Hugh Smith Figure for Steve
tempHS <- create_harvestfig(river = "Hugh Smith Lake", blank_x = FALSE, setbreaks = seq(from=0, to=10000, by=1000)) + 
  labs(y = "Number of Coho Salmon") +
  annotate("text", x = 2000, y = 10000, label = "Hugh Smith Lake Coho Salmon", size = 3.5) +
  theme(legend.position=c(.6,.8), legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(1,"line")) 
#ggsave(tempHS, filename = here::here("output/tempHSplot.png"), width = 6.5, height = 4, units = "in")





library(corrr)

##### IN-TEXT CALCULATION #####
## STOCK STATUS - Full Indicator Stocks
totalrun <- indic_totalrun %>%
  dplyr::select(-EscapementGoal_Lower, -EscapementGoal_Upper) %>%
  group_by(River, Year) %>%
  summarise(totalrun = sum(Count)) 

totalrun_taku <- taku_harvest_can %>%
  pivot_wider(names_from = Fishery, values_from = Count) %>%
  mutate(Harvest = `Alaska Troll` + `Other Alaska` + Canada) %>%
  dplyr::select(Year, Escapement, Harvest) %>%
  mutate(River = "Taku River", 
         totalrun = Harvest + Escapement) %>%
  dplyr::select(River, Year, totalrun) 

totalrun_chilkat <- chilkat_harvest %>%
  group_by(Year) %>%
  summarise(Harvest = sum(Coho_Harvest_Count)) %>%
  left_join(SEAK_escape %>% 
              filter(River == "Chilkat River") %>%
              dplyr::select(Year, Escapement_Count),
            by = c("Year" = "Year")) %>%
  mutate(totalrun = Harvest + Escapement_Count,
         River = "Chilkat River") %>%
  dplyr::select(Year, River, everything())

totalrun <- rbind(totalrun, totalrun_chilkat, totalrun_taku)
rm(totalrun_chilkat, totalrun_taku)


summary(lm(totalrun ~ Year, data = totalrun %>% filter(River == "Auke Creek")))
summary(lm(totalrun ~ Year, data = totalrun %>% filter(River == "Berners River")))
summary(lm(totalrun ~ Year, data = totalrun %>% filter(River == "Ford Arm Lake")))
summary(lm(totalrun ~ Year, data = totalrun %>% filter(River == "Hugh Smith Lake")))
summary(lm(totalrun ~ Year, data = totalrun %>% filter(River == "Taku River")))
summary(lm(totalrun ~ Year, data = totalrun %>% filter(River == "Chilkat River")))

# Summary: Auke, Berners, Chilkat have declined, Ford Arm increased, HS/Taku no change
# but note diff time frame for Chilkat & Ford Arm




# chilkat_harvest_esc <- chilkat_harvest %>%
#   pivot_wider(names_from = Fishery_Type, values_from = Coho_Harvest_Count) %>%
#   mutate(Harvest = Troll+Seine+`Drift Gillnet`+`Sport (marine)`+`Sport (freshwater)`+Subsistence) %>%
#   dplyr::select(Year, Harvest) %>%
#   left_join(SEAK_escape %>% 
#               filter(River == "Chilkat River") %>% 
#               dplyr::select(Year, Escapement_Count)) # We'll use this variable later, save it
# 
# totalrun_chilkat <- chilkat_harvest_esc %>% 
#   mutate(River = "Chilkat River", 
#          totalrun = Harvest + Escapement_Count) %>%
#   dplyr::select(River, Year, totalrun) 



totalrun %>%
  pivot_wider(names_from = River, values_from = totalrun) %>%
  corrr::correlate(use="pairwise.complete.obs", method = "spearman")
# Berners:Chilkat highly corr, 





## STOCK STATUS - Smolt and Presmolt production 
SEAK_smolt_tidy <- SEAK_smolt %>%
  pivot_longer(cols = `Auke Creek`:`Ford Arm Creek`, names_to = "River", values_to = "Count")
lm(Count ~ SmoltYear, data = SEAK_smolt_tidy %>% filter(River == "Auke Creek")) %>% summary()
lm(Count ~ SmoltYear, data = SEAK_smolt_tidy %>% filter(River == "Auke Creek", SmoltYear <= 2008)) %>% summary()
lm(Count ~ SmoltYear, data = SEAK_smolt_tidy %>% filter(River == "Berners River")) %>% summary()
lm(Count ~ SmoltYear, data = SEAK_smolt_tidy %>% filter(River == "Chilkat River")) %>% summary()
lm(Count ~ SmoltYear, data = SEAK_smolt_tidy %>% filter(River == "Taku River")) %>% summary()
lm(Count ~ SmoltYear, data = SEAK_smolt_tidy %>% filter(River == "Ford Arm Creek")) %>% summary()
lm(Count ~ SmoltYear, data = SEAK_smolt_tidy %>% filter(River == "Hugh Smith Lake")) %>% summary()
# No annual trend in smolt production for Auke, Berners, Taku, Hugh Smith
# Signif decline in Chilkat smolt prod, signif increase in Ford Arm pre-smolt numbers


t.test((SEAK_smolt_tidy %>% filter(River == "Auke Creek", SmoltYear <= 2008))$Count,
       (SEAK_smolt_tidy %>% filter(River == "Auke Creek", SmoltYear >= 2009))$Count)
t.test((SEAK_smolt_tidy %>% filter(River == "Berners River", SmoltYear <= 2008))$Count,
       (SEAK_smolt_tidy %>% filter(River == "Berners River", SmoltYear >= 2009))$Count)
t.test((SEAK_smolt_tidy %>% filter(River == "Chilkat River", SmoltYear <= 2008))$Count,
       (SEAK_smolt_tidy %>% filter(River == "Chilkat River", SmoltYear >= 2009))$Count)
t.test((SEAK_smolt_tidy %>% filter(River == "Taku River", SmoltYear <= 2008))$Count,
       (SEAK_smolt_tidy %>% filter(River == "Taku River", SmoltYear >= 2009))$Count)
t.test((SEAK_smolt_tidy %>% filter(River == "Ford Arm Creek", SmoltYear <= 2008))$Count,
       (SEAK_smolt_tidy %>% filter(River == "Ford Arm Creek", SmoltYear >= 2009))$Count)
t.test((SEAK_smolt_tidy %>% filter(River == "Hugh Smith Lake", SmoltYear <= 2008))$Count,
       (SEAK_smolt_tidy %>% filter(River == "Hugh Smith Lake", SmoltYear >= 2009))$Count)

# The Chilkat River has signif lower smolt production in most recent decade than before
# All other rivers have insignif differences in smolt production


## STOCK STATUS - Marine Survival


## STOCK STATUS - Exploitation Rates
exploitrate <- indic_totalrun %>%
  filter( !(River =="Berners River" & Year < 1989)) %>% # These years are incorrect, exclude
  dplyr::select(-EscapementGoal_Lower, -EscapementGoal_Upper) %>%
  group_by(Year, River) %>%
  #this is a hacky way to do it but oh well
  mutate(freq = Count / sum(Count),
         total = if_else(Fishery == "Escapement", 1-freq, 0),
         index = if_else(Fishery == "Escapement", total,
                         ifelse(Fishery == "Alaska Troll", freq, NA))) %>%
  filter(Fishery != "Other") %>%
  mutate(Fishery = recode(Fishery, "Escapement" = "All Gear Exploitation",
                          "Alaska Troll" = "Troll Exploitation"),
         Fishery = fct_relevel(Fishery, "All Gear Exploitation", "Troll Exploitation")) %>%
  dplyr::select(-Count, -freq, -total) %>% 
  filter(Fishery != "Other Harvest")



summary(lm(index ~ Year, data = exploitrate %>% 
             filter(Fishery == "All Gear Exploitation", River == "Auke Creek")))
summary(lm(index ~ Year, data = exploitrate %>% 
             filter(Fishery == "All Gear Exploitation", River == "Berners River")))
summary(lm(index ~ Year, data = exploitrate %>% 
             filter(Fishery == "All Gear Exploitation", River == "Ford Arm Lake")))
summary(lm(index ~ Year, data = exploitrate %>% 
             filter(Fishery == "All Gear Exploitation", River == "Hugh Smith Lake")))
# No change in all gear exploit rate for Auke, sig decrease for Berners Hugh Smith
# Signif increase for Ford Arm

summary(lm(index ~ Year, data = exploitrate %>% 
             filter(Fishery == "Troll Exploitation", River == "Auke Creek")))
summary(lm(index ~ Year, data = exploitrate %>% 
             filter(Fishery == "Troll Exploitation", River == "Berners River")))
summary(lm(index ~ Year, data = exploitrate %>% 
             filter(Fishery == "Troll Exploitation", River == "Ford Arm Lake")))
summary(lm(index ~ Year, data = exploitrate %>% 
             filter(Fishery == "Troll Exploitation", River == "Hugh Smith Lake")))
# Signif decrease in troll exploitation for Auke, Berners, Hugh Smith. No change Ford Arm

exploitrate %>% filter(Fishery == "Troll Exploitation") %>% 
  group_by(River) %>%
  summarise(mean = mean(index, na.rm = TRUE),
            range = range(index, na.rm = TRUE))
  

# Does exploitation rate of Berners River fish increase in odd years?  (1999 onward)
bernersexp <- exploitrate %>% pivot_wider(values_from = "index", names_from = "Fishery") %>%
    filter(River == "Berners River", Year >= 1999) %>%
    rename("trollexp" = `Troll Exploitation`)

bernodd <- bernersexp %>%
  filter(Year %% 2 == 1)

berneven <- bernersexp %>%
  filter(Year %% 2 == 0) %>% 
  ungroup() %>%
  dplyr::select("trollexp") %>% rename("trollexpeven" = "trollexp") %>%
  add_row(trollexpeven = NA) # uneven lengths of even/odd years

combined <- bernodd %>%
  ungroup() %>%
  dplyr::select("trollexp") %>% rename("trollexpodd" = "trollexp") %>%
  add_column(berneven)

summary(aov(data = combined, trollexpodd ~ trollexpeven))
summary(lm(trollexpodd ~ trollexpeven, data = combined))
t.test(combined$trollexpodd, combined$trollexpeven)

mean(combined$trollexpodd)
mean(combined$trollexpeven, na.rm = TRUE)

rm(bernodd, berneven, combined)





## STOCK STATUS - Regional Wild Abundance
harvest_abund <- trollharvest %>%
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
                               "Harvest: Troll Fishery"),
         Decade = ifelse(Year <= 1989, "1980s",
                         ifelse(between(Year, 1990, 1999), "1990s", 
                                ifelse(between(Year, 2000, 2009), "2000s", 
                                       ifelse(between(Year, 2010, 2019), "2010s", "Error" )))))

summary(lm(Wild_count ~ Year, data = harvest_abund %>% filter(Fishery == "Regional Wild Abundance")))
# Wild abundance has significantly increased over time
harvest_abund %>%
  filter(Fishery == "Regional Wild Abundance") %>% 
  group_by(Decade) %>%
  summarise(meanabund = mean(Count_mil))

summary(aov(Wild_count ~ Decade, data = harvest_abund %>% filter(Fishery == "Regional Wild Abundance")))
# There is not a significant overall difference between decades. 
summary(lm(Wild_count ~ Decade, data = harvest_abund %>% filter(Fishery == "Regional Wild Abundance")))
# The 2010s are signifcantly higher than previous decades

harvest_abund %>%
  dplyr::select(-Count_mil) %>%
  pivot_wider(names_from = Fishery, values_from = Wild_count) %>%
  mutate(Harvest_abund_ratio = `Harvest: All Gear` / `Regional Wild Abundance`) %>%
  filter(Year >= 2010) %>%
  #group_by(Year) %>%
  summarise(mean(Harvest_abund_ratio))
# The mean ratio of all gear harvest : abundance 2010-2019 is 40.0%
harvest_abund %>%
  dplyr::select(-Count_mil) %>%
  pivot_wider(names_from = Fishery, values_from = Wild_count) %>%
  mutate(Harvest_abund_ratio = `Harvest: Troll Fishery` / `Regional Wild Abundance`) %>%
  filter(Year >= 2010) %>%
  #group_by(Year) %>%
  summarise(mean(Harvest_abund_ratio))
# The mean ratio of troll harvest : abundance 2010-2019 is 25.5%




## DISCUSSION - Exploitation Rates
taku_harvest_can %>%
  pivot_wider(names_from = Fishery, values_from = Count) %>%
  mutate(Harvest = `Alaska Troll` + `Other Alaska` + Canada) %>%
  dplyr::select(Year, Escapement, Harvest) %>%
  mutate(ExploitRate = Harvest / (Escapement + Harvest)) %>% View()


chilkat_harvest_esc %>%
  mutate(ExploitRate = Harvest / (Escapement_Count + Harvest)) %>% View()

indic_totalrun %>%
  dplyr::select(-EscapementGoal_Lower, -EscapementGoal_Upper) %>%
  pivot_wider(names_from = Fishery, values_from = Count) %>%
  mutate(Harvest = `Alaska Troll` + `Other Harvest`) %>%
  dplyr::select(Year, River, Escapement, Harvest) %>%
  mutate(ExploitRate = Harvest / (Escapement + Harvest)) %>%
  dplyr::select(-Escapement, -Harvest) %>%
  pivot_wider(names_from = River, values_from = ExploitRate) %>% View()
  
  
  

######## OTHER CALCS
# Data from Table 4, wild/hatchery harvest & abundance
df <- data.frame(totaltroll = c(1.322, 1.280, 1.134, 1.606, 2.130, 1.042, 0.500, 1.370,
                                1.851, 1.721, 1.929, 2.408, 3.462, 1.750, 1.907, 1.170,
                                1.636, 2.273, 1.125, 1.845, 1.315, 1.223, 1.917, 2.038,
                                1.363, 1.378, 1.293, 1.592, 1.343, 1.312, 1.201,
                                2.394, 2.245, 1.241, 1.387, 2.149, 0.942), 
                 wildtroll = c(1.286, 1.227, 1.062, 1.500, 1.850, 0.951, 0.472, 1.248,
                               1.560, 1.337, 1.509, 2.014, 2.947, 1.414, 1.458, 0.928,
                               1.307, 1.758, 0.876, 1.481, 0.980, 0.936, 1.605, 1.705,
                               1.146, 1.069, 1.019, 1.344, 1.058, 0.970, 0.891, 1.661,
                               1.625, 0.857, 1.049, 1.762, 0.627),
                 estwildabund = c(3.752, 3.280, 2.874, 3.878, 4.200, 2.671, 1.544, 2.399,
                                  3.617, 4.172, 3.871, 4.173, 6.652, 4.055, 3.408, 2.692,
                                  3.225, 4.165, 2.526, 4.432, 4.696, 3.744, 3.983, 4.723,
                                  3.617, 2.712, 3.825, 3.958, 3.644, 4.472, 3.523,
                                  5.065, 6.626, 3.224, 4.653, 5.157, 3.299))

plot(df$totaltroll ~ df$estwildabund)

summary(lm(estwildabund ~ wildtroll + totaltroll, data = df))
summary(lm(estwildabund ~ totaltroll, data = df))
summary(lm(estwildabund ~ wildtroll, data = df))


summary(lm(`Berners River` ~ Year, data = trollindex))
summary(lm(`Auke Creek` ~ Year, data = trollindex))
summary(lm(`Hugh Smith Lake` ~ Year, data = trollindex))
summary(lm(`Ford Arm Lake` ~ Year, data = trollindex))



  
  
  