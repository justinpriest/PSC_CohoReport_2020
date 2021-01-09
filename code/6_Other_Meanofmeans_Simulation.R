# How is taking the mean of a mean different from the overall mean?

source(here::here("code/1_US_data_import.R"))


#==================#
#### SIMULATION ####

output <- data.frame(meanoftotal = NA, meanofmeans = NA)
for(i in 1:10000){
  # Exploring different non-negative distributions
  #.simdaysfished <- abs(round(rnorm(3, 30, 10))+0.0001)
  #.simharvest <- abs(round(rnorm(3, 200, 50))+0.0001)
  
  # .simdaysfished <- rpois(3, 30) # Too much variance at high lambda 
  # .simharvest <- rpois(3, 200)
  
  .simdaysfished <- rgamma(3, shape = 7.5, rate = 0.25) # mean 30
  .simharvest <- rgamma(3, shape = 100, rate = 0.5)     # mean 200
  
  output[i,1] <- sum(.simharvest) / sum(.simdaysfished)
  output[i,2] <- mean(.simharvest / .simdaysfished, na.rm = TRUE)
  
}
hist(output$meanoftotal)
mean(output$meanoftotal)

hist(output$meanofmeans)
mean(output$meanofmeans, na.rm =TRUE)


ggplot(output, aes(x=meanoftotal)) + 
  geom_histogram(color = "black") + 
  scale_x_continuous(limits = c(0, 20))

ggplot(output, aes(x=meanofmeans)) + 
  geom_histogram(color = "black") + 
  scale_x_continuous(limits = c(0,20))




ggplot(output) + geom_histogram(aes(x=meanoftotal), alpha = 0.4, fill = "red") + 
  geom_histogram( aes(x=meanofmeans), alpha = 0.4, fill="#38c0ff") + 
  geom_vline(xintercept =  mean(output$meanoftotal), color = "red") +
  geom_vline(xintercept =  mean(output$meanofmeans), color = "blue") +
  scale_x_continuous(limits = c(0,20)) +
  labs(x="mean", title = "Histograms of taking overall total mean (red) vs mean of means (blue)", 
       subtitle = "Vertical bars are overall means") +
  theme_coho(rotate_text = FALSE)


# SUMMMARY
# Using the mean of a mean will give you a higher answer than just taking the overall mean
# This is especially pronounced the higher your variance  



#===================#
#### Actual Data ####

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


allyr_summary <- troll_cpue %>% 
  filter(StatWeek <= 29, # exclude week 30, based on treaty language
         StatArea != 101-85, StatArea != 101-90, StatArea != 101-95) %>%
  group_by(Year) %>%
  summarise(meanCPUE = mean(CohoCPUE),
            totalCaught = sum(CohoCatch),
            totalEffort = sum(Effort_boatdays),
            numsamples = length(Effort_boatdays))



meanofmean <- troll_cpue %>% 
  filter(StatWeek <= 29, # exclude week 30, based on treaty language
         StatArea != 101-85, StatArea != 101-90, StatArea != 101-95) %>%
  group_by(Year, StatWeek) %>%
  summarise(meanCPUE = mean(CohoCPUE)) %>%
  group_by(Year) %>%
  summarise(meanofmeanCPUE = mean(meanCPUE)) %>% left_join(allyr_summary) %>%
  mutate(diff = meanofmeanCPUE - meanCPUE)



sum(meanofmean$diff) # If there were no difference, this value should be close to zero
mean(meanofmean$diff) # Several units of CPUE higher on average
plot(meanofmeanCPUE ~ meanCPUE, data = meanofmean)            
abline(a=0, b=1)

