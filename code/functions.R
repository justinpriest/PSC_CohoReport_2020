# Functions and Themes Needed



##### CUSTOM THEME #####
theme_coho <- function(base_size = 12, base_family = "Times New Roman", rotate_text=TRUE){
  require(ggsidekick) # Need package ggsidekick for theme_sleek()
  require(extrafont)  # Need package extrafont for fonts. See pkg docs
  theme_sleek(base_size = base_size, base_family = base_family) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #axis.line = element_line(size = .5), panel.border = element_blank()# Optional to remove border
    ) +
    if(rotate_text==TRUE){
      theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
    } else{
      theme(axis.text.x = element_text(angle = 0, vjust=0, hjust=0))
    }
}




##### ASSIGN STAT WEEK #####
statweek <- function(x) {
  as.numeric(format(as.Date(x), "%U")) - as.numeric(format(as.Date(cut(x, "year")), "%U")) + 1
}
# Function modified from:
# https://stackoverflow.com/questions/17286983/calculate-statistical-week-starting-1st-january-as-used-in-fisheries-data





##### IMPUTE MISSING VALUES #####
globalimpute <- function(dfname, Year_column="Year", StreamName_column="River", 
                         Count_column = "index", outputprefix = "default"){
  # This function will create a new dataframe from your old dataframe of the imputed values
  # JTP: There are known issues with using a "global" impute, namely that more years of data will change
  #      the underlying relationship (e.g., having run an impute in 2000 will give diff answers than 2020).
  #      However, this is the standard method that Leon used so I'm replicating it. 
  require(dplyr)
  .test <- dfname %>% rename(Year = Year_column, River = StreamName_column, Count = Count_column)
  .test <- .test %>% dplyr::select(Year, River, Count)
  .test <- .test %>% mutate(imputed = is.na(Count))
  
  j=1
  repeat{
    for(i in 1:nrow(.test)){
      .temprow = .test[i,] 
      
      if(.temprow$imputed == TRUE){
        .sumyr = sum((.test %>% filter(Year == .temprow$Year) )$Count, na.rm = TRUE)
        .sumrvr = sum((.test %>% filter(River == .temprow$River) )$Count, na.rm = TRUE)
        .sumall = sum(.test$Count, na.rm = TRUE)
        .test$Count[i] = .sumyr * .sumrvr / .sumall
        # this is multiplicative imputation as per Blick
      }
    }
    j=j+1
    if(j>10){break} # repeat the above 100 times
  }
  assign(paste0((outputprefix), "_imputed"), .test, envir = parent.frame() ) # use if you want a dynamic name
  # imputedsurvey <- .test # use this if you want a static name
}
# This function is used to calculate the troll index for the missing Ford Arm values





##### CREATE US FIGURE 5 #####
create_figure5 <- function(river = "Auke Creek", setbreaks = c(0,500, 1000, 1500), minyear = 1980, blank_x = TRUE) {
  SEAK_escape %>% 
    filter(River == river, Year >= minyear) %>%
    ggplot(aes(x = Year, y = Escapement_Count)) + 
    geom_col(fill = "gray", color = "black") +
    geom_line(aes(y=EscapementGoal_Lower), size = 1) +
    geom_line(aes(y=EscapementGoal_Upper), size = 1) +
    expand_limits(x = 1980) +
    scale_x_continuous(breaks = seq(from = 1980, to = 2019, by = 2)) +
    scale_y_continuous(labels = comma, breaks = setbreaks) +
    labs(x = "", y = "") + 
    theme_coho(base_family = "Arial") +
    if(blank_x==TRUE){
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    } else{
      labs(x = "Year")
    }
}

##### CREATE US HARVEST FIGURES #####
create_harvestfig <- function(dataframe = indic_totalrun, river = "Auke Creek", 
                              setbreaks = c(0, 1000, 2000, 3000), minyear = 1982, blank_x = TRUE) {
  dataframe %>% 
    filter(River == river, Year >= minyear) %>%
    ggplot(aes(x=Year, y = Count, fill = Fishery)) + 
    geom_col(color = "black", width = 0.7, size=0.5) + 
    geom_line(aes(y=EscapementGoal_Lower), size = 1) +
    geom_line(aes(y=EscapementGoal_Upper), size = 1) +
    scale_x_continuous(breaks = seq(from=1982, to=2019, by = 2)) +
    scale_y_continuous(labels = comma, breaks = setbreaks) +
    scale_fill_manual(values = c("white", "black", "gray")) + 
    labs(y = "Number of Coho Salmon") + 
    theme_coho(base_family = "Arial") + 
    theme(legend.position="none") +
    if(blank_x==TRUE){
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    } else{
      labs(x = "Year")
    }
}




