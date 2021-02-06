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
      theme(axis.text.x = element_text(angle = 0))
    }
}



# A better version of theme_coho() is theme_crisp() which doesn't require ggsidekick
theme_crisp <- function(base_size = 12, base_family = "Arial", rotate_text=TRUE, rmborder=FALSE) {
  # This is based heavily on Sean Anderson's theme_sleek from ggsidekick
  # https://github.com/seananderson/ggsidekick
  
  require(extrafont)  # Need package extrafont for fonts. See pkg docs
  
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, color = NA),
      strip.text.x = element_text(color = "gray30"),
      strip.text.y = element_text(color = "gray30"),
      axis.text = element_text(color = "gray30"),
      axis.title = element_text(color = "gray30"),
      legend.title = element_text(color = "gray30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, color = "gray70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), color = "gray30"),
      legend.key = element_rect(color = NA, fill = NA),
      legend.background = element_rect(color = NA, fill = NA),
      plot.title = element_text(color = "gray30", size = rel(1)),
      plot.subtitle = element_text(color = "gray30", size = rel(.85))
    ) +
    {if(rmborder==TRUE){
      theme(axis.line = element_line(size = 0.5, color = "gray70"),
            panel.border = element_blank())
    }
      else{
        theme()
      }} + # If modifying in future, need {} around entire if statement
    if(rotate_text==TRUE){
      theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
    } else{
      theme(axis.text.x = element_text(angle = 0))
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
create_figure5 <- function(river = "Auke Creek", setbreaks = c(0,500, 1000, 1500), minyear = 1980, blank_x = TRUE, ...) {
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

############################
##### UNUSED FUNCTIONS #####

##### DUPLICATE ROWS IF THEY ARE ALREADY SUMMARIZED #####
duplicaterows <- function(dataframename, duplicatecolname = "specimen_count", replacenaswithone = FALSE){
  require(tidyverse)
  
  # Make an index of which rows will be repeated, and how many times
  .dupcount <- dataframename %>% dplyr::select(duplicatecolname) %>% tibble::deframe()
  
  # NAs will normally make this fail. We can replace NAs though
  # This replaces NAs with 1. THIS IS A LARGE ASSUMPTION SO BE CAREFUL
  if(sum(is.na(.dupcount) > 0) && replacenaswithone == TRUE){
    .dupcount <- replace_na(.dupcount, 1)
  }
  
  # Now repeat this for every row to duplicate. 
  # A specimen count of 1 will mean the row isn't duplicated; a count of 5, repeats the row 5 times 
  dataframename[rep(1:nrow(dataframename), .dupcount), ] %>%
    dplyr::select(-duplicatecolname) # Removes the count row now that it is incorrect!
  
  # Use like so: duplicaterows(dataframename = newdf, duplicatecolname = "Number.of.Specimens")
  
  # Thanks to: https://stackoverflow.com/questions/29743691/duplicate-rows-in-a-data-frame-in-r
}



##### SUMMARIZE PROPORTION #####
# https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
count_pct <- function(df) {
  return(
    df %>%
      tally %>% 
      mutate(n_pct = 100*n/sum(n))
  )
}


