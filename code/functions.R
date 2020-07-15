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






