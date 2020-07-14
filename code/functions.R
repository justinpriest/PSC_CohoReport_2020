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





