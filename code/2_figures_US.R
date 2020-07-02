

##### KTN & HUGH SMITH INDICES ####

# Figure 2a
(Fig2a <- ggplot(data = ktn_index_hist,
                 aes(x=Year, y = surveycount)) + 
   geom_bar(stat="identity", fill="lightgray", color="black") +
   geom_hline(yintercept=c(4250, 8500)) +
   scale_y_continuous(breaks = seq(from = 0, to = 18000, by = 2000),
                      labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
   #scale_x_continuous(breaks = seq(from=1982, to=2019, by = 2)) + #rm this to make a whole plot
   expand_limits(x = c(1982, 2019), y = c(0, 18000)) +
   labs(y = "Spawners") +
   annotate("text", x= 2000, y=18000, label = "Ketchikan Index (Survey)", size = 4.5) +
   theme_coho(base_family = "Arial", rotate_text = TRUE) +
   theme(axis.text.x = element_blank(), axis.title.x = element_blank()))

# Figure 2b
(Fig2b <- ggplot(data = hs,
                 aes(x=Year, y = Escapement_Weir)) + 
    geom_bar(stat="identity", fill="lightgray", color="black") +
    geom_hline(yintercept=c(500, 1600)) +
    scale_y_continuous(breaks = seq(from = 0, to = 4000, by = 500),
                       labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_x_continuous(breaks = seq(from=1982, to=2019, by = 2)) +
    expand_limits(y = c(0, 4150)) +
    labs(x="Adult Return Year", y = "Spawners") +
    annotate("text", x= 2000, y=3750, label = 'atop("Hugh Smith Lake","(Weir/Mark-Recapture)")', parse=TRUE, size = 4.5) +
    theme_coho(base_family = "Arial", rotate_text = TRUE))

Fig2 <- Fig2a / Fig2b
ggsave(Fig2, filename = here::here("plotoutput/Fig2.png"), width = 6, height = 6, units = "in")





