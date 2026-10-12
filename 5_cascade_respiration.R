library(tidyverse)
cascade_resp = read_csv("data/sonde/Respiration_Data_Final_v2.csv") %>%
  mutate(sampledate = mdy(date), 
         month_day = as.Date(paste0("2000-",month(sampledate), 
                                    "-", day(sampledate))),
         lake = case_when(lake == "L" ~ "Paul",
                          lake == "R" ~ "Peter",
                          lake == "T" ~ "Tuesday"))

ggplot(cascade_resp, 
       aes(x = month_day, y = resp, color = lake)) + 
  geom_line(linewidth = 1) +
  facet_grid(lake~year) + 
  ylim (-4, 0.5) +
  # scale_y_continuous(transform = "log10") +
  theme_bw() + theme(legend.position = 'none') +
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  labs(x = "", y = "Nighttime Respiration Rate") + 
  scale_color_manual(values = c("#005f73", "#6a994e", "#ee9b00")) 


ggplot(cascade_resp, 
       aes(x = resp, fill = lake)) + 
  geom_histogram(stat = "bin") +
  facet_grid(lake~year) + 
  xlim (-4, 0.5) +
  # scale_y_continuous(transform = "log10") +
  theme_bw() + theme(legend.position = 'none') +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(y = "Count", x = "Nighttime Respiration Rate") +   
  scale_fill_manual(values = c("#005f73", "#6a994e", "#ee9b00")) 


