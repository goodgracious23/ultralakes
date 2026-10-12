library(tidyverse)

sparkling_hf = read_csv("data/sonde/ntl4_1_v8_sparkling.csv") %>%
  mutate(sampledate = mdy(sampledate),
         yday = yday(sampledate),
         month_day = as.Date(paste0("2000-", 
                                    month(sampledate), 
                                    "-", day(sampledate)))) %>%
  # filter(year4>=2004) %>%
  filter(year4 == 2012 | year4 == 2013 | 
         year4 == 2021 | year4 == 2022,
         !yday < 120, !yday > 275) %>%
  mutate(treatment = case_when(year4 == 2012 ~ "1_ice",
                               year4 == 2013 ~ "1_ice",
                               year4 == 2021 ~ "2_cisco",
                               year4 == 2022 ~ "2_cisco"))

ggplot(sparkling_hf %>% filter(!avg_do_sat < 95), 
       aes(x = month_day, y = avg_do_sat, color = factor(year4))) + 
  geom_line(linewidth = 1) + 
  scale_color_manual(values = c("#ee9b00", "#5f0f40", "#6a994e", "#005f73")) + 
  facet_wrap(~treatment, ncol = 1) +
  # scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme_bw() + 
  theme(strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = 'none') +
   geom_hline(yintercept = 100, linetype = "dashed") + 
  xlab("") + ylab("DO %Sat")
