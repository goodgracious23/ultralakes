library(tidyverse)

sp_hf = read_csv("data/sonde/ntl4_1_v8_sparkling.csv") %>%
  mutate(sampledate = mdy(sampledate),
         lake = "Sparkling") %>%
  select(lake, year4, sampledate, avg_do_sat, flag_avg_do_sat)

cb_hf = read_csv("data/sonde/ntl69_crystalbog.csv") %>%
  mutate(sampledate = mdy(sampledate),
         year4 = year(sampledate),
         lake = "Crystal Bog",
         flag_avg_do_sat = NA) %>%
  rename(avg_do_sat = do_sat) %>%
  select(lake, year4, sampledate, avg_do_sat, flag_avg_do_sat)

tr_hf = read_csv("data/sonde/ntl117_1_v9_trout.csv") %>%
  mutate(sampledate = mdy(sampledate),
         lake = "Trout") %>%
  select(lake, year4, sampledate, avg_do_sat, flag_avg_do_sat)

tb_hf = read_csv("data/sonde/ntl118_troutbog.csv") %>%
  mutate(sampledate = mdy(sampledate),
         year4 = year(sampledate),
         lake = "Trout Bog",
         flag_avg_do_sat = NA) %>%
  rename(avg_do_sat = do_sat) %>%
  select(lake, year4, sampledate, avg_do_sat, flag_avg_do_sat)

cascade_squeal1 = read_csv("data/sonde/cascade_hf_2008_2011.csv") %>%
  rename(year4 = year, do_sat = doSat) %>%
  mutate(datetime = mdy_hm(datetime),
         yday = yday(datetime)) %>%
  group_by(lake, year4, yday) %>%
  summarize(avg_do_sat = mean(do_sat, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sampledate = as.Date(paste0(year4, "-01-01")) + yday - 1,
         flag_avg_do_sat = NA) %>%
  filter(!year4 == 2011) %>%
  select(lake, year4, sampledate, avg_do_sat, flag_avg_do_sat)

cascade_hf = read_csv("data/sonde/cascade_daily_2011_2019.csv") %>%
  rename(lake = Lake, year4 = Year, avg_do_sat = DO_Sat) %>%
  mutate(sampledate = as.Date(paste0(year4, "-01-01")) + DOY - 1,
         flag_avg_do_sat = NA,
         lake = case_when(lake == "L" ~ "Paul",
                          lake == "R" ~ "Peter",
                          lake == "T" ~ "Tuesday")) %>%
  select(lake, year4, sampledate, avg_do_sat, flag_avg_do_sat)

all_hf = rbind(cb_hf, sp_hf, tb_hf, tr_hf, cascade_squeal1, cascade_hf) %>%
  filter(!avg_do_sat < 0) %>%
  mutate(month_day = as.Date(paste0("2000-", month(sampledate), 
                                    "-", day(sampledate))),
         month = month(sampledate)) %>%
  filter(month >= 5, month <= 9)

ggplot(all_hf %>% filter(is.na(flag_avg_do_sat),
                         !lake == "SP" & !year4 == 2010, 
                         !lake == "TR" & !year4 == 2008,
                         !lake == "Tuesday"),
       aes(x = sampledate, y = avg_do_sat, color = lake, fill = lake)) + 
  # geom_point(size = 0.2, alpha = 0.2) +
  # facet_wrap(~lake, scale = "free_y") + 
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_smooth(method = "gam") +
  theme_bw() + theme(legend.title = element_blank()) +
  scale_color_manual(values = c("#5f0f40", "#9a031e", "#e36414", "#ee9b00", "#6a994e", "#005f73")) +   
  scale_fill_manual(values = c("#5f0f40", "#9a031e", "#e36414", "#ee9b00", "#6a994e", "#005f73")) + 
  xlab("") + ylab("DO %Saturation")



