library(tidyverse)
library(corrplot)
library(neonUtilities)

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

cascade_recent = read_csv("data/sonde/Cascade_2022_2024_2025_dailyDO.csv") %>%
  mutate(sampledate = mdy(sampledate))

do_neon = read_csv("data/sonde/dailyDO_NEON.csv") %>%
  mutate(sampledate = mdy(sampledate))

# Your NEON API token should be stored as an environment variable
# usethis::edit_r_environ()
# token <- Sys.getenv("NEON_TOKEN")
# 
# # Download high-frequency water-quality data from NEON
# neon_wq <- loadByProduct(dpID = "DP1.20288.001",
#   site = c("LIRO", "CRAM"),
#   startdate = "2016-01", enddate = "2026-10",
#   package = "basic", release = "current",
#   include.provisional = TRUE, check.size = FALSE,
#   token = token)
# 
# names(neon_wq)
# map(neon_wq, names)
# 
# do_neon <- neon_wq$waq_instantaneous %>%
#   select(siteID,
#     startDateTime,
#     dissolvedOxygen,
#     localDissolvedOxygenSat,
#     dissolvedOxygenFinalQF,
#     everything()) %>%
#   mutate(sampledate = as.Date(startDateTime), 
#          year4 = year(startDateTime),
#          sample_day = ymd(sampledate)) %>%
#   filter(sensorDepth < 1, 
#          !dissolvedOxygen < 0, !dissolvedOxygen > 200) %>%
#   group_by(siteID, sample_day) %>%
#   summarize(avg_do_sat = mean(seaLevelDissolvedOxygenSat, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(year4 = year(sample_day), 
#          flag_avg_do_sat = NA,
#          lake = case_when(siteID == "CRAM" ~ "Crampton",
#                           siteID == "LIRO" ~ "Little Rock")) %>%
#   rename(sampledate = sample_day) %>% select(-siteID)

# write.csv(do_neon, file = "data/sonde/dailyDO_NEON.csv")


all_hf = rbind(cb_hf, sp_hf, tb_hf, tr_hf, cascade_squeal1, cascade_hf, cascade_recent, do_neon) %>%
  filter(!avg_do_sat < 0) %>%
  mutate(month_day = as.Date(paste0("2000-", month(sampledate), 
                                    "-", day(sampledate))),
         month = month(sampledate)) %>%
  filter(month >= 5, month <= 9)

ggplot(all_hf %>% filter(is.na(flag_avg_do_sat),
                         !lake == "SP" & !year4 == 2010, 
                         !lake == "TR" & !year4 == 2008,
                         !lake == "Tuesday",
                         !year4 == 2026),
       aes(x = sampledate, y = avg_do_sat, color = lake, fill = lake)) +
  geom_hline(yintercept = 100, linetype = 'dashed') +
  geom_smooth(method = "gam") +
  theme_bw() + theme(legend.title = element_blank(),
                     legend.position = c(0.35, 0.25)) +
  guides(color = guide_legend(nrow = 4, byrow = FALSE),
         fill  = guide_legend(nrow = 4, byrow = FALSE)) +
  scale_color_manual(values = c("#432818", "#5f0f40", "#9a031e", "#e36414", "#ee9b00", "#6a994e", "#005f73", "#023047")) +   
  scale_fill_manual(values = c("#432818", "#5f0f40", "#9a031e", "#e36414", "#ee9b00", "#6a994e", "#005f73", "#023047")) + 
  xlab("") + ylab("DO %Saturation")



# Prepare daily DO saturation data
do_annually <- all_hf %>% 
  filter(is.na(flag_avg_do_sat),
         !(lake == "SP" & year4 == 2010),
         !(lake == "TR" & year4 == 2008),
         lake != "Tuesday",
         !year4 == 2026) %>%
  group_by(year4, lake) %>%
  summarize(annual_do = mean(avg_do_sat, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = lake, values_from = annual_do)

# Correlation matrix
do_cor <- do_annually %>%
  select(-year4) %>%
  cor(use = "pairwise.complete.obs",
      method = "pearson")

corrplot(abs(do_cor),
  method = "color", type = "lower",
  order = "original",
  diag = FALSE,
  addCoef.col = "black",
  tl.col = "black", tl.srt = 0, 
  is.corr = FALSE, 
  outline = TRUE,
  col = COL1("Greys"),
  col.lim = c(0,1), mar = c(0.5, 0.5, 1, 0.5),
  cl.pos = "n", cl.cex = 1, cl.ratio = 0.2, cl.length = 6)


