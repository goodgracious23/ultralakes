# Sonde data availability figure

# a = nominal conditions
# b = Fish addition
# c = Invasive removal
# d = Species invasion
# e = Nutrient addition
# f = Aquashade dye addition
# g = Dye + Nutrient addition
# h = Lake mixing experiment

library(tidyverse)

lakes = read_csv("C:/Users/grace/OneDrive/Desktop/ultralakes.csv") %>%
  mutate(lake = factor(lake, levels = c("Peter", "Paul", "Tuesday", 
                                        "LittleRock", "Crampton",
                                        "CrystalBog", "TroutBog",
                                        "Crystal", "Trout", "Sparkling")),
         event = case_when(event == 'a' ~ 'no manipulation',
                           event == 'b' ~ 'fish addition',
                           event == 'c' ~ 'invasive removal',
                           event == 'd' ~ 'zoop invasion',
                           event == 'e' ~ 'nutrient addition',
                           event == 'f' ~ 'dye & nutrient addition',
                           event == 'g' ~ 'dye addition',
                           event == 'h' ~ 'lake mixing'),
         event = factor(event, levels = c('fish addition',
                                          'invasive removal',
                                          'zoop invasion',
                                          'nutrient addition',
                                          'dye & nutrient addition',
                                          'dye addition',
                                          'lake mixing', 'no manipulation'))) %>%
  filter(!is.na(event)) %>% rename(manipulation = event)

ggplot(lakes, aes(x = year, y = lake, fill = manipulation)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 3, shape = 21) +
  scale_fill_manual(values = c("#f9c74f", "#f8961e","#f94144",
                                "#90be6d", "#43aa8b", "#277da1", "#6a4c93", "gray90")) +
  theme_minimal() + xlab("") + ylab("")


