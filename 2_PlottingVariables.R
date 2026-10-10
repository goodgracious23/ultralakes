
# g440 =========================================
ggplot(g440 %>% filter(!lakename == "Crystal Lake", !lakename == "Tuesday Lake", !g440<0), 
       aes(x = year4, y = g440, color = lakename)) + 
  geom_point() + 
  # scale_y_continuous(transform = "log10") + 
  theme_bw() +
  geom_smooth(method = "gam") +
  facet_wrap(~lakename, scale = "free_y")

ggplot(g440 %>% filter(!g440<0, month >= 4, month <= 6),
       aes(x = ice_off_doy, y = g440, color = lakename)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~lakename, scale = "free_y")

# Light Extinction ==============================
ggplot(light_extinction, 
       aes(x = year4, y = extcoef, color = lakename)) + geom_point() + 
  scale_y_continuous(transform = "log10") +
  # facet_wrap(~lakeid, scale = "free_y") + 
  geom_smooth(method = "gam") + 
  theme_bw() + 
  labs(y = "Light Extinction Coefficient (m-1)",
       x = "")

ggplot(light_extinction %>% filter(month >= 5, month <= 6) %>%
         filter(!(lakeid == "CR" & year4 == 2012),
                !(lakeid == "CR" & year4 == 2013),
                !(lakeid == "R" & year4 == 2024),
                !(lakeid == "R" & year4 == 2025)),
       aes(x = ice_off_doy, y = extcoef, color = lakename, fill = lakename)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") + 
  facet_wrap(~groups, scale = "free_y") +
  theme_bw() +
  # theme(legend.position = "none") +
  labs(y = "Early Summer\nLight Extinction Coefficient (m-1)",
       x = "Ice Off Day of Year")


# Chlorophyll ==============================
ggplot(chlorophyll %>% filter(!chlor > 110), 
       aes(x = year4, y = chlor, color = lakename)) + geom_point() + 
  # scale_y_continuous(transform = "log10") +
  facet_wrap(~lakeid, scale = "free_y") +
  geom_smooth(method = "gam") + 
  theme_bw() 

ggplot(summer_chlorophyll %>% filter(!lakename == "Tuesday Lake", 
                                     # !(lakename == "Peter Lake" & year4 == 2024),
                                     !groups == "Dystrophic",
                                     !lakename == "Crystal Lake"), 
       aes(x = mean_chl, y = mean_density, color = lakename)) +
  geom_point(size = 2) + 
  geom_path(linewidth = 1) +
  geom_text(aes(label = year4), vjust = -0.7, size = 3) +
  facet_wrap(~groups, scale = "free") + 
  # scale_y_continuous(transform = "log10") + scale_x_continuous(transform = "log10") +
  theme_bw() + 
  labs(x = "Mean Summer Chlorophyll", y = "Mean Large-bodied Cladoceran Density")
