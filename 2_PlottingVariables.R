
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
                !(lakeid == "R" & year4 == 2025),
                !groups == "Dystrophic"),
       aes(x = ice_off_doy, y = log(100)/extcoef, 
           color = lakename, fill = lakename)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("#9b2226", "#0a9396","#bb3e03", "#ee9b00", "#005f73")) +
  scale_fill_manual(values = c("#9b2226","#0a9396","#bb3e03", "#ee9b00", "#005f73")) +
  # facet_wrap(~groups, scale = "free_y") +
  theme_bw() +
  # theme(legend.position = "none") +
  labs(y = "Photic Zone Depth (m)",
       x = "Ice Off Day of Year") + 
  ylim(20,3)


# Precipitation ==============================
ggplot(wateryear_precip %>% filter(year4 >= 2000, !year4 == 2026), 
       aes(x = year4, y = summer_precip_anomaly)) + 
  geom_line(color = "#005f73", linewidth = 1.5) + 
  geom_line(aes(x = year4, y = total_snow_anomaly/10), 
            color = "#bb3e03", linewidth = 1.5)

ggplot(wateryear_precip %>% filter(year4 >= 2005), 
       aes(x = summer_precip_anomaly, total_snow_anomaly, 
           color = ice_off_anomaly)) + 
  geom_point(alpha = 1, size = 3) + 
  ggrepel::geom_text_repel(aes(label = year4), vjust = -0.7, size = 3) + 
  scale_color_gradient2(low = "#bb3e03", mid = "gray80", high = "#005f73", midpoint = 0) + 
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Total Summer Precipitation Anomaly (mm)") + 
  ylab("Total Winter Snowfall Anomaly (mm)") +
  xlim(-350, 350) + ylim(-1900, 1900)


# Chlorophyll ==============================
ggplot(chlorophyll %>% filter(!chlor > 110), 
       aes(x = year4, y = chlor, color = lakename)) + geom_point() + 
  # scale_y_continuous(transform = "log10") +
  facet_wrap(~lakeid, scale = "free_y") +
  geom_smooth(method = "gam") + 
  theme_bw() 

ggplot(summer_chlorophyll %>% 
         filter(lakename %in% c("Sparkling Lake", "Trout Lake"),
                year4 > 2007, !is.na(mean_biomass)),
       aes(x = mean_chl, y = mean_biomass, color = treatment)) +
  stat_ellipse(aes(fill = treatment, group = treatment),
    geom = "polygon", color = NA, alpha = 0.3, level = 0.8) +
  scale_color_manual(values = c("#ee9b00","#ca6702","#005f73", "#0a9396")) +
  scale_fill_manual(values = c("#ee9b00","#ca6702","#005f73", "#0a9396")) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_path(linewidth = 0.7, alpha = 0.5) +
  theme_bw() + 
  labs(x = "Mean Summer Chlorophyll",
       y = "Cladoceran Biomass (mg/L)")

