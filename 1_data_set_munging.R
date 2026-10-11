library(tidyverse)

# NTL LTER Data Sets =======================
ntl_nuts = 
  read_csv("ntl1_v15_nuts.csv") %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR", year4 >= 2005, depth == 0)

ntl_ions = 
  read_csv("ntl2_v15_ions.csv") %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR", year4 >= 2005, depth == 0)


ntl_ice = 
  read_csv("ntl32_v10_ice.csv") %>%
  rename(year4 = year) %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR") %>%
  mutate(lastopen = mdy(lastopen), lastopen_doy = yday(lastopen),
         ice_on = mdy(ice_on), ice_on_doy = yday(ice_on),
         lastice = mdy(lastice), lastice_doy = yday(lastice),
         ice_off = mdy(ice_off), ice_off_doy = yday(ice_off),
         ice_on_doy = case_when(ice_on_doy < 200 ~ ice_on_doy+365,
                                TRUE ~ ice_on_doy))

ntl_ice_avg = ntl_ice %>%
  group_by(year4) %>%
  summarize(ice_on_doy = mean(ice_on_doy, na.rm = TRUE),
            ice_off_doy = mean(ice_off_doy, na.rm = TRUE),
            duration = mean(duration, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ice_off_anomaly = ice_off_doy - 111.36)

ntl_met = 
  read_csv("ntl18_v11_met.csv") %>%
  mutate(sampledate = mdy(sampledate), month = month(sampledate),
         wateryear = case_when(month < 10 ~ year4, 
                               month >= 10 ~ year4+1)) 

summer_precip = ntl_met %>%
  filter(month >= 5, month <= 9) %>%
  group_by(wateryear) %>%
  summarize(summer_precip = sum(precip, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(summer_precip_anomaly = summer_precip - 495.6)

total_precip = ntl_met %>%
  group_by(wateryear) %>%
  summarize(total_precip = sum(precip, na.rm = TRUE),
            total_snow = sum(snow, na.rm = TRUE),
            snow_depth = mean(snow_depth, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_snow_anomaly = total_snow-2345.5)

wateryear_precip = left_join(total_precip, summer_precip, by = "wateryear") %>% rename(year4 = wateryear) %>%
  left_join(., ntl_ice_avg, by = "year4")

ntl_chl = 
  read_csv("ntl35_v6_chl.csv") %>%
  mutate(sampledate = mdy(sampledate), month = month(sampledate)) %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR", year4 >= 2005, depth == 0, month >= 5, month <= 9) %>%
  select(lakeid, year4, sampledate, depth, chlor, phaeo, month)

ntl_color = 
  read_csv("ntl87_v15_color.csv") %>%
  filter(wavelength == 440) %>%
  mutate(sampledate = mdy(sampledate), 
         month = month(sampledate),
         g440 = 2.303 * (value/(cuvette/100))) %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR", year4 >= 2005) %>%
  select(lakeid, year4, month, sampledate, g440)

ntl_light = 
  read_csv("ntl259_v11_light.csv") %>%
  mutate(sampledate = mdy(sampledate), year4 = year(sampledate), month = month(sampledate)) %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR", year4 >= 2005, month >= 5, month <= 9) %>%
  select(lakeid, sampledate, year4, extcoef)

ntl_zoops = 
  read_csv("ntl37_v15_zoops.csv") %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR", year4 >= 2005)

ntl_daphnia = ntl_zoops %>%
  mutate(sampledate = mdy(sample_date), month = month(sampledate)) %>%
  filter(month >= 5, month <= 8,
         stringr::str_detect(species_name, "DAPHNIA")) %>%
  mutate(avg_length = case_when(year4 <= 2022 ~ avg_length,
                                year4 >2022 ~ avg_length/1000),
         biomass = ((1.5*10^-8)*(avg_length*1000)^2.84)*density) %>%
  group_by(lakeid, year4) %>%
  summarize(mean_biomass = mean(biomass, na.rm = TRUE)) %>%
  ungroup()

         
ntl_fish = 
  read_csv("ntl7_v15_fish.csv") %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR", year4 >= 2005)


# Cascade Data ============================================
cascade_carbon = read_csv("cascade_carbon_v05.csv") %>%
  select(lakeid, lakename, year4, daynum, sampledate, depth, depth_id, doc, absorbance) %>%
  filter(year4 >= 2008, lakeid == "R" | lakeid == "L" | lakeid == "T",
         depth == "PML") %>%
  mutate(g440 = 2.303 * (absorbance/(10/100)), 
         sampledate = mdy(sampledate),
         month = month(sampledate)) %>%
  select(lakeid, year4, month, sampledate, g440)

cascade_light = read_csv("cascade_PhysicalChemicalLimnology_v0.7.csv") %>%
  filter(year4 >= 2005, lakeid == "R" | lakeid == "L" | lakeid == "T",
         !is.na(irradianceWater), !is.na(irradianceDeck),
         !irradianceWater <= 0) %>%
  mutate(sampledate = mdy(sampledate)) %>%
  mutate(fraction_surface_irradiance = irradianceWater/irradianceDeck,
         ln_surface_irradiance = log(fraction_surface_irradiance)) %>%
  group_by(lakeid, sampledate) %>%
  summarise(extcoef = (lm(ln_surface_irradiance ~ depth)$coefficients['depth'])*-1, 
            r2 = summary(lm(ln_surface_irradiance ~ depth))$r.squared) %>%
  ungroup() %>%
  mutate(year4 = year(sampledate), extcoef = as.numeric(extcoef)) %>%
  select(-r2)

cascade_chl = read_csv("cascade_processData_V0.5_routines.csv") %>%
  select(lakeid, year4, sampledate, depth, chla, phaeo) %>%
  filter(year4 >= 2005, lakeid == "R" | lakeid == "L" | lakeid == "T", depth == 0) %>%
  rename(chlor = chla) %>%
  mutate(sampledate = mdy(sampledate), month = month(sampledate))

cascade_zoops = read_csv("cascade_zooplankton_v07.csv") %>%
  select(lakeid, year4, sampledate, standard_taxon_code, group_code, taxon_name, mean_length, abundance, biomass) %>%
  filter(year4 >= 2008, lakeid == "R" | lakeid == "L" | lakeid == "T") %>%
  mutate(sampledate = mdy(sampledate), month = month(sampledate))

cascade_daphnia = cascade_zoops %>% 
  mutate(month = month(sampledate)) %>%
  filter(month >= 8) %>%
  filter(standard_taxon_code == "DAPH" | standard_taxon_code == "DIAPH" | 
           standard_taxon_code == "CDAP" | standard_taxon_code == "HGIB" ) %>%
  group_by(lakeid, year4) %>%
  summarize(mean_biomass = median(biomass*10, na.rm = TRUE)) %>%
  ungroup()

# Combined Data ==========================================
g440 = rbind(cascade_carbon, ntl_color) %>%
  mutate(lakename = case_when(lakeid == "CB" ~ "Crystal Bog",
                              lakeid == "CR" ~ "Crystal Lake",
                              lakeid == "L" ~ "Paul Lake",
                              lakeid == "R" ~ "Peter Lake",
                              lakeid == "T" ~ "Tuesday Lake",
                              lakeid == "SP" ~ "Sparkling Lake",
                              lakeid == "TB" ~ "Trout Bog",
                              lakeid == "TR" ~ 'Trout Lake')) %>%
  left_join(., ntl_ice_avg, by = "year4") %>%
  mutate(groups = case_when(lakeid == "CB" ~ "Dystrophic",
                            lakeid == "T" ~ "Dystrophic",
                            lakeid == "TB" ~ "Dystrophic",
                            lakeid == "R" ~ "Lightly Stained",
                            lakeid == "L" ~ "Lightly Stained",
                            lakeid == "TR" ~ "Clear",
                            lakeid == "CR" ~ "Clear",
                            lakeid == "SP" ~ "Clear"),
         groups = factor(groups, levels = c("Clear", "Lightly Stained", "Dystrophic")))

light_extinction = rbind(cascade_light, ntl_light) %>%
  mutate(lakename = case_when(lakeid == "CB" ~ "Crystal Bog",
                              lakeid == "CR" ~ "Crystal Lake",
                              lakeid == "L" ~ "Paul Lake",
                              lakeid == "R" ~ "Peter Lake",
                              lakeid == "T" ~ "Tuesday Lake",
                              lakeid == "SP" ~ "Sparkling Lake",
                              lakeid == "TB" ~ "Trout Bog",
                              lakeid == "TR" ~ 'Trout Lake'),
         month = month(sampledate)) %>%
  left_join(., ntl_ice_avg, by = "year4") %>%
  mutate(groups = case_when(lakeid == "CB" ~ "Dystrophic",
                            lakeid == "T" ~ "Dystrophic",
                            lakeid == "TB" ~ "Dystrophic",
                            lakeid == "R" ~ "Lightly Stained",
                            lakeid == "L" ~ "Lightly Stained",
                            lakeid == "TR" ~ "Clear",
                            lakeid == "CR" ~ "Clear",
                            lakeid == "SP" ~ "Clear"),
         groups = factor(groups, levels = c("Clear", "Lightly Stained", "Dystrophic")))

chlorophyll = rbind(cascade_chl, ntl_chl) %>%
  mutate(lakename = case_when(lakeid == "CB" ~ "Crystal Bog",
                              lakeid == "CR" ~ "Crystal Lake",
                              lakeid == "L" ~ "Paul Lake",
                              lakeid == "R" ~ "Peter Lake",
                              lakeid == "T" ~ "Tuesday Lake",
                              lakeid == "SP" ~ "Sparkling Lake",
                              lakeid == "TB" ~ "Trout Bog",
                              lakeid == "TR" ~ 'Trout Lake')) 

daphnia = rbind(cascade_daphnia, ntl_daphnia) %>%
  mutate(treatment = case_when(lakeid == "SP" & year4 < 2021 ~ "Sparkling reference",
                                 lakeid == "SP" & year4 >= 2021 ~ "Sparkling cisco addition",
                                 lakeid == "L" ~ "Paul reference",
                                 lakeid == "R" & year4 <= 2011 ~ "Peter bass addition",
                                 lakeid == "R" & year4 > 2011 ~ "Peter nutrient additions",
                                 lakeid == "TR" & year4 <= 2013 ~ "Trout lake trout addition",
                                 lakeid == "TR" & year4 > 2013 ~ "Trout bythotrephes invasion"))

summer_chlorophyll = chlorophyll %>%
  group_by(lakeid, year4) %>%
  summarize(mean_chl = median(chlor, na.rm = TRUE)) %>%
  left_join(., daphnia, by = c("lakeid", "year4")) %>%
  mutate(lakename = case_when(lakeid == "CB" ~ "Crystal Bog",
                              lakeid == "CR" ~ "Crystal Lake",
                              lakeid == "L" ~ "Paul Lake",
                              lakeid == "R" ~ "Peter Lake",
                              lakeid == "T" ~ "Tuesday Lake",
                              lakeid == "SP" ~ "Sparkling Lake",
                              lakeid == "TB" ~ "Trout Bog",
                              lakeid == "TR" ~ 'Trout Lake')) %>%
  mutate(groups = case_when(lakeid == "CB" ~ "Dystrophic",
                            lakeid == "T" ~ "Dystrophic",
                            lakeid == "TB" ~ "Dystrophic",
                            lakeid == "R" ~ "Lightly Stained",
                            lakeid == "L" ~ "Lightly Stained",
                            lakeid == "TR" ~ "Clear",
                            lakeid == "CR" ~ "Clear",
                            lakeid == "SP" ~ "Clear"),
         groups = factor(groups, levels = c("Clear", "Lightly Stained", "Dystrophic")))

