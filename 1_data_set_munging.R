library(tidyverse)

# NTL LTER Data Sets =======================
ntl_nuts = 
  read_csv("ntl1_v15_nuts.csv") %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR",
         year4 >= 2005, depth == 0)

ntl_ions = 
  read_csv("ntl2_v15_ions.csv") %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR",
         year4 >= 2005, depth == 0)

ntl_met = 
  read_csv("ntl17_1_v21_dailymet.csv") 

ntl_ice = 
  read_csv("ntl32_v10_ice.csv") %>%
  rename(year4 = year) %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR",
         year4 >= 2005) %>%
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
  ungroup()

ntl_chl = 
  read_csv("ntl35_v6_chl.csv") %>%
  mutate(sampledate = mdy(sampledate), month = month(sampledate)) %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR",
         year4 >= 2005, depth == 0,
         month >= 5, month <= 9) %>%
  select(lakeid, year4, sampledate, depth, chlor, phaeo, month)

ntl_color = 
  read_csv("ntl87_v15_color.csv") %>%
  filter(wavelength == 440) %>%
  mutate(sampledate = mdy(sampledate), 
         month = month(sampledate),
         g440 = 2.303 * (value/(cuvette/100))) %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR",
         year4 >= 2005) %>%
  select(lakeid, year4, month, sampledate, g440)

ntl_light = 
  read_csv("ntl259_v11_light.csv") %>%
  mutate(sampledate = mdy(sampledate), year4 = year(sampledate), month = month(sampledate)) %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR",
         year4 >= 2005,
         month >= 5, month <= 9) %>%
  select(lakeid, sampledate, year4, extcoef)

ntl_zoops = 
  read_csv("ntl37_v15_zoops.csv") %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR",
         year4 >= 2005)

ntl_daphnia = ntl_zoops %>%
  filter(stringr::str_detect(species_name, "DAPHNIA") |
         stringr::str_detect(species_name, "DIAPHANOSOMA")) %>%
  mutate(sampledate = mdy(sample_date)) %>%
  group_by(lakeid, year4, sampledate) %>%
  summarize(density = sum(density, na.rm = TRUE),
            avg_length = mean(avg_length, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(month = month(sampledate)) %>%
  filter(month >= 5, month <= 8) %>%
  group_by(lakeid, year4) %>%
  summarize(mean_density = median(density, na.rm = TRUE),
            mean_length = mean(avg_length, na.rm = TRUE)) %>%
  ungroup() 
         
ntl_fish = 
  read_csv("ntl7_v15_fish.csv") %>%
  filter(lakeid == "SP" | lakeid == "TR" | lakeid == "CB" | lakeid == "TB" | lakeid == "CR",
         year4 >= 2005)


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
  select(lakeid, year4, sampledate, standard_taxon_code, taxon_name, mean_length, abundance, biomass) %>%
  filter(year4 >= 2008, lakeid == "R" | lakeid == "L" | lakeid == "T") %>%
  mutate(sampledate = mdy(sampledate), month = month(sampledate))

cascade_daphnia = cascade_zoops %>% 
  filter(standard_taxon_code == "DAPH" | standard_taxon_code == "DIAPH" | standard_taxon_code == "CDAP") %>%
  group_by(lakeid, year4) %>%
  summarize(mean_density = median(abundance, na.rm = TRUE)/1000,
            mean_biomass = mean(biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mean_density = case_when(lakeid == "R" ~ mean_density/12,
                                  lakeid == "L" ~ mean_density/8,
                                  lakeid == "T" ~ mean_density/10))

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

light_extinction = rbind(cascade_light, ntsl_light) %>%
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

daphnia = full_join(cascade_daphnia, ntl_daphnia, by = c("lakeid", "year4", "mean_density"))


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

