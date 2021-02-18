# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


## Loading R libraries for script
library(tidyr) #wide to long df
library(dplyr) #data munging
library(stringr) #work with strings
library(forcats) #work with factors
library(scales) #comma()
library(envreportutils) #to_titlecase()


## Read in raw data from 01_load.R if not already in environment
if (!exists("bc_ghg")) load("tmp/raw_data.RData")

elevate_sectors <- function(df, sectors) {
  mutate(df, 
  elevate = sector %in% sectors,
  sector = ifelse(elevate, subsector_level1, sector),
  subsector_level1 = ifelse(elevate, subsector_level2, subsector_level1),
  subsector_level2 = ifelse(elevate, subsector_level3, subsector_level2), 
  subsector_level3 = ifelse(elevate, NA_character_, subsector_level3)) %>% 
  select(-elevate)
}

## Convert ghg data from wide to long format 
bc_ghg_long <- bc_ghg %>% 
  gather(key =  year, value = ktCO2e,
       -sector, -subsector_level1,
       -subsector_level2, -subsector_level3) %>%
  mutate(ktCO2e = as.numeric(ktCO2e),
         year = as.integer(as.character(year))) %>% 
  elevate_sectors(c("IPPU, AGRICULTURE, AND WASTE", "AFFORESTATION AND DEFORESTATION")) %>% 
  mutate(across(contains("sector"), ~ {
    x <- str_replace(to_titlecase(.x), "(\\b)and(\\b)", "\\1&\\2")
    str_remove(x, regex("\\s\\(ippu\\)", ignore_case = TRUE))
    }
    ))


## Summarize ghg emissions per sector per year and 
## convert to MtCO2e for (from ktCO2e) for plotting
ghg_sector_sum <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(sector, year) %>%
  summarise(sum = round(sum(ktCO2e, na.rm = TRUE)/1000, digits = 1)) %>% 
  ungroup() %>% 
  mutate(sector = fct_reorder(sector, sum))
  

## Calculate ghg annual totals and convert to MtCO2e (from ktCO2e) for plotting
bc_ghg_sum <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(year) %>%
  summarise(ghg_estimate = round(sum(ktCO2e, na.rm = TRUE)/1000, digits = 1)) %>% 
  mutate(sector = "British Columbia") %>% 
  select(sector, year, ghg_estimate)


## Add ghg totals in MtCO2e by year to bc_pop_gdp data
bc_measures <- bc_pop_gdp %>% 
  mutate(year = as.integer(year)) %>%
  left_join(bc_ghg_sum) %>% 
  select(-sector)


## Make normalized dateframe for relative comparisons and convert to long format
normalized_measures <- bc_measures %>% 
  mutate(norm_ghg = ghg_estimate/ghg_estimate[year == min(year)],
         norm_gdp = gdp_estimate/gdp_estimate[year == min(year)],
         norm_population = population_estimate/population_estimate[year == min(year)]) %>% 
  select(year, starts_with("norm")) %>% 
  gather(key = measure, value = estimate, -year)


## Calculate GHG emissions per capita & per unit GDP 
## and convert to tCO2e (from MtCO2e) for plotting
bc_ghg_per_capita <- bc_measures %>% 
  mutate(ghg_per_capita = (ghg_estimate/population_estimate)*1000000,
         ghg_per_unit_gdp = (ghg_estimate/gdp_estimate)*1000000) %>% 
  select(year, ghg_per_capita, ghg_per_unit_gdp)


# Cleaning steps for economic sector data
## Convert economic sector data from wide to long format, modifying original function

## Convert ghg data from wide to long format 
ghg_econ_long <- ghg_econ %>% 
  gather(key =  year, value = ktCO2e,
         -sector, -subsector_level1,
         -subsector_level2) %>%
  mutate(ktCO2e = as.numeric(ktCO2e),
         year = as.integer(as.character(year))) %>% 
  mutate(across(contains("sector"), ~ {
    x <- str_replace(to_titlecase(.x), "(\\b)and(\\b)", "\\1&\\2")
    str_remove(x, regex("\\s\\(ippu\\)", ignore_case = TRUE))
  }
  ))

# To shorten names for plotting
ghg_econ_long <- ghg_econ_long %>%
  mutate(sector = case_when(sector == "Light Manufacturing, Construction, & Forest Resources" ~ "Other Industry",
                            sector == "Afforestation & Deforestation" ~ "Deforestation",
                            TRUE ~ sector))

# Summarise by economic sector, convert to MtCo2
econ_sector_sum <- ghg_econ_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(sector, year) %>%
  summarise(sum = round(sum(ktCO2e, na.rm = TRUE)/1000, digits = 1)) %>% 
  ungroup() %>% 
  mutate(sector = fct_reorder(sector, sum))

# Summarise by subsector - use the smallest sector level as final (i.e. subsector_level2 if it exists, if not subsector_level1)
ghg_econ_sub <- ghg_econ_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  mutate(subsector_final = if_else(is.na(subsector_level2), subsector_level1, subsector_level2)) %>%
  mutate(MtCO2e = ktCO2e/1000)%>%
  arrange(subsector_final, year) %>% 
  select(sector, year, subsector_final, MtCO2e) %>% 
  mutate(subsector_final=if_else(is.na(subsector_final), sector, subsector_final)) 

ghg_econ_sub <- ghg_econ_sub %>% left_join(econ_sector_sum, by= c('sector', 'year')) #add in sector sums for each year

#change names for clarity
ghg_econ_sub <- ghg_econ_sub %>%
  mutate(subsector_final = case_when
         (subsector_final == "Bus, Rail, & Domestic Aviation" ~ "Passenger: Bus, Rail, & Domestic Aviation",
           subsector_final == "Domestic Aviation & Marine" ~ "Freight: Domestic Aviation & Marine",
           subsector_final == "Cars, Trucks, & Motorcycles" ~ "Passenger: Cars, Trucks, & Motorcycles",
           subsector_final == "Heavy-Duty Trucks & Rail" ~ "Freight: Heavy-Duty Trucks & Rail",
                            TRUE ~ subsector_final))
## Data summaries 

## total in ktCO2e for most recent year 
ghg_est_ktco2e <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(year) %>%
  summarise(ghg_estimate = round(sum(ktCO2e, na.rm = TRUE), digits = 0)) %>% 
  mutate(sector = "British Columbia") %>% 
  select(sector, year, ghg_estimate) %>% 
  filter(year == max_ghg_yr) %>% 
  pull()
ghg_est_ktco2e

ghg_est_Mtco2e <- round(ghg_est_ktco2e/1000, digits = 1)

## Calculate ghg annual totals in ktCO2e
bc_ghg_sum_ktco2e <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(year) %>%
  summarise(ghg_estimate = round(sum(ktCO2e, na.rm = TRUE), digits = 0)) %>% 
  mutate(sector = "British Columbia") %>% 
  select(sector, year, ghg_estimate)


## GHG emission estimate comparison among years
calc_inc <- function(ghg_vector, year_vector, since) {
  if (length(ghg_vector) != length(year_vector)) {
    stop("ghg and years must be the same length", call. = FALSE)
  }
  
  ghg_now <- ghg_vector[which.max(year_vector)]
  ghg_then <- ghg_vector[year_vector == since]
  
  perc <- ((ghg_now/ghg_then) - 1)*100
  round(perc, digits = 1)
}

## Comparisons
previous_year <- calc_inc(bc_ghg_sum_ktco2e$ghg_estimate, bc_ghg_sum_ktco2e$year, since = max_ghg_yr - 1 )
previous_year
baseline_year <- calc_inc(bc_ghg_sum_ktco2e$ghg_estimate, bc_ghg_sum_ktco2e$year, since = 2007)
baseline_year
three_year <- calc_inc(bc_ghg_sum_ktco2e$ghg_estimate, bc_ghg_sum_ktco2e$year, since = max_ghg_yr - 3)
three_year
ten_year <- calc_inc(bc_ghg_sum_ktco2e$ghg_estimate, bc_ghg_sum_ktco2e$year, since = max_ghg_yr - 10)
ten_year

# Calculate CleanBC target level for 2025 compared to 2007 baseline

baseline_2007 <- bc_ghg_sum_ktco2e$ghg_estimate[bc_ghg_sum_ktco2e$year==2007]

clean_bc_2025 <- ((baseline_2007 * 0.84)/1000) #based on Clean BC target of 16% reduction from 2007 levels

current_ghg <- bc_ghg_sum_ktco2e$ghg_estimate[bc_ghg_sum_ktco2e$year==max_ghg_yr]

cleanbc_reduction <- round((1-(clean_bc_2025/ghg_est_Mtco2e))*100, digits=1)

reduction_mt <- ghg_est_Mtco2e - clean_bc_2025

# Create tmp folder if not already there and store clean data in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(bc_ghg_long, ghg_sector_sum, bc_ghg_sum, normalized_measures,
     bc_ghg_per_capita, max_ghg_yr,
     ghg_est_Mtco2e, previous_year, baseline_year, 
     ghg_econ_long, econ_sector_sum, ghg_econ_sub, 
     baseline_2007, clean_bc_2025, current_ghg, 
     cleanbc_reduction, reduction_mt,
     file = "tmp/clean_data.RData")

