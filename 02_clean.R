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


## Convert ghg data from wide to long format 
bc_ghg_long <- bc_ghg %>% 
  gather(key =  year, value = ktCO2e,
       -sector, -subsector_level1,
       -subsector_level2, -subsector_level3) %>%
  mutate(ktCO2e = as.numeric(ktCO2e),
         year = as.integer(as.character(year)))

## Summarize ghg emissions per sector per year and 
## convert to MtCO2e for (from ktCO2e) for plotting
ghg_sector_sum <- bc_ghg_long %>%
  filter(sector != "OTHER LAND USE (Not included in total B.C. emissions)") %>%
  group_by(sector, year) %>%
  summarise(sum = round(sum(ktCO2e, na.rm = TRUE)/1000, digits = 1)) %>% 
  ungroup() %>% 
  mutate(sector = str_replace(to_titlecase(sector), "and", "&"),
         sector = fct_reorder(sector, sum))
  

## Calculate ghg annual totals and convert to MtCO2e (from ktCO2e) for plotting
bc_ghg_sum <- bc_ghg_long %>%
  filter(sector != "OTHER LAND USE (Not included in total B.C. emissions)") %>%
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


## Create separate ENERGY dataframe
bc_ghg_energy <- bc_ghg_long %>% 
  filter(sector == "ENERGY") %>% 
  mutate(general_source = case_when(subsector_level2 == "Road Transportation" ~ "Road",
                                    subsector_level2 == "Railways" ~ "Railway",
                                    subsector_level3 == "Pipeline Transport" ~ "Pipeline Transport",
                                    subsector_level2 == "Other Transportation" ~ "Off-Road",
                                    TRUE ~ subsector_level2),
         general_source = str_replace(general_source, "and", "&")) %>% 
  select(sector, subsector_level1, general_source, year, ktCO2e)

## Summarize ghg emissions in Energy sector by subsector and general sources 
## and convert to MtCO2e (from ktCO2e) for plotting
ghg_energy_group <- bc_ghg_energy %>%
  group_by(sector, subsector_level1, general_source, year) %>%
  summarise(sum = round(sum(ktCO2e, na.rm = TRUE)/1000, digits = 1)) %>%
  filter(subsector_level1 != "CO2 Transport and Storage") %>% 
  ungroup() %>% 
  mutate(general_source = fct_reorder(general_source, -sum))


## Data summaries 

## total in ktCO2e for most recent year 
ghg_est_ktco2e <- bc_ghg_long %>%
  filter(sector != "OTHER LAND USE (Not included in total B.C. emissions)") %>%
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
  filter(sector != "OTHER LAND USE (Not included in total B.C. emissions)") %>%
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

# Create tmp folder if not already there and store clean data in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(bc_ghg_long, ghg_sector_sum, bc_ghg_sum, normalized_measures,
     bc_ghg_per_capita, bc_ghg_energy,ghg_energy_group,
     ghg_est_Mtco2e, previous_year, baseline_year, file = "tmp/clean_data.RData")

