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
library(stringr) 


## Read in raw data from 01_load.R if not already in environment
if (!exists("bc_ghg")) load("tmp/raw_data.RData")


## Convert ghg data from wide to long format 
bc_ghg_long <- bc_ghg %>% 
  gather(key =  year, value = ktCO2e,
       -sector, -subsector_level1,
       -subsector_level2, -subsector_level3) %>%
  mutate(ktCO2e = as.numeric(ktCO2e),
         year = as.integer(as.character(year)))


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


## Calculate ghg totals
bc_ghg_sum <- bc_ghg_long %>%
  filter(sector != "OTHER LAND USE (Not included in total B.C. emissions)") %>%
  group_by(year) %>%
  summarise(ghg_estimate = round(sum(ktCO2e, na.rm=TRUE), digits = 0)) %>% 
  mutate(sector = "British Columbia") %>% 
  select(sector, year, ghg_estimate)


## Add ghg totals by year to bc_pop_gdp 
bc_measures <- bc_pop_gdp %>% 
  mutate(year = as.integer(as.character(year))) %>%
  left_join(bc_ghg_sum) %>% 
  select(-sector)


## Make normalized dateframe for relative comparisons and convert to long format
normalized_measures <- bc_measures %>% 
  mutate(norm_ghg = ghg_estimate/ghg_estimate[year == min(year)],
         norm_gdp = gdp_estimate/gdp_estimate[year == min(year)],
         norm_population = population_estimate/population_estimate[year == min(year)]) %>% 
  select(year, starts_with("norm")) %>% 
  gather(key = measure, value = estimate, -year)


# Create tmp folder if not already there and store objects in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(bc_ghg_long, bc_ghg_energy, bc_ghg_sum,
     bc_measures, normalized_measures, file = "tmp/clean_data.RData")

