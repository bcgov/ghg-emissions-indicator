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


## Summarize ghg emissions per sector per year
ghg_sector_sum <- bc_ghg_long %>%
  filter(sector != "OTHER LAND USE (Not included in total B.C. emissions)") %>%
  group_by(sector, year) %>%
  summarise(sum = sum(ktCO2e, na.rm=TRUE) %>%
              round (digits = 0)) %>% 
  ungroup() %>% 
  mutate(sector = str_replace(sector, "AND", "&"),
         sector = recode(sector, 
                         `Afforestation and Deforestation` = "AFFORESTATION & DEFORESTATION"),
         sector = fct_reorder(sector, sum))
  

## Calculate ghg annual totals
bc_ghg_sum <- bc_ghg_long %>%
  filter(sector != "OTHER LAND USE (Not included in total B.C. emissions)") %>%
  group_by(year) %>%
  summarise(ghg_estimate = round(sum(ktCO2e, na.rm=TRUE), digits = 0)) %>% 
  mutate(sector = "British Columbia") %>% 
  select(sector, year, ghg_estimate)


## Add ghg totals by year to bc_pop_gdp data
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


## Calculate GHG emissions per capita & per unit GDP and convert to tCO2e (from ktCO2e)
bc_ghg_per_capita <- bc_measures %>% 
  mutate(ghg_per_capita = (ghg_estimate/population_estimate)*1000,
         ghg_per_unit_gdp = (ghg_estimate/gdp_estimate)*1000) %>% 
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
ghg_energy_group <- bc_ghg_energy %>%
  group_by(sector, subsector_level1, general_source, year) %>%
  summarise(sum = sum(ktCO2e, na.rm=TRUE) %>%
              round(digits = 0)) %>%
  filter(subsector_level1 != "CO2 Transport and Storage") %>% 
  ungroup() %>% 
  mutate(general_source = fct_reorder(general_source, -sum))


## Data summaries 

## 2016 total
ghg_est <- bc_ghg_sum %>% 
  filter(year == "2016") %>% 
  pull() %>% 
  signif(digits = 3) %>% 
  comma()


## GHG emission estimate comparison among years
calc_inc <- function(ghg_now, ghg_then) {
  perc <- ((ghg_now/ghg_then)-1)*100
  round(perc, digits = 1)
}

previous_year <- calc_inc(bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2016"], bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2015"])
previous_year
baseline_year <- calc_inc(bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2016"], bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2007"])
baseline_year
three_year <- calc_inc(bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2016"], bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2013"])
three_year
ten_year <- calc_inc(bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2016"], bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2006"])
ten_year


# Create tmp folder if not already there and store clean data in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(bc_ghg_long, ghg_sector_sum, bc_ghg_sum, normalized_measures,
     bc_ghg_per_capita, bc_ghg_energy,ghg_energy_group,
     ghg_est, three_year, ten_year, file = "tmp/clean_data.RData")

