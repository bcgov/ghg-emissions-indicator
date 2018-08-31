# Copyright 2016 Province of British Columbia
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
library(dplyr) #data munging


## Read in clean data from 02_clean.R if not already in environment
if (!exists("bc_ghg_long")) load("tmp/clean_data.RData")


## Calculate GHG emissions per capita & per unit GDP and convert to tCO2e (from ktCO2e)
bc_ghg_per_capita <- bc_measures %>% 
  mutate(ghg_per_capita = (ghg_estimate/population_estimate)*1000,
         ghg_per_unit_gdp = (ghg_estimate/gdp_estimate)*1000) %>% 
  select(year, ghg_per_capita, ghg_per_unit_gdp)

## Summarize ghg emissions per sector per year
ghg_sector_sum <- bc_ghg_long %>%
  filter(sector != "OTHER LAND USE (Not included in total B.C. emissions)") %>%
  group_by(sector, year) %>%
  summarise(sum = sum(ktCO2e, na.rm=TRUE) %>%
              round (digits = 0))

# ##merge ghg by year and ghg by sector-year dataframes for single summed dataset
# ghgmerge <- rbind(ghgyear, ghgsector)

## Summarize ghg emissions in Energy sector by subsector and general sources
ghg_energy_group <- bc_ghg_energy %>%
  group_by(sector, subsector_level1, general_source, year) %>%
  summarise(sum = sum(ktCO2e, na.rm=TRUE) %>%
              round(digits = 0)) %>%
  filter(subsector_level1 != "CO2 Transport and Storage") 


# Create tmp folder if not already there and store plotting dataframes in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(bc_ghg_sum, bc_ghg_per_capita,
     ghg_sector_sum, ghg_energy_group, file = "tmp/plot_data.RData")


## GHG emission estimate comparison among years
calc_inc <- function(ghg_now, ghg_then) {
  perc <- ((ghg_now/ghg_then)-1)*100
  paste(perc, "%")
}

previous_year <- calc_inc(bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2016"], bc_ghg_sum$ghg_estimate[bc_ghg_sum$year =="2015"])
previous_year
baseline_year <- calc_inc(bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2016"], bc_ghg_sum$ghg_estimate[bc_ghg_sum$year == "2007"])
baseline_year

