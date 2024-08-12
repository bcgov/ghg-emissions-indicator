# Copyright 2024 Province of British Columbia
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
         subsector_level1 = ifelse(elevate, subsector_level2, subsector_level1)) %>% 
    select(-elevate)
}

## Convert ghg data from wide to long format 
bc_ghg_long <-bc_ghg %>%
  ## Create new sectors including Transportation, Oil and Gas, Other Industry, and Buildings and Communities  
  mutate(new_sector = case_when(
    subsector_level2 %in% c('Domestic Aviation', 'Road Transport', 'Railways', 
                            'Domestic Marine', 'Off-Road Transport') ~ 'Transportation', 
    subsector_level2 %in% c('Petroleum Refining Industries', 'Oil and Gas Extraction',
                            'Pipeline Transport', 'Oil and Natural Gas') ~ 'Oil and Gas',
    subsector_level2 %in% c('Public Electricity and Heat Production', 'Mining', 
                            'Manufacturing Industries', 'Construction', 'Agriculture and Forestry', 
                            'Coal Mining', 'Mineral Products', 'Chemical Industry', 'Metal Production', 
                            'Production and Consumption of Halocarbons, SF6, and NF3', 
                            'Non-Energy Products from Fuels and Solvent Use', 
                            'Other Product Manufacture and Use', 'Enteric Fermentation',
                            'Manure Management', 'Agricultural Soils', 'Field Burning of Agricultural Residues',
                            'Liming, Urea Application, and Other Carbon-Containing Fertilizers') ~ 'Other Industry',
    subsector_level2 %in% c('Commercial and Institutional', 'Residential', 'Solid Waste Disposal',
                            'Biological Treatment of Solid Waste', 'Wastewater Treatment and Discharge',
                            'Incineration and Open Burning of Waste', 'Industrial Wood Waste Landfills',
                            'Deforestation', 'Afforestation', 'Grassland Converted to Cropland', 
                            'Other Land Converted to Wetlands') ~ 'Buildings and Communities')) %>%
  ## Create subsectors for new sectors
  mutate(new_sector_level1 = case_when(
    subsector_level2 %in% c('Road Transport') ~ 'Road Transportation',
    subsector_level2 %in% c('Domestic Aviation', 'Railways', 'Domestic Marine') ~ 'Other Transport',
    subsector_level2 %in% c('Off-Road Transport') ~ 'Off-Road Transportation',
    subsector_level2 %in% c('Petroleum Refining Industries', 'Oil and Gas Extraction',
                            'Pipeline Transport') ~ 'Fuel Use',
    subsector_level2 %in% c('Oil and Natural Gas') ~ 'Fugitives',
    subsector_level2 %in% c('Manufacturing Industries') ~ 'Manufacturing',
    subsector_level2 %in% c('Mineral Products', 'Chemical Industry', 'Metal Production',
                            'Production and Consumption of Halocarbons, SF6, and NF3', 
                            'Non-Energy Products from Fuels and Solvent Use',
                            'Other Product Manufacture and Use') ~ 'Industrial Processes',
    subsector_level2 %in% c('Enteric Fermentation', 'Manure Management', 'Agricultural Soils', 
                            'Field Burning of Agricultural Residues',
                            'Liming, Urea Application, and Other Carbon-Containing Fertilizers') ~ 'Agriculture',
    subsector_level2 %in% c('Public Electricity and Heat Production', 'Mining', 'Construction',
                            'Agriculture and Forestry', 'Coal Mining') ~ 'Other Industry',
    subsector_level2 %in% c('Residential') ~ 'Residential Buildings',
    subsector_level2 %in% c('Commercial and Institutional') ~ 'Commercial Buildings',
    subsector_level2 %in% c('Solid Waste Disposal', 'Biological Treatment of Solid Waste', 'Wastewater Treatment and Discharge',
                            'Incineration and Open Burning of Waste', 'Industrial Wood Waste Landfills') ~ 'Waste',
    subsector_level2 %in% c('Deforestation', 'Afforestation', 'Grassland Converted to Cropland', 
                            'Other Land Converted to Wetlands') ~ 'Land-Use Change')) %>%
  # Unit: MtCO2e
  gather(key = year, value = MtCO2e,
         -sector, -subsector_level1,
         -subsector_level2, -new_sector, -new_sector_level1) %>%
  mutate(MtCO2e = as.numeric(MtCO2e),
         year = as.integer(as.character(year))) %>% 
  elevate_sectors(c("IPPU, AGRICULTURE, AND WASTE", "AFFORESTATION AND DEFORESTATION")) %>% 
  mutate(across(contains("sector"), ~ {
    #x <- str_replace(to_titlecase(.x), "(\\b)and(\\b)", "\\1&\\2")
    x <- to_titlecase(.x)
    str_remove(x, regex("\\s\\(ippu\\)", ignore_case = TRUE))
  }
  )) %>% 
  mutate(sector = ifelse(sector == 'Land-Use Change', 'Afforestation & Deforestation', sector)) %>% 
  mutate(subsector_level1 = ifelse(subsector_level1 == 'Transport', 'Transportation', subsector_level1))

## Summarize ghg emissions (Unit: MtCO2e) per sector per year
ghg_sector_sum <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(new_sector, year) %>%
  summarise(sum = sum(MtCO2e, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(new_sector = fct_reorder(new_sector, sum))

## Summarize ghg emissions (Unit: MtCO2e) per sub_sector per year
ghg_new_sector_level1_sum <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(new_sector_level1, year) %>%
  summarise(sum = sum(MtCO2e, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(new_sector_level1 = fct_reorder(new_sector_level1, sum))

## Calculate ghg annual totals (Unit: MtCO2e) for plotting
bc_ghg_sum <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(year) %>%
  summarise(ghg_estimate = sum(MtCO2e, na.rm = TRUE)) %>%
  mutate(sector = "British Columbia") %>% 
  select(sector, year, ghg_estimate)

## Calculate non-forest ghg annual totals (Unit: MtCO2e) for plotting
## the afforestation & deforestation emissions were removed.
bc_ghg_sum_no_forest <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  filter(sector != "Afforestation & Deforestation") %>%
  group_by(year) %>%
  summarise(ghg_no_forest = sum(MtCO2e, na.rm = TRUE)) %>%
  mutate(sector = "British Columbia") %>% 
  select(sector, year, ghg_no_forest)

## Add ghg totals in MtCO2e by year to bc_pop_gdp data
bc_measures <- bc_pop_gdp %>% 
  mutate(year = as.integer(year)) %>%
  left_join(bc_ghg_sum_no_forest) %>%
  left_join(bc_ghg_sum)%>%
  select(-sector)

## Make normalized dateframe for relative comparisons and convert to long format
normalized_measures <- bc_measures %>% 
  mutate(norm_ghg = ghg_estimate/ghg_estimate[year == 1990],
         norm_gdp = gdp_estimate/gdp_estimate[year == 1990],
         norm_population = population_estimate/population_estimate[year == 1990]) %>% 
  select(year, starts_with("norm")) %>% 
  gather(key = measure, value = estimate, -year)


## Calculate GHG emissions per capita & per unit GDP 
## and convert to tCO2e (from MtCO2e) for plotting (1 MtCO2e = 1000000 tCO2e )
bc_ghg_per_capita <- bc_measures %>% 
  mutate(ghg_per_capita = (ghg_no_forest/population_estimate)*1000000,
         ghg_per_unit_gdp = (ghg_estimate/gdp_estimate)*1000000) %>% 
  select(year, ghg_per_capita, ghg_per_unit_gdp)

# Summarise by subsector - use the smallest sector level as final (i.e. new_sector_level1)
ghg_sub <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(new_sector_level1, year, new_sector) %>%
  summarise(MtCO2e = sum(MtCO2e, na.rm = TRUE)) %>%
  arrange(new_sector_level1, year) %>% 
  select(new_sector, year, new_sector_level1, MtCO2e)

ghg_sub <- ghg_sub %>% left_join(ghg_sector_sum, by= c('new_sector', 'year')) #Add in sector sums for each year

# Change names for clarity
ghg_sub <- ghg_sub %>%
  mutate(new_sector_level1 = case_when
         (new_sector_level1 == "Other Industry" ~ "Other Non-oil and Gas Industry",
           new_sector_level1 == "Other Transport" ~ "Railways, Domestic Aviation and Marine",
           TRUE ~ new_sector_level1))

#Cleaning steps for ghg type
## Convert ghg gases data from wide to long format 
ghg_gases_long <- ghg_gases %>%
  mutate(new_sector = case_when(
    subsector_level2 %in% c('Domestic Aviation', 'Road Transport', 'Railways', 
                            'Domestic Marine', 'Off-Road Transport') ~ 'Transportation', 
    subsector_level2 %in% c('Petroleum Refining Industries', 'Oil and Gas Extraction',
                            'Pipeline Transport', 'Oil and Natural Gas') ~ 'Oil and Gas',
    subsector_level2 %in% c('Public Electricity and Heat Production', 'Mining', 
                            'Manufacturing Industries', 'Construction', 'Agriculture and Forestry', 
                            'Coal Mining', 'Mineral Products', 'Chemical Industry', 'Metal Production', 
                            'Production and Consumption of Halocarbons, SF6, and NF3', 
                            'Non-Energy Products from Fuels and Solvent Use', 
                            'Other Product Manufacture and Use', 'Enteric Fermentation',
                            'Manure Management', 'Agricultural Soils', 'Field Burning of Agricultural Residues',
                            'Liming, Urea Application, and Other Carbon-Containing Fertilizers') ~ 'Other Industry',
    subsector_level2 %in% c('Commercial and Institutional', 'Residential', 'Solid Waste Disposal',
                            'Biological Treatment of Solid Waste', 'Wastewater Treatment and Discharge',
                            'Incineration and Open Burning of Waste', 'Industrial Wood Waste Landfills',
                            'Deforestation', 'Afforestation', 'Grassland Converted to Cropland', 
                            'Other Land Converted to Wetlands') ~ 'Buildings and Communities')) %>%
  mutate(new_sector_level1 = case_when(
    subsector_level2 %in% c('Road Transport') ~ 'Road Transportation',
    subsector_level2 %in% c('Domestic Aviation', 'Railways', 'Domestic Marine') ~ 'Other Transport',
    subsector_level2 %in% c('Off-Road Transport') ~ 'Off-Road Transportation',
    subsector_level2 %in% c('Petroleum Refining Industries', 'Oil and Gas Extraction',
                            'Pipeline Transport') ~ 'Fuel Use',
    subsector_level2 %in% c('Oil and Natural Gas') ~ 'Fugitives',
    subsector_level2 %in% c('Manufacturing Industries') ~ 'Manufacturing',
    subsector_level2 %in% c('Mineral Products', 'Chemical Industry', 'Metal Production',
                            'Production and Consumption of Halocarbons, SF6, and NF3', 
                            'Non-Energy Products from Fuels and Solvent Use',
                            'Other Product Manufacture and Use') ~ 'Industrial Processes',
    subsector_level2 %in% c('Enteric Fermentation', 'Manure Management', 'Agricultural Soils', 
                            'Field Burning of Agricultural Residues',
                            'Liming, Urea Application, and Other Carbon-Containing Fertilizers') ~ 'Agriculture',
    subsector_level2 %in% c('Public Electricity and Heat Production', 'Mining', 'Construction',
                            'Agriculture and Forestry', 'Coal Mining') ~ 'Other Industry',
    subsector_level2 %in% c('Residential') ~ 'Residential Buildings',
    subsector_level2 %in% c('Commercial and Institutional') ~ 'Commercial Buildings',
    subsector_level2 %in% c('Solid Waste Disposal', 'Biological Treatment of Solid Waste', 'Wastewater Treatment and Discharge',
                            'Incineration and Open Burning of Waste', 'Industrial Wood Waste Landfills') ~ 'Waste',
    subsector_level2 %in% c('Deforestation', 'Afforestation', 'Grassland Converted to Cropland', 
                            'Other Land Converted to Wetlands') ~ 'Land-Use Change')) %>%
  gather(key =  year, value = MtCO2e, -gas,
         -sector, -subsector_level1,
         -subsector_level2, -new_sector, -new_sector_level1) %>%
  mutate(MtCO2e = as.numeric(MtCO2e),
         year = as.integer(as.character(year))) %>% 
  elevate_sectors(c( "LAND-USE CHANGE")) %>% 
  mutate(across(contains("sector"), ~ {
    #x <- str_replace(to_titlecase(.x), "(\\b)and(\\b)", "\\1&\\2")
    x <- to_titlecase(.x)
    str_remove(x, regex("\\s\\(ippu\\)", ignore_case = TRUE))
  }
  )) %>% 
  mutate(sector = ifelse(sector == 'Land-Use Change', 'Afforestation & Deforestation', sector)) %>% 
  mutate(subsector_level1 = ifelse(subsector_level1 == 'Transport', 'Transportation', subsector_level1))

## Calculate ghg annual totals by gas for plotting (Unit: MtCO2e)
ghg_gases_sum <- ghg_gases_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(gas, year) %>%
  summarise(ghg_estimate = sum(MtCO2e, na.rm = TRUE)) %>%
  mutate(sector = "British Columbia") %>% 
  select(gas, sector, year, ghg_estimate)

#Calculate excluding deforestation and afforestation
ghg_gases_sum_no_forest <- ghg_gases_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  filter(sector != "Afforestation & Deforestation") %>%
  group_by(gas, year) %>%
  summarise(ghg_estimate = sum(MtCO2e, na.rm = TRUE)) %>%
  mutate(sector = "British Columbia") %>% 
  select(gas, sector, year, ghg_estimate)

## Data summaries 
## Total in MtCO2e for most recent year 
ghg_est_MtCO2e <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(year) %>%
  summarise(ghg_estimate = sum(MtCO2e, na.rm = TRUE)) %>% 
  mutate(sector = "British Columbia") %>% 
  select(sector, year, ghg_estimate) %>% 
  filter(year == max_ghg_yr) %>% 
  pull()
ghg_est_MtCO2e

## Calculate ghg annual totals in MtCO2e
bc_ghg_sum_MtCO2e <- bc_ghg_long %>%
  filter(!grepl("other emissions not included", sector, ignore.case = TRUE)) %>%
  group_by(year) %>%
  summarise(ghg_estimate = sum(MtCO2e, na.rm = TRUE)) %>% 
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
  perc
}

## Comparisons
previous_year <- calc_inc(bc_ghg_sum_MtCO2e$ghg_estimate, bc_ghg_sum_MtCO2e$year, since = max_ghg_yr - 1 )
previous_year
baseline_year <- calc_inc(bc_ghg_sum_MtCO2e$ghg_estimate, bc_ghg_sum_MtCO2e$year, since = 2007)
baseline_year
three_year <- calc_inc(bc_ghg_sum_MtCO2e$ghg_estimate, bc_ghg_sum_MtCO2e$year, since = max_ghg_yr - 3)
three_year
ten_year <- calc_inc(bc_ghg_sum_MtCO2e$ghg_estimate, bc_ghg_sum_MtCO2e$year, since = max_ghg_yr - 10)
ten_year

# Calculate CleanBC target level for 2030 and 2025 compared to 2007 baseline

baseline_2007 = bc_ghg_sum$ghg_estimate[bc_ghg_sum$year==2007]
baseline_2007_transport = ghg_sector_sum$sum[ghg_sector_sum$year==2007 & ghg_sector_sum$new_sector=="Transportation"]
baseline_2007_Other_industry = ghg_sector_sum$sum[ghg_sector_sum$year==2007 & ghg_sector_sum$new_sector=="Other Industry"]
baseline_2007_Buildings_Communities = ghg_sector_sum$sum[ghg_sector_sum$year==2007 & ghg_sector_sum$new_sector=="Buildings and Communities"]
baseline_2007_Oils_Gas = ghg_sector_sum$sum[ghg_sector_sum$year==2007 & ghg_sector_sum$new_sector=="Oil and Gas"]

clean_bc_2025 <- (baseline_2007 * 0.84) # Based on Clean BC target of 16% reduction from 2007 levels
clean_bc_2030 <- baseline_2007*(1+(-0.4)) # Based on Clean BC target of at least 40% reduction from 2007 levels
clean_bc_2030_transport <- baseline_2007_transport*(1+(-0.27)) # Based on Clean BC target of at least 27% reduction from 2007 levels
clean_bc_2030_Other_industry <- baseline_2007_Other_industry*(1+(-0.38)) # Based on Clean BC target of at least 38% reduction from 2007 levels
clean_bc_2030_Buildings_Communities <- baseline_2007_Buildings_Communities*(1+(-0.59)) # Based on Clean BC target of at least 59% reduction from 2007 levels
clean_bc_2030_Oils_Gas <- baseline_2007_Oils_Gas*(1+(-0.33)) # Based on Clean BC target of at least 33% reduction from 2007 levels

current_ghg <- bc_ghg_sum_MtCO2e$ghg_estimate[bc_ghg_sum_MtCO2e$year==max_ghg_yr]

cleanbc_reduction_2025 <- (1-(clean_bc_2025/ghg_est_MtCO2e))*100
cleanbc_reduction_2030 <- (1-(clean_bc_2030/ghg_est_MtCO2e))*100

reduction_mt_2025 <- ghg_est_MtCO2e - clean_bc_2025
reduction_mt_2030 <- ghg_est_MtCO2e - clean_bc_2030

# Create tmp folder if not already there and store clean data in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(bc_ghg_long, ghg_sector_sum, bc_ghg_sum, bc_ghg_sum_no_forest, normalized_measures,
     bc_ghg_per_capita, max_ghg_yr,
     ghg_est_MtCO2e, previous_year, three_year, baseline_year, 
     ghg_gases_long, ghg_gases_sum, ghg_gases_sum_no_forest, 
     baseline_2007, clean_bc_2025, clean_bc_2030, clean_bc_2030_transport, clean_bc_2030_Other_industry, 
     clean_bc_2030_Buildings_Communities, clean_bc_2030_Oils_Gas, current_ghg, 
     cleanbc_reduction_2025, cleanbc_reduction_2030, reduction_mt_2025, reduction_mt_2030,
     file = "tmp/clean_data.RData")

