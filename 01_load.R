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
library(readr) #read in csv file
library(cansim) #get Statistics Canada CANSIM data
library(dplyr)
library(bcdata)
library(envreportutils)

if(!dir.exists('tmp'))dir.create('tmp')

## Remember to first run the envreportbc-snippets project 
## (specifically, ghg_pi_econ-sector.R and ghg_pi_ipcc-sector.R) and then transfer
## the two .CSV files produced into the tmp folder.

## Get British Columbia Greenhouse Gas Emissions estimates from B.C. Data Catalogue 
## from https://catalogue.data.gov.bc.ca/dataset/british-columbia-greenhouse-gas-emissions
## Data is released under the Open Government License - British Columbia 
## https://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61


# If pulling in from Data Catalogue, use bcdata() functions below, if pulling from local repo - use lines #38-39

bc_ghg <- bcdc_get_data(record="24c899ee-ef73-44a2-8569-a0d6b094e60c",
                        resource='11b1da01-fabc-406c-8b13-91e87f126dec')

# bc_ghg = read.csv('tmp/bc_ghg_emissions_by_ipcc_sector_1990-2021.csv') |>
#   as_tibble()

#If the year columns have had an 'X' added to them... sometimes happens.
colnames(bc_ghg) = gsub(pattern = '^X', replacement = '', x = names(bc_ghg))

# get the most recent year in numeric format
bc_ghg_yr <- bc_ghg |>
  select(max(matches("^20"))) |>
  colnames()

max_ghg_yr <- as.numeric(bc_ghg_yr)

#bring in economic sector data
# url https://catalogue.data.gov.bc.ca/dataset/british-columbia-greenhouse-gas-emissions/resource/1baa8e16-f1fd-4ea9-9a1d-15f46a5ca066

ghg_econ <- bcdc_get_data(record='24c899ee-ef73-44a2-8569-a0d6b094e60c', 
                           resource='1baa8e16-f1fd-4ea9-9a1d-15f46a5ca066')

# ghg_econ_d = read.csv('tmp/bc_ghg_emissions_by_economic_sector_1990-2021.csv') |> 
#   as_tibble()

#If the year columns have had an 'X' added to them... sometimes happens.
colnames(ghg_econ) = gsub(pattern = '^X', replacement = '', x = names(ghg_econ))

## Get British Columbia Population Estimates [Table: 17-10-0005-01 
## (formerly CANSIM  051-0001)] and Gross Domestic Product 
## [Table: 36-10-0222-01 (formerly CANSIM  384-0038)] from Statistics Canada
## Data is released under the Statistics Canada Open Licence Agreement 
## https://www.statcan.gc.ca/eng/reference/licence)

#add individual gas data
# ghg_gases = read.csv('tmp/bc_ghg_emissions_by_economic_sector_by_gas_1990-2021.csv')

ghg_gases <- bcdc_get_data(record='24c899ee-ef73-44a2-8569-a0d6b094e60c', 
                          resource='99540512-0962-4f51-9bd8-886b6d792b1a')


#If the year columns have had an 'X' added to them... sometimes happens.
colnames(ghg_gases) = gsub(pattern = '^X', replacement = '', x = names(ghg_gases))


#Load BC population data for 1990-2021
bc_pop <- get_cansim(1710000501) |> 
  filter(GEO == "British Columbia",
         REF_DATE >= 1990 & REF_DATE <= max_ghg_yr,
         Sex == "Both sexes",
         `Age group` == "All ages") |> 
  select(year = REF_DATE, population_estimate = VALUE)
  

bc_gdp <- get_cansim(3610022201) |>
  filter(GEO == "British Columbia",
         REF_DATE >= 1990 & REF_DATE <= max_ghg_yr,
         Prices == "Chained (2012) dollars",
         Estimates == "Gross domestic product at market prices") |> 
  select(year = REF_DATE, gdp_estimate = VALUE)

bc_pop_gdp <- bc_pop |> 
  left_join(bc_gdp)


# Write out related indicators as CSV (pop & gdp)
write_csv(bc_pop_gdp, "tmp/bc_ghg_related_data.csv")

# Create tmp folder if not already there and store objects in local repository
save(bc_ghg, bc_pop_gdp, max_ghg_yr, ghg_econ, ghg_gases, file = "tmp/raw_data.RData")
