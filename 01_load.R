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
library(dplyr) #data munging


## Get British Columbia Greehhosue Gas Emissions estimates from B.C. Data Catalogue 
## from https://catalogue.data.gov.bc.ca/dataset/24c899ee-ef73-44a2-8569-a0d6b094e60c
## Data is released under the Open Government License - British Columbia 
## https://www2.gov.bc.ca/gov/content?id=A519A56BC2BF44E4A008B33FCF527F61

# ghgdata <- read_csv("https://catalogue.data.gov.bc.ca/dataset/24c899ee-ef73-44a2-8569-a0d6b094e60c/resource/11b1da01-fabc-406c-8b13-91e87f126dec/download/bcghgemissions.csv",
#                     na=c("-","","NA"))

bc_ghg <- read_csv("tmp/DRAFT_2016_bc_ghg_emissions.csv")


## Get British Columbia Population Estimates [Table: 17-10-0005-01 
## (formerly CANSIM  051-0001)] and Gross Domestic Product 
## [Table: 36-10-0222-01 (formerly CANSIM  384-0038)] from Statistics Canada
## Data is released under the Statistics Canada Open Licence Agreement 
## https://www.statcan.gc.ca/eng/reference/licence)


bc_pop <- get_cansim(1710000501) %>% 
  filter(GEO == "British Columbia",
         REF_DATE >= 1990 & REF_DATE <= 2016,
         Sex == "Both sexes",
         `Age group` == "All ages") %>% 
  select(year = REF_DATE, population_estimate = VALUE)
  

bc_gdp <- get_cansim(3610022201) %>%
  filter(GEO == "British Columbia",
         REF_DATE >= 1990 & REF_DATE <= 2016,
         Prices == "Chained (2012) dollars",
         Estimates == "Gross domestic product at market prices") %>% 
  select(year = REF_DATE, gdp_estimate = VALUE)

bc_pop_gdp <- bc_pop %>% 
  left_join(bc_gdp)

# Write out related indicators as CSV (pop & gdp)
write_csv(bc_pop_gdp, "tmp/bc_ghg_related_data.csv")

# Create tmp folder if not already there and store objects in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(bc_ghg, bc_pop_gdp, file = "tmp/raw_data.RData")

