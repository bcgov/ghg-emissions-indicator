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

##loading R libraries for script
library(dplyr) #pipe function, data manipulation


##filter and prep data for graphs

##summarize ghg emissions per year
ghgyear <- ghgmelt %>%
  filter(sector != "OTHER LAND USE") %>%
  group_by(year) %>%
  summarise(sum = sum(ktCO2e, na.rm=TRUE) %>%
              round (digits = 0))

##adding category column and re-ordering columns so category is first
ghgyear["sector"] <- "N/A"
ghgyear$"sector" <- "British Columbia"
ghgyear <- ghgyear[c(3,1,2)]
#ghgyear$sum <- as.integer(ghgyear$sum)

##performing calculations for ghg emission comparison with previous years
calc_inc <- function(ghg_14, ghg_pre) {
  perc <- ((ghg_14/ghg_pre)-1)*100
  paste(perc, "%")
}

calc_inc(ghgyear$sum[ghgyear$year == "2014"], ghgyear$sum[ghgyear$year =="2013"])
calc_inc(ghgyear$sum[ghgyear$year == "2014"], ghgyear$sum[ghgyear$year == "2011"])
calc_inc(ghgyear$sum[ghgyear$year == "2014"], ghgyear$sum[ghgyear$year == "2007"])
calc_inc(ghgyear$sum[ghgyear$year == "2014"], ghgyear$sum[ghgyear$year == "2004"])

##summarize ghg emissions per sector per year
ghgsector <- ghgmelt %>%
  filter(sector != "OTHER LAND USE") %>%
  group_by(sector, year) %>%
  summarise(sum = sum(ktCO2e, na.rm=TRUE) %>%
              round (digits = 0))

##merge ghg by year and ghg by sector-year dataframes for single summed dataset
ghgmerge <- rbind(ghgyear, ghgsector)

##summarize ghg in Energy by subsector and general sources
ghgenergygroup <- ghgenergymelt %>%
  group_by(sector, subsector_level1, general_source, Year) %>%
  summarise(sum = sum(ktCO2e, na.rm=TRUE) %>%
              round(digits = 0)) %>%
  filter(subsector_level1 != "CO2 Transport and Storage") 
