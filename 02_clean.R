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

library(tidyr) #wide df to long df
library(dplyr) #data munging
library(stringr)

## Read in raw data from 01_load.R if not already in environment
if (!exists("bc_ghg")) load("tmp/raw_data.RData")

## Convert wide to long format 
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
                                    subsector_level3 == "Off-Road Gasoline" ~ "Off-Road",
                                    subsector_level3 == "Off-Road Diesel" ~ "Off-Road",
                                    TRUE ~ subsector_level2),
         general_source = str_replace(general_source, "and", "&")) %>% 
  select(sector, subsector_level1, general_source, year, ktCO2e)


## Make 'normalized' columns in bc_pop_gdp gdpdata file for comparison of measures
##adding ghg pop and gdp normalized columns
gdpdata$ghgnorm <- gdpdata$BC_GHG_Emissions_ktCO2e/gdpdata$BC_GHG_Emissions_ktCO2e[gdpdata$Year == min(gdpdata$Year)]
gdpdata$gdpnorm <- gdpdata$GDP/gdpdata$GDP[gdpdata$Year == min(gdpdata$Year)]
gdpdata$popnorm <- gdpdata$Population/gdpdata$Population[gdpdata$Year == min(gdpdata$Year)]

gdpdata.norm <- subset(gdpdata, select=c("Year", "ghgnorm", "gdpnorm", "popnorm"))


##Creating separate ENERGY sector dataframe
##subsetting rows
ghgenergy <- subset(ghgdata, ghgdata$sector == "ENERGY")


##creating 16 generalized subsubsector labels for facet plot
ghgenergy$general_source <- ghgenergy$subsector_level2
ghgenergy$general_source[ghgenergy$general_source == "Road Transportation"] <- "Road"
ghgenergy$general_source[ghgenergy$general_source == "Railways"] <- "Railway"
ghgenergy$general_source[ghgenergy$subsector_level3 == "Pipeline Transport"] <- "Pipeline Transport"
ghgenergy$general_source[ghgenergy$subsector_level3 == "Off-Road Gasoline"] <- "Off-Road"
ghgenergy$general_source[ghgenergy$subsector_level3 == "Off-Road Diesel"] <- "Off-Road"

ghgenergy <- subset(ghgenergy, select=c("sector", "subsector_level1", "general_source",
                                        "1990","1991","1992","1993","1994","1995",
                                        "1996","1997","1998","1999","2000","2001",
                                        "2002","2003","2004","2005","2006","2007",
                                        "2008","2009","2010","2011", "2012", "2013", "2014"))

##Editing some source names for plot label formatting
ghgenergy$general_source <- gsub("and", "&", ghgenergy$general_source)


##Converting wide files to long format 
##using reshape to melt the data 


ghgenergymelt <- reshape2::melt(ghgenergy, id.vars=c("sector","subsector_level1","general_source"),
                                variable.name="Year", value.name="ktCO2e")

gdpnormmelt <- reshape2::melt(gdpdata.norm, id.vars=c("Year"),
                              variable.name="Indicator",
                              value.name="Amount")


##Changing year from factor to character manually
ghgmelt$year <- as.integer(as.character(ghgmelt$year))
ghgenergymelt$Year <- as.integer(as.character(ghgenergymelt$Year))
gdpnormmelt$Year <- as.integer(as.character(gdpnormmelt$Year))


