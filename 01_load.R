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

## Loading B.C. GHG emissions and related data from BC Data Catalogue (Open Government License - BC)
## from https://catalogue.data.gov.bc.ca/dataset/british-columbia-greenhouse-gas-emissions

library(readr) #loading the data


ghgdata <- read_csv("https://catalogue.data.gov.bc.ca/dataset/24c899ee-ef73-44a2-8569-a0d6b094e60c/resource/11b1da01-fabc-406c-8b13-91e87f126dec/download/bcghgemissions.csv",
                    na=c("-","","NA"))


gdpdata <- read_csv("https://catalogue.data.gov.bc.ca/dataset/24c899ee-ef73-44a2-8569-a0d6b094e60c/resource/ac3f0132-204a-4a5b-b145-6165fc99a0b3/download/bcghgrelateddata.csv",
                    na=c("-","","NA"))[ ,1:6]

