<a rel="Delivery" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Trends in Greenhouse Gas Emissions in B.C.  
 
A set of R scripts to populate an indicator on trends in greenhouse gas emissions in British Columbia. These scripts reproduce the results and graphs published on [Environmental Reporting BC](http://www.env.gov.bc.ca/soe/indicators/sustainability/ghg-emissions.html).

## Usage

### Data
[British Columbia Greenhouse Gas Emissions](https://catalogue.data.gov.bc.ca/dataset/24c899ee-ef73-44a2-8569-a0d6b094e60c) data are sourced from the [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset?download_audience=Public), released under the
[Open Government Licence - British Columbia](http://www2.gov.bc.ca/gov/content/governments/about-the-bc-government/databc/open-data/open-government-license-bc).

British Columbia population estimates ([Table: 17-10-0005-01](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501)) and gross domestic product ([Table: 36-10-0222-01](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3610022201)) data are sourced from [Statistics Canada](https://www.statcan.gc.ca/eng/start), released under the [Statistics Canada Open Licence Agreement](https://www.statcan.gc.ca/eng/reference/licence). 


### Code
There are four core scripts that are required for the indicator, they need to be run in order:

- 01_clean.R
- 02_analysis.R
- 03_visualize.R
- 04_output.R

The `run_all.R` script can be `source`ed to run it all at once.

Most packages used in the analysis can be installed from CRAN using `install.packages()`, but you will need to install [envreportutils](https://github.com/bcgov/envreportutils) and [cansim](https://github.com/mountainMath/cansim) using remotes:


```r
install.packages("remotes") # If you don't already have it installed

remotes::install_github("bcgov/envreportutils")
remotes::install_github("mountainmath/cansim")
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/ghg-emissions-indicator/issues).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## License

    Copyright 2016 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
    
This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC) for a complete list of our repositories on GitHub.
