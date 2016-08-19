# Trends in Greenhouse Gas Emissions in B.C.

A set of R scripts to populate an indicator on trends in B.C.'s greenhouse gas (GHG) emissions. These scripts reproduce the graphs published on [Environmental Reporting BC](http://www.env.gov.bc.ca/soe/indicators/sustainability/ghg-emissions.html) in August 2016.

### Usage

#### Data
The [British Columbia Greenhouse Gas Emissions](https://catalogue.data.gov.bc.ca/dataset/24c899ee-ef73-44a2-8569-a0d6b094e60c) data used for the indicator is available from the [BC Data Catalogue](https://catalogue.data.gov.bc.ca/dataset?download_audience=Public) under the
[Open Government License - BC](http://www2.gov.bc.ca/gov/content/governments/about-the-bc-government/databc/open-data/open-government-license-bc)


####Code
There are four core scripts that are required for the indicator, they need to be run in order:

- 01_clean.R
- 02_analysis.R
- 03_visualize.R
- 04_output.R

The `run_all.R` script can be `source`ed to run it all at once.

Most packages used in the analysis can be installed from CRAN using `install.packages()`, but you will need to install [envreportutils](https://github.com/bcgov/envreportutils) using devtools:


```r
install.packages("devtools") # If you don't already have it installed

library(devtools)
install_github("bcgov/envreportutils")
```

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/<repo-name>/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.


### License

    Copyright 2015 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
    
This repository is maintained by [Environmental Reporting BC](http://www2.gov.bc.ca/gov/content?id=FF80E0B985F245CEA62808414D78C41B). Click [here](https://github.com/bcgov/EnvReportBC-RepoList) for a complete list of our repositories on GitHub.
