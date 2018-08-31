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
library(ggplot2) #plotting
library(envreportutils) #for theme_soe and theme_soe_facet
library(scales) #for label = comma
library(forcats) # fct_rev() for stacking order
library(RColorBrewer) #for colour palette
library(dplyr)


## Read in plotting data from 02_clean.R if not already in environment
if (!exists("bc_ghg_sum")) load("tmp/clean_data.RData")

## @knitr pre

## @knitr ghgtrends

## Line plot of total GHG emissions over time in British Columbia
ghg_time <- ggplot(data = bc_ghg_sum, aes(x = year, y = ghg_estimate)) + 
  geom_line(colour = "#e41a1c", size = 1.5) + 
  ggtitle("Greenhouse Gas Emissions\nTotal") +
  xlab(NULL) + ylab("ktCO2e") +
  scale_y_continuous(limits = c(50000, 72000), breaks=seq(50000, 72000, 2000),
                     expand=c(0,0), label = comma) +
  scale_x_continuous(limits = c(1990, 2016.5), breaks=seq(1992, 2016, 2), expand=c(0,0)) +
  theme_soe() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.margin = unit(c(6,6,6,2),"mm"))
plot(ghg_time)


## @knitr ghgpop

## Line plot of total GHG emissions per person over time in British Columbia
ghg_pop <- ggplot(data = bc_ghg_per_capita, aes(x = year, y = ghg_per_capita)) + 
  geom_line(colour = "#e41a1c", size = 1.5) + 
  ggtitle("Greenhouse Gas Emissions\n per Person") +
  xlab(NULL) + ylab("tCO2e per person") +
  scale_y_continuous(limits = c(12,17.5), breaks=seq(12, 17.5, .5),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1990, 2016.5),  breaks=seq(1992, 2016, 2), expand=c(0,0)) +
  theme_soe() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.margin = unit(c(6,6,6,2),"mm")) 
plot(ghg_pop)


## @knitr ghggdp

## Line plot of total GHG emisions per unit GDP over time 
gdp_time <- ggplot(data = bc_ghg_per_capita, aes(x = year, y = ghg_per_unit_gdp)) + 
  geom_line(colour = "#e41a1c", size = 1.5) + 
  ggtitle("Greenhouse Gas Emissions\n per Unit Gross Domestic Product") +
  xlab (NULL) + ylab ("tCO2e per unit GDP") +
  scale_y_continuous(limits = c(215,490), breaks=seq(215, 490, 25),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1990, 2016.5),  breaks=seq(1992, 2016, 2), expand=c(0,0)) +
  theme_soe() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.margin = unit(c(6,6,6,2),"mm")) 
plot(gdp_time)


## @knitr norm

## Line plot of normalised GHG emissions, GDP and population change over time 
#colour palette for 3 measures
normpal <- c("norm_gdp" = "#e41a1c",
             "norm_ghg" = "#377eb8",
             "norm_population" = "#4daf4a")

norm <- ggplot(data = normalized_measures, aes(x = year, y = estimate,
                                             group = measure, colour = measure)) + 
  geom_line(size = 1.5) +
  scale_y_continuous(limits = c(.9,2.01), breaks=seq(.9, 2, .1),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1990, 2016.5), breaks=seq(1992, 2016, 2), expand=c(0,0)) +
  ggtitle("Relative Greenhouse Gas Emissions, Gross Domestic\n Product & Population Size") +
  xlab(NULL) + ylab("Ratios relative to 1990 levels") +
  scale_colour_manual(name="", values = normpal, guide = FALSE) +
  annotate("text", label = "GDP", colour = "#e41a1c", x = 2004.1, y = 1.56,
           size = 5) +
  annotate("text", label = "GHG", colour = "#377eb8", x = 2010, y = 1.14,
           size = 5) +
  annotate("text", label = "Population", colour = "#4daf4a", x = 2009, y = 1.41,
           size = 5) +
  theme_soe() +
  theme(axis.text = element_text(size =10),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.margin = unit(c(6,6,6,2),"mm")) 
plot(norm)


## @knitr sector
## Stacked area chart of GHG emissions over time by sector

#colour palette for sector plot
sector.order <- rev(levels(ghg_sector_sum$sector))
sector.no <- length(sector.order) + 1
sector.pal <- brewer.pal(sector.no, "Set1")
names(sector.pal) <- sector.order

ghg_stack <- ggplot(data = ghg_sector_sum, aes(x = year, y = sum, fill = fct_rev(sector))) + 
  geom_area(size = .2, alpha = .6) + 
  geom_line(data = bc_ghg_sum, aes(x = year, y = ghg_estimate),
            colour = "black", size = 1, show.legend = FALSE) +
  # ggtitle("Greenhouse Gas Emissions by Sector") +
  xlab (NULL) + ylab ("ktCO2e") +
  scale_y_continuous(limits = c(0,70000), minor_breaks = waiver(),
                     breaks=seq(0, 70000, 10000), expand=c(0,0), label = comma) +
  scale_x_continuous(limits = c(1990, 2016.5), breaks=seq(1992, 2016, 2),
                     expand=c(0,0)) +
  scale_fill_manual(name = "Sector",values = sector.pal,
                    breaks = sector.order) +
  theme_soe() +
  theme(plot.title = element_text(hjust=0.02),
        panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.background = element_rect(colour = "white"))
plot(ghg_stack)


## @knitr energy 
## Facetted line plot of GHG emissions over time by Energy Source

#creating a list for Energy subsector order 
subsector.order <- c("Transport","Stationary Combustion Sources", 
                     "Fugitive Sources")

#creating colour palette for Energy subsector graphs
subsector.no <- length(subsector.order)
subsector.pal <- brewer.pal(subsector.no, "Set1")
names(subsector.pal) <- subsector.order

#generate a background dataset
ghg_energy_group_bg <- ghg_energy_group %>% 
  ungroup() %>% 
  select(-subsector_level1) %>% 
  rename(general_source_line = general_source)

#facet plot
ghg_energy_trends <- ggplot(data = ghg_energy_group,
                            aes(x = year, y = sum, colour = subsector_level1)) + 
  geom_line(data = ghg_energy_group_bg, aes(group = general_source_line), size = .8, colour = "grey", alpha = 0.5) +
  geom_line(size = 1) +
  facet_wrap( ~ general_source, ncol = 4, 
              labeller = label_wrap_gen(width = 25, multi_line = TRUE)) + 
  xlab (NULL) + ylab ("ktCO2e") + 
  #  ggtitle("Sources of Greenhouse Gas Emissions in the Energy Sector") +
  scale_y_continuous(limits = c(0,20000), breaks=seq(0, 20000, 4000), label = comma) +
  scale_x_continuous(limits = c(1990, 2016.5), breaks = seq(1992, 2016, 4), expand = c(0,0)) +
  scale_colour_manual(name = "Energy Subsectors:", values = subsector.pal,
                      breaks = subsector.order) +
  theme_soe_facet() +
  theme(legend.position = ("bottom"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 10, hjust = .8),
        #     axis.text.x = element_text(angle = 45, size = 7, vjust = 0.5),
        axis.text = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        plot.margin = unit(c(5,5,0,2),"mm"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank())
plot(ghg_energy_trends)

## @knitr stop


## Create a folder in directory called out for image files
if (!exists("out"))  dir.create('out', showWarnings = FALSE)


## Printing plots for web in SVG formats

#total ghg over time
svg_px("./out/ghg_plot.svg", width = 500, height = 500)
plot(ghg_time)
dev.off()

#total ghg/gdp over time
svg_px("./out/ghg_gdp_plot.svg", width = 500, height = 500)
plot(gdp_time)
dev.off()

#total ghg per pop over time
svg_px("./out/ghg_pop_plot.svg", width = 500, height = 500)
plot(ghg_pop)
dev.off()

#normalized ghg, gdp and pop compared over time
svg_px("./out/norm_plot.svg", width = 500, height = 500)
plot(norm)
dev.off()

#total ghg by sector over time stacked area chart
svg_px("./out/sector_plot.svg", width = 850, height = 430)
plot(ghg_stack)
dev.off()

#total ghg over time by source for energy sector facet plot
svg_px("./out/energy_plot.svg", width = 836, height = 650)
plot(ghg_energy_trends)
dev.off()

# PNG outputs

#total ghg over time
png_retina(filename = "./out/ghg_plot.png", width = 500, height = 500,
           units = "px", type = "cairo-png")
plot(ghg_time)
dev.off()

#total ghg/gdp over time
png_retina(filename = "./out/ghg_gdp_plot.png", width = 500, height = 500,
           units = "px", type = "cairo-png")
plot(gdp_time)
dev.off()

#total ghg per pop over time
png_retina(filename = "./out/ghg_pop_plot.png", width = 500, height = 500,
           units = "px", type = "cairo-png")
plot(ghg_pop)
dev.off()

#normalized ghg, gdp and pop compared over time
png_retina(filename = "./out/norm_plot.png", width = 500, height = 500,
           units = "px", type = "cairo-png")
plot(norm)
dev.off()

#total ghg by sector over time stacked area chart
png_retina(filename = "./out/sector_plot.png", width = 850, height = 430,
           units = "px", type = "cairo-png")
plot(ghg_stack)
dev.off()

#total ghg over time by source for energy sector facet plot
png_retina(filename = "./out/energy_plot.png", width = 836, height = 650,
           units = "px", type = "cairo-png")
plot(ghg_energy_trends)
dev.off()