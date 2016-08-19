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

#loading libraries for script
library(ggplot2) #for plotting
library(RColorBrewer) #for colour palette
library(extrafont) #for Verdana font
library(grid) #for direct labelling of plots
library(Cairo) #for exporting png with nice text
library(dplyr) #pipe function, data manipulation
library(envreportutils) #for theme_soe and theme_soe_facet


## @knitr pre

##font selection
chart_font_web <- "Verdana"


##PLOTS


## @knitr ghgtrends

##line plot of total GHG emissions over time in BC
ghgtime <- ggplot(data=ghgyear, aes(x = year, y = sum)) + 
  geom_line(colour = "#e41a1c", size = 1.5) + 
  ggtitle("Greenhouse Gas Emissions\nTotal") +
  xlab ("Year") + ylab ("ktCO2e") +
  scale_y_continuous(limits = c(52000, 72000), breaks=seq(0, 72000, 2000),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1990, 2014), breaks=seq(1990, 2014, 2), expand=c(0,0)) +
  theme_soe() +
  theme(axis.text = element_text(size = 9),
        plot.title = element_text(size = 12), plot.margin = unit(c(5,5,0,0),"mm"))
plot(ghgtime)


## @knitr ghgpop

##line plot of total GHG emissions per person over time in BC
ghgpop <- ggplot(data=gdpdata, aes(x = Year, y = GHGs_per_Capita_tCO2e_person)) + 
  geom_line(colour = "#e41a1c", size = 1.5) + 
  ggtitle("Greenhouse Gas Emissions\n per Person") +
  xlab ("Year") + ylab ("tCO2e per person") +
  scale_y_continuous(limits = c(13,18), breaks=seq(13, 18, .5),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1990, 2014),  breaks=seq(1990, 2014, 2), expand=c(0,0)) +
  theme_soe() +
  theme(axis.text = element_text(size = 9),
        plot.title = element_text(size = 12), plot.margin = unit(c(5,5,0,0),"mm")) 
plot(ghgpop)


## @knitr ghggdp

##line plot of total GHG per unit GDP over time 
gdptime <- ggplot(data=gdpdata, aes(x = Year, y = GHGs_per_GDP_tCO2e_million_GDP)) + 
  geom_line(colour = "#e41a1c", size = 1.5) + 
  ggtitle("Greenhouse Gas Emissions\n per Unit Gross Domestic Product") +
  xlab ("Year") + ylab ("tCO2e per unit GDP") +
  scale_y_continuous(limits = c(240,490), breaks=seq(240, 490, 25),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1990, 2014),  breaks=seq(1990, 2014, 2), expand=c(0,0)) +
  theme_soe() +
  theme(axis.text = element_text(size = 9),
        plot.title = element_text(size = 12), plot.margin = unit(c(5,5,0,0),"mm")) 
plot(gdptime)


## @knitr norm

##line plot of normalised GHG emissions, GDP and pop change over time 

##colour palette for 3 measures
normpal <- c("gdpnorm" = "#e41a1c", "ghgnorm" = "#377eb8", "popnorm" = "#4daf4a")

ghg.gdp.norm <- ggplot(data=gdpnormmelt, aes(x = Year, y = Amount,
                                             group = Indicator, colour = Indicator)) + 
  geom_line(size = 1.5) +
  scale_y_continuous(limits = c(.9,1.9), breaks=seq(.9, 1.9, .1),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1990, 2014), breaks=seq(1990, 2014, 2), expand=c(0,0)) +
  ggtitle("Relative Greenhouse Gas Emissions, Gross Domestic\n Product & Population Size") +
  xlab ("Year") + ylab ("Ratios relative to 1990 levels") +
  scale_colour_manual(name="", values = normpal, guide = FALSE) +
  annotate("text", label = "GDP", colour = "#e41a1c", x = 2004.1, y = 1.56,
           size = 5, family = chart_font_web) +
  annotate("text", label = "GHG", colour = "#377eb8", x = 2010, y = 1.14,
           size = 5, family = chart_font_web) +
  annotate("text", label = "Population", colour = "#4daf4a", x = 2009, y = 1.41,
           size = 5, family = chart_font_web) +
  theme_soe() +
  theme(axis.text = element_text(size = 9),
        plot.title = element_text(size = 12),
        plot.margin = unit(c(5,5,0,0),"mm")) 
plot(ghg.gdp.norm)


## @knitr sector

##stacked area chart of GHG emissions over time by sector

## order the data frame by secotr, based on mean sum of emissions
ghgsector <- order_df(ghgsector, target_col = "sector", value_col = "sum", fun = mean)

sector.order <- rev(levels(ghgsector$sector))

##creating colour palette for sector graphs
sector.no <- length(sector.order)+1
sector.pal <- brewer.pal(sector.no, "Set1")
names(sector.pal) <- sector.order

#stacked sector plot
ghgstack <- ggplot(data=ghgsector, aes(x = year, y = sum, fill = sector)) + 
  geom_area(size=.2, alpha=.6) + 
  geom_line(data=ghgyear, aes(x = year, y = sum),
            colour = "black", size = 1, show.legend = FALSE) +
  # ggtitle("Greenhouse Gas Emissions by Sector") +
  xlab ("Year") + ylab ("ktCO2e") +
  scale_y_continuous(limits = c(0,75000), minor_breaks = waiver(), 
                     breaks=seq(0, 70000, 10000), expand=c(0,0)) +
  scale_x_continuous(limits = c(1990, 2014), breaks=seq(1990, 2014, 2), expand=c(0,0)) +
  scale_fill_manual(name = "Sector",values = sector.pal,
                    breaks = sector.order) +
  theme_soe() +
  theme(plot.title = element_text(hjust=0.02),
        panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9), 
        legend.background = element_rect(colour = "white"))
plot(ghgstack)


## @knitr energy 

##line plots of GHG emissions over time by Energy Source using facets

#creating a list for Energy subsector order 
subsector.order <- c("Transport","Stationary Combustion Sources", 
                     "Fugitive Sources")

#creating colour palette for Energy subsector graphs
subsector.no <- length(subsector.order)
subsector.pal <- brewer.pal(subsector.no, "Set1")
names(subsector.pal) <- subsector.order

#Ordering column
ghgenergygroup <- order_df(ghgenergygroup, "general_source", "sum", mean, na.rm = TRUE, desc = TRUE)

#Background dataset
ghgenergygroup_bg <- ghgenergygroup %>% 
  ungroup() %>% 
  select(-subsector_level1) %>% 
  rename(general_source_line = general_source)

#facet plot
ghgenergytrends <- ggplot(data=ghgenergygroup,
                          aes(x = Year, y = sum, colour = subsector_level1)) + 
  geom_line(data=ghgenergygroup_bg, aes(group = general_source_line), size=.8, colour = "grey", alpha = 0.5) +
  geom_line(size=1) +
  facet_wrap( ~ general_source, ncol=4, 
              labeller = label_wrap_gen(width = 25, multi_line = TRUE)) + 
  xlab ("Year") + ylab ("ktCO2e") + 
  #  ggtitle("Sources of Greenhouse Gas Emissions in the Energy Sector") +
  scale_y_continuous(limits = c(0,17000), breaks=seq(0, 17000, 4000)) +
  scale_x_continuous(limits = c(1990, 2014), breaks=seq(1994, 2014, 4), expand=c(0,0)) +
  scale_colour_manual(name = "Energy Subsectors:", values = subsector.pal,
                      breaks = subsector.order) +
  theme_soe_facet() +
  theme(legend.position = ("bottom"),
        legend.title = element_text(size = 10),
        
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 6, hjust = .8),
        #     axis.text.x = element_text(angle = 45, size = 7, vjust = 0.5),
        axis.text = element_text(size = 8),
        strip.text.x = element_text(size = 9),
        plot.margin = unit(c(10,7,0,0),"mm"),
        panel.grid.major.x = element_blank(),
        legend.background = element_blank())
plot(ghgenergytrends)


## @knitr stop

##Printing plots for web 

## create a folder in directory called out for png files
dir.create('out', showWarnings = FALSE)

## Set plot dimensions:
lg_w.px <- 627
lg_h.px <- 431
sm_w.px <- 455
sm_h.px <- 315

lg_dpi <- 95
sm_dpi <- 72

##small plots

##total ghg over time by year
ggsave("./out/ghg_plot_small.png", plot = ghgtime, type = "cairo-png", 
       width = sm_w.px / sm_dpi, height = sm_h.px / sm_dpi, units="in", 
       dpi=sm_dpi)

##total ghg/gdp over time by year
ggsave("./out/ghg_gdp_plot_small.png", plot = gdptime, type = "cairo-png", 
       width = sm_w.px / sm_dpi, height = sm_h.px / sm_dpi, units="in", 
       dpi=sm_dpi)

##total ghg per pop over time by year
ggsave("./out/ghg_pop_plot_small.png", plot = ghgpop, type = "cairo-png", 
       width = sm_w.px / sm_dpi, height = sm_h.px / sm_dpi, units="in", 
       dpi=sm_dpi)

##ghg, gdp and pop compared over time by year
ggsave("./out/norm_plot_small.png", plot = ghg.gdp.norm, type = "cairo-png", 
       width = sm_w.px / sm_dpi, height = sm_h.px / sm_dpi, units="in", 
       dpi=sm_dpi)


##large plots

##total ghg over time by year
ggsave("./out/ghg_plot.png", plot = ghgtime, type = "cairo-png", 
       width = lg_w.px / lg_dpi, height = lg_h.px / lg_dpi, units="in", 
       dpi=lg_dpi)

##total ghg/gdp over time by year
ggsave("./out/ghg_gdp_plot.png", plot = gdptime, type = "cairo-png", 
       width = lg_w.px / lg_dpi, height = lg_h.px / lg_dpi, units="in", 
       dpi=lg_dpi)

##total ghg per pop over time by year
ggsave("./out/ghg_pop_plot.png", plot = ghgpop, type = "cairo-png", 
       width = lg_w.px / lg_dpi, height = lg_h.px / lg_dpi, units="in", 
       dpi=lg_dpi)

##ghg, gdp and pop compared over time by year
ggsave("./out/norm_plot.png", plot = ghg.gdp.norm, type = "cairo-png", 
       width = lg_w.px / lg_dpi, height = lg_h.px / lg_dpi, units="in", 
       dpi=lg_dpi)

##total ghg by sector over time by year stacked area chart
ggsave("./out/sector_plot.png", plot = ghgstack, type = "cairo-png", 
       width = 8.50, height = 4.30, units="in", 
       dpi=100)

##total ghg over time by source for energy sector facet plot
ggsave("./out/energy_plot.png", plot = ghgenergytrends, type = "cairo-png", 
       width = 8.36, height = 6.50, units="in", dpi=100)

