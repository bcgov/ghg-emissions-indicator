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
library(RColorBrewer)#for colour palette
library(plyr)
library(dplyr) #data munging
library(reshape2) #data melting
library(ggrepel)
library(filesstrings) #for removing spaces in filename
library(plotly)

## Read in plotting data from 02_clean.R if not already in environment
if (!exists("bc_ghg_sum")) load("tmp/clean_data.RData")

## Line plot theme
theme_lineplots <- theme(
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16,
                              margin = margin(t = 0, r = 10, b = 0, l = 0,
                                              unit = "pt")),
  plot.title = element_text(size = 17, hjust = 0.5),
  plot.margin = unit(c(6,6,6,6),"mm"),
    legend.title.align = 0
)

# Set plotting parameters common to many plots:
x_scale <- scale_x_continuous(limits = c(1990, max_ghg_yr + 1), 
                              breaks = c(1990, seq(1993, max_ghg_yr + 1, 5),2021), 
                              expand = c(0,0))

## Line plot of total GHG emissions over time in British Columbia
ghg_time = ggplot(data = bc_ghg_sum, aes(x = year, y = ghg_estimate)) + 
  geom_line(colour = "#1B9E77", size = 1.5) +
  geom_point(x=2025, y=clean_bc_2025, color="black", shape=19, size=2)+
  geom_point(x=2007, y=baseline_2007, color="black", shape=19, size=2)+
  geom_segment(aes(x = 2004, xend = 2007, y = 62, yend = baseline_2007), size = 0.7) +
  annotate("text", x=2019, y=clean_bc_2025, label="B.C. 2025 emission target", size = 4)+
  annotate("text", x=2004, y=61.5, label = "2007 baseline", size = 4)+
  labs(title = "Total GHG Emissions") +
  xlab(NULL) + 
  ylab(bquote(Mt~CO[2]*e)) +
  scale_y_continuous(limits = c(50, 72), breaks = seq(52, 72, 2),
                     expand = c(0,0), labels = comma) +
  scale_x_continuous(limits = c(1990, 2026), 
                     breaks = seq(1990, 2025, 5), 
                     expand = c(0,0)) +
  theme_soe() +
  theme_lineplots

plot(ghg_time)

## Line plot of total GHG emissions per person over time in British Columbia
ghg_pop <- ggplot(data = bc_ghg_per_capita, aes(x = year, y = ghg_per_capita)) + 
  geom_line(colour = "#7570B3", linewidth = 1.5) + 
  ggtitle("GHG Emissions per Person") +
  xlab(NULL) + 
  ylab(bquote(t~CO[2]*e~" per Person")) +
  scale_y_continuous(limits = c(11,16), breaks = seq(11, 16, .5),
                     expand = c(0,0)) +
  x_scale +
  theme_soe() +
  theme_lineplots
plot(ghg_pop)


## Line plot of total GHG emisions per unit GDP over time 
gdp_time <- ggplot(data = bc_ghg_per_capita, 
                   aes(x = year, y = ghg_per_unit_gdp)) + 
  geom_line(colour = "#D95F02", linewidth = 1.5) + 
  ggtitle("GHG Emissions per Million Dollars of GDP") +
  xlab(NULL) + 
  ylab(bquote(t~CO[2]*e~" per million dollars of GDP")) +
  scale_y_continuous(limits = c(200,500), breaks = seq(200, 500, 25),
                     expand = c(0,0)) +
  x_scale +
  theme_soe() +
  theme_lineplots
plot(gdp_time)


## Line plot of normalised GHG emissions, GDP and population change over time 
#colour palette for 3 measures

norm.order <- unique(normalized_measures$measure) #gets rid of unused factors
norm.cols<-3
norm.pal <- brewer.pal(norm.cols, "Dark2")
names(norm.pal) <- norm.order

norm_base <- ggplot(data = normalized_measures, 
                    aes(x = year, y = estimate, group = measure, 
                        colour = measure)) + 
  geom_line(linewidth = 1.5) +
  scale_y_continuous(limits = c(.9,2.3), breaks = seq(.9, 2.3, .1),
                     expand = c(0,0)) +
  x_scale+
  labs(title = "Relative GHG Emissions, GDP & Population Size") +
  xlab(NULL) + ylab("Values Indexed Relative to 1990") +
  scale_colour_manual(name="", values = norm.pal, guide = FALSE) +
  theme_soe() +
  theme_lineplots 

norm <- norm_base +
  annotate("text", label = "GDP", colour = "#D95F02",
           x = 2004.1, y = 1.56, size = 5) +
  annotate("text", label = "GHG", colour = "#1B9E77",
           x = 2010, y = 1.14, size = 5) +
  annotate("text", label = "Population", colour = "#7570B3",
           x = 2009, y = 1.41, size = 5)
plot(norm)

norm_print <- norm_base +
  annotate("text", label = "GDP", colour = "#D95F02",
           x = 2004.1, y = 1.56, size = 3) +
  annotate("text", label = "GHG", colour = "#1B9E77",
           x = 2010, y = 1.19, size = 3) +
  annotate("text", label = "Population", colour = "#7570B3",
           x = 2009, y = 1.41, size = 3)
plot(norm_print)

#Breakdown of change in individual ghgs over time
#Remove NF3 as no data
ghg_gases_sum = ghg_gases_sum %>%
  filter(!gas == "NITROGEN TRIFLUORIDE (NF3)e") %>%
  mutate(gas = factor(gas))

#Convert factor labels for gases to html code for plotly

ghg_gases_sum_html = ghg_gases_sum %>%
  mutate(gas = case_when
         (gas == "CARBON DIOXIDE (CO2)" ~ "Carbon Dioxide (CO<sub>2</sub>)",
           gas == "METHANE (CH4)" ~ "Methane (CH<sub>4</sub>)",
           gas == "NITROUS OXIDE (N2O)b" ~ "Nitrous Oxide (N<sub>2</sub>0)",
           gas == "HYDROFLUOROCARBONS (HFCs)c" ~ "Hydroflourocarbons (HFCs)",
           gas == "PERFLUOROCARBONS (PFCs)c" ~ "Perflourocarbons (PFCs)",
           gas == "SULPHUR HEXAFLUORIDE (SF6)d" ~ "Sulphur Hexaflouride (SF<sub>6</sub>)",
           gas == "METHANE (CH4)a" ~ "Methane (CH<sub>4</sub>)")) %>%
  mutate(gas = as.factor(gas))

gas.order <- levels(droplevels(ghg_gases_sum_html$gas)) #gets rid of unused factors
gas.no <- length(gas.order) + 1
nb.cols<-6
gas.pal <- colorRampPalette(brewer.pal(gas.no, "Dark2"))(nb.cols)
col_db <- melt(data.frame(gas.order,gas.pal)) #for use in plotting individual gases
names(gas.pal) <- gas.order

# ghg_gases_year <- ggplot(ghg_gases_sum) + 
#   geom_line(aes(x=year, y=ghg_estimate, col = gas, 
#                 text = paste0(gas, " (", year, "): ", ghg_estimate, " MtCO<sub>2</sub>e"), 
#                 group=gas), linewidth = 1) +  
#   scale_color_manual(name = "Greenhouse Gas", values = gas.pal,
#                      limits = gas.order) +
#   x_scale +
#   labs(x="Year", y="Emissions (MtCO<sub>2</sub>e)<br>by Greenhouse Gas")+
#   theme_soe()+ 
#   theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         axis.text.y = element_text(size = 8),
#         axis.text.x = element_text(size = 10),
#         axis.title.y = element_text(size = 10,
#                                     margin = margin(t = 0, r = 10, b = 0, l = 0,
#                                                     unit = "pt")),
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 10), 
#         legend.background = element_rect(colour = "white"))
# 
# ghg_gases_year

#Annual change of percentage of emission for each gas
ghg_gas_prop = ghg_gases_sum_html %>%
  group_by (year) %>%
  mutate(percentage = (ghg_estimate/sum(ghg_estimate))*100)

labels_pdf = c(expression(Carbon~Dioxide~"("~CO[2]~")"),
               expression(Methane~"("~CH[4]~")"),
               expression(Nitrous~Oxide~"("~N[2]*O~")"),
              paste("Hydroflourocarbons (HFCs)"),
              paste("Perflourocarbons (PFCs)"),
              expression(Sulphur~Hexaflouride~"("~SF[6]~")"))

ghg_gases_prop = ggplot(ghg_gas_prop) +
  geom_bar(aes(x = year,
               y = percentage,
               fill = gas,
               group = gas,
               text = paste0(gas, " (", year, "): ", round(percentage,1), "%")),
           width = 1,
           col = "black",
           linewidth=0.1,
           position="stack", stat="identity") +
  scale_fill_manual(name = "Greenhouse Gas", values = gas.pal,
                  labels = labels_pdf)+
  x_scale + 
  labs(x="", y="Percentage of total emissions for each \nGHG from 1990 to 2021")+
  theme_soe()+
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_rect(colour = "white"))

ghg_gases_prop

ghg_gas_prop = ghg_gases_sum_html %>%
  group_by (year) %>%
  mutate(percentage = (ghg_estimate/sum(ghg_estimate))*100)


ghg_gases_prop_html = ggplot(ghg_gas_prop) +
  geom_bar(aes(x = year,
                            y = percentage,
                            fill = gas,
                            group = gas,
                            text = paste0(gas, " (", year, "): ", round(percentage,1), "%")),
                          width = 1,
                           col = "black",
                           linewidth=0.1,
           position="stack", stat="identity") +
  scale_fill_manual(name = "Greenhouse Gas", values = gas.pal,
                     limits = gas.order)+
  labs(x="", y="Percentage of total emissions for each GHG \nfrom 1990 to 2021")+
  theme_soe()+
  x_scale + 
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_rect(colour = "white"))

ghg_gases_prop_html

#Add summary table (for stats used in report)
ghg_gases_summary = ghg_gas_prop %>%
  group_by(gas) %>%
  summarise(mean = mean(percentage),
            min = min(percentage),
            max = max(percentage))

ghg_gases_summary

#Net displacement from 1990 levels
ghg_gases_net_1990 <- ghg_gases_sum_html %>% 
  group_by(gas) %>%
  mutate(net_ghg = (ghg_estimate)-(ghg_estimate[year == 1990])) %>%
  select(gas, year, starts_with("net"))

ghg_net_1990 <- ggplot(ghg_gases_net_1990) + 
  geom_line(aes(x = year, y = net_ghg, col = gas, group = gas),
            linewidth = 1) +
  xlab("") + 
  ylab(bquote(atop("Annual change in "~Mt~CO[2]*e~ "from 1990", "by Greenhouse Gas")))+
  # xlab(NULL)+
  # ylab(bquote(atop(paste("  Annual change in " ~Mt~CO[2]*e ~" "),paste("from 1990 by Greenhouse Gas")))) +
  scale_color_manual(name = "Greenhouse Gas", values = gas.pal, labels = labels_pdf) +
  x_scale + 
  theme_soe()+ 
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 6,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.background = element_rect(colour = "white"))

ghg_net_1990

ghg_net_1990_html <- ggplot(ghg_gases_net_1990) + 
  geom_line(aes(x = year, y = net_ghg, col = gas, group = gas,
                text = paste0(gas, " (", year, "): ", round(net_ghg,1), " MtCO<sub>2</sub>e")),
            linewidth = 1) +
  labs(x="", y="Annual change in MtCO<sub>2</sub>e from 1990  \nby Greenhouse Gas")+
  # xlab(NULL)+
  # ylab(bquote(atop(paste("  Annual change in " ~Mt~CO[2]*e ~" "),paste("from 1990 by Greenhouse Gas")))) +
  scale_color_manual(name = "Greenhouse Gas", values = gas.pal,
                     limits = gas.order) +
  x_scale +
  theme_soe()+ 
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.background = element_rect(colour = "white"))

ghg_net_1990_html

## Setting up data to provide information on economic sectors
# Remove sectors with no data in any year
econ_sector_sum_data <- econ_sector_sum %>%
  group_by(sector) %>%
  filter(sum(sum)!=0)%>%
  ungroup()

# Set colour palette for sector plot
sector.order <- rev(levels(droplevels(econ_sector_sum_data$sector))) #gets rid of unused factors
sector.no <- length(sector.order) + 1
nb.cols<-9
sector.pal <- colorRampPalette(brewer.pal(sector.no, "Dark2"))(nb.cols)
col_db <- melt(data.frame(sector.order,sector.pal)) #for use in plotting individual sectors
names(sector.pal) <- sector.order


label_static <- econ_sector_sum_data %>%
  filter(year==max(year))


# Line plot of each sector sum over time 
ghg_sector <- ggplot(econ_sector_sum_data, aes(x=year, y=sum, color=fct_rev(sector))) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = sector.pal) +
  # geom_text_repel(aes(label=sector, size=1),
  #                 data = label_static,
  #                 nudge_x=2, direction = "y",
  #                 segment.size = 0.5,
  #                 xlim = c(max(label_static$year),
  #                          max(label_static$year) + 5))+
  xlab(NULL) +  
  ylab(bquote(Mt~CO[2]*e~" by Economic Sector")) + labs(color = "Economic Sector") +
  
  scale_x_continuous(limits = c(1990, max_ghg_yr+1), 
                     breaks = c(1990, seq(1993, max_ghg_yr + 1, 5), 2021), 
                     expand = c(0,0))+
  
  coord_cartesian(clip = "off") +
  theme_soe()+ 
  theme(panel.grid.major = element_line(linewidth = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.background = element_rect(colour = "white"))+
  theme(plot.margin = unit(c(0.5,3.5,0.5,0.5), "cm")) 

plot(ghg_sector)

## Interactive sector plot for ggplotly html output
ghg_sector_html <- ggplot(econ_sector_sum_data) + 
  geom_line(aes(x = year, y = sum, color=fct_rev(sector), 
                text = paste0(sector, " (", year, "): ", round(sum,1), " MtCO<sub>2</sub>e"),
                group = sector),
            linewidth = 1) +
  scale_color_manual(name = "Economic Sector", values = sector.pal,
                     limits = sector.order) +
  x_scale +
  labs(x="", y="Emissions (MtCO<sub>2</sub>e)<br>by Economic Sector")+
  theme_soe() +
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.background = element_rect(colour = "white"))

plot(ghg_sector_html)

### Absolute difference in CO2e emissions by economic sector
abs_diff_econ <- plyr::ddply(econ_sector_sum_data, .(sector), 
                             transform, abs.diff = (sum - sum[year==1990])) 

abs_label_static <- abs_diff_econ %>%
  filter(year==max(year)) %>%
  select(-sum)

ghg_abs_diff <- ggplot(data = abs_diff_econ, 
                       aes(x = year, y = abs.diff, color = fct_rev(sector))) + 
  geom_line(size=1) +
  xlab(NULL) +  ylab(bquote(atop("Annual Change in "~Mt~CO[2]*e~" from 1990", "by Economic Sector"))) +
  labs(color = "Economic Sector") +
  x_scale +
  scale_color_manual(values = sector.pal) +
  # geom_text_repel(aes(label=sector, size=1),
  #                 data = abs_label_static, 
  #                 nudge_x=2, direction = "y", 
  #                 segment.size = 0.5,
  #                 xlim = c(max(abs_label_static$year),
  #                          max(abs_label_static$year) + 5))+
  coord_cartesian(clip = "off") +
  theme_soe() +
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.background = element_rect(colour = "white"))+
  theme(plot.margin = unit(c(0.5,3.5,0.5,0.5), "cm"))

plot(ghg_abs_diff)

## Interactive abs diff plot for ggplotly html output

ghg_abs_diff_html <- ggplot(abs_diff_econ) + 
  geom_line(aes(x = year, y = abs.diff, color=fct_rev(sector), 
                text = paste0(sector, " (", year, "): ", round(abs.diff,1), " MtCO<sub>2</sub>e"),
                group = sector),
            size = 1) +
  scale_color_manual(name = "Economic Sector", values = sector.pal,
                     limits = sector.order) +
  x_scale +
  labs(x="", y="Annual Change in Emissions from 1990<br>(MtCO<sub>2</sub>e)")+
  theme_soe() +
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.background = element_rect(colour = "white"))

plot(ghg_abs_diff_html)


## Create a folder in directory called out for image files
if (!exists("out"))  dir.create('out', showWarnings = FALSE)


for (i in 1:length(sector.order)){
  
  x <- sector.order[i]
  plotcolor <- sector.pal[i]
  p <- ghg_econ_sub %>% filter(sector == x)
  s <- nlevels(as.factor(p$subsector_final))
  
  
  g <- ggplot(p, aes(x = year, y = sum)) +
    geom_area(fill = "gray85", alpha = 0.6) +
    facet_wrap(~fct_reorder(subsector_final, MtCO2e, .desc=TRUE),
               nrow = ifelse(s > 3, 2, 1), 
               labeller = label_wrap_gen(width = 25, multi_line = TRUE)) +
    xlab(NULL) + ylab(bquote(Mt~CO[2]*e)) +
    scale_x_continuous(limits = c(1990, max_ghg_yr + 1), breaks = c(seq(1993, max_ghg_yr, 10),2021), 
                       expand = c(0,0)) +
    theme_soe_facet() +
    theme(legend.position = ("none"),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 12),
          strip.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                                 unit = "pt")),
          plot.margin = unit(c(5,5,0,2),"mm"),
          panel.grid.major.x = element_blank(),
          legend.background = element_blank())
  
  g <- g + geom_area(data = p, aes(x = year, y = MtCO2e, fill = sector), size = 0.2, alpha = 0.8)+
    scale_fill_manual(values = plotcolor)
  
  plot(g)
  
  if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
  save(x, file = paste0("tmp/",x,".RData"))
  
  svg_px(paste0("./out/",x,".svg"), width = 850, height = 430)
  plot(g)
  dev.off()
  
  png_retina(filename = paste0("./out/",x,".png"), width = 850, height = 430,
             units = "px", type = "cairo-png", antialias = "default")
  plot(g)
  dev.off()
  
}

remove_filename_spaces(dir = "out", pattern = " ", replacement = "") #remove spaces in filenames
remove_filename_spaces(dir = "tmp", pattern = " ", replacement = "") 

## Create tmp folder if not already there and store plot objects in local repository
if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
save(ghg_time, ghg_pop, gdp_time, ghg_gases_prop, 
     ghg_gases_prop_html, ghg_net_1990, ghg_net_1990_html, norm, norm_print,
     ghg_sector, ghg_sector_html, ghg_abs_diff,ghg_abs_diff_html,
     file = "tmp/plots.RData")

## Printing plots for web in SVG formats (and PNG) 
#total ghg over time
svg_px("./out/ghg_plot.svg", width = 500, height = 400)
plot(ghg_time)
dev.off()

png_retina(filename = "./out/ghg_plot.png", width = 500, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(ghg_time)
dev.off()

#total ghg per pop over time
svg_px("./out/ghg_pop_plot.svg", width = 500, height = 400)
plot(ghg_pop)
dev.off()

png_retina(filename = "./out/ghg_pop_plot.png", width = 500, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(ghg_pop)
dev.off()


#total ghg/gdp over time
svg_px("./out/ghg_gdp_plot.svg", width = 500, height = 400)
plot(gdp_time)
dev.off()

png_retina(filename = "./out/ghg_gdp_plot.png", width = 500, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(gdp_time)
dev.off()


#normalized ghg, gdp and pop compared over time
svg_px("./out/norm_plot.svg", width = 500, height = 400)
plot(norm)
dev.off()

png_retina(filename = "./out/norm_plot.png", width = 500, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(norm)
dev.off()

#total ghg by gas over time
# svg_px("./out/ghg_gases_plot.svg", width = 850, height = 400)
# plot(ghg_gases_year)
# dev.off()

#proportion of gases by year
svg_px("./out/ghg_gases_plot.svg", width = 850, height = 500)
plot(ghg_gases_prop)
dev.off()

png_retina(filename = "./out/ghg_gases_plot.png", width = 850, height = 400,
           units = "px", type = "cairo-png", antialias = "default")
plot(ghg_gases_prop)
dev.off()

#Net ghg by gas since 1990
svg_px("./out/ghg_net_plot.svg", width = 850, height = 500)
plot(ghg_net_1990)
dev.off()

png_retina(filename = "./out/ghg_net_plot.png", width = 850, height = 500,
           units = "px", type = "cairo-png", antialias = "default")
plot(ghg_net_1990)
dev.off()

#total ghg by sector over time stacked area chart
svg_px("./out/ghg_sector.svg", width = 850, height = 500)
plot(ghg_sector)
dev.off()

png_retina(filename = "./out/ghg_sector.png", width = 850, height = 500,
           units = "px", type = "cairo-png", antialias = "default")
plot(ghg_sector)
dev.off()

# absolute difference in co2e from 1990
svg_px("./out/econ_sector_abs_diff.svg", width = 850, height = 500)
plot(ghg_abs_diff)
dev.off()

png_retina(filename = "./out/econ_sector_abs_diff.png", width = 850, height = 500,
           units = "px", type = "cairo-png", antialias = "default")
plot(ghg_abs_diff)
dev.off()


# # Copy PNG files from /out folder to figure-HTML folders.
if (!exists("print_ver/images_for_HTML")) dir.create("print_ver/images_for_HTML", showWarnings = FALSE)
list.files(path = './out', pattern = '.png') %>%
  purrr::map( ~ {
    #To local machine's 'print_ver' folder...
    file.copy(from = paste0('out/',.x),to = paste0('print_ver/images_for_HTML/',.x),
              overwrite = T)})

#END

# 
# ## Line plot theme
# theme_lineplots <- theme(
#   axis.text.y = element_text(size = 14),
#   axis.text.x = element_text(size = 14, vjust = -2),
#   axis.title.y = element_text(size = 16,
#                               margin = margin(t = 0, r = 10, b = 0, l = 0,
#                                               unit = "pt")),
#   plot.title = element_text(size = 17, hjust = 0.5),
#   plot.margin = unit(c(6,6,6,6),"mm")
# )
# 
# # Set plotting parameters common to many plots:
# x_scale <- scale_x_continuous(limits = c(1990, max_ghg_yr + 1), 
#                               breaks = seq(1990, 2020, 5), 
#                               expand = c(0,0))
# 
# ## Line plot of total GHG emissions over time in British Columbia
# ghg_time = ggplot(data = bc_ghg_sum, aes(x = year, y = ghg_estimate)) + 
#   geom_line(colour = "#1B9E77", linewidth = 1.5) +
#   geom_point(x=2025, y=clean_bc_2025, color="black", shape=19, size=3)+
#   geom_point(x=2007, y=baseline_2007, color="black", shape=19, size=3)+
#   geom_segment(aes(x = 2006, xend = 2007, y = 64.2, yend = 65.5), size = 1) +
#   annotate("text", x=2021, y=clean_bc_2025, label="B.C. 2025 emission target", size = 5)+
#   annotate("text", x=2006, y=64, label = "2007 baseline", size = 5)+
#   labs(title = "Total GHG Emissions") +
#   xlab(NULL) + 
#   ylab(bquote(Mt~CO[2]*e)) +
#   scale_y_continuous(limits = c(50, 72), breaks = seq(52, 72, 2),
#                      expand = c(0,0), labels = comma) +
#   scale_x_continuous(limits = c(1990, 2026), 
#                      breaks = seq(1990, 2025, 5), 
#                      expand = c(0,0)) +
#   theme_soe() +
#   theme_lineplots
# # theme(axis.text.x = element_text(vjust = -2))
# plot(ghg_time)
# 
# ## Line plot of total GHG emissions per person over time in British Columbia
# ghg_pop <- ggplot(data = bc_ghg_per_capita, aes(x = year, y = ghg_per_capita)) + 
#   geom_line(colour = "#7570B3", size = 1.5) + 
#   ggtitle("GHG Emissions per Person") +
#   xlab(NULL) + 
#   ylab(bquote(t~CO[2]*e~" per Person")) +
#   scale_y_continuous(limits = c(12,17.5), breaks = seq(12.5, 17.5, .5),
#                      expand = c(0,0)) +
#   x_scale +
#   theme_soe() +
#   theme_lineplots
# plot(ghg_pop)
# 
# 
# ## Line plot of total GHG emisions per unit GDP over time 
# gdp_time <- ggplot(data = bc_ghg_per_capita, 
#                    aes(x = year, y = ghg_per_unit_gdp)) + 
#   geom_line(colour = "#D95F02", size = 1.5) + 
#   ggtitle("GHG Emissions per Million Dollars of GDP") +
#   xlab(NULL) + 
#   ylab(bquote(t~CO[2]*e~" per million dollars of GDP")) +
#   scale_y_continuous(limits = c(225,500), breaks = seq(250, 500, 25),
#                      expand = c(0,0)) +
#   x_scale +
#   theme_soe() +
#   theme_lineplots
# plot(gdp_time)
# 
# 
# ## Line plot of normalised GHG emissions, GDP and population change over time 
# #colour palette for 3 measures
# 
# norm.order <- unique(normalized_measures$measure) #gets rid of unused factors
# norm.cols<-3
# norm.pal <- brewer.pal(norm.cols, "Dark2")
# names(norm.pal) <- norm.order
# 
# norm_base <- ggplot(data = normalized_measures, 
#                     aes(x = year, y = estimate, group = measure, 
#                         colour = measure)) + 
#   geom_line(size = 1.5) +
#   scale_y_continuous(limits = c(.9,2.2), breaks = seq(1, 2.2, .1),
#                      expand = c(0,0)) +
#   x_scale+
#   labs(title = "Relative GHG Emissions, GDP & Population Size") +
#   xlab(NULL) + ylab("Values Indexed Relative to 1990") +
#   scale_colour_manual(name="", values = norm.pal, guide = FALSE) +
#   theme_soe() +
#   theme_lineplots 
# 
# norm <- norm_base +
#   annotate("text", label = "GDP", colour = "#D95F02",
#            x = 2004.1, y = 1.56, size = 5) +
#   annotate("text", label = "GHG", colour = "#1B9E77",
#            x = 2010, y = 1.14, size = 5) +
#   annotate("text", label = "Population", colour = "#7570B3",
#            x = 2009, y = 1.41, size = 5)
# plot(norm)
# 
# norm_print <- norm_base +
#   annotate("text", label = "GDP", colour = "#D95F02",
#            x = 2004.1, y = 1.56, size = 3) +
#   annotate("text", label = "GHG", colour = "#1B9E77",
#            x = 2010, y = 1.14, size = 3) +
#   annotate("text", label = "Population", colour = "#7570B3",
#            x = 2009, y = 1.41, size = 3)
# plot(norm_print)
# 
# ## Setting up data to provide information on economic sectors
# 
# # Remove sectors with no data in any year
# econ_sector_sum_data <- econ_sector_sum %>%
#   group_by(sector) %>%
#   filter(sum(sum)!=0)%>%
#   ungroup()
# 
# # Set colour palette for sector plot
# sector.order <- rev(levels(droplevels(econ_sector_sum_data$sector))) #gets rid of unused factors
# sector.no <- length(sector.order)# + 1
# nb.cols<- sector.no
# sector.pal <- colorRampPalette(brewer.pal(sector.no, "Dark2"))(nb.cols)
# col_db <- melt(data.frame(sector.order,sector.pal)) #for use in plotting individual sectors
# names(sector.pal) <- sector.order
# 
# 
# label_static <- econ_sector_sum_data %>%
#   filter(year==max(year))
# 
# 
# # Line plot of each sector sum over time 
# ghg_sector <- ggplot(econ_sector_sum_data, aes(x=year, y=sum, color=fct_rev(sector))) + 
#   geom_line(size = 1) +
#   scale_color_manual(values = sector.pal) +
#   # geom_text_repel(aes(label=sector),
#   #                 data = label_static, 
#   #                 nudge_x=2, direction = "y", 
#   #                 segment.size = 0.5,
#   #                 xlim = c(max(label_static$year),
#   #                          max(label_static$year) + 5))+
#   xlab(NULL) +  ylab(bquote(Mt~CO[2]*e~" by Economic Sector")) +
#   x_scale +
#   
#   coord_cartesian(clip = "off") +
#   labs(title = "GHG Emissions by Sector",
#        color = "Sector") +
#   theme_soe()+ 
#   theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         title = element_text(size = 16),
#         axis.text.y = element_text(size = 14),
#         axis.text.x = element_text(size = 14),
#         axis.title.y = element_text(size = 16,
#                                     margin = margin(t = 0, r = 10, b = 0, l = 0,
#                                                     unit = "pt")))+
#   theme(plot.margin = unit(c(0.5,3.5,0.5,0.5), "cm"))
#   # theme(legend.position = "none")
# 
# plot(ghg_sector)
# 
# ## Interactive sector plot for ggplotly html output
# 
# ghg_sector_html <- ggplot(econ_sector_sum_data) + 
#   geom_line(aes(x = year, y = sum, color=fct_rev(sector), 
#                 text = paste0(sector, " (", year, "): ", sum, " MtCO<sub>2</sub>e"),
#                 group = sector),
#             size = 1) +
#   scale_color_manual(name = "Economic Sector", values = sector.pal,
#                      limits = sector.order) +
#   x_scale +
#   labs(x="Year", y="Emissions (MtCO<sub>2</sub>e)<br>by Economic Sector")+
#   theme_soe() +
#   theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         axis.text.y = element_text(size = 8),
#         axis.text.x = element_text(size = 10),
#         axis.title.y = element_text(size = 10,
#                                     margin = margin(t = 0, r = 10, b = 0, l = 0,
#                                                     unit = "pt")),
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 10), 
#         legend.background = element_rect(colour = "white"))
# 
# plot(ghg_sector_html)
# 
# ### Absolute difference in CO2e emissions by economic sector
# abs_diff_econ <- plyr::ddply(econ_sector_sum_data, .(sector), 
#                              transform, abs.diff = round((sum - sum[year==1990]),1))
# 
# abs_label_static <- abs_diff_econ %>%
#   filter(year==max(year)) %>%
#   select(-sum)
# 
# ghg_abs_diff <- ggplot(data = abs_diff_econ, 
#                        aes(x = year, y = abs.diff, color = fct_rev(sector))) + 
#   geom_line(size=1) +
#   xlab(NULL) +  ylab(bquote("Net Difference in Emissions from 1990 ("~Mt~CO[2]*e~")")) +
#   x_scale +
#   scale_color_manual(values = sector.pal) +
#   scale_size_continuous(guide = 'none') +
#   # geom_text_repel(aes(label=sector, size=1),
#   #                 data = abs_label_static, 
#   #                 nudge_x=2, direction = "y", 
#   #                 segment.size = 0.5,
#   #                 xlim = c(max(abs_label_static$year),
#   #                          max(abs_label_static$year) + 5))+
#   labs(title = "Change in GHG Emissions by Sector",
#        color = "Sector") +
#   coord_cartesian(clip = "off") +
#   theme_soe() +
#   theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         title = element_text(size = 16),
#         axis.text.y = element_text(size = 14),
#         axis.text.x = element_text(size = 14),
#         axis.title.y = element_text(size = 16,
#                                     margin = margin(t = 0, r = 10, b = 0, l = 0,
#                                                     unit = "pt")))+
#   theme(plot.margin = unit(c(0.5,3.5,0.5,0.5), "cm"))
#   # theme(legend.position = "none")
# 
# plot(ghg_abs_diff)
# 
# ## Interactive abs diff plot for ggplotly html output
# 
# ghg_abs_diff_html <- ggplot(abs_diff_econ) + 
#   geom_line(aes(x = year, y = abs.diff, color=fct_rev(sector), 
#                 text = paste0(sector, " (", year, "): ", abs.diff, " MtCO<sub>2</sub>e"),
#                 group = sector),
#             size = 1) +
#   scale_color_manual(name = "Economic Sector", values = sector.pal,
#                      limits = sector.order) +
#   x_scale +
#   labs(x="Year", y="Net Difference in Emissions from 1990<br>(MtCO<sub>2</sub>e)")+
#   theme_soe() +
#   theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         axis.text.y = element_text(size = 8),
#         axis.text.x = element_text(size = 10),
#         axis.title.y = element_text(size = 10,
#                                     margin = margin(t = 0, r = 10, b = 0, l = 0,
#                                                     unit = "pt")),
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 10), 
#         legend.background = element_rect(colour = "white"))
# 
# plot(ghg_abs_diff_html)
# 
# ## Create a folder in directory called out for image files
# if (!exists("out"))  dir.create('out', showWarnings = FALSE)
# 
# 
# for (i in 1:length(sector.order)){
#   
#   print(i)
#   
#   x <- sector.order[i]
#   plotcolor <- sector.pal[i]
#   p <- ghg_econ_sub %>% filter(sector == x)
#   s <- nlevels(as.factor(p$subsector_final))
#   
#   
#   g <- ggplot(p, aes(x = year, y = sum)) +
#     geom_area(fill = "gray85", alpha = 0.6) +
#     facet_wrap(~fct_reorder(subsector_final, MtCO2e, .desc=TRUE),
#                nrow = ifelse(s > 3, 2, 1), 
#                labeller = label_wrap_gen(width = 25, multi_line = TRUE)) +
#     xlab(NULL) + ylab(bquote(Mt~CO[2]*e)) +
#     scale_x_continuous(limits = c(1990, max_ghg_yr + 1), breaks = c(seq(1994, 2016, 5),2020),
#                        expand = c(0,0)) +
#     theme_soe_facet() +
#     theme(legend.position = ("none"),
#           axis.text.x = element_text(size = 11),
#           axis.text.y = element_text(size = 12),
#           strip.text.x = element_text(size = 14),
#           axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0,
#                                                                  unit = "pt")),
#           plot.margin = unit(c(5,5,0,2),"mm"),
#           panel.grid.major.x = element_blank(),
#           legend.background = element_blank())
#   
#   g <- g + geom_area(data = p, aes(x = year, y = MtCO2e, fill = sector), size = 0.2, alpha = 0.8)+
#     scale_fill_manual(values = plotcolor)
#   
#   plot(g)
#   
#   if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
#   save(x, file = paste0("tmp/",x,".RData"))
#   
#   svg_px(paste0("./out/",x,".svg"), width = 850, height = 430)
#   plot(g)
#   dev.off()
#   
#   png_retina(filename = paste0("./out/",x,".png"), width = 850, height = 430,
#              units = "px", type = "cairo-png", antialias = "default")
#   plot(g)
#   dev.off()
#   
# }
# 
# remove_filename_spaces(dir = "out", pattern = " ", replacement = "") #remove spaces in filenames
# remove_filename_spaces(dir = "tmp", pattern = " ", replacement = "") 
# 
# ## Create tmp folder if not already there and store plot objects in local repository
# if (!exists("tmp")) dir.create("tmp", showWarnings = FALSE)
# save(ghg_time, ghg_pop, gdp_time, norm, norm_print,
#      ghg_sector, ghg_sector_html, ghg_abs_diff,ghg_abs_diff_html,
#      file = "tmp/plots.RData")
# 
# ## Printing plots for web in SVG formats (and PNG) 
# #total ghg over time
# svg_px("./out/ghg_plot.svg", width = 1000, height = 700)
# plot(ghg_time)
# dev.off()
# 
# # ggsave("./out/ghg_plot.png", ghg_time, 
# #        width = 1500, height = 750, 
# #        dpi = 100,
# #        units = 'px')
# 
# png_retina(filename = "./out/ghg_plot.png", width = 6, height = 3,
#            units = "in", type = "cairo-png", antialias = "default")
# plot(ghg_time)
# dev.off()
# 
# 
# #total ghg per pop over time
# svg_px("./out/ghg_pop_plot.svg", width = 500, height = 400)
# plot(ghg_pop)
# dev.off()
# 
# png_retina(filename = "./out/ghg_pop_plot.png", width = 6, height = 3,
#            units = "in", type = "cairo-png", antialias = "default")
# plot(ghg_pop)
# dev.off()
# 
# 
# #total ghg/gdp over time
# svg_px("./out/ghg_gdp_plot.svg", width = 500, height = 400)
# plot(gdp_time)
# dev.off()
# 
# png_retina(filename = "./out/ghg_gdp_plot.png", width = 6, height = 3,
#            units = "in", type = "cairo-png", antialias = "default")
# plot(gdp_time)
# dev.off()
# 
# 
# #normalized ghg, gdp and pop compared over time
# svg_px("./out/norm_plot.svg", width = 500, height = 400)
# plot(norm)
# dev.off()
# 
# png_retina(filename = "./out/norm_plot.png",width = 6, height = 3,
#            units = "in", type = "cairo-png", antialias = "default")
# plot(norm)
# dev.off()
# 
# #total ghg by sector over time stacked area chart
# svg_px("./out/ghg_sector.svg", width = 850, height = 500)
# plot(ghg_sector)
# dev.off()
# 
# png_retina(filename = "./out/ghg_sector.png", width = 6, height = 3,
#            units = "in", type = "cairo-png", antialias = "default")
# plot(ghg_sector)
# dev.off()
# 
# # absolute difference in co2e from 1990
# svg_px("./out/econ_sector_abs_diff.svg", width = 850, height = 500)
# plot(ghg_abs_diff)
# dev.off()
# 
# png_retina(filename = "./out/econ_sector_abs_diff.png", width = 6, height = 3,
#            units = "in", type = "cairo-png", antialias = "default")
# plot(ghg_abs_diff)
# dev.off()
# 

#     
#     #Rename 5 figures that we copied to D: drive.
#     figure_alt_name = case_when(
#       .x == 'ghg_gdp_plot.png' ~ 'ghggdp.png',
#       .x == 'ghg_plot.png' ~ 'ghgtrends.png',
#       .x == 'ghg_pop_plot.png' ~ 'ghgpop.png',
#       .x == 'norm_plot.png' ~ 'norm.png',
#       T ~ .x
#     )
#     
#     #Copy just the 4 static images to D: drive ('question' development server)'s '.\sustainability\ghg_files\figure-html' folder.
#     if(.x %in% c('ghg_gdp_plot.png','ghg_plot.png','ghg_pop_plot.png','norm_plot.png')){
#       file.copy(from = paste0('out/',.x),to = paste0('D:/indicators/sustainability/ghg_files/figure-html/',figure_alt_name),
#                 overwrite = T)
#     }
#     
#   })
# 
# 
# #END