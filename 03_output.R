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
  axis.text.x = element_text(size = 14, vjust = -2),
  axis.title.y = element_text(size = 16,
                              margin = margin(t = 0, r = 10, b = 0, l = 0,
                                              unit = "pt")),
  plot.title = element_text(size = 17, hjust = 0.5),
  plot.margin = unit(c(6,6,6,6),"mm")
)

# Set plotting parameters common to many plots:
x_scale <- scale_x_continuous(limits = c(1990, max_ghg_yr + 1), 
                              breaks = seq(1990, 2020, 5), 
                              expand = c(0,0))

## Line plot of total GHG emissions over time in British Columbia
ghg_time = ggplot(data = bc_ghg_sum, aes(x = year, y = ghg_estimate)) + 
  geom_line(colour = "#1B9E77", size = 1.5) +
  geom_point(x=2025, y=clean_bc_2025, color="black", shape=19, size=3)+
  geom_point(x=2007, y=baseline_2007, color="black", shape=19, size=3)+
  geom_segment(aes(x = 2006, xend = 2007, y = 64.2, yend = 65.5), size = 1) +
  annotate("text", x=2021, y=clean_bc_2025, label="B.C. 2025 emission target", size = 5)+
  annotate("text", x=2006, y=64, label = "2007 baseline", size = 5)+
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
# theme(axis.text.x = element_text(vjust = -2))
plot(ghg_time)

## Line plot of total GHG emissions per person over time in British Columbia
ghg_pop <- ggplot(data = bc_ghg_per_capita, aes(x = year, y = ghg_per_capita)) + 
  geom_line(colour = "#7570B3", size = 1.5) + 
  ggtitle("GHG Emissions per Person") +
  xlab(NULL) + 
  ylab(bquote(t~CO[2]*e~" per Person")) +
  scale_y_continuous(limits = c(12,17.5), breaks = seq(12.5, 17.5, .5),
                     expand = c(0,0)) +
  x_scale +
  theme_soe() +
  theme_lineplots
plot(ghg_pop)


## Line plot of total GHG emisions per unit GDP over time 
gdp_time <- ggplot(data = bc_ghg_per_capita, 
                   aes(x = year, y = ghg_per_unit_gdp)) + 
  geom_line(colour = "#D95F02", size = 1.5) + 
  ggtitle("GHG Emissions per Million Dollars of GDP") +
  xlab(NULL) + 
  ylab(bquote(t~CO[2]*e~" per million dollars of GDP")) +
  scale_y_continuous(limits = c(225,500), breaks = seq(250, 500, 25),
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
  geom_line(size = 1.5) +
  scale_y_continuous(limits = c(.9,2.2), breaks = seq(1, 2.2, .1),
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
           x = 2010, y = 1.14, size = 3) +
  annotate("text", label = "Population", colour = "#7570B3",
           x = 2009, y = 1.41, size = 3)
plot(norm_print)

## Setting up data to provide information on economic sectors

# Remove sectors with no data in any year
econ_sector_sum_data <- econ_sector_sum %>%
  group_by(sector) %>%
  filter(sum(sum)!=0)%>%
  ungroup()

# Set colour palette for sector plot
sector.order <- rev(levels(droplevels(econ_sector_sum_data$sector))) #gets rid of unused factors
sector.no <- length(sector.order)# + 1
nb.cols<- sector.no
sector.pal <- colorRampPalette(brewer.pal(sector.no, "Dark2"))(nb.cols)
col_db <- melt(data.frame(sector.order,sector.pal)) #for use in plotting individual sectors
names(sector.pal) <- sector.order


label_static <- econ_sector_sum_data %>%
  filter(year==max(year))


# Line plot of each sector sum over time 
ghg_sector <- ggplot(econ_sector_sum_data, aes(x=year, y=sum, color=fct_rev(sector))) + 
  geom_line(size = 1) +
  scale_color_manual(values = sector.pal) +
  # geom_text_repel(aes(label=sector),
  #                 data = label_static, 
  #                 nudge_x=2, direction = "y", 
  #                 segment.size = 0.5,
  #                 xlim = c(max(label_static$year),
  #                          max(label_static$year) + 5))+
  xlab(NULL) +  ylab(bquote(Mt~CO[2]*e~" by Economic Sector")) +
  x_scale +
  
  coord_cartesian(clip = "off") +
  labs(title = "GHG Emissions by Sector",
       color = "Sector") +
  theme_soe()+ 
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")))+
  theme(plot.margin = unit(c(0.5,3.5,0.5,0.5), "cm"))
  # theme(legend.position = "none")

plot(ghg_sector)

## Interactive sector plot for ggplotly html output

ghg_sector_html <- ggplot(econ_sector_sum_data) + 
  geom_line(aes(x = year, y = sum, color=fct_rev(sector), 
                text = paste0(sector, " (", year, "): ", sum, " MtCO<sub>2</sub>e"),
                group = sector),
            size = 1) +
  scale_color_manual(name = "Economic Sector", values = sector.pal,
                     limits = sector.order) +
  x_scale +
  labs(x="Year", y="Emissions (MtCO<sub>2</sub>e)<br>by Economic Sector")+
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
                             transform, abs.diff = round((sum - sum[year==1990]),1))

abs_label_static <- abs_diff_econ %>%
  filter(year==max(year)) %>%
  select(-sum)

ghg_abs_diff <- ggplot(data = abs_diff_econ, 
                       aes(x = year, y = abs.diff, color = fct_rev(sector))) + 
  geom_line(size=1) +
  xlab(NULL) +  ylab(bquote("Net Difference in Emissions from 1990 ("~Mt~CO[2]*e~")")) +
  x_scale +
  scale_color_manual(values = sector.pal) +
  scale_size_continuous(guide = 'none') +
  # geom_text_repel(aes(label=sector, size=1),
  #                 data = abs_label_static, 
  #                 nudge_x=2, direction = "y", 
  #                 segment.size = 0.5,
  #                 xlim = c(max(abs_label_static$year),
  #                          max(abs_label_static$year) + 5))+
  labs(title = "Change in GHG Emissions by Sector",
       color = "Sector") +
  coord_cartesian(clip = "off") +
  theme_soe() +
  theme(panel.grid.major = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(size = 0.5, colour = "grey85"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0,
                                                    unit = "pt")))+
  theme(plot.margin = unit(c(0.5,3.5,0.5,0.5), "cm"))
  # theme(legend.position = "none")

plot(ghg_abs_diff)

## Interactive abs diff plot for ggplotly html output

ghg_abs_diff_html <- ggplot(abs_diff_econ) + 
  geom_line(aes(x = year, y = abs.diff, color=fct_rev(sector), 
                text = paste0(sector, " (", year, "): ", abs.diff, " MtCO<sub>2</sub>e"),
                group = sector),
            size = 1) +
  scale_color_manual(name = "Economic Sector", values = sector.pal,
                     limits = sector.order) +
  x_scale +
  labs(x="Year", y="Net Difference in Emissions from 1990<br>(MtCO<sub>2</sub>e)")+
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
  
  print(i)
  
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
    scale_x_continuous(limits = c(1990, max_ghg_yr + 1), breaks = c(seq(1994, 2016, 5),2020),
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
save(ghg_time, ghg_pop, gdp_time, norm, norm_print,
     ghg_sector, ghg_sector_html, ghg_abs_diff,ghg_abs_diff_html,
     file = "tmp/plots.RData")

## Printing plots for web in SVG formats (and PNG) 
#total ghg over time
svg_px("./out/ghg_plot.svg", width = 1000, height = 700)
plot(ghg_time)
dev.off()

# ggsave("./out/ghg_plot.png", ghg_time, 
#        width = 1500, height = 750, 
#        dpi = 100,
#        units = 'px')

png_retina(filename = "./out/ghg_plot.png", width = 6, height = 3,
           units = "in", type = "cairo-png", antialias = "default")
plot(ghg_time)
dev.off()


#total ghg per pop over time
svg_px("./out/ghg_pop_plot.svg", width = 500, height = 400)
plot(ghg_pop)
dev.off()

png_retina(filename = "./out/ghg_pop_plot.png", width = 6, height = 3,
           units = "in", type = "cairo-png", antialias = "default")
plot(ghg_pop)
dev.off()


#total ghg/gdp over time
svg_px("./out/ghg_gdp_plot.svg", width = 500, height = 400)
plot(gdp_time)
dev.off()

png_retina(filename = "./out/ghg_gdp_plot.png", width = 6, height = 3,
           units = "in", type = "cairo-png", antialias = "default")
plot(gdp_time)
dev.off()


#normalized ghg, gdp and pop compared over time
svg_px("./out/norm_plot.svg", width = 500, height = 400)
plot(norm)
dev.off()

png_retina(filename = "./out/norm_plot.png",width = 6, height = 3,
           units = "in", type = "cairo-png", antialias = "default")
plot(norm)
dev.off()

#total ghg by sector over time stacked area chart
svg_px("./out/ghg_sector.svg", width = 850, height = 500)
plot(ghg_sector)
dev.off()

png_retina(filename = "./out/ghg_sector.png", width = 6, height = 3,
           units = "in", type = "cairo-png", antialias = "default")
plot(ghg_sector)
dev.off()

# absolute difference in co2e from 1990
svg_px("./out/econ_sector_abs_diff.svg", width = 850, height = 500)
plot(ghg_abs_diff)
dev.off()

png_retina(filename = "./out/econ_sector_abs_diff.png", width = 6, height = 3,
           units = "in", type = "cairo-png", antialias = "default")
plot(ghg_abs_diff)
dev.off()


