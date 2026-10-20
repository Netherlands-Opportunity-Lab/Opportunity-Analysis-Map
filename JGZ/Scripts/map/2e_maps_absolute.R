#----------------------------
# Figure 2e: kansenkaart maps of the Netherlands for absolute mobility (intercept + slope)
#----------------------------


#packages
library(tidyverse)
library(openxlsx)
library(sf)
library(scales)
library(ggtext)
options(scipen=999)


# create paths
fig_path <- "figures/2_regional/"
dat_path <- "data/2_regional/"
sf_path <- "resources/shapefiles/"


#------------------------------------
# DEFINE OPTIONS 

# color option
opportunity_colors <- c("#9C2830", # red
                        "#DE986B", # orange
                        "#FCF4B8", # yellow
                        "#90B4A6", # green
                        "#35697F") # blue

if (exists("black_white") && isTRUE(black_white)) {
  opportunity_colors <- c("#525252", 
                          "#737373", 
                          "#BDBDBD", 
                          "#D9D9D9", 
                          "#F0F0F0") 
} 


# create limtis for 10th and 90th percentile
limits_legend <- function(variable) {
  number <- as.vector(quantile(variable, c(0.05, 0.95), na.rm = TRUE))
  return(number)
}

# shapefile 2022
load_shapefile <- function(region_input) {

  if (region_input == "gemeente") {
    sf <- st_read(file.path(sf_path, "WijkBuurtkaart_2022_v1/gemeente_2022_v1.shp")) %>%
      filter(H2O == "NEE") %>%
      mutate(region_id = str_extract(GM_CODE, "[0-9]+")) %>%
      select(-GM_CODE)

  } else if (region_input == "postcode3") {
    sf <- st_read(file.path(sf_path, "Postcodevlakken_PC_3/PC3.shp")) %>%
      rename("region_id" = "PC3")

  } else if (region_input == "postcode4") {
    sf <- st_read(file.path(sf_path, "Postcodevlakken_PC_4/PC4.shp")) %>%
      mutate(PC4 = as.character(PC4)) %>%
      rename("region_id" = "PC4")

  }
}


#------------------------------------
# CREATE FIGURE - MUNICIPALITY


# load data
dat <- read.xlsx(file.path(dat_path, "maps_income_estimates.xlsx"), 
                 sheet = "gemeente") %>%
  filter(income_group == "low", 
         gender_group == "all", 
         migration_group == "all") %>%
  mutate(est = est*100)

# load shapefile
sf <- load_shapefile("gemeente")

# join shapefile
dat <- left_join(sf, dat, by = "region_id")

# create maps
ggplot(data = dat) +   
  geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
  ggtitle("A. Upward Mobility: Mean Child rank for Parents at 25th Percentile by Municipality") + 
  labs(fill = "") +
  scale_fill_gradientn(colors = opportunity_colors, 
                       limits = limits_legend(dat$est), 
                       oob = squish, 
                       na.value = "#DADCE0") + 
  theme_minimal() +
  theme(plot.title = element_textbox_simple(size = 16, hjust = 0.5, 
                                            margin = margin(20, 0, 3, 0), face="bold"),
        plot.subtitle=element_textbox_simple(hjust = 0, size = 14, 
                                             vjust = 1, margin = margin(10,0,15,0)),
        rect = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"), 
        line = element_blank(), 
        legend.key.size = unit(1.25, "lines"), 
        panel.grid.major = element_line(color = "transparent"))


# save figure
ggsave(paste0(fig_path, "2_mun_ned_abs.png"), 
       width = 18, height = 18, units = "cm")



#------------------------------------
# CREATE FIGURE - ZIP CODE 3

# load data
dat <- read.xlsx(file.path(dat_path, "maps_income_estimates.xlsx"), 
                 sheet = "postcode3") %>%
  filter(income_group == "low", 
         gender_group == "all", 
         migration_group == "all") %>%
  mutate(est = est*100)

# load shapefile
sf <- load_shapefile("postcode3")

# join shapefile
dat <- left_join(sf, dat, by = "region_id")

# create maps
ggplot(data = dat) +   
  geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
  ggtitle("A. Upward Mobility: Mean Child rank for Parents at 25th Percentile by Postcode 3") + 
  labs(fill = "") +
  scale_fill_gradientn(colors = opportunity_colors, 
                       limits = limits_legend(dat$est), 
                       oob = squish, 
                       na.value = "#DADCE0") + 
  theme_minimal() +
  theme(plot.title = element_textbox_simple(size = 16, hjust = 0.5, 
                                            margin = margin(20, 0, 3, 0), face="bold"),
        plot.subtitle=element_textbox_simple(hjust = 0, size = 14, 
                                             vjust = 1, margin = margin(10,0,15,0)),
        rect = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"), 
        line = element_blank(), 
        legend.key.size = unit(1.25, "lines"), 
        panel.grid.major = element_line(color = "transparent"))


# save figure
ggsave(paste0(fig_path, "2_pc3_ned_abs.png"), 
       width = 18, height = 18, units = "cm")


#------------------------------------
# CREATE FIGURE - ZIP CODE 4


# load data
dat <- read.xlsx(file.path(dat_path, "maps_income_estimates.xlsx"), 
                 sheet = "postcode4") %>%
  filter(income_group == "low", 
         gender_group == "all", 
         migration_group == "all") %>%
  mutate(est = est*100)

# load shapefile
sf <- load_shapefile("postcode4")

# join shapefile
dat <- left_join(sf, dat, by = "region_id")

# create maps
ggplot(data = dat) +   
  geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
  ggtitle("B. Upward Mobility: Mean Child rank for Parents at 25th Percentile by Postcode 4") + 
  labs(fill = "") +
  scale_fill_gradientn(colors = opportunity_colors, 
                       limits = limits_legend(dat$est), 
                       oob = squish, 
                       na.value = "#DADCE0") + 
  theme_minimal() +
  theme(plot.title = element_textbox_simple(size = 16, hjust = 0.5, 
                                            margin = margin(20, 0, 3, 0), face="bold"),
        plot.subtitle=element_textbox_simple(hjust = 0, size = 14, 
                                             vjust = 1, margin = margin(10,0,15,0)),
        rect = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"), 
        line = element_blank(), 
        legend.key.size = unit(1.25, "lines"), 
        panel.grid.major = element_line(color = "transparent"))


# save figure
ggsave(paste0(fig_path, "2_pc4_ned_abs.png"), 
       width = 18, height = 18, units = "cm")

