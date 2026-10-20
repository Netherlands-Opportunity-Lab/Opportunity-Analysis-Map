#----------------------------
# Figure 2g: kansenkaart maps of the Netherlands for selected areas
#----------------------------
# a.	Noord Nederland op postcode 3 niveau
# b.	Grote steden (Amsterdam, Rottterdam) op postcode 4 niveau


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

  sf_path <- "/Users/helenlam/Dropbox (Erasmus Universiteit Rotterdam)/Helen/CBS/Shapefiles/Shapefiles 2022"

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
# 1) AMSTERDAM
# source postcode: https://postcodebijadres.nl/postcodes-nederland

# load data of postcode
pc_amsterdam <- read.xlsx(file.path(dat_path, "postcode_cities.xlsx"), 
                         sheet = "Amsterdam") %>%
  select(Postcode) %>%
  mutate(Postcode = as.character(Postcode)) %>%
  unique()

# load estimates data
dat <- read.xlsx(file.path(dat_path, "maps_income_estimates.xlsx"), 
                 sheet = "postcode4") %>%
  mutate(est = est*100) %>%
  filter(income_group == "all", gender_group == "all", migration_group == "all")

# load shapefile
sf <- load_shapefile("postcode4")

# merge
dat <- left_join(sf, dat, by = "region_id")
pc_amsterdam <- inner_join(dat, pc_amsterdam, by = c("region_id" = "Postcode"))


# create maps
ggplot(data = pc_amsterdam) +   
  geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
  ggtitle("A. Mean Child income rank for Parents at 25th Percentile by ZIP code 4 in Amsterdam") + 
  labs(fill = "") +
  scale_fill_gradientn(colors = opportunity_colors, 
                       limits = limits_legend(pc_amsterdam$est), 
                       oob = squish, 
                       na.value = "#DADCE0") + 
  theme_minimal() +
  theme(plot.title = element_textbox_simple(size = 16, 
                                            margin = margin(20, 0, 3, 0), face="bold"),
        plot.subtitle=element_textbox_simple(hjust = 0, size = 14, 
                                             vjust = 1, margin = margin(10,0,15,0)),
        legend.box.background = element_blank(),
        rect = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"), 
        line = element_blank(), 
        legend.key.size = unit(1.25, "lines"), 
        panel.grid.major = element_line(color = "transparent"))


# save figure
ggsave(file.path(fig_path, "2_pc4_ams_abs.png"), 
       width = 18, height = 18, units = "cm")
rm(pc_amsterdam)

#------------------------------------
# 2) ROTTERDAM
# source postcode: https://postcodebijadres.nl/postcodes-nederland


# load data of postcode
pc_rotterdam <- read.xlsx(file.path(dat_path, "postcode_cities.xlsx"), 
                          sheet = "Rotterdam") %>%
  filter(Gemeente == "Rotterdam") %>%
  select(Postcode) %>%
  mutate(Postcode = as.character(Postcode)) %>%
  unique()

# load estimates data
dat <- read.xlsx(file.path(dat_path, "maps_income_estimates.xlsx"), 
                 sheet = "postcode4") %>%
  mutate(est = est*100) %>%
  filter(income_group == "all", gender_group == "all", migration_group == "all")

# load shapefile
sf <- st_read(file.path(sf_path, "Postcode 4/PC4.shp")) %>%
  mutate(PC4 = as.character(PC4))

# merge
dat <- left_join(sf, dat, by = c("PC4" = "region_id"))
pc_rotterdam <- inner_join(dat, pc_rotterdam, by = c("PC4" = "Postcode"))

# create maps
ggplot(data = pc_rotterdam) +   
  geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
  ggtitle("B. Mean Child income rank for Parents at 25th Percentile by ZIP code 4 in Rotterdam") + 
  labs(fill = "") +
  scale_fill_gradientn(colors = opportunity_colors, 
                       limits = limits_legend(pc_rotterdam$est), 
                       oob = squish, 
                       na.value = "#DADCE0") + 
  theme_minimal() +
  theme(plot.title = element_textbox_simple(size = 16, 
                                            margin = margin(20, 0, 3, 0), face="bold"),
        plot.subtitle=element_textbox_simple(hjust = 0, size = 14, 
                                             vjust = 1, margin = margin(10,0,15,0)),
        legend.box.background = element_blank(),
        rect = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"), 
        line = element_blank(), 
        legend.key.size = unit(1.25, "lines"), 
        panel.grid.major = element_line(color = "transparent"))


# save figure
ggsave(file.path(fig_path, "2_pc4_rot_abs.png"), 
       width = 18, height = 18, units = "cm")
rm(pc_rotterdam)

#------------------------------------
# 3) NORTHERN NETHERLANDS

# load data of postcode
pc_north <- read.xlsx(file.path(dat_path, "postcode_cities.xlsx"), 
                          sheet = "North") %>%
  select(Postcode) %>%
  mutate(Postcode = as.character(Postcode), 
         Postcode = substr(Postcode, 1, 3)) %>% 
  unique()


# load estimates data
dat <- read.xlsx(file.path(dat_path, "maps_income_estimates.xlsx"), 
                 sheet = "postcode3") %>%
  mutate(est = est*100) %>%
  filter(income_group == "all", gender_group == "all", migration_group == "all")

# load shapefile
sf <- load_shapefile("postcode3")


# merge
dat <- left_join(sf, dat, by = c("region_id" = "region_id"))
pc_north <- inner_join(dat, pc_north, by = c("region_id" = "Postcode"))

# create maps
ggplot(data = pc_north) +   
  geom_sf(aes(fill = est), color = "#FFFFFF", linewidth = 0.05) + 
  ggtitle("A. Mean Child income rank for Parents at 25th Percentile by ZIP code 3 in Northern Netherlands") + 
  labs(fill = "") +
  scale_fill_gradientn(colors = opportunity_colors, 
                       limits = limits_legend(pc_north$est), 
                       oob = squish, 
                       na.value = "#DADCE0") + 
  theme_minimal() +
  theme(plot.title = element_textbox_simple(size = 16, 
                                            margin = margin(20, 0, 3, 0), face="bold"),
        plot.subtitle=element_textbox_simple(hjust = 0, size = 14, 
                                             vjust = 1, margin = margin(10,0,15,0)),
        legend.box.background = element_blank(),
        rect = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"), 
        line = element_blank(), 
        legend.key.size = unit(1.25, "lines"), 
        panel.grid.major = element_line(color = "transparent"))


# save figure
ggsave(file.path(fig_path, "2_pc3_north_abs.png"), 
       width = 18, height = 18, units = "cm")
rm(pc_north)

