#----------------------------
# Maps for JGZ data outcomes
#----------------------------


#packages
library(tidyverse)
library(openxlsx)
library(sf)
library(scales)
library(ggtext)
options(scipen=999)

setwd("C:/Users/nilsv/Erasmus University Rotterdam/Opportunity Lab - Documenten/Opportunity-Analysis-Map/JGZ")

# create paths
dat_path <- "Export 27-03-2025/"
fig_path <- "maps/"   # output folder for all maps (relative to the JGZ folder above)

# create all output subfolders up front, so every ggsave() below has a target
for (d in c("pc4_bmi", "pc4_wiechen", "pc4_sdq", "pc4_bereik",
            "pc3_bmi", "pc3_wiechen", "pc3_sdq", "pc3_bereik")) {
  dir.create(file.path(fig_path, d), recursive = TRUE, showWarnings = FALSE)
}



#------------------------------------
# DEFINE OPTIONS 

# color option
opportunity_colors <- c("#9C2830", # red
                        "#DE986B", # orange
                        "#FCF4B8", # yellow
                        "#90B4A6", # green
                        "#35697F") # blue

opportunity_colors <- c("#35697F",  # blue
                        "#90B4A6", # green
                        "#FCF4B8", # yellow
                        "#DE986B", # orange
                        "#9C2830") # red

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
  
  if (region_input == "postcode3") {
    sf <- st_read("Postcodevlakken_PC_3/PC3.shp") %>%
      rename("region_id" = "PC3")
    
  } else if (region_input == "postcode4") {
    sf <- st_read("Postcodevlakken_PC_4/PC4.shp") %>%
      mutate(PC4 = as.character(PC4)) %>%
      rename("region_id" = "PC4")
    
  }
}


#------------------------------------
# CREATE FIGURE - ZIP CODE 4
# load shapefile
sf <- load_shapefile("postcode4")


##########
###BMI####
##########
# MEAN
time <- c("4wks", "8wks", "3months", "4months", "6months", "9months",
           "11months", "14months", "18months", "2years", "3years", 
           "3years9m", "groep2", "groep7", "klas2")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_bmi_export.xlsx"), 
                   sheet = paste0("bmi_", t)) %>% 
    mutate(postcode = as.character(postcode),
           bmi_mean = as.numeric(bmi_mean),
           bmi_sd = as.numeric(bmi_sd)
    ) %>%
    rename(region_id = postcode,
           est = bmi_mean)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  if (all(is.na(dat$est))) next
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": ", "Gemiddelde BMI voor kinderen voor Postcode 4")) + 
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
  ggsave(paste0(fig_path, paste0("pc4_bmi/pc4_", t, "_mean.png")), 
         width = 18, height = 18, units = "cm")

} 



# 90th percentile % 

time <- c("4wks", "8wks", "3months", "4months", "6months", "9months",
          "11months", "14months", "18months", "2years", "3years", 
          "3years9m", "groep2", "groep7", "klas2")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_bmi_export.xlsx"), 
                   sheet = paste0("bmi_90p_", t)) %>% 
    mutate(postcode = as.character(postcode),
           bmi_90p_perc = as.numeric(bmi_90p_perc)) %>% 
    rename(region_id = postcode,
           est = bmi_90p_perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t, ": " , "Percentage in de top 10% voor bmi voor Postcode 4")) + 
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
  ggsave(paste0(fig_path, paste0("pc4_bmi/pc4_", t, "_90p.png")), 
         width = 18, height = 18, units = "cm")
  
} 






#### WIECHEN ####
# taal_ont_achts_perc

time <- c("2years", "3y9m")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_wiechen_export.xlsx"), 
                   sheet = paste0("wiechen_taal_", t, "_pc4")) %>% 
    mutate(postcode = as.character(postcode), 
           taal_ont_achts_perc = as.numeric(taal_ont_achts_perc)
           ) %>%
    rename(region_id = postcode,
           est = taal_ont_achts_perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": Percentage met een taalontwikkelingsachterstand: Van Wiechen item 41 of 42 onvoldoende voor Postcode 4")) + 
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
  ggsave(paste0(fig_path, paste0("pc4_wiechen/", t, "_taal_ont_achts_perc.png")), 
         width = 18, height = 18, units = "cm")
  
} 


# wiechen_2years

for (o in c("zin2w_onvol", "pop6_onvol")) {
    # load data
    dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_wiechen_export.xlsx"), 
                     sheet = "wiechen_2years_pc4") %>% 
      mutate(postcode = as.character(postcode),
             perc = as.numeric(perc)
             ) %>% 
      filter(outcome == o) %>%
      rename(region_id = postcode,
             est = perc)
    
    # join shapefile
    dat <- left_join(sf, dat, by = "region_id")  
    
    # create maps
    ggplot(data = dat) +   
      geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
      ggtitle(paste0("2 years: Percentage die onvoldoende scoren op Van Wiechen item ", o, " voor Postcode 4")) + 
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
    ggsave(paste0(fig_path, paste0("pc4_wiechen/", "2years_", o, ".png")), 
           width = 18, height = 18, units = "cm")
}

# wiechen_3y9m

for (o in c("vragen1_onvol", "verstaan2_onvol", "vragen2_onvol")) {
    # load data
    dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_wiechen_export.xlsx"), 
                     sheet = "wiechen_3y9m_pc4") %>% 
      mutate(postcode = as.character(postcode), 
             perc = as.numeric(perc)
             ) %>% 
      filter(outcome == o) %>%
      rename(region_id = postcode,
             est = perc)
    
    # join shapefile
    dat <- left_join(sf, dat, by = "region_id")  
    
    # create maps
    ggplot(data = dat) +   
      geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
      ggtitle(paste0("3y9m: Percentage die onvoldoende scoren op Van Wiechen item ", o, " voor Postcode 4")) + 
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
    ggsave(paste0(fig_path, paste0("pc4_wiechen/", "3y9m_", o, ".png")), 
           width = 18, height = 18, units = "cm")
}





#### SDQ ####
# sdq_11_perc

time <- c("groep2", "groep7", "klas2", "3years9m")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_sdq_export.xlsx"), 
                   sheet = paste0("sdq11_", t, "_pc4")) %>% 
    mutate(postcode = as.character(postcode), 
           sdq_11_perc = as.numeric(sdq_11_perc)) %>% 
    rename(region_id = postcode,
           est = sdq_11_perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": Percentage met een SDQ totaalscore van 11 of hoger voor Postcode 4")) + 
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
  ggsave(paste0(fig_path, paste0("pc4_sdq/", t, "_sdq_11_perc.png")), 
         width = 18, height = 18, units = "cm")
  
} 


# sdq_14_perc
time <- c("groep2", "groep7", "klas2", "3years9m")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_sdq_export.xlsx"), 
                   sheet = paste0("sdq14_", t, "_pc4")) %>% 
    mutate(postcode = as.character(postcode), 
           sdq_14_perc = as.numeric(sdq_14_perc)
           ) %>% 
    rename(region_id = postcode,
           est = sdq_14_perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": Percentage met een SDQ totaalscore van 14 of hoger voor Postcode 4")) + 
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
  ggsave(paste0(fig_path, paste0("pc4_sdq/", t, "_sdq_14_perc.png")), 
         width = 18, height = 18, units = "cm")
  
} 


# mean
time <- c("groep2", "groep7", "klas2", "3years9m")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_sdq_export.xlsx"), 
                   sheet = paste0("sdq_", t, "_pc4")) %>% 
    mutate(postcode = as.character(postcode), 
           sdq_mean = as.numeric(sdq_mean)
           ) %>% 
    rename(region_id = postcode,
           est = sdq_mean)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": Gemiddelde SDQ totaalscore voor Postcode 4")) + 
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
  ggsave(paste0(fig_path, paste0("pc4_sdq/", t, "_sdq_mean.png")), 
         width = 18, height = 18, units = "cm")
  
} 






#------------------------------------
# CREATE FIGURE - ZIP CODE 3

# load shapefile
sf <- load_shapefile("postcode3")




#### BMI ####
# MEAN
time <- c("4weeks", "8weeks", "3months", "4months", "6months", "9months",
          "11months", "14months", "18months", "2years", "3years", 
          "3years9m", "groep2", "groep7", "klas2")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode3_bmi_export.xlsx"), 
                   sheet = paste0("bmi_", t)) %>% 
    mutate(postcode3 = as.character(postcode3),
           bmi_mean = as.numeric(bmi_mean),
           bmi_sd = as.numeric(bmi_sd)
    ) %>%
    rename(region_id = postcode3,
           est = bmi_mean)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": ", "Gemiddelde BMI voor kinderen voor Postcode 3")) + 
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
  ggsave(paste0(fig_path, paste0("pc3_bmi/pc3_", t, "_mean.png")), 
         width = 18, height = 18, units = "cm")
  
} 



# 90th percentile % 

time <- c("4weeks", "8weeks", "3months", "4months", "6months", "9months",
          "11months", "14months", "18months", "2years", "3years", 
          "3years9m", "groep2", "groep7", "klas2")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode3_bmi_export.xlsx"), 
                   sheet = paste0("bmi_90p_", t)) %>% 
    mutate(postcode3 = as.character(postcode3),
           bmi_90p_perc = as.numeric(bmi_90p_perc)) %>% 
    rename(region_id = postcode3,
           est = bmi_90p_perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t, ": " , "Percentage in de top 10% voor bmi voor Postcode 3")) + 
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
  ggsave(paste0(fig_path, paste0("pc3_bmi/pc3_", t, "_90p.png")), 
         width = 18, height = 18, units = "cm")
  
} 






#### WIECHEN ####
# taal_ont_achts_perc

time <- c("2years", "3y9m")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_wiechen_export.xlsx"), 
                   sheet = paste0("wiechen_taal_", t, "_pc3")) %>% 
    mutate(postcode3 = as.character(postcode3), 
           taal_ont_achts_perc = as.numeric(taal_ont_achts_perc)
    ) %>%
    rename(region_id = postcode3,
           est = taal_ont_achts_perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": Percentage met een taalontwikkelingsachterstand: Van Wiechen item 41 of 42 onvoldoende voor Postcode 3")) + 
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
  ggsave(paste0(fig_path, paste0("pc3_wiechen/", t, "_taal_ont_achts_perc.png")), 
         width = 18, height = 18, units = "cm")
  
} 


# wiechen_2years

for (o in c("zin2w_onvol", "pop6_onvol")) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_wiechen_export.xlsx"), 
                   sheet = "wiechen_2years_pc3") %>% 
    mutate(postcode3 = as.character(postcode3),
           perc = as.numeric(perc)
    ) %>% 
    filter(outcome == o) %>%
    rename(region_id = postcode3,
           est = perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0("2 years: Percentage die onvoldoende scoren op Van Wiechen item ", o, " voor Postcode 3")) + 
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
  ggsave(paste0(fig_path, paste0("pc3_wiechen/", "2years_", o, ".png")), 
         width = 18, height = 18, units = "cm")
}

# wiechen_3y9m

for (o in c("vragen1_onvol", "verstaan2_onvol", "vragen2_onvol")) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_wiechen_export.xlsx"), 
                   sheet = "wiechen_3y9m_pc3") %>% 
    mutate(postcode3 = as.character(postcode3), 
           perc = as.numeric(perc)
    ) %>% 
    filter(outcome == o) %>%
    rename(region_id = postcode3,
           est = perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0("3y9m: Percentage die onvoldoende scoren op Van Wiechen item ", o, " voor Postcode 3")) + 
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
  ggsave(paste0(fig_path, paste0("pc3_wiechen/", "3y9m_", o, ".png")), 
         width = 18, height = 18, units = "cm")
}





#### SDQ ####
# sdq_11_perc

time <- c("groep2", "groep7", "klas2", "3years9m")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_sdq_export.xlsx"), 
                   sheet = paste0("sdq11_", t, "_pc3")) %>% 
    mutate(postcode3 = as.character(postcode3), 
           sdq_11_perc = as.numeric(sdq_11_perc)) %>% 
    rename(region_id = postcode3,
           est = sdq_11_perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": Percentage met een SDQ totaalscore van 11 of hoger voor Postcode 3")) + 
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
  ggsave(paste0(fig_path, paste0("pc3_sdq/", t, "_sdq_11_perc.png")), 
         width = 18, height = 18, units = "cm")
  
} 


# sdq_14_perc
time <- c("groep2", "groep7", "klas2", "3years9m")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_sdq_export.xlsx"), 
                   sheet = paste0("sdq14_", t, "_pc3")) %>% 
    mutate(postcode3 = as.character(postcode3), 
           sdq_14_perc = as.numeric(sdq_14_perc)
    ) %>% 
    rename(region_id = postcode3,
           est = sdq_14_perc)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": Percentage met een SDQ totaalscore van 14 of hoger voor Postcode 3")) + 
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
  ggsave(paste0(fig_path, paste0("pc3_sdq/", t, "_sdq_14_perc.png")), 
         width = 18, height = 18, units = "cm")
  
} 


# mean
time <- c("groep2", "groep7", "klas2", "3years9m")

for (t in time) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "output_cases_postcode4_3_sdq_export.xlsx"), 
                   sheet = paste0("sdq_", t, "_pc3")) %>% 
    mutate(postcode3 = as.character(postcode3), 
           sdq_mean = as.numeric(sdq_mean)
    ) %>% 
    rename(region_id = postcode3,
           est = sdq_mean)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(t,": Gemiddelde SDQ totaalscore voor Postcode 3")) + 
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
  ggsave(paste0(fig_path, paste0("pc3_sdq/", t, "_sdq_mean.png")), 
         width = 18, height = 18, units = "cm")
  
} 


#Proportions bereik
# mean
time <- c("", "_6")
variables <- c("d_obs_always", "d_observed")

for (t in time) {
  for(v in variables) {
  # load data
  dat <- read.xlsx(file.path(dat_path, "proportions_export.xlsx"), 
                   sheet = paste0("pc3_proportions", t, ".xlsx")) 
  colnames(dat)[1] <- "region_id"
  dat <- dat %>% 
    mutate(region_id = as.character(region_id), 
           pr = as.numeric(pr)
    ) %>% 
    filter(variable == v) %>%
    rename(
           est = pr)
  
  # join shapefile
  dat <- left_join(sf, dat, by = "region_id")  
  
  if (v == "d_observed") {
    expl <- "Percentage minstens één keer geobserveerd op 2 jaar, 3 jaar of 3 jaar 9 maanden"
  } else {
    expl <- "Percentage altijd geobserveerd op 2 jaar, 3 jaar en 3 jaar 9 maanden"
  }
  
  if (t == "_6") {
    place <- " in Noord-Limburg"
  } else {
    place <-""
  }
  
  # create maps
  ggplot(data = dat) +   
    geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
    ggtitle(paste0(expl, place)) + 
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
  ggsave(paste0(fig_path, paste0("pc3_bereik/", v, t, ".png")), 
         width = 18, height = 18, units = "cm")
  
  } 
}





#Proportions bereik
sf <- load_shapefile("postcode4")

variables <- c("d_obs_always", "d_observed")

  for(v in variables) {
    # load data
    dat <- read.xlsx(file.path(dat_path, "proportions_export.xlsx"), 
                     sheet = paste0("pc4_3y_proportions_6.xlsx")) 
    colnames(dat)[1] <- "region_id"
    dat <- dat %>% 
      mutate(region_id = as.character(region_id), 
             pr = as.numeric(pr)
      ) %>% 
      filter(variable == v) %>%
      rename(
        est = pr)
    
    # join shapefile
    dat <- left_join(sf, dat, by = "region_id")  
    
    if (v == "d_observed") {
      expl <- "Percentage minstens één keer geobserveerd op 2 jaar, 3 jaar of 3 jaar 9 maanden"
    } else {
      expl <- "Percentage altijd geobserveerd op 2 jaar, 3 jaar en 3 jaar 9 maanden"
    }
    
    
    # create maps
    ggplot(data = dat) +   
      geom_sf(aes(fill = est), color = "#FFFFFF", size = 0.05) + 
      ggtitle(paste0(expl, place)) + 
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
    ggsave(paste0(fig_path, paste0("pc4_bereik/", v, ".png")), 
           width = 18, height = 18, units = "cm")
    
  } 

