# Installations
pkgs <- c("lubridate","broom", "stringr", "dplyr", "readr", "readxl","tidyr", 
          "here","ggplot2", "hrbrthemes", "Kendall","sf", "USAboundaries",
          "USAboundariesData","knitr","captioner")

for(i in pkgs){
  if(!i %in% installed.packages()){
    install.packages(i, repos = c("http://packages.ropensci.org", 
                                  "https://cran.rstudio.com"))
  }
}

if(!requireNamespace("LAGOSNE")){
  devtools::install_github("cont-limno/LAGOSNE")
}

# Library
library(lubridate)
library(broom)
library(stringr)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(here)
library(ggplot2)
library(hrbrthemes)
library(Kendall)
library(LAGOSNE)
library(sf)
library(USAboundaries)
library(USAboundariesData)
library(knitr)
library(captioner)
library(cowplot)

#' Function to filter on number of years sampled per site
#' @param df data frame to filter
#' @param num_yrs minimum number of years
filter_year <- function(df, num_yrs){
  df %>% 
    select(year,station_name) %>%
    unique() %>%
    group_by(station_name) %>%
    tally() %>%
    filter(n >= num_yrs) %>%
    pull(station_name)
}

#' Function to generate figures
#' @param df data frame with data to plot
#' @param wqparam the water quality parameter to plot
#' @param yvar the actual variable to plot, defaults to anomaly
#' @param num_yrs minimum number of years needed to include a site
#' @param write write out data used in the plot to a csv file
#' @param title title for plot
#' @param start_yr only plots years for which we have data for all params
wq_trend_gg <- function(df, wqparam, 
                        yvar = c("measurement_anmly","measurement_scale"), 
                        num_yrs = 10, write = NULL, title = "",
                        start_yr = 1993, error_bar = c("se", "sd"), ...){
  yvar <- rlang::sym(match.arg(yvar))
  error_bar <- rlang::sym(match.arg(error_bar))
  df1 <- df %>%
    filter(param == wqparam) %>%
    filter(year >= start_yr) %>%
    filter(station_name %in% filter_year(., num_yrs))
  
  
  df2 <- df1 %>%
    #This should take care of pseudoreplication by using the per site/year means
    #results in n for years being equal to number of sites per year
    group_by(station_name,year) %>%
    summarize(mn_value_station = mean(!!yvar)) %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(mn_value = mean(mn_value_station),
              sd = sd(mn_value_station),
              n = n(),
              se = sd/sqrt(n())) %>%
    mutate(col_group = case_when(mn_value < 0 ~ 
                                   "Less than long-term site average",
                                 mn_value > 0 ~ 
                                   "Greater than long-term site average",
                                 TRUE ~ "Equal to long-term site average"))
  
  if(!is.null(write)){
    write_csv(df2, write, append = FALSE)
  }
  
  kt <- with(df2, Kendall(year, mn_value))
  regress <- lm(mn_value ~ year, data = df2) %>% 
    tidy() %>%
    slice(2) %>%
    select(slope = estimate, p.value) 
  
  gg <- ggplot(df2,aes(x = year, y = mn_value)) + 
    geom_pointrange(aes(ymin=mn_value-!!error_bar, ymax=mn_value+!!error_bar, 
                        color = col_group), size = 1, fatten = 1.75) +
    #geom_point(aes(color = col_group), size=3.5) +
    geom_smooth(method = "lm", se=FALSE, color = "black") +
    theme_ipsum() +
    labs(..., title = title, subtitle = paste0("slope: ", signif(regress$slope,2),
                             " p-value: ", signif(regress$p.value, 2))) +
    scale_color_manual(values = c("red3","darkblue")) + 
    theme(legend.position="none", 
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size=10, face="plain")) + 
    scale_x_continuous(labels = c(1990,1995,2000,2005,2010,2015),
                       breaks = c(1990,1995,2000,2005,2010,2015),
                       minor_breaks = NULL,
                       limits = c(1993,2016)) +
    scale_y_continuous(labels = c(-2, -1, 0, 1, 2),
                       breaks = c(-2, -1, 0, 1, 2),
                       limits = c(-2.25, 2.25))
  
  list(gg, kt, df2, regress)
}


 