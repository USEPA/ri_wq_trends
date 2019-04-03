# Installations
pkgs <- c("lubridate","broom", "stringr", "dplyr", "readr", "readxl","tidyr", 
          "here","ggplot2", "hrbrthemes", "Kendall","sf", "USAboundaries",
          "USAboundariesData","knitr","captioner")

for(i in pkgs){
  if(!i %in% installed.packages()){
    browser()
    install.packages(i, repos = c("http://packages.ropensci.org", 
                                  "https://cran.rstudio.com"))
  }
}

if(!require(LAGOSNE)){devtools::install_github("cont-limno/LAGOSNE")}

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
wq_trend_gg <- function(df, wqparam, 
                        yvar = c("measurement_anmly","measurement_scale"), 
                        num_yrs = 10, write = NULL, ...){
  yvar <- rlang::sym(match.arg(yvar))
  
  df1 <- df %>%
    filter(param == wqparam) %>%
    filter(station_name %in% filter_year(., num_yrs))
  
  df2 <- df1 %>%
    group_by(year) %>%
    summarize(mn_value = mean(!!yvar),
              n = n(),
              se = sd(!!yvar)/sqrt(n())) %>%
    ungroup() %>%
    mutate(col_group = case_when(mn_value <= 0 ~ 
                                   "Less than long-term site average",
                                 mn_value > 0 ~ 
                                   "Greater than long-term site average"))
  
   num_lakes <- df1 %>%
     group_by(year) %>%
     summarize(n_lakes = length(unique(station_name)))
  
   df2 <- df2 %>%
     left_join(num_lakes)
  
  if(!is.null(write)){
    write_csv(df2, write, append = FALSE)
  }
  
  kt <- with(df2, Kendall(year, mn_value))
  regress <- lm(mn_value ~ year, data = df2) %>% 
    tidy() %>%
    slice(2) %>%
    select(slope = estimate, p.value) 
  
  gg <- ggplot(df2,aes(x = year, y = mn_value)) + 
    geom_pointrange(aes(ymin=mn_value-se, ymax=mn_value+se, 
                        color = col_group), size = 1, fatten = 1.75) +
    #geom_point(aes(color = col_group), size=3.5) +
    geom_smooth(method = "lm", se=FALSE, color = "black") +
    theme_ipsum() +
    labs(..., title = paste0("slope: ", signif(regress$slope,2),
                             " p-value: ", signif(regress$p.value, 2))) +
    scale_color_manual(values = c("red3","darkblue")) + 
    theme(legend.position="none", plot.title = element_text(size=10, 
                                                            face="plain")) + 
    scale_x_continuous(labels = c(1990,1995,2000,2005,2010,2015),
                       breaks = c(1990,1995,2000,2005,2010,2015),
                       minor_breaks = NULL) +
    scale_y_continuous(labels = c(-0.8, -0.4, 0, 0.4, 0.8),
                       breaks = c(-0.8, -0.4, 0, 0.4, 0.8),
                       limits = c(-0.8, 0.8))
  
  list(gg, kt, df2, regress)
}


 