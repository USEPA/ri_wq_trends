# Installations
pkgs <- c("lubridate","broom", "stringr", "dplyr", "readr", "readxl","tidyr", 
          "here","ggplot2", "hrbrthemes", "Kendall","sf", "USAboundaries",
          "USAboundariesData","knitr","captioner", "pander", "cowplot")

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

#' Function to filter on sample months
#' 
#' Reviewer comment 1-1: To avoid possible seasonal effects due to missed samples, 
#' make sure to include only lake-years with a minimum of one sample in each of
#' May/June, Jul/Aug, Sep/Oct per parameter.
#' 
#' @param df data frame to filter
filter_months <- function(df){
  
  stations_min_samp <- df %>%
    mutate(may_jun = case_when(month == 5 | month == 6 ~
                                 1, 
                               TRUE ~ 0),
           jul_aug = case_when(month == 7 | month == 8 ~
                                 1,
                               TRUE ~ 0),
           sep_oct = case_when(month == 9 | month == 10 ~
                                 1,
                               TRUE ~ 0)) %>%
    group_by(station_name, year, param) %>%
    filter(sum(any(may_jun>0), any(jul_aug>0), any(sep_oct>0))>=3) %>%
    ungroup() 
  
  stations_min_samp
}

#' Function to filter on sample years
#' 
#' Reviewer comment 1-2: To avoid possible effects from having sites only 
#' measured early or only measured late in the 1993-2016 time frame. Using 
#' approach similar to Oliver et al. 
#' 
#' @param df data frame to filter
#' @param cutoff The year to divide.  Included in the early period, not the late
#'               period.
filter_early_late <- function(df, cutoff){
  
  early_late <- df %>%
    group_by(station_name, param) %>%
    mutate(early = year <= cutoff,
           late = year > cutoff) %>%
    filter(any(early) & any(late)) %>%
    ungroup() %>%
    select(-early, -late)
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
                        yvar = c("measurement_anmly","measurement_scale", "measurement_anmly_mdn"), 
                        num_yrs = 10, write = NULL, title = "",
                        start_yr = 1993, error_bar = c("se", "sd", "iqr"), ...){
  
  yvar <- rlang::sym(match.arg(yvar))
  error_bar <- rlang::sym(match.arg(error_bar))
  
  df1 <- df %>%
    filter(param == wqparam) %>%
    filter(year >= start_yr) %>%
    filter(station_name %in% filter_year(., num_yrs))
  
  
  df2 <- df1 %>%
    group_by(year) %>%
    summarize(mdn_value = median(!!yvar),
              sd = sd(!!yvar),
              iqr = IQR(!!yvar),
              iqr_low = quantile(!!yvar, 0.25),
              iqr_high = quantile(!!yvar, 0.75),
              min = min(!!yvar),
              max = max(!!yvar),
              n = n(),
              se = sd/sqrt(n())) %>%
    mutate(col_group = case_when(mdn_value < 0 ~ 
                                   "Less than long-term site median",
                                 mdn_value > 0 ~ 
                                   "Greater than long-term site median",
                                 TRUE ~ "Equal to long-term site median"))
  # Nutrient data were scarce in 1995-1998  To include data in plots, had to 
  # have a minimum of three sites.  If only three sites, then data plotted are 
  # median and min/max. 
  df2 <- df2 %>%
    filter(n >= 3)
  
  if(!is.null(write)){
    write_csv(df2, write, append = FALSE)
  }
  
  kt <- with(df2, Kendall(year, mdn_value))
  regress <- lm(mdn_value ~ year, data = df2) %>% 
    tidy() %>%
    slice(2) %>%
    select(slope = estimate, p.value) 
  
  #Calc y-axis ranges
  if(yvar == "measurement_scale"){
    label_break <- c(-2,-1,0,1,2)
    limits <- c(-2.75, 2.75)
  } else if(yvar == "measurement_anmly"){
    limit<-max(abs(c(floor(1.1*min(df2$mdn_value - df2[[error_bar]])), 
                     ceiling(1.1*max(df2$mdn_value + df2[[error_bar]])))))
    label_break <- c(-limit, floor(-limit/2), 0, ceiling(limit/2), limit)
    limits <- c(-limit,limit)
  } else if(yvar == "measurement_anmly_mdn"){
    limit<-max(abs(c(floor(1.1*min(df2$iqr_low)), 
                     ceiling(1.1*max(df2$iqr_high)))))
    label_break <- c(-limit, floor(-limit/2), 0, ceiling(limit/2), limit)
    limits <- c(-limit,limit)
  }
  
  # Calc colors
  if(length(unique(df2$col_group))==3){
    my_colors <- c("grey30", "red3","darkblue")
  } else {
    my_colors <- c("red3","darkblue")
  }
  
  text_offset <- 0.1*max(limits)
  
  # set min max on range
  if(error_bar == "se" | error_bar == "sd"){
    y_min <- df2[df2$n > 3,]$mdn_value - df2[df2$n > 3,][[error_bar]]
    y_max <- df2[df2$n > 3,]$mdn_value + df2[df2$n > 3,][[error_bar]]
  } else if(error_bar == "iqr"){
    y_min <- df2[df2$n > 3,]$iqr_low
    y_max <- df2[df2$n > 3,]$iqr_high
  }
  
  
  gg <- ggplot(df2,aes(x = year, y = mdn_value)) + 
    geom_pointrange(data = df2[df2$n > 3,], aes(ymin=y_min, 
                                                ymax=y_max, 
                                                color = col_group), 
                    size = 1, fatten = 1.75) 
  if(nrow(df2[df2$n == 3,]) != 0){ gg <- gg +
      geom_pointrange(data = df2[df2$n == 3,], aes(ymin=min, ymax=max, 
                                                   color = col_group), 
                         size = 1, fatten = 1.75) +
      geom_text(data = df2[df2$n == 3,], aes(x = year, y = max + text_offset,
                                             label = "*"), color = "darkblue")
    }
    #geom_point(aes(color = col_group), size=3.5) +
  gg <- gg + 
    geom_smooth(method = "lm", se=FALSE, color = "black") +
    theme_ipsum_rc() +
    labs(..., title = title, subtitle = paste0("slope = ", signif(regress$slope,2),
                             " p = ", signif(regress$p.value, 2))) +
    scale_color_manual(values = my_colors) + 
    theme(legend.position="none", 
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size=10, face="plain")) + 
    scale_x_continuous(labels = c(1990,1995,2000,2005,2010,2015),
                       breaks = c(1990,1995,2000,2005,2010,2015),
                       minor_breaks = NULL,
                       limits = c(1993,2016)) +
    scale_y_continuous(labels = label_break,
                       breaks = label_break,
                       limits = limits)
  
  list(gg, kt, df2, regress)
}

#' Function to calculate Carlson(-like) TSI
#' 
#' Equations directly from NALMS
tsi <- function(value, param = c("chla", "tp", "tn", "secchi")){
  param <- match.arg(param)
  
  chla_tsi <- function(value){
    round(9.81*log(value)+30.6,0)
  }
  
  tp_tsi <- function(value){
    round(14.42*log(value)+4.15,0)
  }
  
  secchi_tsi <- function(value){
    round(60-14.41*log(value),0)
  }
  
  tn_tsi <- function(value){
    round(54.45 + 14.43 * log((value/1000)),0)
  }
  
  switch(param,
         chla = chla_tsi(value),
         tp = tp_tsi(value),
         secchi = secchi_tsi(value),
         tn = tn_tsi(value))
}


# Function to calc tsi table
tsi_table <- function(){

  chla <- c(0.042, 0.12, 0.33, 0.90, 2.5, 6.9, 20, 53, 147, 406, 1123)
  chla_tsi <- tsi(chla, "chla")
  tp <- c(0.73, 1.5, 2.9, 5.9, 12, 24, 47, 93, 186, 372, 745)
  tp_tsi <- tsi(tp, "tp")
  tn <- c(23, 45, 89, 178, 355, 710, 1420, 2838, 5675, 11350, 22700)
  tn_tsi <- tsi(tn, "tn")
  
  xdf <- tibble(chla, tp, tn, tsi = chla_tsi)
  
  xdf <- mutate(xdf, delta_chla = zoo::rollapply(chla,2,function(x) x[2]-x[1], 
                                                 fill = NA, align = "right"),
                delta_tp = zoo::rollapply(tp,2,function(x) x[2]-x[1], 
                                          fill = NA, align = "right"),
                delta_tn = zoo::rollapply(tn,2,function(x) x[2]-x[1], 
                                          fill = NA, align = "right"))
  
  xdf
  
}

#' Function to generate trophic state figures
#' @param df data frame with data to plot
#' @param wqparam the water quality parameter to plot
#' @param yvar the actual variable to plot, defaults to anomaly
#' @param num_yrs minimum number of years needed to include a site
#' @param write write out data used in the plot to a csv file
#' @param title title for plot
#' @param start_yr only plots years for which we have data for all params
wq_trophic_trend_gg <- function(df, wqparam, 
                        yvar = c("measurement_anmly","measurement_scale", "measurement_anmly_mdn"), 
                        num_yrs = 10, write = NULL, title = "",
                        start_yr = 1993, error_bar = c("se", "sd", "iqr"), ...){
  
  yvar <- rlang::sym(match.arg(yvar))
  error_bar <- rlang::sym(match.arg(error_bar))
  
  df1 <- df %>%
    #filter(param == wqparam) %>%
    filter(year >= start_yr) %>%
    filter(station_name %in% filter_year(., num_yrs))
  
  
  df2 <- df1 %>%
    filter(!is.na(trophic_state)) %>%
    group_by(year, param, trophic_state) %>%
    summarize(mdn_value = median(!!yvar),
              sd = sd(!!yvar),
              iqr = IQR(!!yvar),
              iqr_low = quantile(!!yvar, 0.25),
              iqr_high = quantile(!!yvar, 0.75),
              min = min(!!yvar),
              max = max(!!yvar),
              n = n(),
              se = sd/sqrt(n())) %>%
    mutate(col_group = case_when(mdn_value < 0 ~ 
                                   "Less than long-term site median",
                                 mdn_value > 0 ~ 
                                   "Greater than long-term site median",
                                 TRUE ~ "Equal to long-term site median"))
  # Nutrient data were scarce in 1995-1998  To include data in plots, had to 
  # have a minimum of three sites.  If only three sites, then data plotted are 
  # median and min/max. 
  df2 <- df2 %>%
    filter(n >= 3)
  
  
  if(!is.null(write)){
    write_csv(df2, write, append = FALSE)
  }
  
  kt <- with(df2, Kendall(year, mdn_value))
  regress <- lm(mdn_value ~ year, data = df2) %>% 
    tidy() %>%
    slice(2) %>%
    select(slope = estimate, p.value) 
  
  #Calc y-axis ranges
  if(yvar == "measurement_scale"){
    label_break <- c(-2,-1,0,1,2)
    limits <- c(-2.75, 2.75)
  } else if(yvar == "measurement_anmly"){
    limit<-max(abs(c(floor(1.1*min(df2$mdn_value - df2[[error_bar]])), 
                     ceiling(1.1*max(df2$mdn_value + df2[[error_bar]])))))
    label_break <- c(-limit, floor(-limit/2), 0, ceiling(limit/2), limit)
    limits <- c(-limit,limit)
  } else if(yvar == "measurement_anmly_mdn"){
    limit<-max(abs(c(floor(1.1*min(df2$iqr_low)), 
                     ceiling(1.1*max(df2$iqr_high)))))
    label_break <- c(-limit, floor(-limit/2), 0, ceiling(limit/2), limit)
    limits <- c(-limit,limit)
  }
  
  # Calc colors
  if(length(unique(df2$col_group))==3){
    my_colors <- c("grey30", "red3","darkblue")
  } else {
    my_colors <- c("red3","darkblue")
  }
  
  text_offset <- 0.1#*max(limits)
  
  # set min max on range
  if(error_bar == "se" | error_bar == "sd"){
    y_min <- df2[df2$n > 3,]$mdn_value - df2[df2$n > 3,][[error_bar]]
    y_max <- df2[df2$n > 3,]$mdn_value + df2[df2$n > 3,][[error_bar]]
  } else if(error_bar == "iqr"){
    y_min <- df2[df2$n > 3,]$iqr_low
    y_max <- df2[df2$n > 3,]$iqr_high
  }
  
  gg <- ggplot(df2,aes(x = year, y = mdn_value)) + 
    geom_pointrange(data = df2[df2$n > 3,], aes(ymin=y_min, 
                                                ymax=y_max, 
                                                color = col_group), 
                    size = 1, fatten = 1.75) +
    facet_grid(param ~ trophic_state, scales = "free")
  if(nrow(df2[df2$n == 3,]) != 0){ gg <- gg +
    geom_pointrange(data = df2[df2$n == 3,], aes(ymin=min, ymax=max, 
                                                 color = col_group), 
                    size = 1, fatten = 1.75) +
    geom_text(data = df2[df2$n == 3,], aes(x = year, y = max + text_offset,
                                           label = "*"), color = "darkblue")
  }
  #geom_point(aes(color = col_group), size=3.5) +
  gg <- gg + 
    geom_smooth(method = "lm", se=FALSE, color = "black") +
    theme_ipsum_rc() +
    labs(..., title = title) + #, subtitle = paste0("slope = ", signif(regress$slope,2),
                                #               " p = ", signif(regress$p.value, 2))) +
    scale_color_manual(values = my_colors) + 
    theme(legend.position="none", 
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size=10, face="plain")) + 
    scale_x_continuous(labels = c(1995,2005,2015),
                       breaks = c(1995,2005,2015),
                       minor_breaks = NULL,
                       limits = c(1993,2016)) #+
    #scale_y_continuous(labels = label_break,
    #                   breaks = label_break,
    #                   limits = limits)
  
  list(gg, kt, df2, regress)
}