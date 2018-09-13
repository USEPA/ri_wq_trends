# Library
library(lubridate)
library(stringr)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(here)
library(ggplot2)
library(hrbrthemes)
if(!require(LAGOSNE)){devtools::install_github("cont-limno/LAGOSNE")}
library(LAGOSNE)


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
  
  df2 <- df %>%
    filter(param == wqparam) %>%
    filter(station_name %in% filter_year(., num_yrs)) %>%
    group_by(year) %>%
    summarize(mn_value = mean(!!yvar)) %>%
    ungroup() %>%
    mutate(col_group = case_when(mn_value <= 0 ~ "Less than long-term site average",
                                 mn_value > 0 ~ "Greater than long-term site average"))
  
  
  if(!is.null(write)){
    write_csv(df2, write, append = FALSE)
  }
  
  kt <- with(df2, cor(year, mn_value,use = "pairwise.complete.obs"))
  ktp <- with(df2, cor.test(year, mn_value)$p.value)

  gg <- ggplot(df2,aes(x = year, y = mn_value)) + 
    geom_point(aes(color = col_group), size=2.5) +
    geom_smooth(method = "lm", se=FALSE, color = "black") +
    theme_ipsum() +
    labs(..., title = paste0("Kendall's Tau: ", round(kt,3),
                             " p-value: ", round(ktp, 3))) +
    scale_color_manual(values = c("red3","darkblue")) + 
    theme(legend.position="none", plot.title = element_text(size=10, face="plain")) + 
    #geom_label(aes(x = 2006, y = min(mn_value)*0.88), 
    #         label = paste0("Kendall's Tau: ", round(kt,3), "\n",
    #                        "p-value: ", round(ktp, 3)),
    #         hjust = "left", label.size = NA, size = 2.5) +
    scale_x_continuous(labels = c(1990,1995,2000,2005,2010,2015),
                       breaks = c(1990,1995,2000,2005,2010,2015),
                       minor_breaks = NULL)
  
  gg

}
 