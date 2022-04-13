
# Libraries, ggplot themes, and user-defined functions used in this project
# Author: Andrew Olson (andrew.olson@alaska.gov)
# Last modified: April 2019

# Libraries ----
library(tidyverse)
library(readxl)
library(ggthemes)   
library(extrafont)  # for ggplot theme
library(lubridate)
library(gridExtra)  
library(tidyr)       # data processing
library(padr)        # pads time series with 0s or interpolated values
library(anytime)     # easily deals with any date/time format
library(ggridges)    # length comps
#install.packages("devtools")
#devtools::install_github("ben-williams/FNGr")
library("FNGr")
library(data.table)  # data processing

theme_set(theme_sleek())

#COLOR BLIND PALETTE#
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


