# Data Import

# Justin Priest & Ryan Whitmore
# July 2020
# justin.priest@alaska.gov
# Ryan.Whitmore@dfo-mpo.gc.ca

# Libraries
library(tidyverse)
library(here)
library(scales)
library(ggsidekick)
library(patchwork)
library(extrafont)
# font_import()  #this only needs to be run once
loadfonts(device="win") #change for mac users

source(here::here("code/functions.R"))
