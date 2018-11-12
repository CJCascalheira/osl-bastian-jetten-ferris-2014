####### PREPARE WORKSPACE #######

# Load dependencies
library(tidyverse)
library(psych)

# Set working directory
setwd("~/GitHub/osl-bastian-jetten-ferris-2014/src")

# Import data
bastian <- read_csv("../data/Bastian Jetten and Ferris 2014 Experiment 1.csv")

####### CLEAN DATA #######

# Find variables of interest
names(bastian)
