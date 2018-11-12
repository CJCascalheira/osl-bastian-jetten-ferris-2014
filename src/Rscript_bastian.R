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

# Select & rename variables of interest
bastian_clean <- bastian %>%
  select(
    condition = CONDITION,
    group101:task_unpleasantness,
    threat_mean = Threat_MEAN,
    challenge_mean = Challenge_MEAN,
    pos_affect = Pos_PANAS,
    neg_affect = Neg_PANAS
  )

####### ANALYZE #######

### Independent t-Test of Pain ###

### Independent t-Test of Affect ###

### Independent t-Test of Challenge ###

### One-way ANOVA ###