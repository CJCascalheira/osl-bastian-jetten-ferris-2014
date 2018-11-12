####### PREPARE WORKSPACE #######

# Load dependencies
library(tidyverse)
library(psych)
library(car)

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

# Change condition to factor with two levels
bastian_clean$condition <- factor(bastian_clean$condition,
                                  levels = c("Control", "Pain"),
                                  labels = c("Control", "Pain"))

# Ensure factor assignment worked
class(bastian_clean$condition)

# Separate the conditions for t-tests
control <- bastian_clean %>%
  filter(condition == "Control")

pain <- bastian_clean %>%
  filter(condition == "Pain")

# Create bonding variable
bonding_mean <- bastian_clean %>%
  select(group101:group107) %>%
  rowMeans()

bastian_clean <- bastian_clean %>%
  mutate(bonding_mean = bonding_mean)

# Verify new variable
names(bastian_clean)

####### ANALYZE #######

### Independent t-Test of Pain ###

# Outliers?

# Normality?

# Homoscedasticity?

### Independent t-Test of Affect ###

# Outliers?

# Normality?

# Homoscedasticity?

### Independent t-Test of Challenge ###

# Outliers?

# Normality?

# Homoscedasticity?

### One-way ANOVA ###

# Outliers?

# Normality?

# Homoscedasticity?