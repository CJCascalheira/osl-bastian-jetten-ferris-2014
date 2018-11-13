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
                                  labels = c("Control", "Pain"))

# Ensure factor assignment worked
class(bastian_clean$condition)

# Create bonding variable
bonding_mean <- bastian_clean %>%
  select(group101:group107) %>%
  rowMeans()

bastian_clean <- bastian_clean %>%
  mutate(bonding_mean = bonding_mean)

# Verify new variable
head(bastian_clean$bonding_mean)

# Separate the conditions for t-tests
control <- bastian_clean %>%
  filter(condition == "Control")

pain <- bastian_clean %>%
  filter(condition == "Pain")

####### ANALYZE #######

# Descriptive statistics for control group
(control_desc <- control %>%
  select(-starts_with("group"), -condition) %>%
  describe() %>%
  select(mean, sd, se))

# Descriptive statistics for pain group
(pain_desc <- pain %>%
  select(-starts_with("group"), -condition) %>%
  describe() %>%
  select(mean, sd, se))

### Independent t-Test of Pain ###

# Outliers?
bastian_clean %>% 
  select(condition, task_intensity, task_unpleasantness) %>%
  gather(task, pain, -condition) %>%
  ggplot(aes(x = condition, y = pain)) +
    geom_boxplot() +
    facet_wrap(~ task)

# Normality?

# Normality check for task_intensity
with(bastian_clean, shapiro.test(task_intensity[condition == "Control"]))
with(bastian_clean, shapiro.test(task_intensity[condition == "Pain"]))

# Normality check for task_unpleasantness
with(bastian_clean, shapiro.test(task_unpleasantness[condition == "Control"]))
with(bastian_clean, shapiro.test(task_unpleasantness[condition == "Pain"]))

# Visualize normality
bastian_clean %>% 
  select(condition, task_intensity, task_unpleasantness) %>%
  gather(task, pain, -condition) %>%
  ggplot(aes(x = pain)) +
  geom_histogram(bins = 10) +
  facet_grid(~ task + condition)

# Homoscedasticity?
leveneTest(task_intensity ~ condition, data = bastian_clean)
leveneTest(task_unpleasantness ~ condition, data = bastian_clean)

# Independent t-test pain
t.test(pain$task_intensity, control$task_intensity,
       paired = FALSE, var.equal = TRUE)

# Independent t-test unpleasantness
t.test(pain$task_unpleasantness, control$task_unpleasantness,
       paired = FALSE, var.equal = TRUE)

### Independent t-Test of Affect ###

# Outliers?
bastian_clean %>% 
  select(condition, pos_affect, neg_affect) %>%
  gather(valence, rating, -condition) %>%
  ggplot(aes(x = condition, y = rating)) +
  geom_boxplot() +
  facet_wrap(~ valence)

# Normality?

# Normality check for neg_affect
with(bastian_clean, shapiro.test(neg_affect[condition == "Control"]))
with(bastian_clean, shapiro.test(neg_affect[condition == "Pain"]))

# Normality check for pos_affect
with(bastian_clean, shapiro.test(pos_affect[condition == "Control"]))
with(bastian_clean, shapiro.test(pos_affect[condition == "Pain"]))

# Visualize normality
bastian_clean %>% 
  select(condition, pos_affect, neg_affect) %>%
  gather(valence, rating, -condition) %>%
  ggplot(aes(x = rating)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ valence + condition)

# Homoscedasticity?
leveneTest(neg_affect ~ condition, data = bastian_clean)
leveneTest(pos_affect ~ condition, data = bastian_clean)

# Independent t-test neg_affect
t.test(pain$neg_affect, control$neg_affect, 
       paired = FALSE, var.equal = TRUE)

# Independent t-test pos_affect
t.test(pain$pos_affect, control$pos_affect, 
       paired = FALSE, var.equal = TRUE)

### Independent t-Test of Challenge ###

# Outliers?
bastian_clean %>% 
  select(condition, threat_mean, challenge_mean) %>%
  gather(task, rating, -condition) %>%
  ggplot(aes(x = condition, y = rating)) +
  geom_boxplot() +
  facet_wrap(~ task)

# Normality?

# Normality check for challenge_mean
with(bastian_clean, shapiro.test(challenge_mean[condition == "Control"]))
with(bastian_clean, shapiro.test(challenge_mean[condition == "Pain"]))

# Normality check for threat_mean
with(bastian_clean, shapiro.test(threat_mean[condition == "Control"]))
with(bastian_clean, shapiro.test(threat_mean[condition == "Pain"]))

# Visualize normality
bastian_clean %>% 
  select(condition, threat_mean, challenge_mean) %>%
  gather(task, rating, -condition) %>%
  ggplot(aes(x = rating)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ task + condition)

# Homoscedasticity?
leveneTest(challenge_mean ~ condition, data = bastian_clean)
leveneTest(threat_mean ~ condition, data = bastian_clean)

# Independent t.test challenge_mean
t.test(pain$challenge_mean, control$challenge_mean,
       paired = FALSE, var.equal = TRUE)

# Independent t.test threat_mean
t.test(pain$threat_mean, control$threat_mean,
       paired = FALSE, var.equal = TRUE)

### One-way ANOVA ###

# Outliers?

# Normality?

# Homoscedasticity?

####### VISUALIZE DATA #######