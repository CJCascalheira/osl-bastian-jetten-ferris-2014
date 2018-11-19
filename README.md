# Open Stats Lab One-way ANOVA in R Using Data from Bastian, Jetten, & Ferris (2014)
A solution to Kevin P. McIntyre's Open Stats Lab activity on [one-way ANOVA](https://sites.trinity.edu/osl/data-sets-and-activities/one-way-anova-activities) using a data set from Bastian, Jetten, and Ferris (2014). Written in R.

# Open Stats Lab
___
Kevin P. McIntyre developed this amazing resource for students of psychology. Check out [Open Stats Lab](https://sites.trinity.edu/osl/) for a collection of all activities.

Each activity includes an article from *Psychological Science*, a data set, and an activity to complete in SPSS. However, if you are an open source fanatic, you could also complete the activity in [JASP](https://jasp-stats.org/). For tips on how to use JASP, check out [this resource](https://osf.io/t56kg/) created by Buchanan, Hopke, and Donaldson (2018).

I prefer to get my hands deep into the data. Dr. McIntyre does not *yet* offer an R activity to accompany the work of [Bastian, Jetten, and Ferris (2014)](https://journals.sagepub.com/stoken/default+domain/yFi5kZDGWMGBNQY62zIE/full), so here is one possible solution written in R.

# Analysis 
___
I will perform assumption checks for each test prior to running it. We already know that the data meet all assumptions, otherwise the authors would have used a different analytic approach. However, checking the assumptions is helpful because:

1. reproducibility and accuracy can be verified; and
2. if you are a student, then you should form the habit of testing assumptions.

This analysis will follow the data science workflow advocated by [Garrett Grolemund and Hadley Wickham](https://r4ds.had.co.nz/introduction.html). First, we will set-up our session and import the data. Then, we must clean the data. Next, we will transform, model, and visualize the data to understand it. Finally, we will communicate our findings.

Our task is to perform four analyses.

1. **Independent t-Test** - did participants differ significantly in their experience of pain intensity and pain unpleasantness?
2. **Independent t-Test** - did participants experience significant differences in affect?
3. **Independent t-Test** - did participants in the pain condition find their activity more threatening and challenging?
4. **One-way ANOVA** - does pain lead to differences in group bonding?

## Import
___
Let's load the packages necessary for this analysis into our workspace.

```r
library(tidyverse)
library(psych)
library(car)
```

We can import the data set using a relative path because our working directory is set.

```r
bastian <- read_csv("../data/Bastian Jetten and Ferris 2014 Experiment 1.csv")
```

## Clean
___

Let's find the variables of interest. After reading the [paper and activity instructions](https://sites.trinity.edu/osl/data-sets-and-activities/one-way-anova-activities), we know that we are looking for the following variables:

* pain versus control condition;
* pain intensity and pain unpleasantness;
* positive and negative affect;
* threatening and challenging; and
* group bonding.

```r
# Find variables of interest
names(bastian)
```

We learned from the article that PANAS is a measure of affect. Let's clean up these data by assigning new variable names. I like to follow `this_convention` when naming objects in R. This prevents me from constantly referring to the `names()` of the data frame to check punctuation, capitalization, etc.

```r
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
```

A one-way ANOVA is performed on a factor with two or more levels. Right now, the `condition` column is an `integer`.

```r
# Change condition to factor with two levels
bastian_clean$condition <- factor(bastian_clean$condition,
                                  labels = c("Control", "Pain"))
# Ensure factor assignment worked
class(bastian_clean$condition)
```

We will need to compute the group bonding variable. Let's do that ahead of time.

```r
# Create bonding variable
bonding_mean <- bastian_clean %>%
  select(group101:group107) %>%
  rowMeans()
bastian_clean <- bastian_clean %>%
  mutate(bonding_mean = bonding_mean)
# Verify new variable
head(bastian_clean$bonding_mean)
```

Independent, unpaired samples t-tests are simpler to execute in R if we separate the groups into variables.

```r
# Separate the conditions for t-tests
control <- bastian_clean %>%
  filter(condition == "Control")
pain <- bastian_clean %>%
  filter(condition == "Pain")
```

Data are ready for analysis!

## Understand
___
An [unpaired samples t-test](https://libguides.library.kent.edu/SPSS/IndependentTTest) shares assumptions with a [univariate ANOVA](https://libguides.library.kent.edu/SPSS/OneWayANOVA). Three of these assumptions derive from experimental design, while the remaining three can be tested after data collection. 

Remember, all samples in psychology should be randomly selected from the population. 

**Experimental Design Assumptions**

1. Dependent, response variable that is a continuous `numeric`.

2. Independent, explanatory variable that is a categorical `factor`.
    - Unpaired sampled t-test compares the means of two groups (i.e., two levels of a factor).
    - One-way ANOVA compares the means of two or more groups (i.e., multiple levels of a factor).

3. Independence of observations.
    - Participants are assigned to one group with no overlap.

All of the experimental design assumptions have been met.

**Data Collection Assumptions**

4. No significant outliers.
    - Tested with boxplots and by comparing discrepancies between mean and median.
    - ANOVA is a robust test. If all levels of the factor are skewed in the same direction, then the results from our ANOVA are still reliable.

5. The data are normally distributed.
    - Tested with a Shapiro-Wilk test.
    - Visualized with histograms and density curves.

6. The data are homoscedastic.
    - Primarily tested with Levene's test.
    - Occasionally tested with Bartlett's test, which is more sensitive to violations of normality.

Remember, the hypotheses of an independent t-test are:

$$ H_0: \mu_1 = \mu_2 \\ H_1: \mu_1 \neq \mu_2 $$

The hypotheses of a one-way ANOVA are:

$$ H_0: \mu_1 = \mu_2 = \mu_3 = \dotso = \mu_k \\ H_1: \text{at least one different} $$

**Note**: Even if you perform the test in question first (as I do below), make sure to check the assumptions.

### Descriptive Statistics
Let's summarize the data with pipes and a call to `psych::describe`.

Save the information as variables to use for graphing later.

```r
# Descriptive statistics for control group
(control_desc <- control %>%
  select(-starts_with("group"), -condition) %>%
  describe() %>%
  select(mean, sd, se))
```

```r
# Descriptive statistics for pain group
(pain_desc <- pain %>%
  select(-starts_with("group"), -condition) %>%
  describe() %>%
  select(mean, sd, se))
```

### Independent t-Test of Pain 

```r
# Independent t-test pain
t.test(pain$task_intensity, control$task_intensity,
       paired = FALSE, var.equal = TRUE)
```

Participants in the pain group (*M* = 6.07, *SD* = 2.00) rated the intensity of their pain higher than those in the control group (*M* = 1.67, *SD* = 0.92), *t*(52) = 10.41, *p* < .001.

```r
# Independent t-test unpleasantness
t.test(pain$task_unpleasantness, control$task_unpleasantness,
       paired = FALSE, var.equal = TRUE)
```

Those subjected to the ice water task and squats (*M* = 6.00, *SD* = 1.96) were more likely than their counterparts (*M* = 1.74, *SD* = 1.20) to report an unpleasant experience, *t*(52) = 9.63, *p* < .001.

**Note**: graphs not shown here, check the R markdown document.

#### Significant outliers?
Outliers skew distributions.
```r
bastian_clean %>% 
  select(condition, task_intensity, task_unpleasantness) %>%
  gather(task, pain, -condition) %>%
  ggplot(aes(x = condition, y = pain)) +
    geom_boxplot() +
    facet_wrap(~ task)
```

Yes, there are outliers in the control group. Normality may be affected.

#### Normality?
The Shapiro-Wilk test enables us to examine if the data are normally distrbuted.

**Normality Check for Task Intensity**
```r
with(bastian_clean, shapiro.test(task_intensity[condition == "Control"]))
```

Since *p* < .001, we reject the null hypothesis that the distribution of task intensity is normal for the control group.

```r
with(bastian_clean, shapiro.test(task_intensity[condition == "Pain"]))
```

The observations of the variable task intensity are normally distrbuted for the pain group (*p* = .321).

**Normality Check for Task Unpleasantness**
```r
with(bastian_clean, shapiro.test(task_unpleasantness[condition == "Control"]))
```

Once again, *p* < .001, so we accept the alternative hypothesis; this distribution is not normal.

```r
with(bastian_clean, shapiro.test(task_unpleasantness[condition == "Pain"]))
```

However, for the pain group, the distrbution of task unpleasantness *is* normal (*p* < .171).

We can visualize normality with histograms.

**Note**: graphs not shown here, check the R markdown document.
```r
bastian_clean %>% 
  select(condition, task_intensity, task_unpleasantness) %>%
  gather(task, pain, -condition) %>%
  ggplot(aes(x = pain)) +
  geom_histogram(bins = 10) +
  facet_grid(~ task + condition)
```

Participants in the control group (i.e., those *not* subjected to ice water and squats) reported little pain and unpleasantness, hence the concentration of scores near 0.

#### Homoscedasticity?
Are the variances homogenous?
```r
leveneTest(task_intensity ~ condition, data = bastian_clean)
```

Since *p* < .05, we reject the null hypothesis. The variances of intensity scores between the two groups are heteroscedastic.

```r
leveneTest(task_unpleasantness ~ condition, data = bastian_clean)
```

In terms of task unpleasantness, the variances between both groups are not equal.

So, why do we run the independent t-test anyways? Well, the [independent t-test is robust](http://oak.ucc.nau.edu/rh232/courses/EPS525/Handouts/Understanding%20the%20Independent%20t%20Test.pdf), which means that violations of normality and homoscedasticity do not necessarily entail the use of a non-parametric test. 

Here, the sample sizes are equal and just shy of the recommended threshold of *n* = 30. This is why we set `var.equal = TRUE` in the call to `t.test()`. Equal sample sizes of `r nrow(control)` make these violations negligible.

### Independent t-Test of Affect 
```r
# Independent t-test neg_affect
t.test(pain$neg_affect, control$neg_affect, 
       paired = FALSE, var.equal = TRUE)
# Independent t-test pos_affect
t.test(pain$pos_affect, control$pos_affect, 
       paired = FALSE, var.equal = TRUE)
```

Groups neither differed between negative affect (pain: *M* = 1.34, *SD* = 0.45; control: *M* = 1.27, *SD* = 0.37, *t*(52) = 0.6, *p* = .554), nor positive affect (pain: *M* = 3.05, *SD* = 0.82; control: *M* = 2.80, *SD* = 0.83, *t*(52) = 1.09, *p* = .283). 

#### Significant outliers?

**Note**: graphs not shown here, check the R markdown document.
```r
bastian_clean %>% 
  select(condition, pos_affect, neg_affect) %>%
  gather(valence, rating, -condition) %>%
  ggplot(aes(x = condition, y = rating)) +
  geom_boxplot() +
  facet_wrap(~ valence)
```

Some outliers in both groups for negative affect.

#### Normality?

**Normality Check for Negative Affect**
```r
with(bastian_clean, shapiro.test(neg_affect[condition == "Control"]))
with(bastian_clean, shapiro.test(neg_affect[condition == "Pain"]))
```

The scores of negative affect violate the assumption of normality (*p*s < .001).
**Normality Check for Positive Affect**
```r
with(bastian_clean, shapiro.test(pos_affect[condition == "Control"]))
with(bastian_clean, shapiro.test(pos_affect[condition == "Pain"]))
```

However, for both the control (*p* = .774) and pain (*p* = .288) groups, reported values of positive affect are normally distributed.

**Note**: graphs not shown here, check the R markdown document.
```r
# Visualize normality
bastian_clean %>% 
  select(condition, pos_affect, neg_affect) %>%
  gather(valence, rating, -condition) %>%
  ggplot(aes(x = rating)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ valence + condition)
```

Between-group distributions for affect look similar.

#### Homoscedasticity?
```r
leveneTest(neg_affect ~ condition, data = bastian_clean)
leveneTest(pos_affect ~ condition, data = bastian_clean)
```

Levene's test revealed that the variances for both groups were homogenous on measures of negative (*p* = .659) and positive (*p* = .789) affect.

### Independent t-Test of Challenge
```r
# Independent t.test challenge_mean
t.test(pain$challenge_mean, control$challenge_mean,
       paired = FALSE, var.equal = TRUE)
# Independent t.test threat_mean
t.test(pain$threat_mean, control$threat_mean,
       paired = FALSE, var.equal = TRUE)
```

The pain group perceived their tasks to be slightly more threatening (*M* = 1.36, *SD* = 0.58) than the control group (*M* = 1.11, *SD* = 0.30), *t*(52) = 1.97, *p* = .054, but not more challenging (pain: *M* = 2.67, *SD* = 0.87; control: *M* = 2.38, *SD* = 0.91, *t*(52) = 1.22, *p* = .227).

#### Significant outliers?

**Note**: graphs not shown here, check the R markdown document.
```r
bastian_clean %>% 
  select(condition, threat_mean, challenge_mean) %>%
  gather(task, rating, -condition) %>%
  ggplot(aes(x = condition, y = rating)) +
  geom_boxplot() +
  facet_wrap(~ task)
```

Outliers are present in both measures.

#### Normality?

**Normality Check for Challenge**
```r
with(bastian_clean, shapiro.test(challenge_mean[condition == "Control"]))
with(bastian_clean, shapiro.test(challenge_mean[condition == "Pain"]))
```

The distributions of perceived challenge for both conditions are normal.

**Normality Check for Threat**
```r
with(bastian_clean, shapiro.test(threat_mean[condition == "Control"]))
with(bastian_clean, shapiro.test(threat_mean[condition == "Pain"]))
```

The distribution of perceived threat is neither normal for the control group, nor pain group.

**Note**: graphs not shown here, check the R markdown document.
```r
bastian_clean %>% 
  select(condition, threat_mean, challenge_mean) %>%
  gather(task, rating, -condition) %>%
  ggplot(aes(x = rating)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ task + condition)
```

#### Homoscedasticity?
```r
leveneTest(challenge_mean ~ condition, data = bastian_clean)
leveneTest(threat_mean ~ condition, data = bastian_clean)
```

Data for each measure are homoscedastic (both *p*s > 0.05).

### One-way ANOVA 
```r
# Model the variance
bastian_aov <- aov(bonding_mean ~ condition, data = bastian_clean)
# Summarize the ANOVA
anova(bastian_aov)
```
Since *p* = .048 is less than our significance value of $\alpha = 0.05 $, the *F* value exceeds the critical value. We reject the null hypothesis.

Enduring pain with others has a mild effect on social bonding. Participants who perform the pain tasks with others reported a greater sense of being connected (*M* = 3.71, *SD* = 1.01) than those who perform non-painful tasks (*M* = 3.14, *SD* = 1.09), *F*(1, 52) = 4.09, *p* = .048.

#### Significant outliers?

**Note**: graphs not shown here, check the R markdown document.
```r
ggplot(bastian_clean, aes(x = condition, y = bonding_mean)) +
  geom_boxplot()
```

It appears only two participants felt relatively disconnected after sharing pain with strangers.

#### Normality?
```r
with(bastian_clean, shapiro.test(bonding_mean[condition == "Control"]))
with(bastian_clean, shapiro.test(bonding_mean[condition == "Pain"]))
```

Data from the control condition (*p* = .304) are normally distributed, but are non-normal for the pain group (*p* = .002).

It is possible, and advisable, to check for normality among the residuals of an `aov` object.
```r
bastian_residuals <- residuals(object = bastian_aov)
shapiro.test(bastian_residuals)
```

The residuals are not normally distributed. However, like the independent t-test, a one-way ANOVA is *robust*. One condition is normal, while the other is not. The violation of normality is moderate.

**Note**: graphs not shown here, check the R markdown document.
```r
ggplot(bastian_clean, aes(x = bonding_mean)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ condition)
```

Most participants in the pain group rated their sense of bonding above 3.

#### Homoscedasticity?
```r
leveneTest(bonding_mean ~ condition, data = bastian_clean)
```

The *F* value does not exceed the critical value, so we accept the null hypothesis. The data are homoscedastic, *F*(1, 59) = 0.29, *p* = .593.

### Visualize
Now we can create a bar graph depicting the degree of error.

First, construct a `ggplot2` theme consistent with APA-style.

```r
apa_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12))
```

Remember those R objects holding our descriptive statistics?

Only the summary statistics for `bonding_mean` are necessary. After selecting those, we will assign the condition as a new variable.

```r
control_dynamite <- control_desc %>%
  mutate(measure = row.names(control_desc)) %>%
  select(measure, mean, se) %>%
  filter(measure == "bonding_mean") %>%
  mutate(condition = "Control")
pain_dynamite <- pain_desc %>%
    mutate(measure = row.names(pain_desc)) %>%
    select(measure, mean, se) %>%
    filter(measure == "bonding_mean") %>%
    mutate(condition = "Pain")
```

Finally, we will merge the data frames. The creation of a single data frame with the mean and standard error for both conditions makes plotting simple.

```r
(dynamite_plot <- merge(pain_dynamite, control_dynamite,
                       by = c("measure", "condition", "mean", "se"), all = TRUE))
```

This code creates the bar graph.

```r
ggplot(dynamite_plot, aes(x = condition, y = mean, fill = condition)) +
  geom_bar(position = "dodge", stat = "identity", color = "black", size = 0.5) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width = 0.1, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("Control" = "#FFFFFF", "Pain" = "#999999"),
                    labels = c("Control", "Pain")) +
  labs(x = "", y = "Mean Level of Social Bonding") +
  
  expand_limits(y = c(1, 5)) +
  scale_y_continuous(breaks = 0:5) +
  apa_theme +
  theme(legend.position = "none")
```

**Note**: graphs not shown here, check the R markdown document.

**Figure 1.** Results from Experiment 1 showing the mean level of social bonding reported for each group.

## Communicate
___
Participants in the pain group (*M* = 6.07, *SD* = 2.00) rated the intensity of their pain higher than those in the control group (*M* = 1.67, *SD* = 0.92), *t*(52) = 10.41, *p* < .001. Those subjected to the ice water task and squats (*M* = 6.00, *SD* = 1.96) were more likely than their counterparts (*M* = 1.74, *SD* = 1.20) to report an unpleasant experience, *t*(52) = 9.63, *p* < .001. Groups neither differed between negative affect (pain: *M* = 1.34, *SD* = 0.45; control: *M* = 1.27, *SD* = 0.37, *t*(52) = 0.6, *p* = .554), nor positive affect (pain: *M* = 3.05, *SD* = 0.82; control: *M* = 2.80, *SD* = 0.83, *t*(52) = 1.09, *p* = .283). The pain group perceived their tasks to be slightly more threatening (*M* = 1.36, *SD* = 0.58) than the control group (*M* = 1.11, *SD* = 0.30), *t*(52) = 1.97, *p* = .054, but not more challenging (pain: *M* = 2.67, *SD* = 0.87; control: *M* = 2.38, *SD* = 0.91, *t*(52) = 1.22, *p* = .227).

Enduring pain with others has a mild effect on social bonding. Participants who perform the pain tasks with others reported a greater sense of being connected (*M* = 3.71, *SD* = 1.01) than those who perform non-painful tasks (*M* = 3.14, *SD* = 1.09), *F*(1, 52) = 4.09, *p* = .048.

# Acknowledgements
___
I am thankful for my advisor, Dr. Brandt A. Smith for introducing me to R, JASP, and OSL. The discipline of psychology is advocating for preregistered, open materials. His encouragement to utilize open data and open source software has positioned me in the middle of the reproducible movement.

I would still be clicking checkboxes and dropdowns to analyze data if it were not for [DataCamp](https://www.datacamp.com), [Rose Maier](https://rstudio-pubs-static.s3.amazonaws.com/65059_586f394d8eb84f84b1baaf56ffb6b47f.html), and [Alboukadel Kassambara](http://www.sthda.com/english/wiki/r-software).
