---
title: "Linear Regression"
output:
      html_document:
        keep_md: true
---


``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.2
## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.1.0     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.4.1 ──
## ✔ broom        1.0.10     ✔ rsample      1.3.1 
## ✔ dials        1.4.2      ✔ tailor       0.1.0 
## ✔ infer        1.0.9      ✔ tune         2.0.0 
## ✔ modeldata    1.5.1      ✔ workflows    1.3.0 
## ✔ parsnip      1.3.3      ✔ workflowsets 1.1.1 
## ✔ recipes      1.3.1      ✔ yardstick    1.3.2 
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ scales::discard() masks purrr::discard()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ recipes::fixed()  masks stringr::fixed()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ yardstick::spec() masks readr::spec()
## ✖ recipes::step()   masks stats::step()
```

``` r
library(sjPlot)
```

```
## 
## Attaching package: 'sjPlot'
## 
## The following object is masked from 'package:ggplot2':
## 
##     set_theme
```

``` r
library(psych)
```

```
## 
## Attaching package: 'psych'
## 
## The following objects are masked from 'package:scales':
## 
##     alpha, rescale
## 
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

``` r
library(parallel)
library(finalfit)
library(gtsummary)
library(mlbench)
library(vip)
```

```
## 
## Attaching package: 'vip'
## 
## The following object is masked from 'package:utils':
## 
##     vi
```

``` r
library(rsample)
library(tune)
library(recipes)
library(yardstick)
library(parsnip)
library(glmnet)
```

```
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
## 
## Loaded glmnet 4.1-10
```

``` r
library(themis)
library(corrr)
library(performance)
```

```
## 
## Attaching package: 'performance'
## 
## The following objects are masked from 'package:yardstick':
## 
##     mae, rmse
```

``` r
library(utils)
library(see)

data <- read_csv("canpath_imputed.csv")
```

```
## Rows: 41187 Columns: 93
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): ID
## dbl (92): ADM_STUDY_ID, SDC_GENDER, SDC_AGE_CALC, SDC_MARITAL_STATUS, SDC_ED...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## Linear Regression

A linear regression is a type of regression where the outcome variable is continuous and we assume it has a normal distribution. 

#### Feature selection

Feature selection is an important topic in ML because good predictor requires many features that are able to predict unique aspects of the outcome variable. 

## Research question and data

Our research question is:  

**What factors are associated with BMI?**

We have already worked on the imputed data from the missing data class so we are going to use that imputed data here as a starting point for the work. This is partly to save time for the teaching part of the class. Remember from the missing data class that ideally we would

1. Conduct sensitivity analyses with a number of different imputation methods. 
2. Account for uncertainty using pooled models. 

We are going to skip that for now but I will show an example of how `tidymodels` imputes data later in this doc. 

#### Outcome variable

Let's look at the outcome variable, recode, and drop observations that are not relevant. We need to do a histogram and check the distribution. Then we might deal with outliers.  


``` r
summary(data$PM_BMI_SR)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    8.86   23.41   26.62   27.66   30.68   69.40
```

``` r
bmi_histogram <- ggplot(data = data, aes(PM_BMI_SR)) +
                  geom_histogram(binwidth = 2)
plot(bmi_histogram)
```

![](linear_regression_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Nice normal(ish) distribution here. We probably have some outliers on the low and high end with values of 8.86 and 69.40 

We can recode people who are less than 10 and greater than 60 to values of 10 and 60 respectively. 


``` r
data <- data %>%
          mutate(bmi_recode = case_when(
            PM_BMI_SR < 10 ~ 10, 
            PM_BMI_SR > 60 ~ 60,
            TRUE ~ PM_BMI_SR
          ))
summary(data$bmi_recode)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   10.00   23.41   26.62   27.65   30.68   60.00
```

``` r
bmi_recode_histogram <- ggplot(data = data, aes(bmi_recode)) +
                  geom_histogram(binwidth = 2)
plot(bmi_recode_histogram)
```

![](linear_regression_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### Preparing predictor variables

All of the predictors are coded as 0,1,2 are read in as numeric by R so we need to fix that. We could manually fix each variable but we are going to do something a bit different. All of the `_EVER` variables are coded as

    * 0 Never had disease
    * 1 Ever had disease
    * 2 Presumed - Never had disease
    * -7 Not Applicable

We can batch recode all of these variables and make sure that they are factor and not numeric.


``` r
data <- data %>% mutate_at(3, factor)
data <- data %>% mutate_at(5:6, factor)
data <- data %>% mutate_at(8:12, factor)
data <- data %>% mutate_at(15:81, factor)
data <- data %>% mutate_at(83:93, factor)
```

## Recoding

**Age**


``` r
summary(data$SDC_AGE_CALC) 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   30.00   43.00   52.00   51.48   60.00   74.00
```

``` r
### Checking NA

data %>% summarise(
                  n = n_distinct(SDC_AGE_CALC),
                  na = sum(is.na(SDC_AGE_CALC)
                           ))
```

```
## # A tibble: 1 × 2
##       n    na
##   <int> <int>
## 1    45     0
```

**Income**


``` r
glimpse(data$SDC_INCOME)
```

```
##  Factor w/ 8 levels "1","2","3","4",..: 6 6 4 3 4 4 5 3 3 5 ...
```

``` r
table(data$SDC_INCOME)
```

```
## 
##    1    2    3    4    5    6    7    8 
##  574 2447 6815 8008 7667 8826 3965 2885
```

``` r
data <- data %>%
	mutate(income_recode = case_when(
		SDC_INCOME == 1 ~ "Less than 25 000 $",
		SDC_INCOME == 2 ~ "Less than 25 000 $",
		SDC_INCOME == 3 ~ "25 000 $ - 49 999 $",
		SDC_INCOME == 4 ~ "50 000 $ - 74 999 $",
		SDC_INCOME == 5 ~ "75 000 $ - 99 999 $",
		SDC_INCOME == 6 ~ "100 000 $ - 149 999 $",		
		SDC_INCOME == 7 ~ "150 000 $ - 199 999 $",
		SDC_INCOME == 8 ~ "200 000 $ or more"
	))

glimpse(data$income_recode)
```

```
##  chr [1:41187] "100 000 $ - 149 999 $" "100 000 $ - 149 999 $" ...
```

``` r
data$income_recode <- as_factor(data$income_recode)

data$income_recode <- fct_relevel(data$income_recode, "Less than 25 000 $", 
                                                          "25 000 $ - 49 999 $",
                                                          "50 000 $ - 74 999 $",
                                                          "75 000 $ - 99 999 $",
                                                          "100 000 $ - 149 999 $",
                                                          "150 000 $ - 199 999 $",
                                                          "200 000 $ or more"
                                          )
table(data$income_recode)
```

```
## 
##    Less than 25 000 $   25 000 $ - 49 999 $   50 000 $ - 74 999 $ 
##                  3021                  6815                  8008 
##   75 000 $ - 99 999 $ 100 000 $ - 149 999 $ 150 000 $ - 199 999 $ 
##                  7667                  8826                  3965 
##     200 000 $ or more 
##                  2885
```

``` r
table(data$income_recode, data$SDC_INCOME)
```

```
##                        
##                            1    2    3    4    5    6    7    8
##   Less than 25 000 $     574 2447    0    0    0    0    0    0
##   25 000 $ - 49 999 $      0    0 6815    0    0    0    0    0
##   50 000 $ - 74 999 $      0    0    0 8008    0    0    0    0
##   75 000 $ - 99 999 $      0    0    0    0 7667    0    0    0
##   100 000 $ - 149 999 $    0    0    0    0    0 8826    0    0
##   150 000 $ - 199 999 $    0    0    0    0    0    0 3965    0
##   200 000 $ or more        0    0    0    0    0    0    0 2885
```

### Preliminary analysis

#### Correlations 


``` r
data1 <- data %>% select(1:10) ### Removing recoded variables so we don't try and regress them on themselves

pairs.panels(data1,
             scale = FALSE,      # If TRUE, scales the correlation text font
             method = "spearman", # Correlation method (also "spearman" or "kendall")
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             stars = FALSE)       # If TRUE, adds significance level with stars)          
```

![](linear_regression_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### Univariable associations


``` r
data <- data %>% select(!c(ID, PM_BMI_SR, SDC_INCOME)) ### Removing recoded variables so we don't try and regress them on themselves

univ_table <- data %>%
  tbl_uvregression(
    method = glm,
    y = bmi_recode,
    method.args = list(family = gaussian)) 

univ_table %>% as_kable()
```



|**Characteristic**        | **N**  | **Beta** |  **95% CI**  | **p-value** |
|:-------------------------|:------:|:--------:|:------------:|:-----------:|
|ADM_STUDY_ID              | 41,187 |   0.72   |  0.66, 0.77  |   <0.001    |
|SDC_GENDER                | 41,187 |          |              |             |
|1                         |        |    —     |      —       |             |
|2                         |        |  -0.98   | -1.1, -0.86  |   <0.001    |
|SDC_AGE_CALC              | 41,187 |   0.03   |  0.03, 0.04  |   <0.001    |
|SDC_MARITAL_STATUS        | 41,187 |          |              |             |
|1                         |        |    —     |      —       |             |
|2                         |        |   0.17   | -0.05, 0.38  |    0.13     |
|3                         |        |   0.48   |  0.12, 0.84  |    0.008    |
|4                         |        |  -0.04   | -0.35, 0.28  |     0.8     |
|5                         |        |  -0.02   | -0.22, 0.17  |     0.8     |
|SDC_EDU_LEVEL             | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   2.2    |  0.75, 3.7   |    0.003    |
|2                         |        |   1.2    |  -0.17, 2.6  |    0.086    |
|3                         |        |   1.5    |  0.16, 2.9   |    0.028    |
|4                         |        |   0.81   |  -0.56, 2.2  |     0.2     |
|5                         |        |   0.48   |  -0.91, 1.9  |     0.5     |
|6                         |        |   0.31   |  -1.1, 1.7   |     0.7     |
|7                         |        |   0.21   |  -1.2, 1.6   |     0.8     |
|SDC_EDU_LEVEL_AGE         | 41,187 |   0.00   | -0.01, 0.00  |     0.3     |
|SDC_INCOME_IND_NB         | 41,187 |          |              |             |
|1                         |        |    —     |      —       |             |
|2                         |        |   0.01   | -0.16, 0.17  |    >0.9     |
|3                         |        |  -0.09   | -0.30, 0.11  |     0.4     |
|4                         |        |  -0.38   | -0.58, -0.18 |   <0.001    |
|5                         |        |  -0.40   | -0.69, -0.11 |    0.007    |
|6                         |        |   0.00   | -0.50, 0.51  |    >0.9     |
|7                         |        |   0.49   |  -0.49, 1.5  |     0.3     |
|8                         |        |   0.59   |  -0.97, 2.2  |     0.5     |
|9                         |        |   -2.0   |  -5.8, 1.9   |     0.3     |
|10                        |        |   0.14   |  -4.5, 4.7   |    >0.9     |
|11                        |        |   -5.9   |   -13, 1.1   |    0.10     |
|12                        |        |   1.1    |  -7.4, 9.7   |     0.8     |
|13                        |        |   3.4    |   -5.2, 12   |     0.4     |
|22                        |        |   2.5    |  -4.5, 9.5   |     0.5     |
|SDC_HOUSEHOLD_ADULTS_NB   | 41,187 |          |              |             |
|1                         |        |    —     |      —       |             |
|2                         |        |  -0.14   | -0.29, 0.02  |    0.081    |
|3                         |        |   0.09   | -0.13, 0.30  |     0.4     |
|4                         |        |  -0.30   | -0.59, 0.00  |    0.049    |
|5                         |        |   0.41   |  -0.18, 1.0  |     0.2     |
|6                         |        |  -0.03   |  -1.6, 1.5   |    >0.9     |
|7                         |        |   1.5    |  -1.2, 4.1   |     0.3     |
|8                         |        |   1.7    |  -2.1, 5.6   |     0.4     |
|21                        |        |   1.0    |   -11, 13    |     0.9     |
|SDC_HOUSEHOLD_CHILDREN_NB | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.25   | -0.43, -0.07 |    0.005    |
|2                         |        |  -0.57   | -0.75, -0.39 |   <0.001    |
|3                         |        |  -0.47   | -0.79, -0.14 |    0.005    |
|4                         |        |  -0.23   | -0.93, 0.47  |     0.5     |
|5                         |        |   0.95   |  -0.75, 2.7  |     0.3     |
|6                         |        |   -3.4   |  -7.1, 0.22  |    0.066    |
|7                         |        |   2.4    |  -1.4, 6.3   |     0.2     |
|9                         |        |  -0.17   |  -6.2, 5.9   |    >0.9     |
|10                        |        |   -6.3   |   -18, 5.8   |     0.3     |
|HS_GEN_HEALTH             | 41,187 |          |              |             |
|1                         |        |    —     |      —       |             |
|2                         |        |   0.21   | -0.25, 0.67  |     0.4     |
|3                         |        |  -0.65   | -1.1, -0.23  |    0.003    |
|4                         |        |   -2.7   |  -3.1, -2.3  |   <0.001    |
|5                         |        |   -3.4   |  -3.8, -3.0  |   <0.001    |
|PA_TOTAL_SHORT            | 41,187 |   0.00   |  0.00, 0.00  |   <0.001    |
|HS_ROUTINE_VISIT_EVER     | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.65   | -1.2, -0.12  |    0.016    |
|HS_DENTAL_VISIT_EVER      | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   1.5    |  0.36, 2.7   |    0.010    |
|HS_FOBT_EVER              | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.32   |  0.20, 0.44  |   <0.001    |
|HS_COL_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.40   |  0.27, 0.52  |   <0.001    |
|HS_SIG_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.61   |  0.44, 0.77  |   <0.001    |
|HS_SIG_COL_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.40   |  0.28, 0.52  |   <0.001    |
|HS_POLYP_EVER             | 41,187 |          |              |             |
|1                         |        |    —     |      —       |             |
|2                         |        |  -0.12   | -0.33, 0.09  |     0.3     |
|3                         |        |  -0.50   | -0.69, -0.30 |   <0.001    |
|HS_PSA_EVER               | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|0                         |        |   0.72   |  0.54, 0.90  |   <0.001    |
|1                         |        |   1.1    |  0.99, 1.3   |   <0.001    |
|WH_CONTRACEPTIVES_EVER    | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|0                         |        |  -0.98   | -1.2, -0.74  |   <0.001    |
|1                         |        |  -0.99   | -1.1, -0.86  |   <0.001    |
|WH_HFT_EVER               | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|0                         |        |  -0.98   | -1.1, -0.86  |   <0.001    |
|1                         |        |   -1.0   | -1.3, -0.70  |   <0.001    |
|WH_MENOPAUSE_EVER         | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|0                         |        |   -1.3   |  -1.5, -1.2  |   <0.001    |
|1                         |        |  -0.65   | -0.79, -0.51 |   <0.001    |
|WH_HRT_EVER               | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|0                         |        |   -1.1   | -1.2, -0.95  |   <0.001    |
|1                         |        |  -0.68   | -0.87, -0.50 |   <0.001    |
|WH_HYSTERECTOMY_EVER      | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|0                         |        |   -1.1   | -1.2, -0.99  |   <0.001    |
|1                         |        |  -0.31   | -0.52, -0.10 |    0.004    |
|WH_OOPHORECTOMY_EVER      | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|0                         |        |   -1.1   | -1.2, -0.94  |   <0.001    |
|1                         |        |  -0.18   | -0.45, 0.08  |     0.2     |
|HS_MMG_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.51   |  0.37, 0.65  |   <0.001    |
|HS_PAP_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.21   | -0.54, 0.13  |     0.2     |
|DIS_HBP_EVER              | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   2.4    |   2.2, 2.5   |   <0.001    |
|2                         |        |   3.1    |   2.7, 3.4   |   <0.001    |
|DIS_MI_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   1.3    |  0.88, 1.7   |   <0.001    |
|2                         |        |   2.0    |   1.7, 2.3   |   <0.001    |
|DIS_STROKE_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.87   |  0.36, 1.4   |   <0.001    |
|2                         |        |   1.9    |   1.6, 2.2   |   <0.001    |
|DIS_ASTHMA_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.40   |  0.22, 0.57  |   <0.001    |
|2                         |        |   2.2    |   1.9, 2.5   |   <0.001    |
|DIS_COPD_EVER             | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   1.6    |   1.1, 2.0   |   <0.001    |
|2                         |        |   2.1    |   1.8, 2.4   |   <0.001    |
|DIS_DEP_EVER              | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.53   |  0.35, 0.72  |   <0.001    |
|2                         |        |   2.4    |   2.0, 2.7   |   <0.001    |
|DIS_DIAB_EVER             | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   2.1    |   1.9, 2.4   |   <0.001    |
|2                         |        |   2.7    |   2.4, 3.1   |   <0.001    |
|DIS_LC_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.83   |  -0.13, 1.8  |    0.092    |
|2                         |        |   2.1    |   1.8, 2.4   |   <0.001    |
|DIS_CH_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.51   |  -0.20, 1.2  |     0.2     |
|2                         |        |   2.1    |   1.8, 2.4   |   <0.001    |
|DIS_CROHN_EVER            | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.71   |  0.06, 1.4   |    0.033    |
|2                         |        |   1.6    |   1.3, 1.9   |   <0.001    |
|DIS_UC_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.21   | -0.29, 0.71  |     0.4     |
|2                         |        |   1.6    |   1.4, 1.9   |   <0.001    |
|DIS_IBS_EVER              | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.35   |  0.14, 0.55  |   <0.001    |
|2                         |        |   1.8    |   1.5, 2.1   |   <0.001    |
|DIS_ECZEMA_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.05   | -0.13, 0.23  |     0.6     |
|2                         |        |   2.8    |   2.5, 3.2   |   <0.001    |
|DIS_SLE_EVER              | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   1.7    |  0.82, 2.6   |   <0.001    |
|2                         |        |   2.1    |   1.9, 2.4   |   <0.001    |
|DIS_PS_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.25   | -0.02, 0.51  |    0.070    |
|2                         |        |   2.3    |   2.0, 2.6   |   <0.001    |
|DIS_MS_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.89   |  0.20, 1.6   |    0.012    |
|2                         |        |   2.1    |   1.8, 2.3   |   <0.001    |
|DIS_OP_EVER               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.26   |  0.00, 0.53  |    0.049    |
|2                         |        |   1.8    |   1.5, 2.0   |   <0.001    |
|DIS_ARTHRITIS_EVER        | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.72   |  0.58, 0.85  |   <0.001    |
|2                         |        |   2.6    |   2.3, 3.0   |   <0.001    |
|DIS_CANCER_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.22   |  0.03, 0.41  |    0.024    |
|DIS_HBP_FAM_EVER          | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.41   |  0.27, 0.56  |   <0.001    |
|2                         |        |   0.36   |  0.15, 0.57  |   <0.001    |
|DIS_MI_FAM_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.55   |  0.41, 0.69  |   <0.001    |
|2                         |        |   0.40   |  0.24, 0.56  |   <0.001    |
|DIS_STROKE_FAM_EVER       | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.47   |  0.32, 0.61  |   <0.001    |
|2                         |        |   0.32   |  0.18, 0.46  |   <0.001    |
|DIS_ASTHMA_FAM_EVER       | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.33   |  0.18, 0.47  |   <0.001    |
|2                         |        |   0.22   |  0.08, 0.37  |    0.002    |
|DIS_COPD_FAM_EVER         | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.53   |  0.36, 0.70  |   <0.001    |
|2                         |        |   0.30   |  0.17, 0.43  |   <0.001    |
|DIS_DEP_FAM_EVER          | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.41   |  0.27, 0.56  |   <0.001    |
|2                         |        |   0.26   |  0.11, 0.40  |   <0.001    |
|DIS_DIAB_FAM_EVER         | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.59   |  0.46, 0.73  |   <0.001    |
|2                         |        |   0.32   |  0.16, 0.48  |   <0.001    |
|DIS_LC_FAM_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.46   |  0.25, 0.66  |   <0.001    |
|2                         |        |   0.26   |  0.13, 0.38  |   <0.001    |
|DIS_CH_FAM_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.36   |  0.10, 0.62  |    0.006    |
|2                         |        |   0.30   |  0.17, 0.42  |   <0.001    |
|DIS_CROHN_FAM_EVER        | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.47   |  0.23, 0.71  |   <0.001    |
|2                         |        |   0.34   |  0.21, 0.46  |   <0.001    |
|DIS_UC_FAM_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.48   |  0.26, 0.70  |   <0.001    |
|2                         |        |   0.30   |  0.17, 0.42  |   <0.001    |
|DIS_IBS_FAM_EVER          | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.41   |  0.24, 0.57  |   <0.001    |
|2                         |        |   0.32   |  0.18, 0.45  |   <0.001    |
|DIS_ECZEMA_FAM_EVER       | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.59   |  0.44, 0.74  |   <0.001    |
|2                         |        |   0.32   |  0.18, 0.46  |   <0.001    |
|DIS_SLE_FAM_EVER          | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   1.1    |  0.70, 1.4   |   <0.001    |
|2                         |        |   0.37   |  0.25, 0.49  |   <0.001    |
|DIS_PS_FAM_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.71   |  0.54, 0.88  |   <0.001    |
|2                         |        |   0.33   |  0.19, 0.46  |   <0.001    |
|DIS_MS_FAM_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.80   |  0.45, 1.1   |   <0.001    |
|2                         |        |   0.33   |  0.21, 0.45  |   <0.001    |
|DIS_OP_FAM_EVER           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.53   |  0.38, 0.68  |   <0.001    |
|2                         |        |   0.30   |  0.16, 0.44  |   <0.001    |
|DIS_ARTHRITIS_FAM_EVER    | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.56   |  0.42, 0.70  |   <0.001    |
|2                         |        |   0.31   |  0.14, 0.49  |   <0.001    |
|DIS_CANCER_FAM_EVER       | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.27   |  0.15, 0.40  |   <0.001    |
|DIS_CANCER_F_EVER         | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.14   |  0.01, 0.27  |    0.040    |
|DIS_CANCER_M_EVER         | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.27   |  0.14, 0.40  |   <0.001    |
|DIS_CANCER_SIB_EVER       | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.52   |  0.36, 0.67  |   <0.001    |
|DIS_CANCER_CHILD_EVER     | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.03   | -0.32, 0.38  |     0.9     |
|ALC_EVER                  | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.19   | -0.44, 0.07  |    0.15     |
|SMK_CIG_EVER              | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.42   |  0.30, 0.54  |   <0.001    |
|SMK_CIG_WHOLE_EVER        | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|0                         |        |  -0.42   | -0.55, -0.29 |   <0.001    |
|1                         |        |  -0.41   | -0.58, -0.24 |   <0.001    |
|DIS_ENDO_HB_CHOL_EVER     | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.86   |  0.73, 1.0   |   <0.001    |
|2                         |        |   -1.4   |  -1.6, -1.1  |   <0.001    |
|DIS_CARDIO_HD_EVER        | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.83   |  0.51, 1.2   |   <0.001    |
|2                         |        |   0.86   |  0.62, 1.1   |   <0.001    |
|DIS_RESP_SLEEP_APNEA_EVER | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   1.8    |   1.6, 2.0   |   <0.001    |
|2                         |        |   0.75   |  0.46, 1.0   |   <0.001    |
|DIS_MH_ANXIETY_EVER       | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.37   |  0.19, 0.55  |   <0.001    |
|2                         |        |   0.08   | -0.37, 0.52  |     0.7     |
|DIS_MH_ADDICTION_EVER     | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.46   |  0.08, 0.84  |    0.017    |
|2                         |        |   0.83   |  0.59, 1.1   |   <0.001    |
|DIS_NEURO_MIGRAINE_EVER   | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.19   | -0.33, -0.05 |    0.007    |
|2                         |        |   0.82   |  0.51, 1.1   |   <0.001    |
|PSE_ADULT_WRK_DURATION    | 41,187 |   0.04   |  0.04, 0.05  |   <0.001    |
|PSE_WRK_FREQ              | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.16   | -0.05, 0.37  |    0.14     |
|2                         |        |   0.77   |  0.45, 1.1   |   <0.001    |
|3                         |        |   0.68   |  0.37, 0.98  |   <0.001    |
|4                         |        |   0.68   |  0.37, 0.99  |   <0.001    |
|WRK_FULL_TIME             | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.17   | -0.29, -0.05 |    0.007    |
|WRK_PART_TIME             | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.26   | -0.43, -0.10 |    0.002    |
|WRK_RETIREMENT            | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.42   |  0.27, 0.57  |   <0.001    |
|WRK_HOME_FAMILY           | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.57   | -0.82, -0.33 |   <0.001    |
|WRK_UNABLE                | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   1.2    |  0.94, 1.6   |   <0.001    |
|WRK_UNEMPLOYED            | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |   0.46   |  0.18, 0.73  |    0.001    |
|WRK_UNPAID                | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.28   | -0.64, 0.08  |    0.13     |
|WRK_STUDENT               | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.63   | -1.2, -0.07  |    0.028    |
|WRK_EMPLOYMENT            | 41,187 |          |              |             |
|0                         |        |    —     |      —       |             |
|1                         |        |  -0.33   | -0.46, -0.20 |   <0.001    |
|WRK_SCHEDULE_CUR_CAT      | 41,187 |          |              |             |
|-7                        |        |    —     |      —       |             |
|1                         |        |  -0.38   | -0.52, -0.25 |   <0.001    |
|2                         |        |  -0.01   | -0.54, 0.52  |    >0.9     |
|3                         |        |   0.23   | -0.52, 0.97  |     0.6     |
|4                         |        |   0.07   | -0.22, 0.36  |     0.6     |
|5                         |        |  -0.30   | -0.99, 0.39  |     0.4     |
|6                         |        |  -0.15   | -0.40, 0.10  |     0.2     |
|7                         |        |  -0.30   | -0.68, 0.08  |    0.12     |
|income_recode             | 41,187 |          |              |             |
|Less than 25 000 $        |        |    —     |      —       |             |
|25 000 $ - 49 999 $       |        |  -0.10   | -0.37, 0.16  |     0.4     |
|50 000 $ - 74 999 $       |        |  -0.28   | -0.54, -0.02 |    0.034    |
|75 000 $ - 99 999 $       |        |  -0.49   | -0.75, -0.23 |   <0.001    |
|100 000 $ - 149 999 $     |        |  -0.77   | -1.0, -0.51  |   <0.001    |
|150 000 $ - 199 999 $     |        |  -0.86   | -1.2, -0.57  |   <0.001    |
|200 000 $ or more         |        |  -0.90   | -1.2, -0.58  |   <0.001    |

We want to start by doing bivariable regression on the outcome and each variable. This can a be a bit of a process if we have lots of variables. Here we are using the `glm` (General Linear Model) function. 


``` r
model_income <- glm(bmi_recode ~ income_recode, data = data, family = "gaussian")
summary(model_income)
```

```
## 
## Call:
## glm(formula = bmi_recode ~ income_recode, family = "gaussian", 
##     data = data)
## 
## Coefficients:
##                                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                         28.1268     0.1126 249.792  < 2e-16 ***
## income_recode25 000 $ - 49 999 $    -0.1046     0.1353  -0.774 0.439206    
## income_recode50 000 $ - 74 999 $    -0.2799     0.1321  -2.118 0.034167 *  
## income_recode75 000 $ - 99 999 $    -0.4879     0.1329  -3.670 0.000243 ***
## income_recode100 000 $ - 149 999 $  -0.7688     0.1305  -5.893 3.81e-09 ***
## income_recode150 000 $ - 199 999 $  -0.8582     0.1495  -5.742 9.44e-09 ***
## income_recode200 000 $ or more      -0.8995     0.1611  -5.583 2.38e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 38.30313)
## 
##     Null deviance: 1581109  on 41186  degrees of freedom
## Residual deviance: 1577323  on 41180  degrees of freedom
## AIC: 267041
## 
## Number of Fisher Scoring iterations: 2
```

``` r
model_income_table <- tbl_regression(model_income) 

model_income_table %>% as_kable()
```



|**Characteristic**    | **Beta** |  **95% CI**  | **p-value** |
|:---------------------|:--------:|:------------:|:-----------:|
|income_recode         |          |              |             |
|Less than 25 000 $    |    —     |      —       |             |
|25 000 $ - 49 999 $   |  -0.10   | -0.37, 0.16  |     0.4     |
|50 000 $ - 74 999 $   |  -0.28   | -0.54, -0.02 |    0.034    |
|75 000 $ - 99 999 $   |  -0.49   | -0.75, -0.23 |   <0.001    |
|100 000 $ - 149 999 $ |  -0.77   | -1.0, -0.51  |   <0.001    |
|150 000 $ - 199 999 $ |  -0.86   | -1.2, -0.57  |   <0.001    |
|200 000 $ or more     |  -0.90   | -1.2, -0.58  |   <0.001    |

There are advantages and disadvantages to different was to display models. The `summary` method is good because we all of relevant output from the models. On the downside it's very ugly and hard to make nice tables with. The `tbl_regression` way is nice because we get nice output but we can miss things that might be relevant to our models. 

We always want to look at all of the bivariate associations for each independent variable. We can do this quickly with the final fit package. For now ignore the multivariable model results. We just want to look at the bivariable. 

## Machine Learning - Linear Regression 

In a machine learning approach, in general, our interest is less on the specific associations we see between individual variables and the outcome and more on the overall performance of the model in terms of predicting the outcome. We can assess this with R2 or RMSE. In ML, another key concept is model performance on unseen data. With the biostatistics approach, we want to know if the model fits some known distribution (think linear regression) but with ML we don't really care about that, we care about model performance with unseen data. Hopefully, that will sense later. 

### Resampling (Part 1)

More machine learning we need a way to split the data into a training set and a test set. There are a few different approaches too this. Here we are going to use an 70/30 split with 70% of the data going to training and 30 going to testing. This is sort of an older way to split data and I would say that a k-fold cross validation is probably more in line with modern practice. We will test this out later.  


``` r
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(10)

data_split <- initial_split(data, prop = 0.70)

# Create data frames for the two sets:
train_data <- training(data_split)
summary(train_data$bmi_recode)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   10.00   23.43   26.63   27.69   30.73   60.00
```

``` r
test_data  <- testing(data_split)
summary(test_data$bmi_recode)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   10.00   23.34   26.58   27.57   30.61   60.00
```

Now we have split the data, we want to create the model for the training data and save it so it can be applied to the testing set. This is basically exactly what we did before. __Note that we only run the model on the training data__ Not all of the data like you would in a traditional linear regression. Here we won't get the exact same result as our original linear regression because we don't have the same data. We expect there will be some variation but that the results should relatively similar. 

### 3.2 Running the regression

We need to decide what variables are going to go in our model. We could use traditional methods 

* Forward selection
* Backward selection
* Direct Acyclic Graphs

Here we are going to use Lasso and Ridge regression techniques, which are more akin to ML types of approaches. But first, let's just run a model with a few variables to show testing and training. 


``` r
lm_simple <- linear_reg() %>%
        set_engine("glm") %>%
        set_mode("regression") %>%
        fit(bmi_recode ~ SDC_GENDER + SDC_AGE_CALC + SDC_EDU_LEVEL_AGE + SDC_HOUSEHOLD_CHILDREN_NB + HS_GEN_HEALTH + DIS_DIAB_EVER, data = train_data)
```

### 3.3 Test the trained model

Once we `train the model` we want to understand how well our trained model works on new data the model has not seen. This is where the testing data comes in. We can use the `predict` feature for this. What we are doing here is predicting if someone has diabetes (yes/no) from the model we trained using the training data, on the testing data. We had 4293 observations in the training with 4077 people with on diabetes and 216 people with diabetes. Much of this example comes from [https://medium.com/the-researchers-guide/modelling-binary-logistic-regression-using-tidymodels-library-in-r-part-1-c1bdce0ac055](https://medium.com/the-researchers-guide/modelling-binary-logistic-regression-using-tidymodels-library-in-r-part-1-c1bdce0ac055)

The code below outputs the predict class `diabetes (yes/no)` for the test data. 


``` r
pred_bmi <- predict(lm_simple,
                      new_data = test_data)
summary(pred_bmi$.pred)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   21.09   26.38   27.51   27.67   28.91   33.33
```

``` r
summary(test_data$bmi_recode)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   10.00   23.34   26.58   27.57   30.61   60.00
```

Now we want to combine all of our results into one dataframe and just do a quick check. 


``` r
pred_true <- test_data |>
  select(bmi_recode) |>
  bind_cols(pred_bmi)

head(pred_true)
```

```
## # A tibble: 6 × 2
##   bmi_recode .pred
##        <dbl> <dbl>
## 1       23.4  28.3
## 2       41.3  26.6
## 3       21.7  25.7
## 4       22.1  29.0
## 5       28.1  26.2
## 6       23.9  26.4
```

Here we can see the first 6 rows of data. The model predicts some people with NA but others are NA. We didn't do anything with the missing values just yet so the model is quite dependent on having data for the predictors and the outcome.

### 3.3 Model evaluation

There are a number of different methods we must use to evaluate machine learning models. We will walk through those. 


``` r
lm_simple |> 
  extract_fit_engine() |> 
  check_model()
```

![](linear_regression_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

#### Root Mean Squared Error

We can calculate the MAE (Mean absolute error) and RMSE (Root Mean Squared Error), which is typically reported in linear models for ML. 

* MAE: Is a metric that tells us the mean absolute difference between the predicted values and the actual values in a dataset. The lower the MAE, the better a model fits a dataset.
* RMSE: Is a metric that tells us the square root of the average squared difference between the predicted values and the actual values in a dataset. The lower the RMSE, the better a model fits a dataset.


``` r
rsq(pred_true, truth = bmi_recode,
         estimate = .pred)
```

```
## # A tibble: 1 × 3
##   .metric .estimator .estimate
##   <chr>   <chr>          <dbl>
## 1 rsq     standard      0.0512
```


``` r
rmse(lm_simple, truth = bmi_recode,
         estimate = .pred)
```

```
## [1] 6.015543
```

## Tidymodels 

We have started using `tidymodels` but we are going to get into a bit more detail now. Tidymodels uses a 4(ish) step process to make the process of running models, selecting variables, model tuning, and getting results relatively easy. This is different from what you will see in the Intro to Stat learning book. Both are fine, but Tidymodels makes the variable selection, tuning, and visualization process much, much easier. 

### Data splitting 

We already saw the data splitting part as well. 

```{}
# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(10)

data_split <- initial_split(data, prop = 0.70)

# Create data frames for the two sets:
train_data <- training(data_split)
summary(train_data$bmi_recode)

test_data  <- testing(data_split)
summary(test_data$bmi_recode)
```

### Models

We already saw the model building stuff above. Here we use the tidy models to setup a model using `glm` and `regression` and we call the specific model we want to fit. 


``` r
linear_model <- linear_reg() %>%
        set_engine("glm") %>%
        set_mode("regression")
```

### Recipes 

The recipe() function as we used it here has two arguments

1. A formula. Any variable on the left-hand side of the tilde (~) is considered the model outcome (here, arr_delay). On the right-hand side of the tilde are the predictors. Variables may be listed by name, or you can use the dot (.) to indicate all other variables as predictors.
2. The data. A recipe is associated with the data set used to create the model. This will typically be the training set, so data = train_data here. Naming a data set doesn’t actually change the data itself; it is only used to catalog the names of the variables and their types, like factors, integers, dates, etc.

Now we can add roles to this recipe. We can use the update_role() function to let recipes know that `ADM_STUDY_ID` is a variable with a custom role that we called "ID" (a role can have any character value). Whereas our formula included all variables in the training set other than bmi_recode as predictors (that's what the `.` does), this tells the recipe to keep these two variables but not use them as either outcomes or predictors.

### 3.5. Transforming features (variables)

In machine learning these is a lot of emphasis placed on data pre-processing. Here we are going to talk about two approaches that you are probably familiar with but not in the machine learning context. Normalization/Standardization and creating one-hot encoding/dummy variables. 

#### Normalization/Standardization

Common practice in ML is normalize/standardize/z-score all of the continuous variables in a model. This creates variables with a mean of 0 and a standard deviation of 1. Doing this makes the specific coefficients of the association between the feature and outcome less interpretable BUT it helps a lot with model convergence and speed. Having continuous variables on lots of different scales can quickly create problems with model convergence. 

#### One-hot encoding/dummy variables

One-hot encoding or dummy variable coding converts all categorical variables into 0/1 versions of those variables rather than having them as factors with a dataframe. This encoding creates dummy variables from all categorical predictors. This again, speeds up computation time and can help with interpretation. It's not 100% necessary to do this, but you will see that it is common practice and often it's important just to know the words.


``` r
bmi_recipe <- 
  recipe(bmi_recode ~ ., data = train_data) %>% 
  update_role(ADM_STUDY_ID, new_role = "ID") %>% 
  step_normalize(all_numeric_predictors()) %>% ### Mean center and standardize (z-score) the numeric predictors
  step_dummy(all_nominal_predictors()) %>% ### One hot encoding. Dramatically improves model performance with many factors
  step_zv(all_predictors()) ### Remove columns from the data when the training set data have a single value. Zero variance predictor

summary(bmi_recipe)
```

```
## # A tibble: 92 × 4
##    variable                  type      role      source  
##    <chr>                     <list>    <chr>     <chr>   
##  1 ADM_STUDY_ID              <chr [2]> ID        original
##  2 SDC_GENDER                <chr [3]> predictor original
##  3 SDC_AGE_CALC              <chr [2]> predictor original
##  4 SDC_MARITAL_STATUS        <chr [3]> predictor original
##  5 SDC_EDU_LEVEL             <chr [3]> predictor original
##  6 SDC_EDU_LEVEL_AGE         <chr [2]> predictor original
##  7 SDC_INCOME_IND_NB         <chr [3]> predictor original
##  8 SDC_HOUSEHOLD_ADULTS_NB   <chr [3]> predictor original
##  9 SDC_HOUSEHOLD_CHILDREN_NB <chr [3]> predictor original
## 10 HS_GEN_HEALTH             <chr [3]> predictor original
## # ℹ 82 more rows
```

### Workflow

A workflow connects our recipe with out model. The workflow let's us setup the models without actually have run things over and over again. This is helpful because as you will sometimes models can take a long time to run. 


``` r
bmi_workflow <- 
        workflow() %>% 
        add_model(linear_model) %>% 
        add_recipe(bmi_recipe)

bmi_workflow
```

```
## ══ Workflow ════════════════════════════════════════════════════════════════════
## Preprocessor: Recipe
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## 3 Recipe Steps
## 
## • step_normalize()
## • step_dummy()
## • step_zv()
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Linear Regression Model Specification (regression)
## 
## Computational engine: glm
```

### Fit a model 


``` r
bmi_fit <- 
  bmi_workflow %>% 
  fit(data = train_data)
```



``` r
bmi_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()
```

```
## # A tibble: 194 × 5
##    term                   estimate std.error statistic  p.value
##    <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
##  1 (Intercept)            26.0        2.99      8.68   4.15e-18
##  2 SDC_AGE_CALC            0.0238     0.0720    0.331  7.41e- 1
##  3 SDC_EDU_LEVEL_AGE       0.0617     0.0408    1.51   1.31e- 1
##  4 PA_TOTAL_SHORT         -0.137      0.0362   -3.80   1.47e- 4
##  5 PSE_ADULT_WRK_DURATION  0.0723     0.0411    1.76   7.84e- 2
##  6 SDC_GENDER_X2           1.70       1.57      1.09   2.77e- 1
##  7 SDC_MARITAL_STATUS_X2   0.00531    0.157     0.0339 9.73e- 1
##  8 SDC_MARITAL_STATUS_X3   0.00251    0.238     0.0105 9.92e- 1
##  9 SDC_MARITAL_STATUS_X4  -0.347      0.204    -1.70   8.90e- 2
## 10 SDC_MARITAL_STATUS_X5   0.0292     0.161     0.182  8.56e- 1
## # ℹ 184 more rows
```

### Predict on new data


``` r
bmi_predicted <- augment(bmi_fit, test_data)
```

## Session Info


``` r
sessionInfo()
```

```
## R version 4.5.1 (2025-06-13)
## Platform: aarch64-apple-darwin20
## Running under: macOS Tahoe 26.2
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: America/Regina
## tzcode source: internal
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] see_0.12.0         performance_0.15.1 corrr_0.4.5        themis_1.0.3      
##  [5] glmnet_4.1-10      Matrix_1.7-3       vip_0.4.1          mlbench_2.1-6     
##  [9] gtsummary_2.4.0    finalfit_1.1.0     psych_2.5.6        sjPlot_2.9.0      
## [13] yardstick_1.3.2    workflowsets_1.1.1 workflows_1.3.0    tune_2.0.0        
## [17] tailor_0.1.0       rsample_1.3.1      recipes_1.3.1      parsnip_1.3.3     
## [21] modeldata_1.5.1    infer_1.0.9        dials_1.4.2        scales_1.4.0      
## [25] broom_1.0.10       tidymodels_1.4.1   lubridate_1.9.4    forcats_1.0.0     
## [29] stringr_1.5.2      dplyr_1.1.4        purrr_1.1.0        readr_2.1.5       
## [33] tidyr_1.3.1        tibble_3.3.0       ggplot2_4.0.0      tidyverse_2.0.0   
## 
## loaded via a namespace (and not attached):
##  [1] RColorBrewer_1.1-3   rstudioapi_0.17.1    jsonlite_2.0.0      
##  [4] shape_1.4.6.1        datawizard_1.3.0     magrittr_2.0.4      
##  [7] jomo_2.7-6           farver_2.1.2         cardx_0.3.0         
## [10] nloptr_2.2.1         rmarkdown_2.29       vctrs_0.6.5         
## [13] minqa_1.2.8          sparsevctrs_0.3.4    htmltools_0.5.8.1   
## [16] haven_2.5.5          mitml_0.4-5          sass_0.4.10         
## [19] parallelly_1.45.1    bslib_0.9.0          cachem_1.1.0        
## [22] lifecycle_1.0.4      iterators_1.0.14     pkgconfig_2.0.3     
## [25] R6_2.6.1             fastmap_1.2.0        rbibutils_2.3       
## [28] future_1.67.0        digest_0.6.37        furrr_0.3.1         
## [31] patchwork_1.3.2      labeling_0.4.3       timechange_0.3.0    
## [34] mgcv_1.9-3           compiler_4.5.1       bit64_4.6.0-1       
## [37] withr_3.0.2          S7_0.2.0             backports_1.5.0     
## [40] pan_1.9              MASS_7.3-65          lava_1.8.1          
## [43] tools_4.5.1          future.apply_1.20.0  nnet_7.3-20         
## [46] glue_1.8.0           nlme_3.1-168         grid_4.5.1          
## [49] generics_0.1.4       gtable_0.3.6         labelled_2.15.0     
## [52] tzdb_0.5.0           class_7.3-23         data.table_1.17.8   
## [55] hms_1.1.3            utf8_1.2.6           ggrepel_0.9.6       
## [58] foreach_1.5.2        pillar_1.11.1        vroom_1.6.5         
## [61] splines_4.5.1        lhs_1.2.0            lattice_0.22-7      
## [64] survival_3.8-3       bit_4.6.0            tidyselect_1.2.1    
## [67] knitr_1.50           reformulas_0.4.1     xfun_0.53           
## [70] hardhat_1.4.2        timeDate_4041.110    stringi_1.8.7       
## [73] DiceDesign_1.10      yaml_2.3.10          boot_1.3-31         
## [76] evaluate_1.0.5       codetools_0.2-20     cli_3.6.5           
## [79] rpart_4.1.24         Rdpack_2.6.4         jquerylib_0.1.4     
## [82] Rcpp_1.1.0           ROSE_0.0-4           globals_0.18.0      
## [85] gower_1.0.2          bayestestR_0.17.0    GPfit_1.0-9         
## [88] lme4_1.1-37          listenv_0.9.1        broom.helpers_1.22.0
## [91] ipred_0.9-15         prodlim_2025.04.28   insight_1.4.4       
## [94] crayon_1.5.3         rlang_1.1.6          mnormt_2.1.1        
## [97] cards_0.7.0          mice_3.18.0
```



