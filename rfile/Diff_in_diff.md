Difference in difference with LISS propensity to vote and political
orientation and IV on Ukraine war
================

## Read the data

We previously cleaned and joined the data in a csv balanced panel file.
Here we simply read in that data.

``` r
library(readr)
library(bacondecomp)
library(did)
library(eatATA)
library(haven)
library(lfe)
library(panelView)
library(tidyverse)
library(ivDiag)

panel19_23_stacked <- read_csv("panel19_23_stacked.csv")
```

Balanced panel, 5 years of data.

``` r
panel19_23_stacked %>% 
  dplyr:: group_by(survey_year) %>% 
  dplyr:: summarise(count=n())
```

    ## # A tibble: 5 × 2
    ##   survey_year count
    ##         <dbl> <int>
    ## 1        2019  1658
    ## 2        2020  1658
    ## 3        2021  1658
    ## 4        2022  1658
    ## 5        2023  1658

Every year about 866 individuals from our panel are left-leaning and 792
are right-leaning.

``` r
panel19_23_stacked %>% 
  dplyr:: group_by(right) %>% 
  dplyr:: summarise(count=n()/5)
```

    ## # A tibble: 2 × 2
    ##   right count
    ##   <dbl> <dbl>
    ## 1     0   866
    ## 2     1   792

## Create treatment variable

In our case, we need to create a treatment variable - **treatment_z**-
which takes value 1 when year=2022 or 2023 and the individual is
left-leaning (i.e. right==0).

Another possibility is that the effect of the crisis had not yet take
center stage in 2022 at the time when the LISS data was collected (Dec
2021-Mar 2022), with the war commencing enf of Feb 2022. Thus another
instrument we could try is treatment instrument **treatment_23** which
takes value 1 when year=2023 (not 2022) and the individual is
left-leaning.

In addition, political orientation is encoded on a 0 to 10 scale. We
rescale it to 1 to 11 (to avoid having zeros).

``` r
panel19_23_stacked <- panel19_23_stacked %>% 
  mutate(treatment_z = if_else((right==0 & survey_year>=2022), true= 1, false=0)) %>% 
  mutate(treatment_23 =if_else((right==0 & survey_year==2023), true= 1, false=0))
```

``` r
panel19_23_stacked %>% 
  head()
```

    ## # A tibble: 6 × 9
    ##    ...1 nomem_encr   age propensity_to_vote political_orientation survey_year
    ##   <dbl>      <dbl> <dbl>              <dbl>                 <dbl>       <dbl>
    ## 1     1     800009    63                 95                     1        2019
    ## 2     2     800009    64                 95                     2        2020
    ## 3     3     800009    65                100                     2        2021
    ## 4     4     800009    66                100                     2        2022
    ## 5     5     800009    67                 95                     2        2023
    ## 6     6     800015    56                100                     3        2019
    ## # ℹ 3 more variables: right <dbl>, treatment_z <dbl>, treatment_23 <dbl>

## Plotting the data

``` r
panel19_23_stacked %>% 
  dplyr:: group_by(right, survey_year) %>% 
  dplyr:: summarise(mean_prop_to_vote= (mean(propensity_to_vote))) %>% 
  dplyr:: mutate(right=as.factor(right)) %>% 
  ggplot(aes(x=survey_year, y=mean_prop_to_vote, color=right))+
  geom_point()+
  geom_line()+
  labs(title="Mean propensity to vote by political orientation",
       subtitle = "1=right-leaning, 0=left-leaning, no swing",
       caption="LISS Panel data 2019-2023: 1658*5= 8290 observations")
```

    ## `summarise()` has grouped output by 'right'. You can override using the
    ## `.groups` argument.

![](Diff_in_diff_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
panel19_23_stacked %>% 
  mutate(right=as.factor(right)) %>% 
  ggplot(aes(x=propensity_to_vote, y=right, color=right)) +
  geom_point()+
  geom_boxplot() +
  coord_flip() +
  facet_wrap(vars(survey_year)) +
  labs(title="Distribution of propensity to vote by political orientation",
       x="propensity to vote (0 to 100)",
       y="")
```

![](Diff_in_diff_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## TSLS with IV=time of the Russia-Ukraine war for the left-leaning group

Now we estimate the first stage of our dif-in-dif model

``` r
dd_reg <- felm(political_orientation ~ treatment_z | survey_year + nomem_encr, data=panel19_23_stacked)

summary(dd_reg)
```

    ## 
    ## Call:
    ##    felm(formula = political_orientation ~ treatment_z | survey_year +      nomem_encr, data = panel19_23_stacked) 
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2435 -0.2752  0.0144  0.2759  2.8759 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## treatment_z  0.15218    0.02666   5.708  1.2e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5941 on 6627 degrees of freedom
    ## Multiple R-squared(full model): 0.9443   Adjusted R-squared: 0.9304 
    ## Multiple R-squared(proj model): 0.004892   Adjusted R-squared: -0.2447 
    ## F-statistic(full model):67.66 on 1662 and 6627 DF, p-value: < 2.2e-16 
    ## F-statistic(proj model): 32.58 on 1 and 6627 DF, p-value: 1.196e-08

We can see that it seems that the treatment coefficient is highly
significant. We’d have to run a parallel trends regression to see if the
parallel trends assumption holds and the estimators on lagged treatment
are within the confidence interval.

But for now let’s store the main_ATT_1 and the confidence interval
boundaries.

``` r
main_ATT_1= summary(dd_reg)$coefficients["treatment_z", "Estimate"]

ATT_1_CI_lower_boundary = abs(main_ATT_1 - qnorm(0.975)*summary(dd_reg)$coefficients["treatment_z", "Std. Error"])

ATT_1_CI_upper_boundary = abs(main_ATT_1 + qnorm(0.975)*summary(dd_reg)$coefficients["treatment_z", "Std. Error"])
```

``` r
c(main_ATT_1, ATT_1_CI_lower_boundary, ATT_1_CI_upper_boundary)
```

    ## [1] 0.15218437 0.09992452 0.20444422

Event study: parallel trends assumption

``` r
panel19_23_stacked <- panel19_23_stacked %>% 
  mutate(lag19=if_else((survey_year==2019 & right==0), true=1, false=0),
         lag20=if_else((survey_year==2020 & right==0), true=1, false=0),
         lag21=if_else((survey_year==2021 & right==0), true=1, false=0),
         lead22=if_else((survey_year==2022 & right==0), true=1, false=0),
         lead23=if_else((survey_year==2023 & right==0), true=1, false=0))
```

``` r
event_study_reg <- felm(propensity_to_vote ~ lag19 + lag20 + lag21 + lead22 | survey_year + nomem_encr, data=panel19_23_stacked)
```

``` r
summary(event_study_reg)
```

    ## 
    ## Call:
    ##    felm(formula = propensity_to_vote ~ lag19 + lag20 + lag21 + lead22 |      survey_year + nomem_encr, data = panel19_23_stacked) 
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -75.940  -0.482   0.167   1.127  55.966 
    ## 
    ## Coefficients:
    ##        Estimate Std. Error t value Pr(>|t|)
    ## lag19    0.5818     0.7697   0.756    0.450
    ## lag20    0.6105     0.7697   0.793    0.428
    ## lag21   -0.1922     0.7697  -0.250    0.803
    ## lead22   0.3662     0.7697   0.476    0.634
    ## 
    ## Residual standard error: 11.07 on 6624 degrees of freedom
    ## Multiple R-squared(full model): 0.5935   Adjusted R-squared: 0.4913 
    ## Multiple R-squared(proj model): 0.0002593   Adjusted R-squared: -0.251 
    ## F-statistic(full model):5.808 on 1665 and 6624 DF, p-value: < 2.2e-16 
    ## F-statistic(proj model): 0.4295 on 4 and 6624 DF, p-value: 0.7874

``` r
plot_order <-c("lag19", "lag20", "lag21", "lead22")

#Plotting results
leadslags_plot <- tibble(
  sd = summary(event_study_reg)$coefficients[, "Std. Error"],
  mean = coef(event_study_reg)[plot_order],
  label = plot_order
)

leadslags_plot
```

    ## # A tibble: 4 × 3
    ##      sd   mean label 
    ##   <dbl>  <dbl> <chr> 
    ## 1 0.770  0.582 lag19 
    ## 2 0.770  0.611 lag20 
    ## 3 0.770 -0.192 lag21 
    ## 4 0.770  0.366 lead22

``` r
leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-qnorm(0.95)*sd,
             ymax = mean+qnorm(0.95)*sd)) +
  geom_hline(yintercept = ATT_1_CI_lower_boundary, color = "red") +
  geom_hline(yintercept = -ATT_1_CI_lower_boundary, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Years before the war") +
  ylab("Propensity to vote") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")
```

![](Diff_in_diff_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

For the second stage of our two-stage least-squares (TSLS) we need to
store the predicted values of political orientation from the first stage
model above (dd_reg). We’ll store them in a new variable called
pred_political_orientation.

``` r
panel19_23_stacked <- panel19_23_stacked %>% 
  mutate(pred_political_orientation = as.tibble(fitted(dd_reg)) %>% 
  pull(political_orientation))
```

Just to get an idea of how predicted political_orientation and true
political orientation compare, we’ll make a scatterplot.

``` r
panel19_23_stacked %>% 
  mutate(right=as.factor(right)) %>% 
  ggplot(aes(x=political_orientation, y=pred_political_orientation, color=right)) +
  geom_point() +
  facet_wrap(vars(survey_year)) +
  labs(title="Comparison between political orientation and predicted political orientation")
```

![](Diff_in_diff_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

For the second stage, we regress the outcome (propensity to vote) on the
predicted political orientation, with fixed effects.

``` r
dd_reg_2 <- felm(propensity_to_vote ~ pred_political_orientation | survey_year + nomem_encr, data=panel19_23_stacked)

summary(dd_reg_2)
```

    ## 
    ## Call:
    ##    felm(formula = propensity_to_vote ~ pred_political_orientation |      survey_year + nomem_encr, data = panel19_23_stacked) 
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -76.035  -0.673   0.165   1.180  55.821 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)
    ## pred_political_orientation  -0.9874     3.2642  -0.303    0.762
    ## 
    ## Residual standard error: 11.07 on 6627 degrees of freedom
    ## Multiple R-squared(full model): 0.5934   Adjusted R-squared: 0.4914 
    ## Multiple R-squared(proj model): 1.381e-05   Adjusted R-squared: -0.2508 
    ## F-statistic(full model):5.818 on 1662 and 6627 DF, p-value: < 2.2e-16 
    ## F-statistic(proj model): 0.09151 on 1 and 6627 DF, p-value: 0.7623

Our LATE estimand is highly insignificant. We store the estimated values
and confidence intervals.

``` r
LATE= summary(dd_reg_2)$coefficients["pred_political_orientation", "Estimate"]

LATE_CI_lower_boundary = abs(LATE - qnorm(0.975)*summary(dd_reg_2)$coefficients["pred_political_orientation", "Std. Error"])

LATE_CI_upper_boundary = abs(LATE + qnorm(0.975)*summary(dd_reg_2)$coefficients["pred_political_orientation", "Std. Error"])

c(LATE, LATE_CI_lower_boundary, LATE_CI_upper_boundary)
```

    ## [1] -0.987440  7.385149  5.410269

## Standard OLS estimation

We regress propensity to vote on political orientation, the treatment
variable (=1 after the crisis, for left-leaning people) and the
interaction term between treatment and political orientation.

Probably the relationship is endogenous, but just to be able to compare
results.

``` r
library(fixest)
```

    ## 
    ## Attaching package: 'fixest'

    ## The following object is masked from 'package:lfe':
    ## 
    ##     fepois

``` r
ols_reg <- feols(
  propensity_to_vote ~ political_orientation, # Regression formula
  data=panel19_23_stacked,
  vcov = "hc1" #--variance-covariance ratio, hc=heterochedasticity-consistent
)

summary(ols_reg)
```

    ## OLS estimation, Dep. Var.: propensity_to_vote
    ## Observations: 8,290
    ## Standard-errors: Heteroskedasticity-robust 
    ##                        Estimate Std. Error   t value   Pr(>|t|)    
    ## (Intercept)           95.616400   0.375839 254.40756  < 2.2e-16 ***
    ## political_orientation -0.356362   0.067875  -5.25027 1.5564e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 15.5   Adj. R2: 0.002553

Here we obtain a negative and significant coefficient. Increasing
political orientation by one unit = moving to the right is associated
with 0.356 lower propensity to vote.

``` r
library(binsreg)

binscatter <- binsreg(panel19_23_stacked$propensity_to_vote, panel19_23_stacked$political_orientation)
```

    ## Warning in binsreg(panel19_23_stacked$propensity_to_vote,
    ## panel19_23_stacked$political_orientation): Too small effective sample size for
    ## bin selection. # of mass of points or clusters used and by option ignored.

    ## Warning in binsreg(panel19_23_stacked$propensity_to_vote,
    ## panel19_23_stacked$political_orientation): dots=c(0,0) used.

![](Diff_in_diff_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
binscatter$bins_plot +
  labs(y = "Propensity to vote", x = "Political orientation")
```

![](Diff_in_diff_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

Simple TSLS, no fixed, effects:

``` r
feols(
  propensity_to_vote ~ 1 | 0 | political_orientation ~ treatment_z, 
  data = panel19_23_stacked,
  vcov = "hc1"
)
```

    ## TSLS estimation - Dep. Var.: propensity_to_vote
    ##                   Endo.    : political_orientation
    ##                   Instr.   : treatment_z
    ## Second stage: Dep. Var.: propensity_to_vote
    ## Observations: 8,290
    ## Standard-errors: Heteroskedasticity-robust 
    ##                            Estimate Std. Error   t value  Pr(>|t|)    
    ## (Intercept)               93.964346   0.996342 94.309350 < 2.2e-16 ***
    ## fit_political_orientation -0.042841   0.185301 -0.231197   0.81717    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 15.5   Adj. R2: 4.836e-4
    ## F-test (1st stage), political_orientation: stat = 1,750.2    , p < 2.2e-16 , on 1 and 8,288 DoF.
    ##                                Wu-Hausman: stat =     3.63248, p = 0.056697, on 1 and 8,287 DoF.

TSLS with fixed effects. Here we obtain the exact same results as above
when implementing TSLS in two stages.

``` r
feols(
  propensity_to_vote ~ 1 | survey_year + nomem_encr | political_orientation ~ treatment_z, 
  data = panel19_23_stacked,
  vcov = "hc1"
)
```

    ## TSLS estimation - Dep. Var.: propensity_to_vote
    ##                   Endo.    : political_orientation
    ##                   Instr.   : treatment_z
    ## Second stage: Dep. Var.: propensity_to_vote
    ## Observations: 8,290
    ## Fixed-effects: survey_year: 5,  nomem_encr: 1,658
    ## Standard-errors: Heteroskedasticity-robust 
    ##                           Estimate Std. Error   t value Pr(>|t|) 
    ## fit_political_orientation -0.98744    3.32016 -0.297408  0.76616 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 9.90739     Adj. R2:  0.490177
    ##                 Within R2: -0.002345
    ## F-test (1st stage), political_orientation: stat = 32.6    , p = 1.196e-8, on 1 and 6,627 DoF.
    ##                                Wu-Hausman: stat =  0.07705, p = 0.781345, on 1 and 6,626 DoF.

And a Hasman test, which is highly significant, so we shoould not use
OLS results.

``` r
library(ivDiag)

eff_F(panel19_23_stacked, Y = "propensity_to_vote", D = "political_orientation", Z = "treatment_z")
```

    ## [1] 3428.807

## Repeating TSLS for treatment_23 (if we assume the crisis is only apparent in the 2023 data)

Another possibility is that the effect of the crisis had not yet take
center stage in 2022 at the time when the LISS data was collected (Dec
2021-Mar 2022), with the war commencing enf of Feb 2022. Thus another
instrument we could try is treatment instrument **treatment_23** which
takes value 1 when year=2023 (not 2022) and the individual is
left-leaning.

``` r
feols(
  propensity_to_vote ~ 1 | survey_year + nomem_encr | political_orientation ~ treatment_23, 
  data = panel19_23_stacked,
  vcov = "hc1"
)
```

    ## TSLS estimation - Dep. Var.: propensity_to_vote
    ##                   Endo.    : political_orientation
    ##                   Instr.   : treatment_23
    ## Second stage: Dep. Var.: propensity_to_vote
    ## Observations: 8,290
    ## Fixed-effects: survey_year: 5,  nomem_encr: 1,658
    ## Standard-errors: Heteroskedasticity-robust 
    ##                           Estimate Std. Error   t value Pr(>|t|) 
    ## fit_political_orientation -1.68129    3.05966 -0.549503  0.58268 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 9.93219     Adj. R2:  0.487621
    ##                 Within R2: -0.007371
    ## F-test (1st stage), political_orientation: stat = 38.7     , p = 5.15e-10, on 1 and 6,627 DoF.
    ##                                Wu-Hausman: stat =  0.286263, p = 0.592643, on 1 and 6,626 DoF.

The coefficient estimate increases and the p-value improves, but the
conclusion does not change. We cannot reject the null hypothesis that
there is no difference in the impact the Ukraine-Russian was had on
left-leaning as compared to right-leaning Dutch voters.

And a Hausman test, which still is highly significant, so we should not
use the OLS results, which are also reported below for completeness.

``` r
eff_F(panel19_23_stacked, Y = "propensity_to_vote", D = "political_orientation", Z = "treatment_23")
```

    ## [1] 1744.044

Anderson-Rubin confidence set: F is too low.

``` r
ivDiag(panel19_23_stacked, Y="propensity_to_vote", D="political_orientation", Z="treatment_z")$AR
```

    ## Bootstrapping:

    ## Parallelising 1000 reps on 7 cores

    ## Bootstrap took22.393sec.

    ## AR Test Inversion...

    ## Parallelising on 7 cores

    ## $Fstat
    ##         F       df1       df2         p 
    ##    0.0534    1.0000 8288.0000    0.8172 
    ## 
    ## $ci.print
    ## [1] "[-0.4023, 0.3203]"
    ## 
    ## $ci
    ## [1] -0.4023  0.3203
    ## 
    ## $bounded
    ## [1] TRUE

## Panel with the swingers

We redo the same exercise as above, but allowing for swingers from LEFT
to RIGHT at any point during the 5 years.

``` r
panel_swingers <- read_csv("panel_swing_stacked_19_23.csv")
```

``` r
panel_swingers <- panel_swingers %>% 
  mutate(treatment_z = if_else((right==0 & survey_year>=2022), true= 1, false=0)) %>% 
  mutate(treatment_23 =if_else((right==0 & survey_year==2023), true= 1, false=0))
```

``` r
panel_swingers %>% 
  dplyr:: group_by(right, survey_year) %>% 
  dplyr:: summarise(mean_prop_to_vote= (mean(propensity_to_vote))) %>% 
  dplyr:: mutate(right=as.factor(right)) %>% 
  ggplot(aes(x=survey_year, y=mean_prop_to_vote, color=right))+
  geom_point()+
  geom_line()+
  labs(title="Mean propensity to vote by political orientation",
       subtitle = "1=right-leaning, 0=left-leaning, with LEFT to RIGHT swing",
       caption="LISS Panel data 2019-2023: 2224 *5 = 11'120 observations")
```

    ## `summarise()` has grouped output by 'right'. You can override using the
    ## `.groups` argument.

![](Diff_in_diff_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
feols(
  propensity_to_vote ~ 1 | survey_year + nomem_encr | political_orientation ~ treatment_z, 
  data = panel_swingers,
  vcov = "hc1"
)
```

    ## TSLS estimation - Dep. Var.: propensity_to_vote
    ##                   Endo.    : political_orientation
    ##                   Instr.   : treatment_z
    ## Second stage: Dep. Var.: propensity_to_vote
    ## Observations: 11,120
    ## Fixed-effects: survey_year: 5,  nomem_encr: 2,224
    ## Standard-errors: Heteroskedasticity-robust 
    ##                            Estimate Std. Error   t value Pr(>|t|) 
    ## fit_political_orientation -0.353088   0.879299 -0.401556  0.68802 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 10.2     Adj. R2:  0.522507
    ##              Within R2: -0.001258
    ## F-test (1st stage), political_orientation: stat = 266.7    , p < 2.2e-16 , on 1 and 8,891 DoF.
    ##                                Wu-Hausman: stat =   0.37273, p = 0.541536, on 1 and 8,890 DoF.

The estimate coefficient is lower than before.

``` r
library(ivDiag)

eff_F(panel_swingers, Y = "propensity_to_vote", D = "political_orientation", Z = "treatment_z")
```

    ## [1] 3952.492

## Panel with swingers and using “age” as a covariate

We can add age because did not change with the Ukraine-Russian war, but
we will model it as a quadratic function.

``` r
feols(
  propensity_to_vote ~ 1 + age | survey_year + nomem_encr | political_orientation ~ treatment_z + age, 
  data = panel_swingers,
  vcov = "hc1"
)
```

    ## The instrument 'age' has been removed because of collinearity (see $collin.var).

    ## TSLS estimation - Dep. Var.: propensity_to_vote
    ##                   Endo.    : political_orientation
    ##                   Instr.   : treatment_z, age
    ## Second stage: Dep. Var.: propensity_to_vote
    ## Observations: 11,120
    ## Fixed-effects: survey_year: 5,  nomem_encr: 2,224
    ## Standard-errors: Heteroskedasticity-robust 
    ##                            Estimate Std. Error   t value Pr(>|t|) 
    ## fit_political_orientation -0.345948   0.878534 -0.393779  0.69375 
    ## age                        0.169801   0.133479  1.272124  0.20336 
    ## ... 1 variable was removed because of collinearity (age)
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 10.2     Adj. R2:  0.52249
    ##              Within R2: -0.00118
    ## F-test (1st stage), political_orientation: stat = 133.6      , p < 2.2e-16 , on 2 and 8,890 DoF.
    ##                                Wu-Hausman: stat =   0.362035 , p = 0.547393, on 1 and 8,889 DoF.
    ##                                    Sargan: stat =  -2.469e-12, p = 1       , on 1 DoF.

Now let’s try with a quadratic age control.

``` r
panel_swingers <- panel_swingers %>% 
  mutate(age_squared= age^2)
```

``` r
feols(
  propensity_to_vote ~ 1 + age^2 | survey_year + nomem_encr | political_orientation ~ treatment_z + age^2, 
  data = panel_swingers,
  vcov = "hc1"
)
```

    ## The instrument 'I(age^2)' has been removed because of collinearity (see $collin.var).

    ## TSLS estimation - Dep. Var.: propensity_to_vote
    ##                   Endo.    : political_orientation
    ##                   Instr.   : treatment_z, I(age^2)
    ## Second stage: Dep. Var.: propensity_to_vote
    ## Observations: 11,120
    ## Fixed-effects: survey_year: 5,  nomem_encr: 2,224
    ## Standard-errors: Heteroskedasticity-robust 
    ##                            Estimate Std. Error   t value Pr(>|t|) 
    ## fit_political_orientation -0.375629   0.875476 -0.429057  0.66789 
    ## I(age^2)                  -0.001158   0.001373 -0.843588  0.39892 
    ## ... 1 variable was removed because of collinearity (I(age^2))
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 10.2     Adj. R2:  0.522428
    ##              Within R2: -0.00131 
    ## F-test (1st stage), political_orientation: stat = 134.3     , p < 2.2e-16 , on 2 and 8,890 DoF.
    ##                                Wu-Hausman: stat =   0.411576, p = 0.521187, on 1 and 8,889 DoF.
    ##                                    Sargan: stat =   0       , p = 1       , on 1 DoF.
