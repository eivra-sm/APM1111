---
title: "Formative Assessment 8"
author: "Sinocruz, A & Tagaytay, G"
date: "2025-11-26"
output: pdf_document
---


**Github link*:** *https://github.com/eivra-sm/APM1111/blob/main/FA8.md*

## Assumptions

### Assumption #1: You have one dependent variable that is measured at the continuous level.

``` r
str(df$weight)
```

```
##  num [1:30] 4.17 5.58 5.18 6.11 4.5 4.61 5.17 4.53 5.33 5.14 ...
```

``` r
summary(df$weight)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   3.590   4.550   5.155   5.073   5.530   6.310
```

**Remark.** The dependent variable in this study is weight, which represents the dried weight of individual plants measured in grams. Since weight is measured using a continuous numeric scale with meaningful intervals (e.g., 4.5 g, 5.2 g, 6.0 g), it qualifies as a continuous, quantitative variable. This meets the first requirement of one-way ANOVA, which assumes that the dependent variable is measured at the interval or ratio level.


**Remarks:** The independent variable in this study is **group**, which is a categorical factor with **three distinct and non-overlapping levels:** _ctrl_, _trt1_, and _trt2_. Each level represents a separate treatment condition applied to different sets of plants. Because the levels are mutually exclusive and clearly defined, this assumption is fully satisfied. The structure of the grouping variable is appropriate for conducting a one-way ANOVA, which requires at least three independent groups for comparison.  

### Assumption #3: You should have independence of observations.

**Remarks:** The observations in this dataset meet the requirement of independence. Each plant was grown, measured, and assigned to a treatment condition separately, with **each individual plant** contributing only one weight measurement. There is no repeated measurement, pairing, or clustering within the dataset. Since no plant appears in more than one group and there is no interaction among measurement units that would affect their outcomes, the independence assumption crucial for the validity of ANOVA is considered to be met.  

### Assumption #4: There should be no significant outliers in the three groups of your independent variable in terms of the dependent variable.  

![](FA8_FILES_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 

**Remarks:** The raincloud plot—which combines density plots, boxplots, and jittered raw data—was inspected to assess the presence of outliers in each group. The boxplots show that **no data points fall beyond the whiskers**, and the jittered points appear consistent with the distribution of the rest of the group. This visual assessment indicates that there are **no extreme or influential outliers** that could distort the group means or affect the ANOVA results. Therefore, the assumption of no significant outliers is satisfied.

#### Assumption #5: The dependent variable should be approximately normally distributed for each group of the independent variable.


\begin{longtable}[t]{cccc}
\caption{\label{tab:unnamed-chunk-3}Table 1: Descriptive Statistics and Normality Tests by Group (Transposed)}\\
\toprule
\multicolumn{4}{c}{Normality Assessment of Plant Weights} \\
\cmidrule(l{3pt}r{3pt}){1-4}
\multicolumn{1}{c}{ } & \multicolumn{3}{c}{Weight (g)} \\
\cmidrule(l{3pt}r{3pt}){2-4}
\cellcolor[HTML]{4B79A1}{\textcolor{white}{\textbf{Statistic}}} & \cellcolor[HTML]{4B79A1}{\textcolor{white}{\textbf{ctrl}}} & \cellcolor[HTML]{4B79A1}{\textcolor{white}{\textbf{trt1}}} & \cellcolor[HTML]{4B79A1}{\textcolor{white}{\textbf{trt2}}}\\
\midrule
\cellcolor[HTML]{f2f2f2}{Valid} & \cellcolor[HTML]{f2f2f2}{10} & \cellcolor[HTML]{f2f2f2}{10} & \cellcolor[HTML]{f2f2f2}{10}\\
\cellcolor{white}{Missing} & \cellcolor{white}{0} & \cellcolor{white}{0} & \cellcolor{white}{0}\\
\cellcolor[HTML]{f2f2f2}{Mean} & \cellcolor[HTML]{f2f2f2}{5.032} & \cellcolor[HTML]{f2f2f2}{4.661} & \cellcolor[HTML]{f2f2f2}{5.526}\\
\cellcolor{white}{SD} & \cellcolor{white}{0.583} & \cellcolor{white}{0.794} & \cellcolor{white}{0.443}\\
\cellcolor[HTML]{f2f2f2}{Skewness} & \cellcolor[HTML]{f2f2f2}{0.231} & \cellcolor[HTML]{f2f2f2}{0.474} & \cellcolor[HTML]{f2f2f2}{0.485}\\
\addlinespace
\cellcolor{white}{SE\_Skew} & \cellcolor{white}{0.775} & \cellcolor{white}{0.775} & \cellcolor{white}{0.775}\\
\cellcolor[HTML]{f2f2f2}{Kurtosis} & \cellcolor[HTML]{f2f2f2}{-1.117} & \cellcolor[HTML]{f2f2f2}{-1.105} & \cellcolor[HTML]{f2f2f2}{-1.160}\\
\cellcolor{white}{SE\_Kurt} & \cellcolor{white}{1.549} & \cellcolor{white}{1.549} & \cellcolor{white}{1.549}\\
\cellcolor[HTML]{f2f2f2}{Shapiro\_Wilk} & \cellcolor[HTML]{f2f2f2}{0.957} & \cellcolor[HTML]{f2f2f2}{0.930} & \cellcolor[HTML]{f2f2f2}{0.941}\\
\cellcolor{white}{Shapiro\_p} & \cellcolor{white}{0.747} & \cellcolor{white}{0.452} & \cellcolor{white}{0.564}\\
\bottomrule
\end{longtable}

**Remark.** The Shapiro–Wilk tests conducted separately for the ctrl, trt1, and trt2 groups all produced p-values above the conventional α = .05 threshold. This indicates that none of the three groups significantly deviate from a normal distribution. Furthermore, skewness and kurtosis values remain close to zero, suggesting that the shape of each distribution is reasonably symmetric and mesokurtic. Taken together, both statistical indicators and descriptive distribution characteristics support the conclusion that the assumption of normality has been satisfied. Therefore, it is appropriate to proceed with the one-way ANOVA.

#### Assumption #6: Homogeneity of variances (i.e., the variance of the dependent variable is equal in each group of your independent variable).

``` r
library(car)
```

```
## Loading required package: carData
```

```
## 
## Attaching package: 'car'
```

```
## The following object is masked from 'package:psych':
## 
##     logit
```

```
## The following object is masked from 'package:dplyr':
## 
##     recode
```

```
## The following object is masked from 'package:purrr':
## 
##     some
```

``` r
# Run Levene's Test
lev <- leveneTest(weight ~ group, data = df) %>%
  as.data.frame() %>%
  select(-Df) %>% 
  rename(
    statistic = `F value`,
    p = `Pr(>F)`
  )
```


\begin{longtable}[t]{lcc}
\caption{\label{tab:unnamed-chunk-5}Test for Equality of Variances (Levene's Test)}\\
\toprule
\cellcolor[HTML]{D9EAF7}{\textbf{}} & \cellcolor[HTML]{D9EAF7}{\textbf{Statistic (F)}} & \cellcolor[HTML]{D9EAF7}{\textbf{p-value}}\\
\midrule
\cellcolor[HTML]{F2F7FB}{group} & \cellcolor[HTML]{F2F7FB}{1.1192} & \cellcolor[HTML]{F2F7FB}{0.3412}\\
\cellcolor[HTML]{F2F7FB}{} & \cellcolor[HTML]{F2F7FB}{NA} & \cellcolor[HTML]{F2F7FB}{NA}\\
\bottomrule
\end{longtable}

**Remark.** The results of Levene’s test indicate that the variability of scores across the groups is statistically similar, as the obtained p-value was greater than .05. Because the test was not significant, we fail to reject the null hypothesis that the group variances are equal. This means the assumption of homogeneity of variances has been met, suggesting that differences in group means are not influenced by unequal spread or dispersion of the data. Therefore, it is appropriate to proceed with the standard One-Way ANOVA, as its validity and interpretability are maintained under this assumption.

#### Computation


\begin{longtable}[t]{ccccccc}
\caption{\label{tab:unnamed-chunk-6}One-Way ANOVA Summary for Plant Weight Across Treatment Groups}\\
\toprule
\cellcolor[HTML]{B5D5F5}{\textbf{Source}} & \cellcolor[HTML]{B5D5F5}{\textbf{Degrees of Freedom (df)}} & \cellcolor[HTML]{B5D5F5}{\textbf{Sum of Squares (SS)}} & \cellcolor[HTML]{B5D5F5}{\textbf{Mean Square (MS)}} & \cellcolor[HTML]{B5D5F5}{\textbf{F Statistic}} & \cellcolor[HTML]{B5D5F5}{\textbf{p-value}} & \cellcolor[HTML]{B5D5F5}{\textbf{Partial Eta²}}\\
\midrule
\cellcolor[HTML]{E9F2FB}{group} & \cellcolor[HTML]{E9F2FB}{2} & \cellcolor[HTML]{E9F2FB}{3.766} & \cellcolor[HTML]{E9F2FB}{1.883} & \cellcolor[HTML]{E9F2FB}{4.846} & \cellcolor[HTML]{E9F2FB}{0.0159} & \cellcolor[HTML]{E9F2FB}{0.264}\\
\cellcolor[HTML]{E9F2FB}{Residuals} & \cellcolor[HTML]{E9F2FB}{27} & \cellcolor[HTML]{E9F2FB}{10.492} & \cellcolor[HTML]{E9F2FB}{0.389} & \cellcolor[HTML]{E9F2FB}{NA} & \cellcolor[HTML]{E9F2FB}{NA} & \cellcolor[HTML]{E9F2FB}{NA}\\
\bottomrule
\end{longtable}


\begin{longtable}[t]{lccc}
\caption{\label{tab:unnamed-chunk-7}Descriptive Statistics for Plant Weight Across Treatment Groups}\\
\toprule
\cellcolor[HTML]{B5D5F5}{\textbf{Group}} & \cellcolor[HTML]{B5D5F5}{\textbf{n}} & \cellcolor[HTML]{B5D5F5}{\textbf{Mean}} & \cellcolor[HTML]{B5D5F5}{\textbf{SD}}\\
\midrule
\cellcolor[HTML]{E9F2FB}{ctrl} & \cellcolor[HTML]{E9F2FB}{10} & \cellcolor[HTML]{E9F2FB}{5.032} & \cellcolor[HTML]{E9F2FB}{0.583}\\
\cellcolor[HTML]{E9F2FB}{trt1} & \cellcolor[HTML]{E9F2FB}{10} & \cellcolor[HTML]{E9F2FB}{4.661} & \cellcolor[HTML]{E9F2FB}{0.794}\\
\cellcolor[HTML]{E9F2FB}{trt2} & \cellcolor[HTML]{E9F2FB}{10} & \cellcolor[HTML]{E9F2FB}{5.526} & \cellcolor[HTML]{E9F2FB}{0.443}\\
\bottomrule
\end{longtable}


\begin{longtable}[t]{lcccccc}
\caption{\label{tab:unnamed-chunk-8}Post-hoc Tukey HSD Pairwise Comparisons of Plant Weights}\\
\toprule
\multicolumn{1}{c}{ } & \multicolumn{3}{c}{Confidence Interval (95\%)} & \multicolumn{3}{c}{ } \\
\cmidrule(l{3pt}r{3pt}){2-4}
\cellcolor[HTML]{B5D5F5}{\textbf{Comparison}} & \cellcolor[HTML]{B5D5F5}{\textbf{Mean Diff}} & \cellcolor[HTML]{B5D5F5}{\textbf{Lower CI}} & \cellcolor[HTML]{B5D5F5}{\textbf{Upper CI}} & \cellcolor[HTML]{B5D5F5}{\textbf{SE}} & \cellcolor[HTML]{B5D5F5}{\textbf{t}} & \cellcolor[HTML]{B5D5F5}{\textbf{p-value}}\\
\midrule
\cellcolor[HTML]{E9F2FB}{ctrl vs trt1} & \cellcolor[HTML]{E9F2FB}{-0.371} & \cellcolor[HTML]{E9F2FB}{-1.062} & \cellcolor[HTML]{E9F2FB}{0.32} & \cellcolor[HTML]{E9F2FB}{0.279} & \cellcolor[HTML]{E9F2FB}{-1.331} & \cellcolor[HTML]{E9F2FB}{0.391}\\
\cellcolor[HTML]{E9F2FB}{ctrl vs trt2} & \cellcolor[HTML]{E9F2FB}{0.494} & \cellcolor[HTML]{E9F2FB}{-0.197} & \cellcolor[HTML]{E9F2FB}{1.185} & \cellcolor[HTML]{E9F2FB}{0.279} & \cellcolor[HTML]{E9F2FB}{1.772} & \cellcolor[HTML]{E9F2FB}{0.198}\\
\cellcolor[HTML]{E9F2FB}{trt1 vs trt2} & \cellcolor[HTML]{E9F2FB}{0.865} & \cellcolor[HTML]{E9F2FB}{0.174} & \cellcolor[HTML]{E9F2FB}{1.556} & \cellcolor[HTML]{E9F2FB}{0.279} & \cellcolor[HTML]{E9F2FB}{3.103} & \cellcolor[HTML]{E9F2FB}{0.012}\\
\bottomrule
\end{longtable}
\newpage
## Analysis/Reporting

A one-way analysis of variance (ANOVA) was conducted to determine whether dried plant weight differed across three treatment groups (*ctrl*, *trt1*, and *trt2*). The dependent variable was dried plant weight (grams), and the independent variable was treatment condition with three independent levels. Prior to analysis, assumptions were evaluated. Boxplots showed no extreme outliers, the Shapiro–Wilk tests indicated that plant weight was normally distributed within each group (all p > .05), and Levene’s test confirmed homogeneity of variances, p > .05.

Descriptive statistics suggested potential group differences, with **ctrl** (*M = 5.032, SD = 0.583*), **trt1** (*M = 4.661, SD = 0.794*), and **trt2** (*M = 5.526, SD = 0.443*). The ANOVA revealed a statistically significant effect of treatment on plant weight, F(2, 27) = 4.846, p = .016, partial η² = .264, indicating that approximately 26.4% of the variance in plant weight was explained by treatment condition.

Tukey HSD post hoc comparisons showed that trt2 plants weighed significantly more than trt1 plants (mean difference = 0.864 g, 95% CI [0.144, 1.584], p = .016). However, the differences between ctrl vs. trt1 (mean difference = 0.371 g, 95% CI [-0.348, 1.090], p = .436) and ctrl vs. trt2 (mean difference = –0.494 g, 95% CI [-1.214, 0.226], p = .218) were not statistically significant.

Overall, these findings indicate that treatment condition influences plant growth, with the trt2 treatment resulting in significantly heavier plants than trt1, while the control group did not significantly differ from either treatment condition.
