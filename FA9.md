# Formative Assessment 8
### Authors:
SINOCRUZ, ARVIE
TAGAYTAY, GABRIEL
### Date: "2025-11-26"

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
data("PlantGrowth")
df <- PlantGrowth
head(df)
library(tidyverse)
library(rstatix)
library(knitr)
library(dplyr)
library(kableExtra)
```

**Github link:** *https://github.com/eivra-sm/APM1111/blob/main/FA8.md*

## Assumptions

### Assumption #1: You have one dependent variable that is measured at the continuous level.
```{r}
str(df$weight)
summary(df$weight)
```

**Remark.** The dependent variable in this study is weight, which represents the dried weight of individual plants measured in grams. Since weight is measured using a continuous numeric scale with meaningful intervals (e.g., 4.5 g, 5.2 g, 6.0 g), it qualifies as a continuous, quantitative variable. This meets the first requirement of one-way ANOVA, which assumes that the dependent variable is measured at the interval or ratio level.

### Assumption #2: You have one independent variable that consists of three or more categorical, independent groups.

**Remarks:** The independent variable in this study is **group**, which is a categorical factor with **three distinct and non-overlapping levels:** _ctrl_, _trt1_, and _trt2_. Each level represents a separate treatment condition applied to different sets of plants. Because the levels are mutually exclusive and clearly defined, this assumption is fully satisfied. The structure of the grouping variable is appropriate for conducting a one-way ANOVA, which requires at least three independent groups for comparison.  

### Assumption #3: You should have independence of observations.

**Remarks:** The observations in this dataset meet the requirement of independence. Each plant was grown, measured, and assigned to a treatment condition separately, with **each individual plant** contributing only one weight measurement. There is no repeated measurement, pairing, or clustering within the dataset. Since no plant appears in more than one group and there is no interaction among measurement units that would affect their outcomes, the independence assumption crucial for the validity of ANOVA is considered to be met.  

### Assumption #4: There should be no significant outliers in the three groups of your independent variable in terms of the dependent variable.  

```{r libraries, message=FALSE, warning=FALSE}
library(ggplot2)
library(ggdist)
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
ggplot(df, aes(x = group, y = weight, fill = group)) +
  ggdist::stat_halfeye(
    adjust = .7, width = .6, .width = 0,
    justification = -0.3, point_colour = NA
  ) +
  geom_boxplot(width = .25, outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = .05, alpha = 0.8) +
  scale_fill_manual(values = c(
    "ctrl" = "#A6CEE3",
    "trt1" = "#B2DF8A",
    "trt2" = "#FDBF6F"
  )) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Raincloud Plot of Plant Weights by Group",
    x = "Group",
    y = "Plant Weight (g)"
  )
```  

**Remarks:** The raincloud plot—which combines density plots, boxplots, and jittered raw data—was inspected to assess the presence of outliers in each group. The boxplots show that **no data points fall beyond the whiskers**, and the jittered points appear consistent with the distribution of the rest of the group. This visual assessment indicates that there are **no extreme or influential outliers** that could distort the group means or affect the ANOVA results. Therefore, the assumption of no significant outliers is satisfied.

\newpage

#### Assumption #5: The dependent variable should be approximately normally distributed for each group of the independent variable.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(psych)
library(tidyverse)
library(rstatix)
library(kableExtra)

# Descriptives
desc <- df %>%
  group_by(group) %>%
  summarise(
    Valid = as.integer(n()),
    Missing = as.integer(sum(is.na(weight))),
    Mean = mean(weight),
    SD = sd(weight),
    .groups = "drop"
  )

# Skewness / kurtosis
shape_stats <- df %>%
  group_by(group) %>%
  summarise(
    Skewness = psych::skew(weight)[1],
    SE_Skew = sqrt(6 / n()),
    Kurtosis = psych::kurtosi(weight)[1],
    SE_Kurt = sqrt(24 / n()),
    .groups = "drop"
  )

# Shapiro-Wilk
shap <- df %>%
  group_by(group) %>%
  shapiro_test(weight) %>%
  rename(
    Shapiro_Wilk = statistic,
    Shapiro_p = p
  ) %>%
  select(group, Shapiro_Wilk, Shapiro_p)

# Combine all
final <- desc %>%
  left_join(shape_stats, by = "group") %>%
  left_join(shap, by = "group")

# Transpose
final_t <- final %>%
  pivot_longer(-group, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = group, values_from = Value)

# Format numbers
final_t <- final_t %>%
  mutate(across(-Statistic, ~ if_else(Statistic %in% c("Valid", "Missing"), 
                                      as.character(as.integer(.)), 
                                      sprintf("%.3f", .))))

#table with header
final_t %>%
  kable(
    caption = "Table 1: Descriptive Statistics and Normality Tests by Group (Transposed)",
    align = "c",
    col.names = c("Statistic", names(final_t)[-1])  # rename first column
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#4B79A1", color = "white") %>%  
  row_spec(seq(1, nrow(final_t), 2), background = "#f2f2f2") %>%        
  row_spec(seq(2, nrow(final_t), 2), background = "white") %>%          
  add_header_above(c(" " = 1, "Weight (g)" = ncol(final_t) - 1)) %>%
  add_header_above(c("Normality Assessment of Plant Weights" = ncol(final_t)))
```

**Remark.** The Shapiro–Wilk tests conducted separately for the ctrl, trt1, and trt2 groups all produced p-values above the conventional α = .05 threshold. This indicates that none of the three groups significantly deviate from a normal distribution. Furthermore, skewness and kurtosis values remain close to zero, suggesting that the shape of each distribution is reasonably symmetric and mesokurtic. Taken together, both statistical indicators and descriptive distribution characteristics support the conclusion that the assumption of normality has been satisfied. Therefore, it is appropriate to proceed with the one-way ANOVA.

#### Assumption #6: Homogeneity of variances (i.e., the variance of the dependent variable is equal in each group of your independent variable).
```{r}
library(car)

# Run Levene's Test
lev <- leveneTest(weight ~ group, data = df) %>%
  as.data.frame() %>%
  select(-Df) %>% 
  rename(
    statistic = `F value`,
    p = `Pr(>F)`
  )
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
lev %>%
  rename(
    `Statistic (F)` = statistic,
    `p-value` = p
  ) %>%
  kable(
    caption = "Test for Equality of Variances (Levene's Test)",
    digits = 4,
    align = "c",
    booktabs = TRUE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  row_spec(0, background = "#D9EAF7", bold = TRUE) %>%    
  row_spec(1:nrow(lev), background = "#F2F7FB")            
```

**Remark.** The results of Levene’s test indicate that the variability of scores across the groups is statistically similar, as the obtained p-value was greater than .05. Because the test was not significant, we fail to reject the null hypothesis that the group variances are equal. This means the assumption of homogeneity of variances has been met, suggesting that differences in group means are not influenced by unequal spread or dispersion of the data. Therefore, it is appropriate to proceed with the standard One-Way ANOVA, as its validity and interpretability are maintained under this assumption.

#### Computation

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(tidyverse)
library(rstatix)
library(kableExtra)

anova_res <- aov(weight ~ group, data = df)

anova_table <- anova(anova_res) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Source") %>%
  rename(
    `Degrees of Freedom (df)` = "Df",
    `Sum of Squares (SS)` = "Sum Sq",
    `Mean Square (MS)` = "Mean Sq",
    `F Statistic` = "F value",
    `p-value` = "Pr(>F)"
  )

anova_table <- anova_table %>%
  mutate(
    `Partial Eta²` = case_when(
      Source != "Residuals" ~ `Sum of Squares (SS)` / sum(`Sum of Squares (SS)`),
      TRUE ~ NA_real_
    ),
    `Sum of Squares (SS)` = round(`Sum of Squares (SS)`, 3),
    `Mean Square (MS)` = round(`Mean Square (MS)`, 3),
    `F Statistic` = round(`F Statistic`, 3),
    `p-value` = ifelse(!is.na(`p-value`), signif(`p-value`, 3), NA),
    `Partial Eta²` = ifelse(!is.na(`Partial Eta²`), round(`Partial Eta²`, 3), NA)
  )

anova_table <- anova_table %>%
  rename(
    `Partial Eta2` = `Partial Eta²`
  )

colnames(anova_table) <- iconv(colnames(anova_table), to = "ASCII//TRANSLIT")

anova_table %>%
  kable(
    caption = "One-Way ANOVA Summary for Plant Weight Across Treatment Groups",
    align = "c",
    booktabs = TRUE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 8,
    latex_options = "scale_down"
  ) %>%
  row_spec(0, background = "#B5D5F5", bold = TRUE) %>%  
  row_spec(1:nrow(anova_table), background = "#E9F2FB")
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(tidyverse)
library(kableExtra)

desc_weight <- df %>%
  group_by(group) %>%
  summarise(
    `Sample Size (n)` = n(),
    `Mean Weight` = round(mean(weight), 3),
    `Standard Deviation` = round(sd(weight), 3),
    .groups = "drop"
  )

desc_weight %>%
  kable(
    caption = "Descriptive Statistics for Plant Weight Across Treatment Groups",
    align = c("l", "c", "c", "c"),
    booktabs = TRUE,
    col.names = c("Group", "n", "Mean", "SD")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, background = "#B5D5F5", bold = TRUE) %>% 
  row_spec(1:nrow(desc_weight), background = "#E9F2FB")  
```

\newpage

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(tidyverse)
library(rstatix)
library(kableExtra)

df <- df %>% mutate(group = as.factor(group))
anova_res <- aov(weight ~ group, data = df)
tukey <- tukey_hsd(anova_res, "group")
resid_ms <- summary(anova_res)[[1]]$`Mean Sq`[2]

posthoc_table <- tukey %>%
  rowwise() %>%
  mutate(
    SE = sqrt(resid_ms * (1/(df %>% filter(group == group1) %>% nrow()) +
                          1/(df %>% filter(group == group2) %>% nrow()))),
    t = estimate / SE
  ) %>%
  ungroup() %>%
  select(group1, group2, estimate, conf.low, conf.high, SE, t, p.adj) %>%
  rename(
    `Group 1` = group1,
    `Group 2` = group2,
    `Mean Difference` = estimate,
    `Lower CI` = conf.low,
    `Upper CI` = conf.high,
    `p-value (Tukey)` = p.adj
  ) %>%
  mutate(
    `Mean Difference` = round(`Mean Difference`, 3),
    `Lower CI` = round(`Lower CI`, 3),
    `Upper CI` = round(`Upper CI`, 3),
    SE = round(SE, 3),
    t = round(t, 3),
    `p-value (Tukey)` = signif(`p-value (Tukey)`, 3),
    Comparison = paste(`Group 1`, "vs", `Group 2`)
  ) %>%
  select(Comparison, `Mean Difference`, `Lower CI`, `Upper CI`, SE, t, `p-value (Tukey)`)

posthoc_table <- posthoc_table %>% mutate(across(everything(), as.character))

col_headers <- c("Comparison", "Mean Diff", "Lower CI", "Upper CI", "SE", "t", "p-value")

posthoc_table %>%
  kable(
    caption = "Post-hoc Tukey HSD Pairwise Comparisons of Plant Weights",
    col.names = col_headers,
    align = c("l", "c", "c", "c", "c", "c", "c"),
    booktabs = TRUE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  add_header_above(c(" " = 1, "Confidence Interval (95%)" = 3, " " = 3)) %>%
  row_spec(0, background = "#B5D5F5", bold = TRUE) %>%       
  row_spec(1:nrow(posthoc_table), background = "#E9F2FB")    
```
## Analysis/Reporting

A one-way analysis of variance (ANOVA) was conducted to determine whether dried plant weight differed across three treatment groups (*ctrl*, *trt1*, and *trt2*). The dependent variable was dried plant weight (grams), and the independent variable was treatment condition with three independent levels. Prior to analysis, assumptions were evaluated. Boxplots showed no extreme outliers, the Shapiro–Wilk tests indicated that plant weight was normally distributed within each group (all p > .05), and Levene’s test confirmed homogeneity of variances, p > .05.

Descriptive statistics suggested potential group differences, with **ctrl** (*M = 5.032, SD = 0.583*), **trt1** (*M = 4.661, SD = 0.794*), and **trt2** (*M = 5.526, SD = 0.443*). The ANOVA revealed a statistically significant effect of treatment on plant weight, F(2, 27) = 4.846, p = .016, partial η² = .264, indicating that approximately 26.4% of the variance in plant weight was explained by treatment condition.

Tukey HSD post hoc comparisons showed that trt2 plants weighed significantly more than trt1 plants (mean difference = 0.864 g, 95% CI [0.144, 1.584], p = .016). However, the differences between ctrl vs. trt1 (mean difference = 0.371 g, 95% CI [-0.348, 1.090], p = .436) and ctrl vs. trt2 (mean difference = –0.494 g, 95% CI [-1.214, 0.226], p = .218) were not statistically significant.

Overall, these findings indicate that treatment condition influences plant growth, with the trt2 treatment resulting in significantly heavier plants than trt1, while the control group did not significantly differ from either treatment condition.
