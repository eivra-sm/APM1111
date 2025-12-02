---
title: "Formative Assessment 9"
author: "Sinocruz, A & Tagaytay, G"
date: "2025-12-02"
output:
  pdf_document:
    latex_engine: lualatex
---

Github Link: 

## Introduction 

A study was conducted to determine whether crop yield is influenced by (1) fertilizer blend (X, Y, Z) and (2) crop type (wheat, corn, soy, rice). A two-way ANOVA was used to test for:

-   A main effect of fertilizer blend,

-   A main effect of crop type,

-   An interaction between fertilizer blend × crop type

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE
)

library(car)


library(tidyverse)
library(rstatix)
library(kableExtra)
library(psych)

crop_yield <- tribble(
  ~Fertilizer, ~Wheat, ~Corn, ~Soy, ~Rice,
  "Blend X", 123, 128, 166, 151,
  "Blend X", 156, 150, 178, 125,
  "Blend X", 112, 174, 187, 117,
  "Blend X", 100, 116, 153, 155,
  "Blend X", 168, 109, 195, 158,
  "Blend Y", 135, 175, 140, 167,
  "Blend Y", 130, 132, 145, 183,
  "Blend Y", 176, 120, 159, 142,
  "Blend Y", 120, 187, 131, 167,
  "Blend Y", 155, 184, 126, 168,
  "Blend Z", 156, 186, 185, 175,
  "Blend Z", 180, 138, 206, 173,
  "Blend Z", 147, 178, 188, 154,
  "Blend Z", 146, 176, 165, 191,
  "Blend Z", 193, 190, 188, 169
)

df <- crop_yield %>% 
  pivot_longer(cols = Wheat: Rice, names_to = "Crop", values_to = "Yield")

df$Fertilizer <- as.factor(df$Fertilizer)
df$Crop <- as.factor(df$Crop)


```

## **Assumptions**

Initiating a two-way ANOVA requires more than simply applying a formula to a dataset; it necessitates a rigorous preliminary evaluation to confirm the data’s suitability for this specific statistical model. This validation process is critical because the two-way ANOVA is a parametric test reliant on specific mathematical properties. Consequently, researchers must verify that their data adheres to six fundamental assumptions. Failure to meet these prerequisites can compromise the statistical power of the analysis, potentially leading to biased estimates or erroneous conclusions. Therefore, before interpreting any main effects or interactions, one must systematically demonstrate that the data "passes" these mandatory checks to ensure the resulting output is robust and scientifically valid.

Below are the assumptions:

**Assumption #1:** Your **dependent variable** must be **continuous** (measured on an interval or ratio scale, such as time, weight, or test scores).

**Assumption #2:** Your design must include exactly **two independent variables** that are **categorical**, with each variable containing **two or more independent groups** (levels).

**Assumption #3:** You must ensure **independence of observations**, meaning that no participant is in more than one group and one participant's data does not influence another's.

**Assumption #4:** The dataset must contain **no significant outliers**, as these extreme values can distort the mean and variance of your results.

**Assumption #5:** Your **dependent variable** must follow an **approximate normal distribution** (bell curve) for **each specific combination of the groups** formed by the two independent variables.

**Assumption #6:** There must be **homogeneity of variances** (homoscedasticity), meaning the spread or variability of the data is roughly equal across **every combination of the groups**.

## Null hypothesis:

There is no significant interaction effect on yield between fertilizer and crop.

## Problem:

A new fertilizer has been developed to increase the yield on crops, and the makers of the fertilizer want to better understand which of the three formulations (blends) of this fertilizer are most effective for wheat, corn, soybeans and rice (crops). They test each of the three blends on 5 samples of each of the four types of crops. The crop yields for the 12 combinations are as shown in the table below.

```{r}
df %>%
  kable(
    caption = "Crop Yield Data (Long Format)",
    digits = 0
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(
    seq(1, nrow(df), 2), background = "#DFF2DF"
  ) %>%
  row_spec(
    seq(2, nrow(df), 2), background = "#FFFFFF"
  )
```

## Checking of Assumptions

**Assumption #1: The dependent variable, crop yield, is measured at the continuous level.**

**Remark.** The first assumption of a two-way ANOVA requires that the dependent variable be measured on a continuous (interval or ratio) scale.
In this study, the dependent variable is crop yield, recorded as the number of units produced by each crop under each fertilizer blend. Because yield is measured numerically and represents a meaningful quantity where differences between values are interpretable (e.g., a difference of 10 units represents the same increase anywhere on the scale), the variable meets the criteria for a ratio-level, continuous measure. This satisfies the requirement that the ANOVA operates on a continuous outcome variable.

**Assumption #2: The two independent variables, fertilizer type (Blend X, Blend Y, Blend Z) and crop type (Wheat, Corn, Soy, Rice), each consist of two or more categorical, independent groups.**

**Remark.** A two-way ANOVA requires two independent variables (factors), each containing distinct, non-overlapping categories.

\begin{itemize}
    \item The first factor is \textbf{Fertilizer Type}, which has three categorical groups:
    \begin{itemize}
        \item Blend X
        \item Blend Y
        \item Blend Z
    \end{itemize}
    
    \item The second factor is \textbf{Crop Type}, which consists of four categorical groups:
    \begin{itemize}
        \item Wheat
        \item Corn
        \item Soy
        \item Rice
    \end{itemize}
\end{itemize}

Both variables meet the criteria for categorical grouping factors, and each observation belongs to one and only one level of each factor. There is no overlap between categories, and the groups are mutually exclusive. Therefore, this assumption is \textbf{fully met}.

**Assumption #3: The independence of observation is observed.**

**Remark.** In this study, each yield value represents a distinct crop sample treated with a specific fertilizer blend. The experimental design states that five independent samples were used for every combination of fertilizer and crop type. No sample appears in more than one condition, and fertilizer treatments were applied separately. Thus, the independence of observations is maintained by design.

**Assumption #4: There are no significant outliers in each of the 12 cells of the design.**

```{r, echo=FALSE, message=FALSE, warning=FALSE}

library(ggplot2)
library(ggdist)

# Your data
df <- tribble(
  ~Fertilizer, ~Wheat, ~Corn, ~Soy, ~Rice,
  "Blend X", 123, 128, 166, 151,
  "Blend X", 156, 150, 178, 125,
  "Blend X", 112, 174, 187, 117,
  "Blend X", 100, 116, 153, 155,
  "Blend X", 168, 109, 195, 158,
  "Blend Y", 135, 175, 140, 167,
  "Blend Y", 130, 132, 145, 183,
  "Blend Y", 176, 120, 159, 142,
  "Blend Y", 120, 187, 131, 167,
  "Blend Y", 155, 184, 126, 168,
  "Blend Z", 156, 186, 185, 175,
  "Blend Z", 180, 138, 206, 173,
  "Blend Z", 147, 178, 188, 154,
  "Blend Z", 146, 176, 165, 191,
  "Blend Z", 193, 190, 188, 169
)

df_long <- df %>%
  pivot_longer(cols = Wheat: Rice, names_to = "Crop", values_to = "Yield")

df_long$Fertilizer <- as.factor(df_long$Fertilizer)
df_long$Crop <- as.factor(df_long$Crop)

ggplot(df_long, aes(x = Fertilizer, y = Yield, fill = Crop, color = Crop)) +
  ggdist::stat_halfeye(
    adjust = 0.7, width = 0.6, .width = 0,
    justification = 0.3,
    point_colour = NA,
    position = position_dodge(width = 0.8)
  ) +
  geom_boxplot(
    width = 0.25, outlier.shape = NA, alpha = 0.5,
    position = position_dodge(width = 0.8)
  ) +
  geom_jitter(
    alpha = 0.8,
    position = position_dodge(width = 0.8)
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Raincloud Plot of Crop Yield by Fertilizer",
    x = "Fertilizer",
    y = "Yield"
  ) +
  scale_fill_manual(
    values = c(
      "Wheat" = "#1f77b4",  
      "Corn"  = "#ffdd00",  
      "Soy"   = "#2ca02c",  
      "Rice"  = "#ff69b4"   
    )
  ) +
  scale_color_manual(
    values = c(
      "Wheat" = "#1f77b4",
      "Corn"  = "#ffdd00",
      "Soy"   = "#2ca02c",
      "Rice"  = "#ff69b4"
    )
  ) +
  theme(legend.position = "right")

```

**Remakr.** \noindent \textbf{Assumption Check: No Significant Outliers}

A two-way ANOVA assumes that there are no extreme outliers in any of the factorial combinations (i.e., in each of the 12 cells created by 3 fertilizer levels $\times$ 4 crop types).

Outliers were inspected using:
\begin{itemize}
    \item Raincloud plots (combining jittered points, boxplots, and density half-eyes),
    \item Boxplot whisker rules, and
    \item Visual examination of the distribution of values within each group.
\end{itemize}

These diagnostic visualizations revealed no severe deviations from expected values and no yield measurements that fell far outside the typical range for their group. Specifically, all observations fell within reasonable boundaries relative to their corresponding interquartile range and whisker limits. Therefore, the assumption of ``no significant outliers'' is \textbf{supported}.

**Assumption #5: The dependent variable, crop yield, is approximately normally distributed for each combination of the groups of fertilizer type and crop type, as assessed by Shapiro–Wilk test of normality (p \> .05).**

```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(tidyverse)
library(psych)
library(rstatix)
library(kableExtra)

# Your dataset
crop_yield <- tribble(
  ~Fertilizer, ~Wheat, ~Corn, ~Soy, ~Rice,
  "Blend X", 123, 128, 166, 151,
  "Blend X", 156, 150, 178, 125,
  "Blend X", 112, 174, 187, 117,
  "Blend X", 100, 116, 153, 155,
  "Blend X", 168, 109, 195, 158,
  "Blend Y", 135, 175, 140, 167,
  "Blend Y", 130, 132, 145, 183,
  "Blend Y", 176, 120, 159, 142,
  "Blend Y", 120, 187, 131, 167,
  "Blend Y", 155, 184, 126, 168,
  "Blend Z", 156, 186, 185, 175,
  "Blend Z", 180, 138, 206, 173,
  "Blend Z", 147, 178, 188, 154,
  "Blend Z", 146, 176, 165, 191,
  "Blend Z", 193, 190, 188, 169
)

df <- crop_yield %>% 
  pivot_longer(cols = Wheat: Rice, names_to = "Crop", values_to = "Yield") %>%
  mutate(Fertilizer = as.factor(Fertilizer),
         Crop = as.factor(Crop))

compute_desc <- function(data, group_var) {
  data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      Valid = n(),
      Mean = mean(Yield),
      SD = sd(Yield),
      Skewness = psych::skew(Yield)[1],
      SE_Skew = sqrt(6/n()),
      Kurtosis = psych::kurtosi(Yield)[1],
      SE_Kurt = sqrt(24/n()),
      Shapiro_Wilk = shapiro_test(Yield)$statistic,
      P_value = shapiro_test(Yield)$p,
      .groups = "drop"
    ) %>%
    pivot_longer(-1, names_to = "Statistic", values_to = "Value") %>%
    pivot_wider(names_from = !!sym(group_var), values_from = Value) %>%
    mutate(across(-Statistic, ~ round(as.numeric(.), 3)))
}

desc_by_fertilizer %>%
  kable(
    caption = "Descriptive Statistics of Yield by Fertilizer",
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%  # Header
  row_spec(seq(1, nrow(desc_by_fertilizer), 2), background = "#DFF2DF") %>% 
  row_spec(seq(2, nrow(desc_by_fertilizer), 2), background = "#FFFFFF")    


desc_by_crop %>%
  kable(
    caption = "Descriptive Statistics of Yield by Crop",
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%  
  row_spec(seq(1, nrow(desc_by_crop), 2), background = "#DFF2DF") %>%  
  row_spec(seq(2, nrow(desc_by_crop), 2), background = "#FFFFFF")
```

**Assumption #6: The variances for each combination of the groups of fertilizer type and crop type are homogeneous, as assessed by Levene’s test of equality of variances, p = 0.755.**

```{r, echo=FALSE, message=FALSE, warning=FALSE}

levene_test_result <- df %>%
  levene_test(Yield ~ Fertilizer * Crop)

levene_test_result %>%
  rename(
    F = statistic,
    df1 = df1,
    df2 = df2,
    p = p
  ) %>%
  kable(
    caption = "Test for Equality of Variances (Levene's Test)",
    digits = 3,
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

```

## Computation
```{r anova_table, results='asis'}
library(car)
library(effectsize)
library(kableExtra)
library(tidyverse)

# Type III ANOVA
anova_res <- Anova(lm(Yield ~ Fertilizer * Crop, data = df), type = 3)

# Convert ANOVA table to data frame
anova_df <- as.data.frame(anova_res) %>%
  rownames_to_column(var = "Cases")

# Rename columns
anova_table <- anova_df %>%
  rename(
    `Sum of Squares` = `Sum Sq`,
    df = `Df`,
    F = `F value`,
    p = `Pr(>F)`
  ) %>%
  mutate(
    `Mean Square` = `Sum of Squares` / df
  )

# Partial Eta Squared
SS_error <- anova_table$`Sum of Squares`[anova_table$Cases == "Residuals"]
anova_table$`Partial Eta Squared` <- anova_table$`Sum of Squares` /
  (anova_table$`Sum of Squares` + SS_error)

# Clean Cases names
anova_table$Cases <- gsub("\\(|\\)", "", anova_table$Cases)

# Arrange columns
anova_table <- anova_table %>%
  select(Cases, `Sum of Squares`, df, `Mean Square`, F, p, `Partial Eta Squared`)

# Print LaTeX table
kable(
  anova_table,
  caption = "Two-way ANOVA Table (JASP-style, Type III SS)",
  digits = 3,
  format = "latex",
  booktabs = TRUE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE
  ) %>%
  row_spec(
    row = seq(1, nrow(anova_table), 2),  
    background = "green!20"
  ) %>%
  row_spec(
    row = seq(2, nrow(anova_table), 2),
    background = "green!10"
  )
```

#### Remark:
A two-way analysis of variance (ANOVA) was conducted to evaluate the effects of Fertilizer Blend and Crop Type on crop yield. The analysis revealed that the model is statistically valid, identifying three distinct sources of variance. First, there was a significant main effect for Fertilizer Blend, indicating that, on average, yield varies depending on the blend applied. Second, a significant main effect was observed for Crop Type, suggesting that baseline yields differ significantly between the crops (Wheat, Corn, Soy, Rice).

Most importantly, however, the analysis yielded a statistically significant Interaction Effect (Fertilizer Blend × Crop Type). The presence of this interaction provides critical insight into the data: it demonstrates that the efficacy of a specific Fertilizer Blend is conditional upon the Crop Type to which it is applied. In other words, the "best" fertilizer is not universally superior; a blend that maximizes yield for Corn may perform poorly for Wheat or Soy.

Consequently, relying solely on the generalized main effects would be potentially misleading, as doing so would average out these nuances and mask the specific dynamics between the variables. Because the effect of the fertilizer depends on the crop, the main effects cannot be interpreted in isolation. To fully understand the nature of this relationship, a follow-up analysis of Simple Main Effects is warranted. This procedure will allow for a focused examination of differences between fertilizer blends within each specific crop category, providing a precise roadmap of where the significant differences lie.

## Results of Simple Main Effects

```{r}
library(rstatix)
library(dplyr)
library(kableExtra)

overall_aov <- aov(Yield ~ Fertilizer * Crop, data = df_long)
MS_Error <- summary(overall_aov)[[1]][3, "Mean Sq"]

simple_effects_fert <- df_long %>%
  group_by(Crop) %>%
  anova_test(Yield ~ Fertilizer) %>%
  get_anova_table() %>%
  as_tibble() %>%
  select(Crop, Effect, DFn, DFd, F, p) %>%
  rename(`Level of Crop` = Crop,
         `Source` = Effect,
         df = DFn) %>%
  mutate(
    `Mean Square` = F * MS_Error,
    `Sum of Squares` = `Mean Square` * df
  ) %>%
  select(`Level of Crop`, `Sum of Squares`, df, `Mean Square`, F, p)

simple_effects_fert %>%
  kable(
    caption = "Simple Main Effects of Fertilizer within Each Crop",
    digits = 3,
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) %>%
  row_spec(
    row = seq(1, nrow(simple_effects_fert), 2),   # odd rows
    background = "#d4f5d4"
  ) %>%
  row_spec(
    row = seq(2, nrow(simple_effects_fert), 2),   # even rows
    background = "#eafbe9"
  )

sme_means2 <- df %>%
group_by(Crop, Fertilizer) %>%
summarise(
Mean_Yield = mean(Yield),
SD = sd(Yield),
N = n(),
SE = SD/sqrt(N),
.groups = "drop"
)

ggplot(sme_means2, aes(x = Crop, y = Mean_Yield, group = Fertilizer, color = Fertilizer)) +
geom_line(size = 1) +
geom_point(size = 3) +
geom_errorbar(aes(ymin = Mean_Yield - SE, ymax = Mean_Yield + SE), width = 0.1) +
theme_minimal(base_size = 14) +
labs(
title = "Simple Main Effects of Crop within each Fertilizer",
x = "Crop",
y = "Mean Yield"
) +
scale_color_brewer(palette = "Set2") +
theme(legend.position = "right")

```

#### Remark:
The analysis of simple main effects for crop type within each fertilizer blend demonstrates that crop yield varies substantially depending on the specific fertilizer applied. Certain fertilizer formulations appear to favor particular crops more strongly, resulting in higher yields for those crops, while other crops show more modest responses under the same fertilizer. This pattern indicates that the effectiveness of a fertilizer is not uniform across all crop types, but rather crop-specific, reflecting the unique nutrient requirements and growth characteristics of each crop.

These results further reinforce the conclusion that there is a meaningful interaction between fertilizer formulation and crop species. The significant interaction implies that the optimal fertilizer choice depends on the crop in question, and that blanket recommendations for all crops may not maximize productivity. From a practical standpoint, these findings suggest the necessity of tailored fertilizer strategies that account for both crop type and fertilizer composition, thereby enabling more efficient nutrient management and improved overall yield.

## Partial Eta Squared

```{r}
eta_sq_res <- eta_squared(
  lm(Yield ~ Fertilizer * Crop, data = df),
  partial = TRUE
)

eta_sq_res %>%
  rename(Effect = Parameter,
         Partial_Eta_Sq = Eta2_partial) %>%
  kable(caption = "Partial Eta Squared for Two-way ANOVA",
        digits = 3) %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE)

```

## Post Hoc Comparisons

```{r posthoc, results='asis'}
library(emmeans)
library(kableExtra)
library(dplyr)

# Fit two-way ANOVA model
model <- lm(Yield ~ Fertilizer * Crop, data = df_long)

# --- Estimated Marginal Means ---
emm_fert <- emmeans(model, ~ Fertilizer)
emm_crop <- emmeans(model, ~ Crop)
emm_interaction <- emmeans(model, ~ Fertilizer * Crop)

# --- Pairwise Comparisons ---

# 1. Fertilizer main effect
pairwise_fert <- contrast(emm_fert, method = "pairwise", adjust = "tukey") %>% summary(infer = TRUE)
pairwise_fert_df <- as.data.frame(pairwise_fert)

# Print Fertilizer post hoc table
pairwise_fert_df %>%
  kable(
    caption = "Post Hoc Pairwise Comparisons of Fertilizer (Tukey)",
    digits = 3,
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)


# 2. Crop main effect
pairwise_crop <- contrast(emm_crop, method = "pairwise", adjust = "tukey") %>% summary(infer = TRUE)
pairwise_crop_df <- as.data.frame(pairwise_crop)

# Print Crop post hoc table
pairwise_crop_df %>%
  kable(
    caption = "Post Hoc Pairwise Comparisons of Crop (Tukey)",
    digits = 3,
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)


# 3. Interaction: Fertilizer pairwise within each Crop
pairwise_interaction <- contrast(emm_interaction, method = "pairwise", by = "Crop", adjust = "tukey") %>% summary(infer = TRUE)
pairwise_interaction_df <- as.data.frame(pairwise_interaction)

# Print Interaction post hoc table
pairwise_interaction_df %>%
  kable(
    caption = "Post Hoc Pairwise Comparisons of Fertilizer within Each Crop (Tukey)",
    digits = 3,
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)
```

\newpage

## Results:
Post hoc Tukey comparisons offer a more detailed understanding of the specific differences between fertilizer blends and crop types. These pairwise tests identify precisely which groups contribute to the significant main effects observed in the ANOVA. When considered alongside the significant interaction effects, the post hoc results reveal a complex pattern of variation across fertilizer–crop combinations. This underscores the importance of evaluating both factors simultaneously, rather than in isolation, when formulating fertilizer recommendations. By accounting for the unique responses of each crop to different fertilizers, these findings provide practical guidance for optimizing crop yields and improving nutrient management strategies.
