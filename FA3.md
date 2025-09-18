
Formative Assessment 3
---

Authors: 

SINOCRUZ, Arvie 

TAGAYTAY, Gabriel

date: 2025-09-19
---

## Necessary packages
```{r}
library(moments)
library(knitr)
library(kableExtra)
```

## Data
```{r}
scores <- c(
  88, 45, 53, 86, 33, 86, 85, 30, 89, 53, 41, 96, 56, 38, 62,
  71, 51, 86, 68, 29, 28, 47, 33, 37, 25, 36, 33, 94, 73, 46,
  42, 34, 79, 72, 88, 99, 82, 62, 57, 42, 28, 55, 67, 62, 60,
  96, 61, 57, 75, 93, 34, 75, 53, 32, 28, 73, 51, 69, 91, 35)
```
  
## Descriptive statistics
```{r}
n <- length(scores)
mode_val <- as.numeric(names(sort(-table(scores)))[1])
median_val <- median(scores)
mean_val <- mean(scores)
std_val <- sd(scores)
var_val <- var(scores)
skew_val <- skewness(scores)
ses <- sqrt((6*n*(n-1)) / ((n-2)*(n+1)*(n+3)))
kurt_val <- kurtosis(scores)   
sek <- 2 * ses * sqrt((n^2 - 1) / ((n-3)*(n+5)))
```

## Quartiles/Percentiles
```{r}
q1 <- quantile(scores, 0.25)
q2 <- quantile(scores, 0.50)
q3 <- quantile(scores, 0.75)
d9 <- quantile(scores, 0.90)
p95 <- quantile(scores, 0.95)
```

# Building the table
```{r}
desc_stats <- data.frame(
  Statistic = c(
    "Valid",
    "Mode",
    "Median",
    "Mean",
    "Std. Deviation",
    "Variance",
    "Skewness",
    "Std. Error of Skewness",
    "Kurtosis",
    "Std. Error of Kurtosis",
    "Minimum",
    "Maximum",
    "25th percentile",
    "50th percentile",
    "75th percentile",
    "90th percentile",
    "95th percentile"
  ),
  Score = c(
    n,
    mode_val,
    median_val,
    mean_val,
    std_val,
    var_val,
    skew_val,
    ses,
    kurt_val,
    sek,
    min(scores),
    max(scores),
    q1,
    q2,
    q3,
    d9,
    p95
  )
)
```

## Format numbers to 3 decimal places
```{r}
desc_stats$Score <- formatC(desc_stats$Score, format = "f", digits = 3)
```

## Printing the table
```{r}
knitr::kable(
  desc_stats,
  format = "pipe",
  caption = "Descriptive Statistics",
  align = c("l","r")
) %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, bold = TRUE, background = "#A9CCE3") %>%
  row_spec(1, background = "#F9F9F9") %>% 
  row_spec(2, background = "#FCF3CF") %>% 
  row_spec(3, background = "#F9F9F9") %>% 
  row_spec(4, background = "#FCF3CF") %>% 
  row_spec(5, background = "#F9F9F9") %>% 
  row_spec(6, background = "#FCF3CF") %>% 
  row_spec(7, background = "#F9F9F9") %>% 
  row_spec(8, background = "#FCF3CF") %>% 
  row_spec(9, background = "#F9F9F9") %>% 
  row_spec(10, background = "#FCF3CF") %>% 
  row_spec(11, background = "#F9F9F9") %>% 
  row_spec(12, background = "#FCF3CF") %>% 
  row_spec(13, background = "#F9F9F9") %>% 
  row_spec(14, background = "#FCF3CF") %>% 
  row_spec(15, background = "#F9F9F9") %>% 
  row_spec(16, background = "#FCF3CF") %>% 
  row_spec(17, background = "#F9F9F9")
```
