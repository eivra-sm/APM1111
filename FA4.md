# Formative Assessment 4

Authors:

Sinocruz, Arvie

Tagaytay Gabriel

Date: October 03, 2025

--

**Github Link:** 

## Given Data
```{r}
normal <- c(67,70,63,65,68,60,70,64,69,61,66,65,71,62,66,68,64,67,62,66,65,63,66,65,63,69,
            62,67,59,66,65,63,65,60,67,64,68,61,69,65,62,67,70,64,63,68,64,66,65,61,66,64)

skew_right <- c(31,43,30,30,38,26,29,55,46,26,29,57,34,34,36,40,28,26,66,63,30,33,24,35,34,40,
              24,29,24,27,35,33,75,38,34,85,29,40,41,35,26,34,19,23,28,26,31,25,22,28,34)

skew_left <- c(102,55,70,95,73,79,60,73,89,85,72,92,76,93,76,97,10,70,85,25,83,58,10,92,82,87,
             104,75,80,66,93,90,84,73,98,79,35,71,90,71,63,58,82,72,93,44,65,77,81,77)

uniform <- c(12.1,12.1,12.4,12.1,12.1,12.2,12.2,12.2,11.9,12.2,12.3,12.3,11.7,12.3,12.3,12.4,
             12.4,12.1,12.4,12.4,12.5,11.8,12.5,12.5,12.5,11.6,11.6,12.0,11.6,11.6,11.7,12.3,
             11.7,11.7,11.7,11.8,12.5,11.8,11.8,11.8,11.9,11.9,11.9,12.2,11.9,12.0,11.9,12.0,
             12.0,12.0)
```


## Function to compute raw moments

```{r}
raw_moments <- function(x) {
  c(mean(x), mean(x^2), mean(x^3), mean(x^4))
}

# ---- (a) First moments ----
first_moments <- c(
  Normal = raw_moments(normal)[1],
  Skewed_Right = raw_moments(skew_right)[1],
  Skewed_Left = raw_moments(skew_left)[1],
  Uniform = raw_moments(uniform)[1]
)

# ---- (b) Second moments ----
second_moments <- c(
  Normal = raw_moments(normal)[2],
  Skewed_Right = raw_moments(skew_right)[2],
  Skewed_Left = raw_moments(skew_left)[2],
  Uniform = raw_moments(uniform)[2]
)

# ---- (c) Third moments ----
third_moments <- c(
  Normal = raw_moments(normal)[3],
  Skewed_Right = raw_moments(skew_right)[3],
  Skewed_Left = raw_moments(skew_left)[3],
  Uniform = raw_moments(uniform)[3]
)

# ---- (d) Fourth moments ----
fourth_moments <- c(
  Normal = raw_moments(normal)[4],
  Skewed_Right = raw_moments(skew_right)[4],
  Skewed_Left = raw_moments(skew_left)[4],
  Uniform = raw_moments(uniform)[4]
)

# Displaying the results
cat("\n(a) First moments (means):\n"); print(first_moments)
cat("\n(b) Second moments:\n"); print(second_moments)
cat("\n(c) Third moments:\n"); print(third_moments)
cat("\n(d) Fourth moments:\n"); print(fourth_moments)

```

## Function to compute central moments (about the mean)

```{r}
central_moments <- function(x, kmax = 4) {
  mean_x <- mean(x)
  cms <- sapply(1:kmax, function(k) mean((x - mean_x)^k))
  # Force very small values (close to 0) to be exactly 0
  cms[abs(cms) < 1e-10] <- 0
  return(cms)
}

# ---- (a) First central moments ----
first_central <- c(
  Normal = central_moments(normal)[1],
  Skewed_Right = central_moments(skew_right)[1],
  Skewed_Left = central_moments(skew_left)[1],
  Uniform = central_moments(uniform)[1]
)

# ---- (b) Second central moments ----
second_central <- c(
  Normal = central_moments(normal)[2],
  Skewed_Right = central_moments(skew_right)[2],
  Skewed_Left = central_moments(skew_left)[2],
  Uniform = central_moments(uniform)[2]
)

# ---- (c) Third central moments ----
third_central <- c(
  Normal = central_moments(normal)[3],
  Skewed_Right = central_moments(skew_right)[3],
  Skewed_Left = central_moments(skew_left)[3],
  Uniform = central_moments(uniform)[3]
)

# ---- (d) Fourth central moments ----
fourth_central <- c(
  Normal = central_moments(normal)[4],
  Skewed_Right = central_moments(skew_right)[4],
  Skewed_Left = central_moments(skew_left)[4],
  Uniform = central_moments(uniform)[4]
)

# Displaying the results
cat("\n(a) First central moments (exact 0):\n"); print(first_central)
cat("\n(b) Second central moments (variance):\n"); print(second_central)
cat("\n(c) Third central moments:\n"); print(third_central)
cat("\n(d) Fourth central moments:\n"); print(fourth_central)
```
