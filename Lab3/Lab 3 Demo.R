knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(gridExtra)
library(kableExtra)
set.seed(11001)
pop_norm <- rnorm(n=10000, mean = 0, sd = 1) %>% as_tibble()

pop_norm %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "grey") +
  labs(title = "Histogram of Simulated Population with Normal Distribution",
       subtitle = "N = 100000, mean = 0",
       x = "")
norm_mean_container <- vector(mode = "numeric", length = 1000)

set.seed(0000)
for (i in 1:1000){    
    sample <- pop_norm %>% sample_n(size= 100, replace = FALSE)
    norm_mean_container[i] <- mean(sample$value)
  }

head(norm_mean_container, n = 10)

mean = mean(norm_mean_container)
sd_pos = mean + sd(norm_mean_container)
sd_neg = mean - sd(norm_mean_container)
    

norm_mean_container %>% 
  as_tibble() %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.01, fill = "grey", color = "black") +
  labs(title = "Sampling Distribution of the Sample Mean (1000 iterations of sample n = 100)",
       subtitle = "Mean marked by the dashed line, 1 standard deviation around the mean marked by solid lines"
se = 3.25/ sqrt(3.25)
observed_t <- (1.54 - 0)/se
print(observed_t)
#Computer critical value 
qt(p = .5*.05, df = 26)
print(qt)

#Rejection region is (-i,f, -2;055529)
# P-Value 
p = 2*(1 - pt(q = observed_t, df = 26))
p
#observed t is in rejection region so we reject null 

weight_df <- read.csv("/Users/jacobmichaelhood/Downloads/intro_to_stats_2021-main/Lab3/data/weight.csv")
mean(weight_df$change)

# ---- One sample two-tail t-test ---- 
two_tail_t <- t.test(         
  weight_df$change,           # the sample value vector that you want to test
  mu = 0,                     # mean given by your null hypothesis
  alternative = "two.sided",  # direction of alternative hypothesis
  conf.level = 0.95           # significance level
)

## Extract test statistic
two_tail_t$statistic

## Extract p-value
two_tail_t$p.value

## Extract the confidence interval of the mean
two_tail_t$conf.int

two_tail_t

t.test(weight_df$change, mu = 0, alternative = "greater", conf.level = 0.95)

twotail_t <- t.test(
  weight_df$change, 
  mu = 4,
  alternative = "two.sided",
  conf.level = .95
)

twotail_t

onetail_t <-t.test(
  weight_df$change,
  mu = 4,
  alternative = "less",
  conf.level = .95
)

onetail_t

weight_df %>%
  ggplot(aes(x = therapy, y = change)) +
  geom_boxplot() +
  geom_point(shape = 1, alpha = 0.7) +
  labs(title = "Weight Changes by Therapy",
       y = "weight change")

# Filter data for each therapy

weight_f <- weight_df %>% filter(therapy == "f")
weight_c <- weight_df %>% filter(therapy == "c")

t.test(
    x = weight_f$change,
    y = weight_c$change,
    mu = 0,
    alternative = "two.sided"
)

# Two dependent two-tail t test

t.test(
    x = weight_df$before,
    y = weight_df$after,
    mu = 0,
    paried = TRUE,
    alternative = "two.sided",
    conf.level = 0.95
)

weight_b <- weight_df %>% filter(therapy == "b")

t.test(
    x = weight_b$change,
    y = weight_c$change,
    mu = 0,
    alternative = "two.sided",
    conf.level = 0.95
)

\