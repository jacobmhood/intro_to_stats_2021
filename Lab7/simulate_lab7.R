# Simulate data for Lab7
library(tidyverse)


# Simulate
set.seed(7890)
# Simulate IV (black)
black <- rbinom(5000, 1, 0.3)
# Simulate IV (college)
college <- rbinom(5000, 1, 0.3)

# Simulate DV with interaction effect for black and college 
earn <- 15 + 6*college - 3*black + 2*college*black + rnorm(5000, 0, 5) 

df <- tibble(earn = earn,
             college = college,
             black = black)

m_dum <- lm(earn ~ college + black + college*black)

stargazer(m_dum)