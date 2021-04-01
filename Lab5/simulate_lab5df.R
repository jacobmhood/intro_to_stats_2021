# Simulate data for Lab5
library(tidyverse)

set.seed(1234)
# Simulate IV (edu level)
edu <- rpois(1000, lambda = 6) #rpois: Random Poisson Distribution with parameter lamda 
# Simulate IV (gender)
female <- rbinom(1000, 1, 0.5)
# Simulate IV (black)
black <- rbinom(1000, 1, 0.3)
# Simulate IV (other)
other <- rbinom(1000, 1, 0.1)
# Simulate IV (age)
age <- runif(1000, min=20, max=65) %>% ceiling()

# Simulate DV with interaction effect for educatio and gender, race and gender
earn <- 15 + 6*edu - 2*female - 3*edu*female - 1*black - 1*other -1.5*black*female - 1*other*female + 0.2*age + rnorm(1000, 0, 8) 

# Combine dataframe
df <- tibble(earn = earn,
             edu = edu,
             female = female, 
             black = black,
             other = other,
             age= age
) %>%
  mutate(unique_id = row_number() + 10000)

# use 9999 to indicate NA 
set.seed(1234)
df_randomNA <- df %>% sample_n(20) %>% mutate(age = 9999)

# replace in original df
df[match(df_randomNA$unique_id, df$unique_id), ] <- df_randomNA

# use character for race and gender
df <- df %>%
  mutate(sex = ifelse(female == 1, "female", "male"),
         race = ifelse(black == 1, "black", 
                       ifelse(other == 1, "other", "white")))

df_export <- df %>% select(unique_id, earn, edu, sex, race, age)
write.csv(df_export, "lab5_earnings.csv", row.names = F)

# Summary statistics
summary(df)

# Plot
df %>%
  ggplot(aes(x = edu, y = earn, color = as.factor(female))) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm")


