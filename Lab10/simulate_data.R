# Simulate data for Lab 10

# Support Same Sex Marriage (1 = no, 2 = neutral, 3 = yes) 
# = Education + Age + Gender + Race

# -------Simulate IVs-------
set.seed(123)
# Years of education
eduy <- rpois(1000,12) 
hist(eduy)
# Age
age <- rpois(1000,40) 
hist(age)
# Gender dummy
female <- rbinom(1000, 1, 0.5)
table(female)
# Race dummy
black <- rbinom(1000, 1, 0.3)
table(black)

# -------Simulate DV-------
# latent Y*
ystar = 0.8*eduy + (-0.2)*age + 0.8*female + rlogis(1000)

# thresholds
tau = c(-0.5, 2)

# generate obseved Y
y = ifelse(
  ystar < tau[1], 1,
  ifelse(ystar >= tau[1] & ystar < tau[2], 2, 3)
)

# -------Create dataframe-------
support_df <- tibble(
  support_level = as.factor(y),
  eduy = eduy,
  age = age,
  female = female,
  black = black
)

# Save
save(support_df, file = "data/support_level_df.RData")
