## Making Toy Datasets

##### Loading libraries #####
library("pubh")
library("tidyverse")
library("utils")
library("readxl")
library("tibble")
#############################

set.seed(666)

##### Toy Data 1
## Choose a dataset
data(Macmahon)
head(Macmahon)

Toy_Data_1 <- Macmahon %>% 
  rename(
    Age_Childbirth = age,
    Breast_Cancer = cancer
) %>% 
  arrange(desc(Breast_Cancer)) %>%
  mutate(
    SES = c(rep(c(1, 2, 2, 2, 3, 3),times = 2244), 1),
    BRCA = c(sample(c(TRUE, FALSE), 3220, replace = TRUE, prob = c(0.3, 0.7)), sample(c(TRUE, FALSE), 10245, replace = TRUE, prob = c(0.05, 0.95))),
    Drink_Per_Week = c(sample(c(0, 1, 2, 3, 4, 5, 6, 7), 3220, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.4)), sample(c(0, 1, 2, 3, 4, 5, 6, 7), 10245, replace = TRUE, prob = c(0.2, 0.3, 0.2, 0.2, 0.3, 0.2, 0.2, 0.05))),
    Height = rnorm(13465, mean = 160, sd = 5),
    Weight = rnorm(13465, mean = 60, sd = 5)
  )

write.csv(Toy_Data_1, "../ToyData/Toy_Data_1.csv")

##### Toy Data 2

Toy_Data_2 <- read_xlsx("../ToyData/Toy_Data_2.xlsx")


##### Toy Data 3: Messy Data


##### Toy Data 4: Missing Values

# Create a complete dataset
data_complete <- tibble(
  ID = 1:100,
  Age = sample(20:60, 100, replace = TRUE),
  Income = sample(30000:90000, 100, replace = TRUE),
  Health_Score = sample(50:100, 100, replace = TRUE),
  Depression_Score = sample(1:10, 100, replace = TRUE)
)

# Introduce missing values
data_missing <- data_complete %>%
  mutate(
    # MCAR: Randomly drop some values in the 'Health_Score' column
    Health_Score = ifelse(runif(n()) < 0.2, NA, Health_Score),
    
    # MAR: Income is more likely to be missing for younger individuals
    Income = ifelse(Age < 35 & runif(n()) < 0.5, NA, Income),
    
    # MNAR: Depression scores are more likely to be missing for people with high depression
    Depression_Score = ifelse(Depression_Score > 7 & runif(n()) < 0.6, NA, Depression_Score)
  )

write.csv(data_missing, "../ToyData/Toy_Data_4.csv")
