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

write.csv(Toy_Data_1, "./ToyData/Toy_Data_1.csv")

##### Toy Data 3: Messy Data
Messy_Data <- data.frame(
  ID = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 110),  # Duplicate ID
  Name = c("Alice", "Bob", "Charlie", "David", "Eve", "Frank", "George", "Hannah", NA, "Jack", "Jack"),  # Missing value
  Age = c(25, 30, "Thirty-Five", 40, NA, 29, 33, "twenty", 27, 32, 32),  # Inconsistent formatting
  Gender = c("F", "M", "Male", "M", "F", "Female", "M", "F", "F", "M", "M"),  # Inconsistent gender labels
  Income = c("55000", "62k", "65000", "72,000", "NA", "58K", "NA", "61000", "59000", "75000", "75000"),  # Character type with missing values
  Date_of_Birth = c("1998-05-21", "1993/08/14", "01-12-1985", "1980-07-30", "1985/06/12", 
                    "1994-02-20", "1991/10/25", "1999-03-08", "1987-09-10", "1992-11-11", "1992-11-11"),  # Inconsistent date formats
  City = c("New York", "Los Angeles", "Chicago", "New York", "Houston", "Chicago", "Miami", "Los Angeles", "NA", "Houston", "Houston"),  # Missing value and duplication
  Extra_Column = c("Remove", "Remove", "Remove", "Remove", "Remove", "Remove", "Remove", "Remove", "Remove", "Remove", "Remove")  # Irrelevant column
)

save(Messy_Data, file = "./ToyData/Toy_Data_2.RData")


##### Toy Data 2: Missing Values

# Create a complete dataset
Complete_Data <- tibble(
  ID = 1:100,
  Age = sample(20:60, 100, replace = TRUE),
  Income = sample(30000:90000, 100, replace = TRUE),
  Health_Score = sample(50:100, 100, replace = TRUE),
  Depression_Score = sample(1:10, 100, replace = TRUE)
)

# Introduce missing values
Missing_Data <- Complete_Data %>%
  mutate(
    # MCAR: Randomly drop some values in the 'Health_Score' column
    Health_Score = ifelse(runif(n()) < 0.2, NA, Health_Score),
    
    # MAR: Income is more likely to be missing for younger individuals
    Income = ifelse(Age < 35 & runif(n()) < 0.5, NA, Income),
    
    # MNAR: Depression scores are more likely to be missing for people with high depression
    Depression_Score = ifelse(Depression_Score > 7 & runif(n()) < 0.6, NA, Depression_Score)
  )

write.csv(Missing_Data, "./ToyData/Toy_Data_3.csv")

##### Toy Data 4: linelist_raw.xlsx

Toy_Data_4 <- read_xlsx("./ToyData/linelist_raw.xlsx")
write.csv(Toy_Data_4, "./ToyData/Toy_Data_4.csv")

