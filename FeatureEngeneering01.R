# Features Engineering with Categorical Variables

# Dataset: http://arquive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip

# Load Data
dataset_bank <- read.table("bank/bank-full.csv", header = TRUE, sep = ";")
View(dataset_bank)

# Job 1 - Create a Column

# Verify the quantity of jobs
table(dataset_bank$job)

# Verify the quantity of jobs with Graphics
# install.packages("dplyr")
# install.packages("ggplot2")
library(dplyr)
library(ggplot2)

dataset_bank%>%
  group_by(job)%>%
  summarise(n =n())%>%
  ggplot(aes(x = job, y = n))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# add column with technology use information

dataset_bank <- dataset_bank%>%
  mutate(technology_use = 
           case_when(job == 'admin.' ~ 'low',
                     job == 'blue-collar' ~ 'low',
                     job == 'entrepreneur' ~ 'high',
                     job == 'housemaid' ~ 'medium',
                     job == 'management' ~ 'low',
                     job == 'retired' ~ 'medium',
                     job == 'self-employed' ~ 'high',
                     job == 'services' ~ 'medium',
                     job == 'student' ~ 'low',
                     job == 'technician' ~ 'high',
                     job == 'unemployed' ~ 'low',
                     job == 'unknown' ~ 'low'))

View(dataset_bank)


# job 2 - Dummies Variable

# Make the columns Default to Dummies Variable

dataset_bank <- dataset_bank%>%
  mutate(defaulted = ifelse(default == 'yes',1,0))

View(dataset_bank)

# job 3 - One hot encoding

library(caret)

dmy <- dummyVars(" ~ .", data = dataset_bank)
bank.dummies <- data.frame(predict(dmy, newdata = dataset_bank))
View(bank.dummies)


# job 4 - Resource combination

# group by job and marital

dataset_bank %>%
  group_by(job, marital) %>%
  summarise(n = n())

# group by data Visualization

dataset_bank %>%
  group_by(job, marital) %>%
  summarise(n = n())%>%
  ggplot(aes(x = job, y=n, fill = marital))+
  geom_bar(stat = 'identity', position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Combination

dmy <- dummyVars( ~ job:marital, data = dataset_bank)
bank.cross <- predict(dmy, newdata = dataset_bank)
View(bank.cross)








