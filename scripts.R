# Libraries
library(xlsx)
library(tidyverse)
library(ggpubr)
library(dplyr)

# Dfs
task_A <- as.data.frame(read.xlsx("Task A dataset.xlsx",1))
task_B <- as.data.frame(read.xlsx("Task B dataset.xlsx",1))
task_B_country_code <- as.data.frame(read.xlsx("Task B dataset.xlsx",2))

# Task A
## Chart 1
chart1 <- task_A %>% 
  ggplot(aes(x= Month.ID..YYYYMM., y= Defects)) + 
  geom_line(color="blue") + geom_point() +
  labs(x="Time", y= "Defects", title = "Defects Over Time") 


## Chart 2
chart2 <- task_A %>% 
  ggplot(aes(x= Month.ID..YYYYMM., y= Opportunities)) + 
  geom_line(color="red") + geom_point() +
  labs(x="Time", y= "Opportunities", title = "Opportunity Over Time") 


## Chart 3
chart3 <- task_A %>% 
  ggplot(aes(x= Month.ID..YYYYMM., y= Defect.Rates)) + 
  geom_line(color="blue") + geom_point() +
  geom_line(aes(y= Mean.Rate, colour = "mean"), linetype="dashed") +
  geom_line(aes(y= X2.Sigma.Limit, colour= "limit1"), linetype="dashed") +
  geom_line(aes(y= X3.Sigma.Limit, colour= "limit2"), linetype="dashed") +
  labs(x="Time", y= "Defect Rates", title = "Defect Rates Over Time") +
  scale_color_manual(values = c(mean = "red", limit1 = "brown", limit2= "green"),
                     labels = c(mean = "Mean Defect Rate", limit1 = "X2 Sigma Limit", limit2= "X3 Sigma Limit"),
                     limits = c("mean", "limit1", "limit2"),
                     name ="")

## Chart 4
chart4 <- ggarrange(chart3, ggarrange(chart1,chart2, ncol=2, labels = c("A","B")),nrow = 2, labels = "C")


# Task B
task_B_combined <- merge(task_B, task_B_country_code, by="Account.Number")

## B.1
year_2020 <- task_B_combined[task_B$Date < "2021-01-01",] %>%
  group_by(Product) %>%
  summarise(Volume_2020=sum(Quantity))

year_2021 <- task_B_combined[task_B$Date > "2020-12-31",] %>%
  group_by(Product) %>%
  summarise(Volume_2021=sum(Quantity))

taskB_2 <- merge(year_2020, year_2021, by="Product") %>% 
  mutate(Percentage = (Volume_2021-Volume_2020)/Volume_2020 * 100) %>%
  mutate(across("Percentage", round, 2))

taskB_3 <- task_B_combined[task_B$Date < "2021-01-01",] %>%
  group_by(Country) %>%
  summarise(Volume_by_Country = sum(Quantity)) %>%
  arrange(desc(Volume_by_Country)) %>% head(3)

task_B_combined$Category = substr(task_B_combined$Defect,1,2)
taskB_4 <- task_B_combined %>%
  group_by(Category) %>%
  summarise(Volume = sum(Quantity))

