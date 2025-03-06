# INSTALL REQUIRED PACKAGES 
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('readxl')
install.packages('multcomp')

# LOAD REQUIRED LIBRARIES
library(readxl)     
library(tidyverse)
library(ggplot2)
library(multcomp)

# Disable scientific notation for p-values
options(scipen=999)

##################  INITIAL DATA EXPLORATION ##################  
# READ THE NEW DATASET
df <- read_excel(file.choose())  

# Display column names of the dataset
names(df)
# Display basic summary statistics for each variable
summary(df)
# View the imported dataset (opens in a separate viewer)
View(df)

##################  PRE-PROCESS THE DATA ##################  
# Convert 'Cluster' to factor
df$Cluster <- as.factor(df$Cluster)

# Confirm change
is.factor(df$Cluster)

##################  2. ANOVA (Analysis of Variance) ##################  

# 2a. CHECK THE EFFECT OF CLUSTER ON 'TimelyInf'
anov_timelyinf <- aov(TimelyInf ~ Cluster, data = df)
summary(anov_timelyinf)

# Mean and SD for 'TimelyInf' with different clusters
mean_cluster_timelyinf <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(TimelyInf, na.rm = TRUE),
    sd = sd(TimelyInf, na.rm = TRUE)
  )
mean_cluster_timelyinf

# Tukey Pairwise Comparison:
TukeyHSD(anov_timelyinf, conf.level=.95)

# Visualize the results using a boxplot
ggplot(df, aes(x = Cluster, y = TimelyInf, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Timely Information Importance", 
       y = "Timely Information Importance Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2b. CHECK THE EFFECT OF CLUSTER ON 'TaskMgm'
anov_taskmgm <- aov(TaskMgm ~ Cluster, data = df)
summary(anov_taskmgm)

mean_cluster_taskmgm <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(TaskMgm, na.rm = TRUE),
    sd = sd(TaskMgm, na.rm = TRUE)
  )
mean_cluster_taskmgm

TukeyHSD(anov_taskmgm, conf.level=.95)

ggplot(df, aes(x = Cluster, y = TaskMgm, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Task Management Importance", 
       y = "Task Management Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2c. CHECK THE EFFECT OF CLUSTER ON 'DeviceSt'
anov_devicest <- aov(DeviceSt ~ Cluster, data = df)
summary(anov_devicest)

mean_cluster_devicest <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(DeviceSt, na.rm = TRUE),
    sd = sd(DeviceSt, na.rm = TRUE)
  )
mean_cluster_devicest

TukeyHSD(anov_devicest, conf.level=.95)

ggplot(df, aes(x = Cluster, y = DeviceSt, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Device Status", 
       y = "Device Status Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2d. CHECK THE EFFECT OF CLUSTER ON 'Wellness'
anov_wellness <- aov(Wellness ~ Cluster, data = df)
summary(anov_wellness)

mean_cluster_wellness <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(Wellness, na.rm = TRUE),
    sd = sd(Wellness, na.rm = TRUE)
  )
mean_cluster_wellness

TukeyHSD(anov_wellness, conf.level=.95)

ggplot(df, aes(x = Cluster, y = Wellness, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Wellness Importance", 
       y = "Wellness Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2e. CHECK THE EFFECT OF CLUSTER ON 'Athlete'
anov_athlete <- aov(Athlete ~ Cluster, data = df)
summary(anov_athlete)

mean_cluster_athlete <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(Athlete, na.rm = TRUE),
    sd = sd(Athlete, na.rm = TRUE)
  )
mean_cluster_athlete

TukeyHSD(anov_athlete, conf.level=.95)

ggplot(df, aes(x = Cluster, y = Athlete, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Athlete Importance", 
       y = "Athlete Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2f. CHECK THE EFFECT OF CLUSTER ON 'Style'
anov_style <- aov(Style ~ Cluster, data = df)
summary(anov_style)

mean_cluster_style <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(Style, na.rm = TRUE),
    sd = sd(Style, na.rm = TRUE)
  )
mean_cluster_style

TukeyHSD(anov_style, conf.level=.95)

ggplot(df, aes(x = Cluster, y = Style, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Style Importance", 
       y = "Style Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2g. CHECK THE EFFECT OF CLUSTER ON 'AmznP'
anov_amznp <- aov(AmznP ~ Cluster, data = df)
summary(anov_amznp)

mean_cluster_amznp <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(AmznP, na.rm = TRUE),
    sd = sd(AmznP, na.rm = TRUE)
  )
mean_cluster_amznp

TukeyHSD(anov_amznp, conf.level=.95)

ggplot(df, aes(x = Cluster, y = AmznP, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Amazon Purchase Importance", 
       y = "Amazon Purchase Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2h. CHECK THE EFFECT OF CLUSTER ON 'Income'
anov_income <- aov(Income ~ Cluster, data = df)
summary(anov_income)

mean_cluster_income <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(Income, na.rm = TRUE),
    sd = sd(Income, na.rm = TRUE)
  )
mean_cluster_income

TukeyHSD(anov_income, conf.level=.95)

ggplot(df, aes(x = Cluster, y = Income, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Income", 
       y = "Income Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2i. CHECK THE EFFECT OF CLUSTER ON 'Age'
anov_age <- aov(Age ~ Cluster, data = df)
summary(anov_age)
   
mean_cluster_age <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(Age, na.rm = TRUE),
    sd = sd(Age, na.rm = TRUE)
  )
mean_cluster_age

TukeyHSD(anov_age, conf.level=.95)

ggplot(df, aes(x = Cluster, y = Age, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Age", 
       y = "Age Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2j. CHECK THE EFFECT OF CLUSTER ON 'Female'
anov_female <- aov(as.numeric(Female) ~ Cluster, data = df)
summary(anov_female)

mean_cluster_female <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(as.numeric(Female), na.rm = TRUE),
    sd = sd(as.numeric(Female), na.rm = TRUE)
  )
mean_cluster_female

TukeyHSD(anov_female, conf.level=.95)

ggplot(df, aes(x = Cluster, y = as.numeric(Female), fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Female Distribution", 
       y = "Proportion of Female", 
       x = "Cluster Group") +
  theme_minimal()

# 2k. CHECK THE EFFECT OF CLUSTER ON 'Degree'
anov_degree <- aov(as.numeric(Degree) ~ Cluster, data = df)
summary(anov_degree)

mean_cluster_degree <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(as.numeric(Degree), na.rm = TRUE),
    sd = sd(as.numeric(Degree), na.rm = TRUE)
  )
mean_cluster_degree

TukeyHSD(anov_degree, conf.level=.95)

ggplot(df, aes(x = Cluster, y = as.numeric(Degree), fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on Degree", 
       y = "Degree Score", 
       x = "Cluster Group") +
  theme_minimal()

# 2l. CHECK THE EFFECT OF CLUSTER ON 'ConstCom'
anov_constcom <- aov(ConstCom ~ Cluster, data = df)
summary(anov_constcom)

mean_cluster_constcom <- df %>%
  group_by(Cluster) %>%
  summarise(
    count = n(),
    mean = mean(ConstCom, na.rm = TRUE),
    sd = sd(ConstCom, na.rm = TRUE)
  )
mean_cluster_constcom

TukeyHSD(anov_constcom, conf.level=.95)

ggplot(df, aes(x = Cluster, y = ConstCom, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  labs(title = "Effect of Cluster on ConstCom", 
       y = "ConstCom Score", 
       x = "Cluster Group") +
  theme_minimal()

