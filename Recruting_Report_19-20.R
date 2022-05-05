
#Reading and cleaning the raw data

# Set Working directory
setwd("~/Desktop/sport/Rawdata")

#loading Packages used for project
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(broom)
library(ggcorrplot)

# Dataset used for project
team_payroll <- read.csv("~/Desktop/sport/Rawdata/2019-20_nba_team-payroll.csv")
team_statistics1 <- read.csv("~/Desktop/sport/Rawdata/2018-19_nba_team-statistics_1.csv")
team_statistics2 <- read.csv("~/Desktop/sport/Rawdata/2018-19_nba_team-statistics_2.csv")
team_player_statistics <- read.csv("~/Desktop/sport/Rawdata/2018-19_nba_player-statistics.csv")
team_player_salaries <- read.csv("~/Desktop/sport/Rawdata/2018-19_nba_player-salaries.csv")

# Checking NA values in dataset
sum(is.na(team_payroll)) #0
sum(is.na(team_statistics1)) #90 
sum(is.na(team_statistics2)) #0
sum(is.na(team_player_statistics)) #117
sum(is.na(team_player_salaries)) #0


colSums(is.na(team_statistics1)) #X /X.1/ X.2 Columns have na values.
colSums(is.na(team_player_statistics))


# Remove meaningless NA values
team_statistics1<- team_statistics1[, -c(23:25)]
team_player_statistics<- na.omit(team_player_statistics) # plyaers who played less game removed


# Combine data
player_data <- full_join(team_player_salaries, team_player_statistics)
player_data<- na.omit(player_data)
data <- player_data[!duplicated(player_data$player_id), ] # delete players are repeated
sum(is.na(player_data)) #0


# saving cleaned data
write.csv(data, "tidy.data.csv")



#Corelationship between numeric variables
numeric_data<- data %>% select(-c(player_id,player_name, Pos, Tm))
ggcorrplot(cor(numeric_data))


# Salary distribution
summary(data$salary)

ggplot(data = player_data, aes(x = salary)) + 
  geom_histogram(mapping=aes(y=..density..), bins=30 , col="black",fill="magenta") + 
  geom_density(alpha=0.5,fill="dodgerblue")



# Rleationship between points(PTS) and salary
ggplot(data, aes(x= PTS, y=salary))+
  geom_point(size=0.5)+
  geom_smooth(method="lm", size=0.3,color="red")+
  ggtitle("Points X Salary")


# Rleationship between poisition and salary
mean_position<- data %>% group_by(Pos) %>% summarise(mean_Salary=mean(salary))

ggplot(mean_position, aes(x= reorder(Pos, mean_Salary), y=mean_Salary, fill=mean_Salary))+
  geom_bar(stat="identity")+ggtitle("Position X Mean-salary")

mean_position %>% mutate(Ratio = mean_Salary/sum(mean_Salary),
                         budget = Ratio*47.2)

# Relationship Age and points
ggplot(data, aes(x=Age, y=PTS))+
  geom_point()


# Relationship Games and points
ggplot(data, aes(x=G, y=PTS))+
  geom_point()


# Upgrading PTS variable depends on how many matches player played and Age
data_newpoints<- data %>% mutate(Points = PTS+(5*G) -(10*Age))

# Relationship Games and new points
ggplot(data_newpoints, aes(x=G, y=Points))+
  geom_point()

ggplot(data_newpoints, aes(x=Age, y=Points))+
  geom_point()


#select the variables to be used for modeling
select_data<- data %>% 
  select(c(Age, G, GS, MP, FG, FGA, FT, FTA, DRB, TRB, TOV, PTS))

model1<- lm(PTS ~.,data=select_data)


select_data2<-data_newpoints %>% 
  select(c(Age, G, MP, FG, FGA, FT, FTA, DRB, TRB, TOV, Points))

model2<- lm(Points ~.,data=select_data2)

#check model
summary(model1)
tidy(model1, conf.int = T)


summary(model2)
tidy(model2, conf.int = T)

# Extract name of player, salary, position and point valriable applied every aspect
final_data<- data_newpoints %>% 
  select(player_name, salary, Pos, Points) %>% 
  arrange(-Points)

### TOP 3 Best player each poisition within budget

#Center Player(>12.1M)
final_data %>% 
  filter(Pos =="C") %>%
  filter(salary <= 12100000) %>% 
  slice(1:3)


#Power Forward player (>8.64M)
final_data %>% 
  filter(Pos =="PF") %>%
  filter(salary <= 8640000) %>% 
  slice(1:3)


#Point Guard player(>9.74M)
final_data %>% 
  filter(Pos =="PG") %>%
  filter(salary <= 9740000) %>% 
  slice(1:3)


#Small Forward player (>8.95M)
final_data %>% 
  filter(Pos =="SF") %>%
  filter(salary <= 8950000) %>% 
  slice(1:3)


#Shooting Guard player (>7.81M)
final_data %>% 
  filter(Pos =="SG") %>%
  filter(salary <= 7810000) %>% 
  slice(1:3)


#Best 5 Best players

#Center -> Karl-Anthony Towns 
#Power forward -> Pascal Siakam 
#Point Guard -> D'Angelo Russell
#Small Forward -> Jayson Tatum 
#Shooting Guard -> Donovan Mitchell
