#Load all Libraries
library(ggplot2)
library(dplyr)
options(scipen=999)
#Read the CSV File
data <-read.csv('mlb_salaries.csv')
#Glance at Dataset
head(data)
#Summary
summary(data)
# 1. Average Player Salaries Player salaries notoriously seem to increase every year, but to what extent? 
#   
# a. Calculate the average player salaries by year (X-axis: Year, Y-axis: Avg. Player Salary) 
# b. Which year had the highest average player salary? 
average_salary_by_year <-setNames( aggregate(data$Salary, FUN = mean,list(Year = data$Year)), c("year","Average_Salary"))

c1 <-ggplot(data = average_salary_by_year, aes(x = year)) +
  geom_line(data = average_salary_by_year,color ="blue", aes(y= Average_Salary, group =1),
linetype =1,size =1) + geom_point(y = average_salary_by_year$Average_Salary, size = 2.5, color = "brown")+
  ggtitle("Average Salary By Year") +theme(plot.title = element_text(size = 14, face= "bold",
                                                                                          hjust = 0.5, margin = ggplot2::margin(10,0,10,0))) +
  scale_x_discrete(limits = average_salary_by_year$year, name = "Year") + scale_y_continuous(labels = scales::dollar, name ="Average Salary")+
  theme(axis.text.x=element_text(angle=90, hjust=1, size = 14),axis.text.y=element_text(hjust=1, size = 14),
        axis.title = element_text(size = 14),
        legend.position = "top",legend.text=element_text(size=14))
ggsave('Average Salary By Year.png')
#Team Salary Standard Deviation
team_salary_by_year <- setNames(aggregate(data$Salary, FUN = sd,list(year = data$Year, team = data$Team)),c("Year", "Team","Standard_Deviation"))
avg_sd_year <- setNames(aggregate(team_salary_by_year$Standard_Deviation, FUN = mean,list(year = team_salary_by_year$Year)),c("Year","Average_Standard_Deviation"))

ggplot(data = avg_sd_year, aes(x = Year)) +
  geom_line(data = avg_sd_year, aes(y= Average_Standard_Deviation, group =2),
            linetype =1,size =1) +
  ggtitle("Average Team Standard Deviation By Year") +scale_x_discrete(limits = avg_sd_year$Year, name = "Year")+
  theme(axis.text.x=element_text(angle=90, hjust=1, size = 10),axis.text.y=element_text(hjust=1, size = 10),
                                               axis.title = element_text(size = 10),
                                               legend.position = "right",legend.text=element_text(size=9)) + scale_y_continuous(name =" Average Standard Deviation")
ggsave("SD by Year.png")

# 
# ggplot() +geom_line(data = team_salary_by_year, aes(x = Year, y = Standard_Deviation, color = Team), size = 1)+
#   theme(axis.text.x=element_text(angle=90, hjust=1, size = 10),axis.text.y=element_text(hjust=1, size = 10),
#         axis.title = element_text(size = 10),
#         legend.position = "right",legend.text=element_text(size=9)) +scale_x_discrete(limits = team_salary_by_year$Year, name = "Year")+
#   ggtitle("Standard Deviation for Teams Over Years") + ylab("Standard Deviation")
#ggsave("Team SD by Year.png")


df2 <- data %>%
       group_by(Player) %>%
       arrange(Year) %>%
       mutate(pct.chg = (Salary - lag(Salary))/lag(Salary))
