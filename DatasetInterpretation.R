rm(list=ls())
library(ggplot2)
library(ggplot)
library(dplyr)
library(gridExtra)
library(scales)
library(readr)
library(RColorBrewer)
hr_employee <- read_csv("~/Documents/DS_IUB/CSCI_565_DM/Project/Original dataset.csv")
View(hr_employee)
hr_table <- tbl_df(hr_employee)

hr_employee$number_project <- as.factor(hr_employee$number_project)
hr_employee$Work_accident <- as.factor(hr_employee$Work_accident)
hr_employee$left <- as.factor(hr_employee$left)
hr_employee$promotion_last_5years <- as.factor(hr_employee$promotion_last_5years)
hr_employee$time_spend_company <- as.factor(hr_employee$time_spend_company)

glimpse(hr_employee)

#how many percent of employees have quit?
c1<- ggplot(hr_employee, aes(x=left)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)), alpha=0.8, fill="khaki", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..) ), stat= "count", vjust = -0.5) +
  ylab("Percentage") + xlab("Quitting Rate")+ ggtitle("Quitting Percentage")

#23.8% of employees has left the company. Let's see from which department, they left. For that, we need to find out what is the distribution in the department.

#Distribution of Employee's Satisfaction Level
c2 <- ggplot(hr_employee, aes(satisfaction_level, y = ..density..)) + 
  geom_histogram(bins = 20, fill = 'indianred1', color = 'black') + 
  scale_x_continuous(labels = scales::percent_format()) + 
  labs(x = "Satisfaction", title = "Employee's Satisfaction Level Distribution")

#Headcount percent by Departemnt
c3 <- ggplot(hr_employee, aes(x = sales)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),
           alpha = 0.6, fill= "cyan", color="black") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Department") + ylab("Percentage") + ggtitle("Employees distributed across Department") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..) ), stat= "count", vjust = -0.5)


#27.6% of employees are from sales. Now, lets see if the reason for regisnation is from which dept
c4 <- hr_employee %>%
  select(sales,left) %>%
  filter(left == 1) %>%
  group_by(sales) %>%
  summarise(count=n()) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(x=sales, y=percent)) +
  geom_bar(stat="identity", fill= "khaki", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0(round(percent*100,0),"%"),
                y=percent+0.01)) +
  ylab("Percentage") + xlab("Department") +
  ggtitle("Resignation according to Department")

grid.arrange(c1, c2, c3, c4, ncol = 2, nrow = 2)
#which factor is contributing the most for their leavving?
#Let's take a look at 4 variables that should give some hints with the following assumptions:
#Satisfaction: Lower satisfaction, more likely to quit.
#No. of Project: Too many projects, more likely to quit.
#Last Evaluation: Lower last evaluation, more likely to quit.
#Avg Mth Hours: High hours, more likely to quit
#attrition per dept
hr_employee %>%
  select(sales,left) %>%
  group_by(sales, left) %>%
  summarise(count=n()) %>%
  mutate(dep_pct = count/sum(count)) %>%
  ggplot(aes(x=sales, y=dep_pct, fill = left)) + 
  geom_bar(stat="identity", alpha = 0.7) +
  geom_text(aes(label = paste0(round(dep_pct*100,0),"%"),
                y=dep_pct+0.02)) +
  scale_fill_brewer(palette="Paired")+
  ylab("Percentage of Employees") + xlab("Department") +
  ggtitle("Resignation per Department")

p1 <- ggplot(hr_employee, aes(x=satisfaction_level)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Satisfaction Level") + scale_fill_brewer(palette="Paired")
  + theme(legend.position="none") 

p2 <- ggplot(hr_employee, aes(x=number_project)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Number of Project") +
  scale_fill_brewer(palette="Paired")+ ylab("count")

p3 <- ggplot(hr_employee, aes(x=last_evaluation)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Last Evaluation") +
  scale_fill_brewer(palette="Paired") +
  ylab("count")

p4 <- ggplot(hr_employee, aes(x=average_montly_hours)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Average Monthly Hours") +
  scale_fill_brewer(palette="Paired") +
  ylab("count")

p5 <- ggplot(hr_employee, aes(x=time_spend_company)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Time Spent in the Company") +
  scale_fill_brewer(palette="Paired")+ ylab("count")

p6 <- ggplot(hr_employee, aes(x=Work_accident)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Work Accident") + 
  scale_fill_brewer(palette="Paired") +
  ylab("count")

p7 <- ggplot(hr_employee, aes(x=promotion_last_5years)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Promotion in last 5 years") + 
  scale_fill_brewer(palette="Paired") + 
  ylab("count") 
p7 <- ggplot(hr_employee, aes(x=promotion_last_5years)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Promotion in last 5 years") + 
  scale_fill_brewer(palette="Paired") + 
  ylab("count") 
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
grid.arrange(p5, p6, p7, ncol = 2, nrow = 2)
#Time spend in the company and no of projects they have done
ggplot(hr_employee, aes(x=as.integer(number_project), col=time_spend_company, fill=time_spend_company)) +   scale_fill_brewer(palette="Set2") +
  geom_density(alpha=0.9) + facet_grid(.~time_spend_company) + xlab("Number of Project") 

#Those with 2, 3 years get to work only on 2 to 3 projects. That makes sense as they may be deemed “new” so they need more time to learn the business.
#But those with 4 to 5 years of tenure tend to work 3 – 5 projects.
#the older people are doing good here

#how are they getting paid
ggplot(hr_employee, aes(x=number_project, y=last_evaluation, color=salary)) +  	
  geom_jitter(alpha=0.8) + xlab("Number of Project") +ylab("Last Evaluation") +  scale_fill_brewer(palette="Set2") +
  ggtitle("Performance Evaluation and number of Projects")


