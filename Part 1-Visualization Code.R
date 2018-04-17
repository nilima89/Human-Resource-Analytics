rm(list=ls())
library(ggplot2)
library(ggplot)
library(dplyr)
library(gridExtra)
library(scales)
library(readr)
library(RColorBrewer)
hrdata=read.csv(file.choose())
View(hrdata)

#VISUALIZATION

hr_table <- tbl_df(hrdata)

hrdata$number_project <- as.factor(hrdata$number_project)
hrdata$Work_accident <- as.factor(hrdata$Work_accident)
hrdata$left <- as.factor(hrdata$left)
hrdata$promotion_last_5years <- as.factor(hrdata$promotion_last_5years)
hrdata$time_spend_company <- as.factor(hrdata$time_spend_company)

glimpse(hrdata)

#how many percent of employees have quit?
c1<- ggplot(hrdata, aes(x=left)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)), alpha=0.8, fill="khaki", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..) ), stat= "count", vjust = -0.5) +
  ylab("Percentage") + xlab("Quitting Rate")+ ggtitle("Quitting Percentage")

#Distribution of Employee's Satisfaction Level
c2 <- ggplot(hrdata, aes(satisfaction_level, y = ..density..)) + 
  geom_histogram(bins = 20, fill = 'indianred1', color = 'black') + 
  scale_x_continuous(labels = scales::percent_format()) + 
  labs(x = "Satisfaction", title = "Employee's Satisfaction Level Distribution")

#Headcount percent by Departemnt
c3 <- ggplot(hrdata, aes(x = sales)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),
           alpha = 0.6, fill= "cyan", color="black") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Department") + ylab("Percentage") + ggtitle("Employees distributed across Department") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..) ), stat= "count", vjust = -0.5)

c4 <- hrdata %>%
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

#affect of department on resignation rate

hrdata %>%
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

#affect of different factors on employee resignation
p1 <- ggplot(hrdata, aes(x=satisfaction_level)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Satisfaction Level") + scale_fill_brewer(palette="Paired")
+ theme(legend.position="none") 

p2 <- ggplot(hrdata, aes(x=number_project)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Number of Project") +
  scale_fill_brewer(palette="Paired")+ ylab("Count")

p3 <- ggplot(hrdata, aes(x=last_evaluation)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Last Evaluation") +
  scale_fill_brewer(palette="Paired") +
  ylab("Count")

p4 <- ggplot(hrdata, aes(x=average_montly_hours)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Average Monthly Hours") +
  scale_fill_brewer(palette="Paired") +
  ylab("Count")

p5 <- ggplot(hrdata, aes(x=time_spend_company)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Time Spent in the Company") +
  scale_fill_brewer(palette="Paired")+ ylab("Count")

p6 <- ggplot(hrdata, aes(x=Work_accident)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Work Accident") + 
  scale_fill_brewer(palette="Paired") +
  ylab("Count")

p7 <- ggplot(hrdata, aes(x=promotion_last_5years)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Promotion in last 5 years") + 
  scale_fill_brewer(palette="Paired") + 
  ylab("Count") 

p8 <- ggplot(hrdata, aes(x=salary)) + 
  geom_bar(stat="count", aes(fill = left)) + xlab("Salary") + 
  scale_fill_brewer(palette="Paired") + 
  ylab("Count") 

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
grid.arrange(p5, p6, p7,p8, ncol = 2, nrow = 2)

#Time spend in the company and no of projects they have done
ggplot(hrdata, aes(x=as.integer(number_project), col=time_spend_company, fill=time_spend_company)) +   scale_fill_brewer(palette="Set2") +
  geom_density(alpha=0.9) + facet_grid(.~time_spend_company) + xlab("Number of Project") 

#how are they getting paid
ggplot(hrdata, aes(x=number_project, y=last_evaluation, color=salary)) +  	
  geom_jitter(alpha=0.8) + xlab("Number of Project") +ylab("Last Evaluation") +  scale_fill_brewer(palette="Set2") +
  ggtitle("Performance Evaluation and number of Projects")
