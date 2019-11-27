#Import packages
library(dplyr)
library(readxl)
install.packages("plotly")
library(plotly)
install.packages("tidyverse")
library(tidyverse)



library(readxl)
WF <- read_excel("Desktop/R/Alloyi/Web Analytics Case Student Spreadsheet.xls", 
                 +     sheet = "Weekly Visits")
View(WF)  

#CREATING REGIONS 0,1,2,3

WF$period <- 0
WF$period[15:35] <- 1 
WF$period[36:52] <- 2
WF$period[53:nrow(WF)] <- 3


# QUESTION 5 ----------------

REV_v_LBS <- ggplot(WF, aes(WF$`Lbs. Sold`, WF$Revenue)) + geom_point(stat = "identity")

REV_v_LBS_cor <-cor(WF$Revenue, WF$`Lbs. Sold`)

# QUESTION 6 ----------------

ggplot(WF, aes(WF$Visits, WF$Revenue)) + geom_point(stat = "identity")

REV_v_Visit
REV_v_Visit_cor <- cor(WF$Revenue, WF$Visits)

#QUESTION 8 ------------

hist(WF$`Lbs. Sold`, breaks=15, main = "Breaks=15")


View(WF)
# Question- How many people are interested? (1- bounce rate)---------
#Create a new variable in WF named Not_Bounce
for (i in 1:nrow(WF)){WF$Not_Bounce[i] <- 1- WF$`Bounce Rate`[i]}

# Claclulating the mean for not_bounce rate grouping by by each period
NB_by_q <- aggregate(WF$Not_Bounce, by = list(WF$period), FUN = mean)

View(NB_by_q)

#plotting the barchart
NB_graph <-ggplot(NB_by_q, aes(NB_by_q$Group.1, NB_by_q$x)) + geom_bar(stat = "identity")+ labs(title = "Interested Web Visitors by period",  x= "Period", y= "Interest" )



#Question - How retention rate varies across periods? (1 - % new visits) -----
#Creating new col in WF dataframe name Retention_rate
for (i in 1:nrow(WF)){WF$Retention_rate[i] <- 1- WF$`% New Visits`[i]}

#Retention by quarter (1-% of new visitors) 
#Finding the means grouping by each period
Retention_by_q <- aggregate(WF$Retention_rate, by = list(WF$period), FUN = mean)

#Changing the colnames for data frame
View(Retention_by_q)
colname<- c("Period","Retention")
colnames(Retention_by_q) <- colname

#Plotting a bar chart
Retention_graph <-ggplot(Retention_by_q, aes(Retention_by_q$Period, Retention_by_q$Retention)) + geom_bar(stat = "identity")


# Plottting question 2
for (i in 1:nrow(WF)){WF$`Revenue/Unique`[i] <- WF$Revenue[i]/WF$`Unique Visits`[i]}

Rev_Unq <- ggplot(WF, aes(WF$period, WF$`Revenue/Unique`)) + geom_bar(stat = "summary")

AVG_T <- ggplot(WF, aes(WF$period, WF$`Avg. Time on Site (secs.)`)) + geom_bar(stat = "summary")

Page_V<- ggplot(WF, aes(WF$period, WF$Pageviews)) + geom_bar(stat = "summary")

Page_per_Vis<-ggplot(WF, aes(WF$period, WF$`Pages/Visit`)) + geom_bar(stat = "summary")

Rev <- ggplot(WF, aes(WF$period, WF$Revenue)) + geom_bar(stat = "summary")

UNQ <- ggplot(WF, aes(WF$period, WF$`Unique Visits`)) + geom_bar(stat = "summary")

#Plotting the graphs together
ggarrange(Rev, AVG_T, Page_V, Page_per_Vis, UNQ, Rev_Unq)


#My graphs used NB/Retention/Rev_Unq ------

Retention_graph + labs(title = "Retention by period",  x= "Period", y= "Retention" )

Rev_Unq + labs(title = "Unique Revenue by period ",  x= "Period", y= "Revenue/Unique" )

NB_graph + geom_bar(stat = "identity")+ labs(title = "Interested Web Visitors by period",  x= "Period", y= "Interest" )
