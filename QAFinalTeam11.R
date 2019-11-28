

# WebQA Analysis Readme ----------------------------------------------------------

"

Each one can write our final code in each section. 
For simplicity-sake lets use the the same name for the merged dataframe: qaDf
qaDf has the merged visits and financials files and the column date with the 
proper format.

The qaDf is saved in the github repository.
The pre-processing section is for the professor to check how we massaged the data.

NOTE: CAMPAIGN IS TRANSFORMED AS A FACTOR IN THE PREPROCESSING STEP

"




# Packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
library(plotly)
library(tidyr)
library(readr)





# PreProcessing -----------------------------------------------------------

source('helperFunctions.R')

visitsWeek <- read_excel("webAnalyticsCase.xls",
                         sheet = "Weekly Visits")

financials <- read_excel('webAnalyticsCase.xls',
                         sheet = 'Financials')

# Create date column

qaDf <- convertQAWeeksToDate(dataFrameVisits = visitsWeek,
                     dfVisitsAndFinance = financials,
                     mergeDfs = TRUE)


# Initial Period: 1
# Pre Promotion: 2
# Promotion: 3
# Post Promotion: 4
# convertQAWeeksToPeriod only works if the dataframe has the original amount of rows
  # as in the case. 

qaDf <- convertQAWeeksToPeriod(qaDf)

# Write CSV
# write.csv(qaDf, 'mergedVisitsFinancialsWithDateCol.csv',
#           row.names = FALSE)


# Load Data ---------------------------------------------------------------

qaDf <- read_csv('mergedVisitsFinancialsWithDateCol.csv')

#DF only used in Q2 for additional column "Rev/UV"
FV_P <- read_excel("Web_FinVists_Periods.xlsx") 

# Carolina Q1 -------------------------------------------------------------


# Talya Q2 a,b ------------------------------------------------------------
#A How much time (sec)  they spent on the website per period?
T1 <- qaDf$`Avg. Time on Site (secs.)`[1:14]
T2 <- qaDf$`Avg. Time on Site (secs.)`[15:35]
T3 <- qaDf$`Avg. Time on Site (secs.)`[36:52]
T4 <- qaDf$`Avg. Time on Site (secs.)`[53:66]

#Took the avg of secs per period vistors spent on the website (secs)
tm1 <- mean(T1) # 80.28571 
tm2 <- mean(T2) # 95.85714 
tm3 <- mean(T3) # 48.7647
tm4 <- mean(T4) # 70.00000

#BAR CHART for AVg Time per period
timebar <- ggplot(qaDf, aes(qaDf$period, qaDf$`Avg. Time on Site (secs.)`))
timebar + geom_col() +
  xlab("Periods") +
  ylab("Avg Time (secs)") +
  ggtitle("Avg Time on Website per Period")


#B How many pageviews per visits per period?
PVP1 <- FV_P$`Pages/Visit`[1:14]
PVP2 <- FV_P$`Pages/Visit`[15:35]
PVP3 <- FV_P$`Pages/Visit`[36:52]
PVP4 <- FV_P$`Pages/Visit`[53:66]

#Took the avg of pgs/vist and periods 
PM1 <- mean(PVP1) #2.282857
PM2 <- mean(PVP2) #2.669048
PM3 <- mean(PVP3) #1.794706
PM4 <- mean(PVP4) #2.181429

#BAR CHART: Pg views per visit per period
pageviewsbar <- ggplot(qaDf, aes(qaDf$period, qaDf$`Pages/Visit`))
pageviewsbar + geom_col() +
  xlab("Periods") +
  ylab("Pageviews per Visit") +
  ggtitle("Avg PageViews per Visit")


# Elmir Q2 c, d -----------------------------------------------------------
for (i in 1:nrow(FV_P)){FV_P$`Rev/UV`[i] <- FV_P$Revenue[i]/FV_P$`Unique Visits`[i]}

Rev_Unq <- ggplot(FV_P, aes(FV_P$period,FV_P$`Rev/UV`)) + geom_bar(stat = "summary")

AVG_T <- ggplot(qaDf, aes(qaDf$period, qaDf$`Avg. Time on Site (secs.)`)) + geom_bar(stat = "summary")

Page_V<- ggplot(qaDf, aes(qaDf$period, qaDf$Pageviews)) + geom_bar(stat = "summary")

Page_per_Vis<-ggplot(qaDf, aes(qaDf$period, qaDf$`Pages/Visit`)) + geom_bar(stat = "summary")

Rev <- ggplot(qaDf, aes(qaDf$period, qaDf$Revenue)) + geom_bar(stat = "summary")

UNQ <- ggplot(qaDf, aes(qaDf$period, qaDf$`Unique Visits`)) + geom_bar(stat = "summary")

#Plotting the graphs together
ggarrange(Rev, AVG_T, Page_V, Page_per_Vis, UNQ, Rev_Unq)


#My graphs used NB/Retention/Rev_Unq ------

Retention_graph + labs(title = "Retention by period",  x= "Period", y= "Retention" )

Rev_Unq + labs(title = "Unique Revenue by period ",  x= "Period", y= "Revenue/Unique" )

NB_graph + geom_bar(stat = "identity")+ labs(title = "Interested Web Visitors by period",  x= "Period", y= "Interest" )


# Diego Q3 ----------------------------------------------------------------

print('I MADE A CHANGE')

# Possible Modelling  -----------------------------------------------------



# Exploration Data Analysis -----------------------------------------------

# DESCRIPTIVE STATISTICS

" 1) Using data in the Weekly Visits and Financials worksheets, create four column charts (like
                                                                                         Figure 1: Visits to the QA Website per Week) for unique visits over time, revenue over time,
profit over time, and pounds sold over time. You do not have to indicate on these charts the
cutoffs for the four periods.

2) Using the same data, calculate the following summary statistics for visits, unique visits,
revenue, profit, and pounds sold: mean, median, standard deviation, minimum, and
maximum, for the initial, pre-promotion, promotion, and post-promotion periods. So, for
each period you should provide 25 values: five summary measures for each of five variables,
as per the table below for the initial period."

# RELATIONSHIPS BETWEEN VARIABLES

"
5) Start by taking a look at revenue and pounds sold. (Before proceeding, what does your
intuition say about the relationship between these two variables?) Create a scatter diagram of
revenue versus pounds sold. (Revenue should be on the y, or vertical, axis.) Determine the
correlation coefficient of revenue and pounds sold.


6) Now create the scatter diagram of revenue versus visits. (Given your previous work, what
do you expect this plot to look like?) Determine the correlation coefficient of revenue and
visits.

7) Summarize your results. In particular, elaborate on the implications of the relationship
between revenue and number of visits to the website. Feel free to examine any other variable
pairs you think might be important.
"
#Question 1- Daily Visits and Financials 
#Unique Visits 
head(qaDf)
UniqueVisitsbar <- ggplot(qaDf, aes(qaDf$`Week (2008-2009)`, qaDf$`Unique Visits`))
UniqueVisitsbar + geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Unique Visitors") +
  ggtitle("Number of Unique visitors per week")

#Revenue 
revenuebar <- ggplot(qaDf, aes(qaDf$`Week (2008-2009)`, Revenue))
revenuebar + geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Revenue") +
  ggtitle("Revenue Generation per week")

#Profit 
profitbar <- ggplot(qaDf, aes(qaDf$`Week (2008-2009)`, Profit))
profitbar + geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Profit") +
  ggtitle("Profits per week")

#Pounds 
poundsbar <- ggplot(qaDf, aes(qaDf$`Week (2008-2009)`, qaDf$`Lbs. Sold`))
poundsbar + geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Lbs Sold") +
  ggtitle("Lbs Sold per week")



#Question 2- Summary Stats
#Initial Period
#visits
summary(qaDf$Visits[ncol(qaDf):14])
sd(qaDf$Visits[ncol(qaDf):14])

#UniqueVisits
summary(qaDf$`Unique Visits`[ncol(qaDf):14])
sd(qaDf$`Unique Visits`[ncol(qaDf):14])

#Revenue
summary(qaDf$Revenue[ncol(qaDf):14])
sd(qaDf$Revenue[ncol(qaDf):14])

#Profit
summary(qaDf$Profit[ncol(qaDf):14])
sd(qaDf$Profit[ncol(qaDf):14])

#LBs Sold
summary(qaDf$`Lbs. Sold`[ncol(qaDf):14])
sd(qaDf$`Lbs. Sold`[ncol(qaDf):14])

#Pre Promo
#Visits
summary(qaDf$Visits[15:35])
sd(qaDf$Visits[15:35])

#Unique Visits 
summary(qaDf$`Unique Visits`[15:35])
sd(qaDf$`Unique Visits`[15:35])

#Revenue
summary(qaDf$Revenue[15:35])
sd(qaDf$Revenue[15:35])

#Profit
summary(qaDf$Profit[15:35])
sd(qaDf$Profit[15:35])

#Lbs Sold
summary(qaDf$`Lbs. Sold`[15:35])
sd(qaDf$`Lbs. Sold`[15:35])

#Promo
#Visits
summary(qaDf$Visits[36:52])
sd(qaDf$Visits[36:52])

#Unique Visits 
summary(qaDf$`Unique Visits`[36:52])
sd(qaDf$`Unique Visits`[36:52])

#Revenue
summary(qaDf$Revenue[36:52])
sd(qaDf$Revenue[36:52])

#Profit
summary(qaDf$Profit[36:52])
sd(qaDf$Profit[36:52])

#LBs Sold 
summary(qaDf$`Lbs. Sold`[36:52])
sd(qaDf$`Lbs. Sold`[36:52])

#Post Promo
#Visits
summary(qaDf$Visits[53:nrow(FV)])
sd(qaDf$Visits[53:nrow(fv)])

#Unique Visits 
summary(qaDf$`Unique Visits`[53:nrow(FV)])
sd(qaDf$`Unique Visits`[53:nrow(FV)])

#Revenue
summary(qaDf$Revenue[53:nrow(FV)])
sd(qaDf$Revenue[53:nrow(FV)])

#Profit
summary(qaDf$Profit[53:nrow(FV)])
sd(qaDf$Profit[53:nrow(FV)])

#LBs Sold 
summary(qaDf$`Lbs. Sold`[53:nrow(FV)])
sd(qaDf$`Lbs. Sold`[53:nrow(FV)])



#Question 3
PrdRev <- qaDf %>% ggplot(aes(x = period,y = Revenue)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

PrdProf <- qaDf %>% ggplot(aes(x = period,y = Profit)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

PrdLbs <- qaDf %>% ggplot(aes(x = period,y = `Lbs. Sold`)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

PrdVisits <- qaDf %>% ggplot(aes(x = period,y = Visits)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

PrdUniqueVisits<- qaDf %>% ggplot(aes(x = period,y = `Unique Visits`)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

library(ggpubr)
ggarrange(PrdVisits,PrdUniqueVisits,PrdLbs,PrdProf,PrdRev)


#Question 4: Insights from Qs 1-3
# Cost Column
qaDf <- qaDf %>% 
  mutate(cost = Revenue - Profit)
# Price
qaDf <- qaDf %>% 
  mutate(price = Revenue/Lbs._Sold)

# Price Plot 
qaDf %>% ggplot(aes(y = price, x = date)) +
  geom_line() + 
  geom_vline(xintercept = as.numeric(qaDf$date[15]), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(qaDf$date[36]), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(qaDf$date[53]), 
             colour = 'red')

#Correlations between Revenue, Price, and Lbs Sold
cor(qaDf$price, qaDf$Revenue)
cor(qaDf$Lbs._Sold, qaDf$Revenue) 

#Our Insights
  # Price has decreased from quarter 1 to quarter 2 ($32 to $29) 
  # Cost are fluctuating at a similar level, they are not affecting the proffits as much as Revenue (view corr with profit)
  # Hypotheis -- Because Lbs sold went up by 47% in the second quarter, this may be due to the price drop 
    # Revenues didn't increase by same amount!
    # Demand is elastic, affects a lot of the price
    # Price elasticity = Percentage change in quantity demanded / percentage change in price

  # Revenue is what's driving the profit and Lbs Sold is what's driving the revenue (REFER to correlation and Elmir's explanation)  
  # Sticky note -> These are our variables to watch out


#Question 5- Scatter Plot for Rev and Lbs Sold
corr1 <- ggplot(qaDf, aes(qaDf$`Lbs. Sold`, Revenue))
corr1 +geom_point()
cor(qaDf$`Lbs. Sold`,qaDf$Revenue) #Positive Correlation #0.87


#Question 6- Scatter Plot for Rev and Visits
corr2 <- ggplot(qaDf, aes(Visits, Revenue))
corr2 + geom_point()
cor(qaDf$Visits, qaDf$Revenue) #Negative Correlation #-0.06


#Question 7: Insights from Qs 5 & 6
#Revenue vs Lbs Sold
#From our previous analysis and insights from Qs 1-3 we know that Revenue is driven by Lbs Sold 
#Question 5 confirms this further with a strong correlation of about 0.87 

# Revenue vs Visits
# Company is paying for payperclick, people that are entering the website are not buying 
  #products because there is no relationship with the Revenues.
# Hypothesis: QA's Website may not be intuitive, may not be atractive, this is caused by something. 
  # I.e. The website is not good enough in terms of quality and content 
# Possible suggestions: 
  # Based on the hypothesis: Do A/B testing to improve the website. Make it more interactive.  
  # Based on insight: Keep track inquires from website to know sales conversion. In order to know.
    #how many visitors are placing orders through the website. This is to know the ROI of the website.
# Extra suggestion: 
  #Include "How did you hear about us?" in order to better allocate resources in marketing channels.
    #In return -> Giving incentive (discount or a dollar)

qaDf %>%
  ggplot(aes(Visits,Revenue)) +
  geom_point() 
cor(qaDf$Visits, qaDf$Revenue)
cor(log(qaDf$Visits), log(qaDf$Revenue))

qaDf %>% 
  ggplot(aes(`Lbs. Sold`)) +
  geom_histogram(bins = 15, col = 'white') +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(limits = c(0,max(qaDf$`Lbs. Sold`)),  
                     breaks = seq(0, max(qaDf$`Lbs. Sold`), by = 5000))

hist(qaDf$`Lbs. Sold`, breaks = 15
)