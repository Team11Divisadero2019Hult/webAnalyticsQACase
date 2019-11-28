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


# Do the traditional promos drive web traffic, and in turn drive incremental sales?
#   
#   Define traditional promos (mail, magazines adds. New paid listings - December 2008)
# Traditional promotion -> from (may 25-may31) to  (nov 23-nov 29) 
# Web traffic: total visits (question 2)
# Incremental sales: differences of sales (revenues and pounds sold) between weeks/periods 
# Change in incremental sales vs total visits

#   Assumptions: 
# 1) during the 4 periods, QA has done all the traditional promotions
# 2) The brochures is the only traditional promotion that affected the increase in web visits. 

# a) How visits varied from pre promo to post promo?
# I will do a line chart with X axis as Date, Y axis as Visits and the periods will be lines
# from (may 25-may31) to  (nov 23-nov 29) 

# Initial Period: May 25 - Aug 30 (First 14 Rows)
# Pre Promotion: Aug 31 - Jan 24 [15:35]
# Promotion: Jan 25 - May 23 [36:52]
# Post Promotion: May 24 - Aug 29 [53:]




# The peak of visitors is noticeable in the promotion period

# PreProcessing -----------------------------------------------------------


visitsFinancials <- qaDf

colnames(visitsFinancials) <- str_replace_all(colnames(visitsFinancials),' ', '_')


# Part A ------------------------------------------------------------------



visitsFinancialsForVisits <- visitsFinancials[15:66,]

visitsFinancialsForVisits %>% 
  ggplot(aes(date, Visits)) + 
  # geom_bar(stat = 'identity') +
  geom_line() +
  scale_x_date(date_labels="%d%b",date_breaks  ="2 week") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  geom_vline(xintercept = as.numeric(as.Date('2009-01-14')), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(as.Date('2009-05-21')),
             colour = 'red') +
  xlab('Weeks') +
  ylab('Total Visits') +
  ggtitle('Visits Variation Among Periods') + 
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"))


# Part B Pre Processing ------------------------------------------------------------------


# b)  How incremental sales varied from pre promo to post promo?


incrementalDate <- visitsFinancials[15:66,]

# This function creates the lagged Differene of a given Dataframe
  # It outputs a List, this is done in order to be able to store
  # different variable lengths.

diffLag <- function(lagWeeks,nameOfCol, nameOfDF){
  
  tempObjList <- list()
  
  for(name in nameOfCol){

    laggedName <- paste('Lagged_',name)
    tempObjList[[laggedName]] <- nameOfDF[[name]] %>%
                          diff(lag = lagWeeks) %>%
                            append(values = rep(0,
                                  nrow(nameOfDF) - nrow(nameOfDF) + lagWeeks ),
                              after = 0)
    
    }
  
  return(tempObjList)
  
}


# Incremental Revenue Modelling 
# First, a dataframe all the 1-week lagged variables is created
# This is done in order to reuse any lagged difference variable in the future

# DiffLag lags this variables and returns a list
laggedList <- diffLag(2,c("Visits",
            "Unique_Visits",
            "Pageviews",
            "Pages/Visit",
            "Avg._Time_on_Site_(secs.)",
            "Bounce_Rate","%_New_Visits",
            "Revenue",
            "Profit",
            "Lbs._Sold",
            "Inquiries"),incrementalDate) 

# Convert the list to a dataframe, 
 # if the elements have different lengths this will throw an error

laggedDf <- data.frame(matrix(unlist(laggedList), 
                  nrow=length(laggedList[[1]]), 
                  byrow=FALSE)) 

# Create appropriate names of the columns in order to distinguish them 
  # if merged with original dataset 

laggedNames <- c("lagged_Visits", 
                 "lagged_Unique_Visits", 
                 "lagged_Pageviews", 
                 "lagged_Pages/Visit", 
                 "lagged_Avg._Time_on_Site_(secs.)", 
                 "lagged_Bounce_Rate", 
                 "lagged_%_New_Visits", 
                 "lagged_Revenue", 
                 "lagged_Profit", 
                 "lagged_Lbs._Sold", 
                 "lagged_Inquiries")

colnames(laggedDf) <- laggedNames



# Rename Variables for ggplot to have a more descriptive name
  # New columns in incrementalDate are created
  # NOTE: Incremental date contains the observations from prepromotion to 
    # Postpromotion. We are considireing the brochure as the main traditional promotion
      #thus,  the initial period is not included in the following graphs

incrementalDate$incrementalSalesLbsSold <- laggedDf$lagged_Lbs._Sold
incrementalDate$incrementalRevenue <- laggedDf$lagged_Revenue



# Part B.1 Incremental Sales ----------------------------------------------------------------


# plot: Line chart, y axis Incremental Sales in Lbs, x axis Weeks
# Incremental sales vary more during the promotion period, i n general is not
# Stable
# I wouldn't include this graph in the main analysis, there's no actionable insight

incrementalDate %>% 
  ggplot(aes(date, incrementalSalesLbsSold)) + 
  geom_line() +
  scale_x_date(date_labels="%d%b",date_breaks  ="2 week") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  geom_vline(xintercept = as.numeric(as.Date('2009-01-14')), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(as.Date('2009-05-21')),
             colour = 'red') +
  xlab('Weeks') +
  ylab('Incremental Sales') +
  ggtitle('Incremental Sales Variation Among Periods') + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))

# Boxplot among periods
# Same result as before, more variation during the promotion
incrementalDate %>% 
  ggplot(aes(factor(period), incrementalSalesLbsSold)) +
  geom_boxplot() + 
  xlab('Period') +
  ylab('Incremental Sales (Lbs. Sold)') +
  ggtitle('Incremental Sales Variation Among Periods',
          subtitle = 'Incremental Sales: Future Lbs. Sold - Past Lbs. Sold') + 
  scale_x_discrete(labels = c('Pre-promotion','Promotion', 'Post-promotion')) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))



# Part B.2 Inc Revenue ----------------------------------------------------------------

# Yield Same results as before, more variation in the promotion period, 
# The prepromotion had higher revenue
# Not sure if to include this, no actionable insight here

# Incremental Revenue
incrementalDate %>% 
  ggplot(aes(date, Revenue)) + 
  geom_line() +
  scale_x_date(date_labels="%d%b",date_breaks  ="2 week") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  geom_vline(xintercept = as.numeric(as.Date('2009-01-14')), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(as.Date('2009-05-21')),
             colour = 'red') +
  xlab('Weeks') +
  ylab('Incremental Revenue') +
  ggtitle('Incremental Sales Variation Among Periods') + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))

# Boxplot among periods
incrementalDate %>% 
  ggplot(aes(factor(period), incrementalRevenue)) +
  geom_boxplot() + 
  xlab('Period') +
  ylab('Incremental Revenue') +
  ggtitle('Incremental Revenue Variation Among Periods',
          subtitle = 'Incremental Revenue: Future Revenue - Past Revenue') + 
  scale_x_discrete(labels = c('Pre-promotion','Promotion', 'Post-promotion')) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))



# Modelling  --------------------------------------------------------------



# However, this analysis is just for the difference of one week!! 
# Question: How incremental sales vary when we extend the lag week?
# We can compute a moving average of the difference to know how much
# sales vary among periods

# Goal: Find best way to model incremental sales and future sales


# Incremental Revenue Modelling -------------------------------------------


columnNames <-c("Visits",
                "Unique_Visits",
                "Pageviews",
                "Pages/Visit",
                "Avg._Time_on_Site_(secs.)",
                "Bounce_Rate","%_New_Visits",
                "Inquiries") 

# This function crates a dataframe with 
  # n lagged weeks Revenue.
   #  I.e. Revenue shifts m rows. 
  # Then n is indexed from m+1 to nrow(df)
# The output of this function is the DF that 
 # is going to be used in the modelling

laggedModelRevPrep <- function(lagWeeks,columnName) {
  
  # The lagged dataframe is created
  tempDiffLagDf <- diffLag(lagWeeks,columnName,
                           qaDf) %>% 
    unlist() %>% 
    matrix( nrow=nrow(qaDf), 
            byrow=FALSE)
  
  # Name of lagged revenue is creted
  colnames(tempDiffLagDf) <- paste('Lagged_',columnName,sep = '')
 
  # Following columns Dropped: 
  # Week (2008-2009), Revenue, date, period, % New Visits, Profit, Lbs. Sold
  # This is done to not include any of this columns in the model
  qaDf[,c(-1,-9,-13,-14,-8,-10,-11)]
  
  tempDiffLagDf <- tempDiffLagDf %>% 
    cbind(qaDf[,c(-1,-9,-13,-14,-8,-10,-11)])
  
  startIndex <- lagWeeks + 1
  
  tempDiffLagDf <-tempDiffLagDf[startIndex:nrow(tempDiffLagDf), ]
  
  return(tempDiffLagDf)
}



# Incremental Sales have the best fit on the 2nd week lag R2 = 0.20
# Data better to predict values in 2 weeks. This is the prediction to of 
# incremental revenue

for (i in 1:10){
  
  laggedLmDf <- laggedModelRevPrep(i,"Revenue")
  
  laggedModelTemp <- lm(laggedLmDf$Lagged_Revenue ~ . ,data = laggedLmDf) %>% summary()
  
  paste('Lag:',1,' --- ', print(laggedModelTemp$r.squared),'\n')
  
  
}

# THIS IS CREATING A MODEL FOR INCREMENTAL SALES
# 4 WEEKS IS THE OPTIMAL POINT IN TIME
# VISITS AND UNIQUE VISITS ARE THE ONLY SIGNIFICANT VARIABLES < 0.05 
# THE INTERPRETATION HAS NO LOGIC BUSINESS IMPLICATION, 
  # E.G. THE HIGHER THE BOUNCE RATE, THE HIGHER THE SALES 
# ADJ R SQUARED IS 0.122 AND MULTIPLE R SQUARED IS 0.194, MODEL P VALUE: 0.02981

# laggedIncLbsSold <- laggedModelRevPrep(4,"Lbs. Sold")
# 
# laggedModel <- lm(`Lagged_Lbs. Sold` ~ . , data = laggedIncLbsSold)
# lm(formula = `Lagged_Lbs. Sold` ~ Visits + `Unique Visits` + 
#      `Pages/Visit` + `Bounce Rate` + Inquiries, data = laggedIncLbsSold) %>% summary()
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)     -103565.93   57951.88  -1.787  0.07933 . 
# Visits              254.68      79.88   3.188  0.00234 **
# `Unique Visits`    -264.50      82.26  -3.216  0.00216 **
# `Pages/Visit`     15866.72    9465.35   1.676  0.09926 . 
# `Bounce Rate`     99188.50   58121.13   1.707  0.09344 . 
# Inquiries          -663.82     414.67  -1.601  0.11504

# Now we take a closer look to the model
# Page views is the only significant variable of the model
# Beta Estimate is positive

laggedIncRevDf <- laggedModelRevPrep(2, "Revenue")


laggedModel <- lm(laggedIncRevDf$Lagged_Revenue ~ . ,data = laggedIncRevDf) 
laggedModel %>% summary()


# Variable selection through stepwise regression
library(MASS)
stepAIC(laggedModel, direction = 'backward')

# The linear combination of visits and pageviews yields the best model
# The model has a p-value of 0.002 and an R square of 0.15
# Multiple R-squared:  0.2097,	Adjusted R-squared:  0.1109 
# Beta Coefficients: 
  # Visits         -638
  # Pageviews       491

laggedIncRevFinal <-lm(Lagged_Revenue ~ Visits + 
     Pageviews, data = laggedIncRevDf)

laggedIncRevFinal  %>% summary()

require(Mcomp)

# Visualizing Multiple Regression
# Probably including this inthe appendix
library(ggiraphExtra)
ggPredict(laggedIncRevFinal, interactive = TRUE)


# The less amount of weekly visits and the greater amount of weekly page views 
# will yield
# a better revenue in approximately 2 weeks

# Lagged Revenue Model ----------------------------------------------------


# Now let's see how we can improve the future revenue magnitude # 

# We print the output up to a 10-week lag

for(lag in 1:10){
  
  
  laggedRev <- lag(qaDf$Revenue,n = lag)
  laggedRev[is.na(laggedRev)] <- 0
  laggedQa <- qaDf[,c(-1,-9,-13,-14,-8,-10,-11)]
  laggedQa$laggedRev <- laggedRev
  # laggedQa <- laggedQa %>% scale() %>% as.data.frame()
  startIndex1 <- lagNumber +1
  laggedQa <-laggedQa[startIndex1:nrow(laggedQa), ]
  
  # print(cor(qaModel$laggedRev, qaModel$`Unique Visits`))
  tempModel <- lm(laggedQa$laggedRev ~ . , data = laggedQa) %>% summary()
  tempModel$adj.r.squared %>% print()
  
}

# More in detail, best model .16 R squared 3 week lag

lagNumber <- 3
qaModel <- visitsFinancials[,c(-1,-9,-13,-14,-10,-11)]
laggedRev <- lag(qaDf$Revenue,n = lagNumber)
laggedRev[is.na(laggedRev)] <- 0
qaModel$laggedRev <- laggedRev
startIndex <- lagNumber +1
qaModel <-qaModel[startIndex:nrow(qaModel), ]
qaModel<- scale(qaModel) %>% as.data.frame()

# Check Bounce Rate and Inquiries
qaLagRev <- lm(qaModel$laggedRev ~ . , data = qaModel)
qaLagRev  %>% summary()

# Variable Selection
library(MASS)
stepAIC(qaLagRev, direction = 'backward')


# Best Linear Model for lagged Revenue of 3 weeks
# The more Visits, Pages/Visit, AvgTime, BounceRate, and inquiries, 
# The higher the revenue in 3 weeks
# The less amount of pageViews, the higher revenue in 3 weeks. 
# THis model doesn't make much sense, the interecept is positive despite they are 
# losing money in the future!  
# Visits                       1.082930e+00 # NOT SIGNIFICANT!
# Pageviews                   -1.152345e+00
# `Pages/Visit`                1.116428e+00
# `Avg. Time on Site (secs.)`  6.498485e-01
# `Bounce Rate`                1.615064e+00
# Inquiries                    2.901167e-01

lm(formula = qaModel$laggedRev ~ Visits + Unique_Visits + `Pages/Visit`, 
   data = qaModel) %>% summary()

colnames(qaModel)[c(4,5,7)] <- c('pagesPerVisit', 'avgTimeOnSite','percentNewVisits' ) 

qaLagRevBest <- lm(formula = laggedRev ~ Visits + Pageviews + pagesPerVisit + 
                     avgTimeOnSite + Bounce_Rate + Inquiries, 
                   data = qaModel) 

qaLagRevBest %>% summary()



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
 exploration
  ggplot(aes(`Lbs. Sold`)) +
  geom_histogram(bins = 15, col = 'white') +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(limits = c(0,max(qaDf$`Lbs. Sold`)),  
                     breaks = seq(0, max(qaDf$`Lbs. Sold`), by = 5000))

hist(qaDf$`Lbs. Sold`, breaks = 15
)