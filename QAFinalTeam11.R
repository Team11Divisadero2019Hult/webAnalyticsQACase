

# WebQA Analysis Readme ----------------------------------------------------------

"

Each one can write our final code in each section. 
For simplicity-sake lets use the the same name for the merged dataframe: qaDf
qaDf has the merged visits and financials files and the column date with the 
proper format.

The qaDf is saved in the github repository.
The pre-processing section is for the professor to check how we massaged the data.

NOTE: PERIOD IS TRANSFORMED AS A FACTOR IN THE PREPROCESSING STEP

ColNames:

Week (2008-2009) 
Visits 
Unique Visits 
Pageviews 
Pages/Visit 
Avg. Time on Site (secs.) 
Bounce Rate 
% New Visits 
Revenue 
Profit 
Lbs. Sold 
Inquiries 
date 
period
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


# Carolina Q1 -------------------------------------------------------------


# Talya Q2 a,b ------------------------------------------------------------


# Elmir Q2 c, d -----------------------------------------------------------


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
# 






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
  ggtitle('Visits Variation Among Periods')


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
  ggtitle('Incremental Sales Variation Among Periods')

# Boxplot among periods
# Same result as before, more variation during the promotion
incrementalDate %>% 
  ggplot(aes(factor(period), incrementalSalesLbsSold)) +
  geom_boxplot() + 
  xlab('Period') +
  ylab('Incremental Sales') +
  ggtitle('Incremental Sales Variation Among Periods') + 
  scale_x_discrete(labels = c('Pre-promotion','Promotion', 'Post-promotion'))


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
  ggtitle('Incremental Sales Variation Among Periods')

# Boxplot among periods
incrementalDate %>% 
  ggplot(aes(factor(period), incrementalRevenue)) +
  geom_boxplot() + 
  xlab('Period') +
  ylab('Incremental Sales') +
  ggtitle('Incremental Sales Variation Among Periods') + 
  scale_x_discrete(labels = c('Pre-promotion','Promotion', 'Post-promotion'))


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

laggedModelRevPrep <- function(lagWeeks) {
  
  # The lagged dataframe is created
  tempDiffLagDf <- diffLag(lagWeeks,'Revenue',
                           qaDf) %>% 
    unlist() %>% 
    matrix( nrow=nrow(qaDf), 
            byrow=FALSE)
  
  # Name of lagged revenue is creted
  colnames(tempDiffLagDf) <- 'Lagged_Revenue'
 
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
  
  laggedLmDf <- laggedModelRevPrep(i)
  
  laggedModelTemp <- lm(laggedLmDf$Lagged_Revenue ~ . ,data = laggedLmDf) %>% summary()
  
  paste('Lag:',1,' --- ', print(laggedModelTemp$r.squared),'\n')
  
  
}


# Now we take a closer look to the model
# Page views is the only significant variable of the model
# Beta Estimate is positive

laggedIncRevDf <- laggedModelRevPrep(2)


laggedModel <- lm(laggedIncRevDf$Lagged_Revenue ~ . ,data = laggedIncRevDf) 
laggedModel %>% summary()



# Variable selection through stepwise regression
library(MASS)
stepAIC(laggedModel, direction = 'backward')

# The linear combination of visits and pageviews yields the best model
# The model has a p-value of 0.002 and an R square of 0.15
# Beta Coefficients: 
  # Visits         -638
  # Pageviews       491

laggedIncRevFinal <-lm(Lagged_Revenue ~ Visits + 
     Pageviews, data = laggedIncRevDf)

laggedIncRevFinal  %>% summary()



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

# Doesn't work
# ggPredict(qaLagRevBest, interactive = TRUE)

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
