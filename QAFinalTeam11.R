

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


# Carolina Q1 -------------------------------------------------------------


# Talya Q2 a,b ------------------------------------------------------------


# Elmir Q2 c, d -----------------------------------------------------------


# Diego Q3 ----------------------------------------------------------------



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
