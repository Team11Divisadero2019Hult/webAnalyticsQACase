

# WebQA Analysis Readme ----------------------------------------------------------

"""

Each one can write our final code in each section. 
For simplicity-sake let's use the the same name for the merged dataframe: qaDf
qaDf has the merged visits and financials files and the column date with the 
proper format.

The qaDf is saved in the github repository.
The pre-processing section is for the professor to check how we massaged the data.

NOTE: CAMPAIGN IS TRANSFORMED AS A FACTOR IN THE PREPROCESSING STEP
"""



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


