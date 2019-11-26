
# Helper Functions --------------------------------------------------------


# Week to Date function ---------------------------------------------------




# Creation of date column with proper format (useful for plots that have time in the X axis)
# NOTE: The first day of the week is what's transformed to Date!!!

# The function returns a Dataframe with a new column called 'date'
# REMEMBER TO ASSIGN THE OUTPUT TO AN OBJECT FOR LATER USE

# If input is the dataframes with visits and financials, use:
# DataFrameVisits 
# dataFrameFinancials
# mergeDfs = TRUE
# Example: 
# convertQAWeeksToDate(dataFrameVisits = visitsWeek1,
#                     dataFrameFinancials = financials1,
#                     mergeDfs = TRUE)


# If input is the already merged dataframe (visits and financials), use:
# dfVisitsAndFinances 
# Example: 
# convertQAWeeksToDate(dfVisitsAndFinance =  visitsWeek1,
#                                 mergeDfs = FALSE) 



convertQAWeeksToDate <- function(dataFrameVisits, 
                                 dataFrameFinancials,
                                 dfVisitsAndFinance,
                                 mergeDfs=FALSE){
  
  
  require(stringr)
  require(dplyr)
  require(lubridate)
  
  
  # The function returns a Dataframe with a new column called 'date'
  # REMEMBER TO ASSIGN THE OUTPUT TO AN OBJECT FOR LATER USE
  
  # If input is the dataframes with visits and financials, use:
    # DataFrameVisits 
    # dataFrameFinancials
    # mergeDfs = TRUE
      # Example: 
            # convertQAWeeksToDate(dataFrameVisits = visitsWeek1,
            #                     dataFrameFinancials = financials1,
            #                     mergeDfs = TRUE)

  
  # If input is the already merged dataframe (visits and financials), use:
    # dfVisitsAndFinances 
      # Example: 
            # convertQAWeeksToDate(dfVisitsAndFinance =  visitsWeek1,
            #                                 mergeDfs = FALSE) 
  
  if(mergeDfs == TRUE){
    
    # Merge Dataframes with visits and financials 
  
  visitsFinancials <- left_join(visitsWeek, financials, by = "Week (2008-2009)")
  
  
  # Convert column as Date
  # Creation of date column with proper format (useful for plots that have time in the X axis)
    # NOTE: The first day of the week is what's transformed to Date!!!
  

  visitsFinancials$date <- paste(str_extract(visitsFinancials$`Week (2008-2009)`, 
                                             pattern = '\\w{3} \\d{1,2} ') ,2008) %>% 
    parse_date_time(orders = 'mdy')
  
  visitsFinancials$date[33:nrow(visitsFinancials)] <- paste(str_extract(visitsFinancials$`Week (2008-2009)`[33:nrow(visitsFinancials)], 
                                                                        pattern = '\\w{3} \\d{1,2} ') ,2009) %>% 
    parse_date_time(orders = 'mdy')
    
  # It's important to transform the POSIXct as Date format
  
  visitsFinancials$date <- as.Date(visitsFinancials$date)
  
  return(visitsFinancials)
  
  } else if(mergeDfs == FALSE){
    
    visitsFinancials <- dfVisitsAndFinance
    
    visitsFinancials$date <- paste(str_extract(visitsFinancials$`Week (2008-2009)`, 
                                               pattern = '\\w{3} \\d{1,2} ') ,2008) %>% 
      parse_date_time(orders = 'mdy')
    
    visitsFinancials$date[33:nrow(visitsFinancials)] <- paste(str_extract(visitsFinancials$`Week (2008-2009)`[33:nrow(visitsFinancials)], 
                                                                          pattern = '\\w{3} \\d{1,2} ') ,2009) %>% 
      parse_date_time(orders = 'mdy')
    
    # It's important to transform the POSIXct as Date format
    
    visitsFinancials$date <- as.Date(visitsFinancials$date)
    
    return(visitsFinancials)
    
    
    
  } else{
    
    print('Damn there was an error, contact Diego')
    
  }

}





# Initial Period: May 25 - Aug 30 (First 14 Rows)
# Pre Promotion: Aug 31 - Jan 24 [15:35]
# Promotion: Jan 25 - May 23 [36:52]
# Post Promotion: May 24 - Aug 29 [53:]

# THIS FUNCTION REQUIRES THE DATASET TO HAVE THE ORIGINAL ROWS AS 
  # IN THE CASE: 66 rows.

convertQAWeeksToPeriod <- function(dataFrame){

  dataFrame$period <- 1
  dataFrame$period[15:35] <- 2
  dataFrame$period[36:52] <- 3
  dataFrame$period[53:nrow(dataFrame)] <- 4
  return(dataFrame)

}