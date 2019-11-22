setwd('/home/diego/bamodA/R/cases')

# Weekly Visits
  # May 25 2008, Aug 29, 2009
  # Weekly Web Analytics
  # Visits, Unique Visists, PageViews, Pages/Visit, Avg Time on Site, Bounce Rate, % New Visits


# Financials
  # The records do not indicate which of these inquiries were generated directly from the website
  # May 25 2008, Aug 29, 2009
  # Weekly Web Analytics
  # Revenue
  # Profit
  # Lbs Sold
  # Inquiries

# Lbs Sold 
  # Weekly Data
  # Week of Jan 3, 2005 to July 19, 2010
  # Week 
  # Pounds Sold

# Daily Visits 
  # May 25, 2008 - Aug 29,2009
  # Day, Visits

# Import Data -------------------------------------------------------------


library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
library(plotly)


visitsWeek <- read_excel("webAnalyticsCase.xls", 
                               sheet = "Weekly Visits")

financials <- read_excel('webAnalyticsCase.xls', 
                              sheet = 'Financials')

lbSold <- read_excel("webAnalyticsCase.xls", 
                         sheet = "Lbs. Sold")

visitsDaily <- read_excel("webAnalyticsCase.xls", 
                         sheet = "Daily Visits")


# Case Info -----------------------------------------------------
# Goals of website:
# Drive new Sales
# Make product and contact information available
# Give or add legitimacy to their brand

# Asian Market was viewed as important because the wanted to shift towards pacific Rim. <--

# QA commissioned a professionally produced brochure providing an overview of the
# company’s products and services and sent it to potential customers in mid-December 2008

# General Questions -------------------------------------------------------

# How many people visit the website? How do they come to the website?
#   • Is the website generating interest, and does this interest yield actual sales?
#   • Do traditional promotions drive web traffic, and in turn drive incremental sales?
#   • How can visits to the website best be modeled?
#   • Where and how should QA advertise?


# Goal --------------------------------------------------------------------

# Examin the value of QA's website by combining website metrics, financial measures, and amount of merchandise sold. 
# One Focus: Analysis for senior management regarding the effectiveness of QA's promotional effort


# Structure ---------------------------------------------------------------

# Two parts: 1. Executive summary with recommendations 2. Responses to the Analysisis section

# They want to expand, increase profit margins
# It is necessary to check their topline and bottomline
# Question: How do we improve their topline while cutting costs?

# Topline
# 1. How the different stages (initial,prepromo,promo,post) impacted the profit margins? 

# Bottomline 
# 1. Fixed cost and variable costs
# 2. How to minimize costs (Find weights)

# Data Massaging ----------------------------------------------------------

# Weekly Visits

str(visitsWeek)

  # Week Shouldn't be a factor
  # This column can be better usef if new column is created with pre, during, post promotion 
  # Rest of columns need to be numeric

  # Initial period Creation
# Initial Period: May 25 - Aug 30 (First 14 Rows)
# Pre Promotion: Aug 31 - Jan 24 [15:35]
# Promotion: Jan 25 - May 23 [36:52]
# Post Promotion: May 24 - Aug 29 [53:]
  

visitsWeek$campaign <- 0
visitsWeek$campaign[15:35] <- 1 
visitsWeek$campaign[36:52] <- 2
visitsWeek$campaign[53:nrow(visitsWeek)] <- 3



# financials 
  # No need to massage anything 
str(financials)

# Left Join financials with visits
visitsFinancials <- left_join(visitsWeek, financials, by = "Week (2008-2009)")

str(visitsFinancials)


# Creation of date column with proper format (useful for plots that have time in the X axis)
      
    # Example: paste(str_extract(visitsFinancials$`Week (2008-2009)`, pattern = '\\w{3} \\d{1,2} ') ,2008) %>% parse_date_time(orders = 'mdy')

visitsFinancials$date <- paste(str_extract(visitsFinancials$`Week (2008-2009)`, pattern = '\\w{3} \\d{1,2} ') ,2008) %>% 
  parse_date_time(orders = 'mdy')

visitsFinancials$date[33:nrow(visitsFinancials)] <- paste(str_extract(visitsFinancials$`Week (2008-2009)`[33:nrow(visitsFinancials)], 
                                                                      pattern = '\\w{3} \\d{1,2} ') ,2009) %>% 
  parse_date_time(orders = 'mdy')


# First Exploration Iteration ---------------------------------------------

# Want to know how the topline varies among the periods. This is to address the first question:
      # Is the website generating interest, and does this interest yield actual sales?
  
# It seems that is generating attention, it seems that this attention is due to the 
# Promotion 
ggplot(visitsFinancials, aes(x = factor(campaign), y = Visits)) + 
  geom_boxplot() +
  xlab('Time Period Segmented by marketing promotion') +
  scale_x_discrete(labels = c('Initial','Pre-promotion','Promotion', 'Post-promotion'))



# The promotion did not generated any Revenue
# Question: It was because of a bad promotion or the company was doing bad?
ggplotly(ggplot(visitsFinancials, aes(x = factor(campaign), y = Revenue)) + 
  geom_boxplot() +
   xlab('Time Period Segmented by marketing promotion') +
  scale_x_discrete(labels = c('Initial','Pre-promotion','Promotion', 'Post-promotion')) +
  scale_y_continuous(label = dollar_format()  ))
    
    # View outliers by indices 
    which(visitsFinancials$Revenue %in% boxplot(visitsFinancials$Revenue ~ visitsFinancials$campaign)$out)
   visitsFinancials[ visitsFinancials$Revenue %in% boxplot(visitsFinancials$Revenue ~ visitsFinancials$campaign)$out,]

# However if we do the sum of revenue per category, there is a spike in the prepromotion period
    # We can see the sum of revenue per category  
    aggregate(visitsFinancials$Revenue, by = list(visitsFinancials$campaign), FUN=sum) 
    
# let's see what drives this growth in revenue by inspecting the distribution of revenue per category 
    ggplot(visitsFinancials,aes(x=Revenue))+
      geom_histogram()+
      # geom_density() +
      facet_grid(~campaign)+ 
      theme_bw()



# Explore more how Revenue varies per 



ggplot(visitsFinancials) + 
  # geom_line(aes(x = date , y = Revenue)) +
  geom_line(aes(x = date, y = visitsFinancials$`Avg. Time on Site (secs.)`)) +
  geom_vline(xintercept = as.numeric(visitsFinancials$date[15]), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(visitsFinancials$date[36]), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(visitsFinancials$date[53]), 
             colour = 'red')

apply(visitsFinancials, MARGIN = 2, FUN = class)
# colClasses <- str(visitsFinancials)

lm(Revenue ~ Visits + visitsFinancials$Pageviews + visitsFinancials$`Avg. Time on Site (secs.)` +
     visitsFinancials$`Bounce Rate` + visitsFinancials$`% New Visits` + visitsFinancials$Inquiries, visitsFinancials) %>% summary()

lm(Revenue ~ Visits + visitsFinancials$Pageviews + visitsFinancials$`Avg. Time on Site (secs.)` +
     visitsFinancials$`Bounce Rate` + visitsFinancials$`% New Visits` + visitsFinancials$Inquiries +
     visitsFinancials$`Lbs. Sold`, visitsFinancials) %>% summary()

as.numeric(visitsFinancials$date[])

cor(visitsFinancials$Revenue, visitsFinancials$`Avg. Time on Site (secs.)`)


