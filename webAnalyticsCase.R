# Weekly Visits
  # May 25 2008, Aug 29, 2009
  # Weekly Web Analytics
  # Visits, Unique Visists, PageViews, Pages/Visit, Avg Time on Site, Bounce Rate, % New Visits


# Financials
  # The records do not indicate which of tsetwd('/home/diego/bamodA/R/cases/webAnalyticsQACase/')hese inquiries were generated directly from the website
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

# setwd('/home/diego/bamodA/R/cases/webAnalyticsQACase/')

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
  
visitsWeek$campaign <- 1
visitsWeek$campaign[15:35] <- 2 
visitsWeek$campaign[36:52] <- 3
visitsWeek$campaign[53:nrow(visitsWeek)] <- 4




# financials 
  # No need to massage anything 
str(financials)

# Left Join financials with visits
visitsFinancials <- left_join(visitsWeek, financials, by = "Week (2008-2009)")

str(visitsFinancials)
colnames(visitsFinancials) <- str_replace_all(colnames(visitsFinancials),' ', '_')
colnames(visitsFinancials)

# Creation of date column with proper format (useful for plots that have time in the X axis)
      
    # Example: paste(str_extract(visitsFinancials$`Week (2008-2009)`, pattern = '\\w{3} \\d{1,2} ') ,2008) %>% parse_date_time(orders = 'mdy')

visitsFinancials$date <- paste(str_extract(visitsFinancials$`Week_(2008-2009)`, 
                                           pattern = '\\w{3} \\d{1,2} ') ,2008) %>% 
  parse_date_time(orders = 'mdy')

visitsFinancials$date[33:nrow(visitsFinancials)] <- paste(str_extract(visitsFinancials$`Week_(2008-2009)`[33:nrow(visitsFinancials)], 
                                                                      pattern = '\\w{3} \\d{1,2} ') ,2009) %>% 
  parse_date_time(orders = 'mdy')


# First Exploration Iteration ---------------------------------------------


# Website Generating Interest? --------------------------------------------



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






# How many people visit the website? How do they come to the websi --------

# Load demographic Sheets
demographicSheetNames <- excel_sheets('webAnalyticsCase.xls')[7:11]

for(sheet in demographicSheetNames){
  
  assign(sheet,
         read_excel('webAnalyticsCase.xls',sheet = sheet))
  
}

# How many people visit the website? How do they come to the website?

# Total number of unique visitors to the website
    # 65,287
  uniqueVisits<- visitsFinancials$`Unique Visits` %>% sum()

# Total number of visitors to the website 
  # 69k 
totalVisits <- visitsFinancials$Visits %>% sum()

# Number of people that return to the website # 4144
# NEED TO DO MORE WORK, IT DOESN't SUMS UP!!
# totalVisits - uniqueVisits

# Extra QUESTION: Does the number of returning customers impacts the revenue? 
# visitsFinancials <- mutate(visitsFinancials, 
#                            returningVisitors = visitsFinancials$Visits - visitsFinancials$`Unique Visits` )
####

# Where do Clients come from?
# Region
# The website is not attracting the south asia market 
# THIS DOESN'T MEAN THAT THIS REGIOSN DRIVE THE REVENUE
ggplot(TopTenGeographicSources, aes(x = reorder(TopTenGeographicSources$`Top Ten Geographic Sources by Sub Continent Region`,-Visits),
                                    y = Visits)) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab('Regions') +
  ylab('Number of Visits') +
  ggtitle('Visits per Region')

# Referring Sites
# Mostly Google Adwords and Google Page ad
ggplot(TopTenReferringSites, aes(x = reorder(TopTenReferringSites$`Top Ten Referring Sites`,-Visits),
                                    y = Visits)) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab('SEM Sources') +
  ylab('Number of Visits') +
  ggtitle('Visits per Referring Site')


# Mobile Phones vs Computers 
# Percentage of customers that used computers to visualize the webpage
TopTenOperatingSystems$Visits[1:3] %>% sum() / TopTenOperatingSystems$Visits %>% sum() *100


ggplot(visitsFinancials, aes(x = reorder(visitsFinancials$campaign,-Profit),
                                 y = Profit)) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab('Regions') +
  ylab('Profits by Promotion Period') +
  ggtitle('Visits per Referring Site')

ggplot(visitsFinancials, aes(x = reorder(visitsFinancials$campaign,- visitsFinancials$`Lbs. Sold`),
                             y = visitsFinancials$`Lbs. Sold`)) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab('Period') +
  ylab('Number of Pounds Sold Per Period') +
  ggtitle('Pounds Sold By Period')



# Data Analysis Questions -------------------------------------------------
Question2 <- function(){}
# 2) Using the same data, calculate the following summary statistics for visits, unique visits,
# revenue, profit, and pounds sold: mean, median, standard deviation, minimum, and
# maximum, for the initial, pre-promotion, promotion, and post-promotion periods.So, for
# each period you should provide 25 values: five summary measures for each of five variables,
# as per the table below for the initial period.

# TO DEBUG
# visitsFinancials %>% 
#   group_by(campaign) %>% 
#   summarize(mean = mean(Revenue), 
#             median = median(Revenue))
customSummary <- function(variable){
  visitsFinancials %>% 
  group_by(campaign) %>% 
  summarize(mean = mean(variable), 
            median = median(variable), 
            standardDev = sd(variable), 
            Max = max(variable), 
            Min = min(variable))
}


# def customSum(a):
  # print(a)


customSummary(Visits) 
customSummary(Visits) 
customSummary(visitsFinancials$Revenue) %>% View()
customSummary(visitsFinancials$Profit) %>% View()
customSummary(visitsFinancials$`Lbs. Sold`) %>% View()

Question3<-function(){}
# 3) Create a column chart of the mean visits over the four periods—that is, your chart should
# have four columns, the first representing the mean visits during the initial period, the second
# representing the mean visits during the pre-promotion period, etc. Create four more such
# charts, this time using the mean unique visits, mean revenue, mean profit, and mean pounds
# sold statistics

campRev <-visitsFinancials %>% ggplot(aes(x = campaign,y = Revenue)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

campProf <- visitsFinancials %>% ggplot(aes(x = campaign,y = Profit)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

campLbs <- visitsFinancials %>% ggplot(aes(x = campaign,y = Lbs._Sold)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

campVisits <- visitsFinancials %>% ggplot(aes(x = campaign,y = Visits)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

campUniqueVisits<- visitsFinancials %>% ggplot(aes(x = campaign,y = Unique_Visits)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

campUniqueVisits<- visitsFinancials %>% ggplot(aes(x = campaign,y = Unique_Visits)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

campCost<- visitsFinancials %>% ggplot(aes(x = campaign,y = cost)) +
  geom_bar(stat = 'summary',fun.y = 'mean')



library(ggpubr)
ggarrange(campVisits,campUniqueVisits,campLbs,campProf,campRev, campCost)


Question4<- function(){}

# 4. Analysis of Questions 1, 2 and 3
  
# Cost Column
visitsFinancials <- visitsFinancials %>% 
  mutate(cost = Revenue - Profit)

# # Unit Cost Columm
# visitsFinancials <- visitsFinancials %>%
#   mutate(unitCost = cost/Lbs._Sold)
# # Unit Cost don't affect profit 
# lm(visitsFinancials$Profit ~ visitsFinancials$unitCost , 
#    data = visitsFinancials) %>% summary()

# Price
visitsFinancials <- visitsFinancials %>% 
  mutate(price = Revenue/Lbs._Sold)

# Price Plot 
visitsFinancials %>% ggplot(aes(y = price, x = date)) +
  geom_line() + 
  geom_vline(xintercept = as.numeric(visitsFinancials$date[15]), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(visitsFinancials$date[36]), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(visitsFinancials$date[53]), 
             colour = 'red')
# The price decreased from quarter 1 to quarter 2 (32 to 29) 
# Cost are fluctuating at a similar level, they are not affecting the proffits (view corr with profit)
# Pounds sold they went up by 47% in the second quarter, might be due to the price drop -- Hypothesis
          # Revenues didn't increase by same amount!
          # Demand is elastic, affects a lot the price
# Price elasticity = Percentage change in quantity demanded / percentage change in price
# library(quantmod)
# Delt(visitsFinancials$price) 
# The external variable 

# Revenue is what's driving the profit, Lbs Sold is what's driving the revenue REFER to correlation and Elmir's explanation  
# Sticky note -> Variables to watch out

cor(visitsFinancials$price, visitsFinancials$Revenue)
cor(visitsFinancials$Lbs._Sold, visitsFinancials$Revenue)


# To do: Questions 5, 6, and 7. Ask Prof if we need to do Q8. Page 7



