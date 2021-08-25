###############################################################################
##                                                                           ##
## Project        : Uber case study on Supply Demand gap                     ## 
## Script purpose : To identify the root cause of the Uber's supply-demand   ##
##                  problem (i.e. cancellation and non-availability of cars) ##
##                  and recommend ways to improve the situation              ##
##                                                                           ##
###############################################################################

# Check and Import required libraries
options(warn = -1)
libs = c("dplyr", "ggplot2", "lubridate","formattable")
install.lib <- libs[!libs %in% installed.packages()]
for (pkg in install.lib) 
 install.packages(pkg, dependencies = TRUE)
loadlib     <- lapply(libs, library, character.only = TRUE) # load them
remove(list = ls())
options(warn = 0)

# Import input file
uber_requests <- read.csv("Uber Request Data.csv", 
                           stringsAsFactors = F )

# Looking at the data
dim(uber_requests)
str(uber_requests)                               # 6745 obs. of  6 variables
glimpse(uber_requests)
summary(uber_requests)

# Converting columns(Pickup.point & Status) to factors since these 
# are categorical 
uber_requests$Pickup.point  <-  factor(uber_requests$Pickup.point )
uber_requests$Status        <-  factor(uber_requests$Status)
uber_requests$Driver.id     <-  factor(uber_requests$Driver.id)

# Checking for duplicates
length(unique(uber_requests$Request.id)) != dim(uber_requests)[1]
# 6745 IDs matches with 6745 total observations. So no duplicates

# Checking for NA Values
any(is.na(uber_requests))                        # TRUE
sum(is.na(uber_requests))                        # 6564 NAs
sum(complete.cases(uber_requests))               # Only 2831 complete cases
summary(uber_requests)                           # 2650 NAs in Driver.Id column
# There seems to be some NAs in other columns as well since total NAs is 6564 
# and NAs in driver column is only 2650 

# Lets examine all columns for NA values
colSums(is.na(uber_requests))
# Request.id, Pickup.point , Status, Request.timestamp all return 0
# indicating no NA values in those columns
# Driver.id & Drop.timestamp columns have NAs. 

# Lets look at the complete row information where Driver.id is NA
ind <- which(is.na(uber_requests$Driver.id)) 
head(uber_requests[ind, ])
tail(uber_requests[ind, ])
# When Driver.id is NA, the Status looks to be mostly 'No Cars Available' 
# Lets check the count on the Status
table(uber_requests[ind, ]$Status)
# Cancelled No Cars Available    Trip Completed 
#   0              2650                 0 
# Number of 'No cars Available' matches with 2650 NAs in the Driver.id column
# which makes sense that for statuses with 'no cars available' Driver.id is NA

# Lets look at the complete row information where Drop.timestamp is NA
ind <- which(is.na(uber_requests$Drop.timestamp))
head(uber_requests[ind, ])
tail(uber_requests[ind, ])
# When Drop.timestamp is NA, the Status looks to be mostly 'No Cars Available' 
# or  'Cancelled'
# Lets check the count on the Status
table(uber_requests[ind, ]$Status)
# Cancelled   No Cars Available    Trip Completed 
#   1264           2650               0 
# 1264 (Cancelled) + 2650 (No Cars Available) = 3914. This atches with 3914 NAs
# in the Drop.timestamp column which makes sense when there are no cars 
# available or when the trip is cancelled, Drop.timestamp is NA
remove(ind)  # Cleanup temporary variable


###############################################################################
#                                                                             #
#                             Data Cleaning                                   #
#                                                                             #
###############################################################################
# Coerce date time character columns into standard Date time objects
uber_requests$Request.timestamp <- 
 parse_date_time(uber_requests$Request.timestamp, 
                  orders = c('dmy_HMS', "dmy_HM"))

uber_requests$Drop.timestamp    <- 
 parse_date_time(uber_requests$Drop.timestamp, 
                  orders = c('dmy_HMS', "dmy_HM"))

# Now that we have standardised date timstamps 
# lets check if we have any discrepencies between request timestamp 
# and drop timestamp. Check if there are any requests that had 
# the drop timestamp before the request timestamp
sum(uber_requests$Request.timestamp > uber_requests$Drop.timestamp , na.rm = T)
# Returns 0. So drop date timestamp looks good.

# Lets look at the Structure & Summary again
str(uber_requests)
glimpse(uber_requests)  
tail(uber_requests)                              
# All date formats have been converted to standard formats

# Checking for NA Values again
colSums(is.na(uber_requests))
# The NA values are the same and no new NA values added.

# Checking how many days of data we have 
unique(date(uber_requests$Request.timestamp))

# 2016-07-11 2016-07-12 2016-07-13 2016-07-14 2016-07-15 
# The data provided is only for 5 days 11,12,13,14 & 15  in the month of June 
#  for the year 2016


###############################################################################
#                                                                             #
#              Understanding the data - Metadata  description                 #
#                                                                             #
###############################################################################
#
# Description     - Masked data set containing Uber cab request details
# Source          - UpGrad assignment 
# Format          - .csv (comma seperated values)
# Number of rows  - 6745
# Number of cols  - 6
# Each row has    - Uber cab request detail
# Sampling method - Data is for 5 days (Days 11 through 15 in the month of 
#                   June for the year 2016) 
#
# Column Details
# Request.id       - Type: Number   | A unique identifier of the request 
#                    Missing : 0    | Uniques : 6745
# Request.timestamp- Date timestamp | The date and time at which 
#                                     the customer made the trip request
#                    Missing : 0    | Uniques : 5618
# Drop.timestamp   - Date timestamp | The drop-off date and time, 
#                                     in case the trip was completed 
#                    Missing : 3914 | Uniques : 2599 (2598 excluding NA)
# Pickup.point     - Character      | The point from which the request was made
#                    Missing : 0    | Uniques : 2
# Driver.id        - Number         | The unique identifier of the driver
#                    Missing : 2650 | Uniques : 301 ( 300 excluding NA)
# Status           - Character      | The final status of the trip, that 
#                                     can be either completed, cancelled 
#                                     by the driver or no cars available
#                    Missing : 0    | Uniques : 3
###############################################################################

###############################################################################
#                                                                             #
#                         Derive New variables                                #
#                                                                             #
###############################################################################

#  Derive day of the week from the request time stamp
uber_requests$Request.wday <- 
 wday(uber_requests$Request.timestamp, label = T)

#  Derive Hour of the day from the request time stamp
uber_requests$Request.hour <- 
 format(strptime(hour(uber_requests$Request.timestamp), "%H"), 
                       format = "%l %p")

# We could leave the hour as continuos variable and use it in plots 
# or we could convert hours into 12 hour fromat as factors with levels 
# and use it in plots. I prefer the latter. For continuous variables 
# we usually use Histogram plots with bins to analyse the frequency. 
# In the below bar plots have been used with categorical variables
# in X axis and Frequency in Y axis. 

# Convert the hours as Factors with levels (12 AM < 1 AM...<10 PM <11 PM)
uber_requests$Request.hour <-  
 factor(uber_requests$Request.hour, 
 levels = c("12 AM", " 1 AM", " 2 AM", " 3 AM", " 4 AM", 
            " 5 AM", " 6 AM", " 7 AM", " 8 AM", " 9 AM", 
            "10 AM", "11 AM", "12 PM", " 1 PM", " 2 PM",
            " 3 PM", " 4 PM", " 5 PM", " 6 PM", " 7 PM",
            " 8 PM", " 9 PM", "10 PM", "11 PM"))

# Derive duration of the trips from Request and Drop Timestamps
uber_requests$Trip.duration <- 
 as.numeric(round(difftime(uber_requests$Drop.timestamp,
                 uber_requests$Request.timestamp,units = "mins"),0))


###############################################################################
#                                                                             #
#         Univariate Analysis  - Analysing variation of 1 variable            #
#                                                                             #
###############################################################################

# Question 1:  What is the proportion of demand between airport to city 
#              and city to airport?
table(uber_requests$Pickup.point)
# Airport  City 
#  3238    3507 

percent(prop.table(table(uber_requests$Pickup.point)))
# Airport  City 
#  48%     52% 

# Question 2:  What is the total # of drivers available?
drivers_count <-  length(unique(na.omit(uber_requests$Driver.id))) 
# 300

# Question 3:  What is the hourly demand? 
table(uber_requests$Request.hour)

# Question 4 :  During which hours the demand crosses more than 300
as.data.frame(table(uber_requests$Request.hour)) %>% filter(Freq > 300)

# With the available 300 drivers it wouldn't be possible to  meet the peak 
# demand. We would need additional drivers to meet the peak demand. 

# Question 5:  With the available 300 drivers, what is the expected average 
#              number of trips a driver should complete per week inorder to 
#              meet the demand we have.
round(table(uber_requests$Pickup.point)/drivers_count,0)
# Airport    City 
# 11         12 

# Question 6:  What is the current average # of trips a driver takes
round((table(uber_requests$Pickup.point, 
             uber_requests$Status == "Trip Completed")/drivers_count),0)[,2]
# Airport    City 
# 4          5 

# To meet the available demand a driver should complete atleast 23 trips 
# in a week(Mon - Fri) between airport and city whereas the current average 
# is only 9 trips. Let see why we have a huge gap in supply vs demand


# Question 7:  Which day of the week is expected to have more demand?
uber_requests %>% group_by(Request.wday) %>% summarise(count = n()) %>%
ggplot(aes(x = Request.wday, y = count, fill = Request.wday))        +
 geom_bar(stat = "identity", show.legend = F) + 
 labs(x = "Weekday", y = "Demand", title = "Demand by Weekday") + 
 theme_bw() + 
 theme(plot.title = element_text(hjust = 0.5))

percent(prop.table(table(uber_requests$Request.wday)))[2:6]

# Mon Tue Wed Thu Fri 
# 20% 19% 20% 20% 20% 

# Looks like there are no major fluctations in demand between various weekdays

# Question 8: What is the minimum, max  and average duration of the trips 
summary(uber_requests$Trip.duration)
# Min.  1st Qu.  Median    Mean   3rd Qu.  Max. 
# 21.00   41.00   52.00   52.41   64.00   83.00  

plot_theme <- theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 22,face = 'bold'),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 12))


ggplot(uber_requests %>% filter(!is.na(Trip.duration))) + 
  geom_boxplot(aes(x = Pickup.point, y = Trip.duration, fill = Pickup.point),
               show.legend = F) +
  labs(y = "Trip duration", title = "Trip duration vs Pickup Point") +
   plot_theme

# The average trip duration is about 52 mins and there is no major variations 
#  between Airport and City

# Question 9: What is the proportion of various Statuses?
plot1 <- ggplot(uber_requests) +  theme_classic() + 
          plot_theme
plot1 +  geom_bar(aes(x = Status), fill = c("#F8766D","#619CFF", "#00BA38")) + 
          labs(y = "Frequency", title = "Status Vs Frequency")

percent(prop.table(table(uber_requests$Status)),0)
# Cancelled No Cars Available    Trip Completed 
#  19%               39%               42%

# Question 10: How does the demand look like for each hour?

plot1 +  geom_bar(aes(x = Request.hour, fill = "red"), show.legend = F) +
  labs(x = "Hour of the day", y = "Demand", title = "Hourly demand") 
    

# Question 11: What are the peak hours of the requests? 
# Inorder to identify and visualize the trend over time both line & point
# plot has been used below
Hourly_Request_freq <-  as.data.frame(table(uber_requests$Request.hour))
ggplot(Hourly_Request_freq, aes(x = Var1, y = Freq, group = 1)) +
 geom_line(size = 1.3) +
  geom_point(size = 3) + 
  geom_point(data = filter(Hourly_Request_freq, Freq > 200), 
             col = 'red2', size = 5) +
   labs(x = "Request Hour", y = "Frequency of Requests", 
        title = "Hourly Frequency of Requests") +
    plot_theme
# 4 AM to 10 AM & 5 PM to 10 PM


# Lets look at the same using a Heatmap with day of the week added
# (day of Week v.s Hour of day)
ggplot(as.data.frame(table(uber_requests$Request.wday,
                     uber_requests$Request.hour)[2:6,]), 
       aes(x = Var2, y = Var1)) + 
 geom_tile(aes(fill = (Freq/24)),show.legend = F) + 
  scale_fill_gradient(name = 'Requests/hour', 
                      low = 'white', high = 'red2') +
   labs(y  = "Frequency of Requests",  x = "Request Hour", 
      title = "Hourly Frequency of Requests by Weekday") + 
    plot_theme

# 4 AM to 10 AM & 5 PM to 10 PM across all days

# Question 12 : How does the peak hours look based on the pickup points?
# Question 13 : How does the status vary during the peak hours?

plot1 + 
 geom_bar(aes(x = Request.hour, fill = Status)) + 
  labs(x = "Request time", y = "Frequency of Requests", 
      title = "Hourly Frequency of Requests by Status") +
   scale_fill_manual(values = c("#F8766D","#619CFF", "#00BA38")) +
    facet_grid(factor(uber_requests$Pickup.point, 
                      levels = c("City", "Airport"))~.) 
    

# Lets quantify by looking at the same using a table in terms of 
# proportion of status on a hourly basis using conditional formatting 

uber_requests %>% group_by(Request.hour) %>% 
  summarise("Cancelled" = percent(sum(Status == "Cancelled" ) / 
                                    (count = n()),0),
            "No Cars Available" = percent(sum(Status == "No Cars Available" ) / 
                                            (count = n()), 0),
            "Trip Completed" = percent(sum(Status == "Trip Completed" ) / 
                                         (count = n()), 0),
            "Total Demand" = n()) %>%
  formattable(list(
    "Trip Completed" = color_tile("transparent", "green"),
    "Cancelled" = color_tile("transparent", "red"),
    "No Cars Available" = color_tile("transparent", "red"),
    "Total Demand" = color_tile("transparent", "red")
  ))


# Peak times > 200 Requests 
# > 25% Cancellations @ 4 AM to 10 AM (Morning peak time)
# > 45% unavailability of cars @ & 5 PM to 10 PM  (Evening peak time) 

# Fact to note. When we look within the demand for 
# each hour of the day 'No cars available' seems more when compared 
# to other status during the non peak hours (12 AM to 4 AM )too 
# indicating that more cars are becoming unavailable after 5 PM till 4 AM.
# Drivers logoff for the day after say after midnight and so less cabs are
# available. But since the total demand duing this timeslot is less
# we will focus our analysis on peak hours.

# Lets segment further by creating a new variable 
#  Time.segment based on the peak hours and time slots:
#  Early Morning"          : 12:00 AM -  3:59 AM
#  Morning peak"           :  4:00 AM - 10:59 AM
#  Late Morning            : 11:00 AM - 12:00 PM 
#  Afternoon               : 12:00 PM -  4:59 PM
#  Evening peak            :  5:00 PM -  9:59 PM
#  Late Night"             : 10:00 PM - 11:59 PM

TimeSegment <-  function(x){
 if (x < 4) 
  return("Early Morning")
 else if (x >= 4 & x < 11)
  return("Morning peak")
 else if (x >= 11 & x < 12)
  return("Late Morning")
 else if (x >= 12 & x < 17)
  return("Afternoon")
 else if (x >= 17 & x < 23)
  return("Evening peak")
 else
  return("Late Night")
}

uber_requests$Time.segment <- 
  sapply(as.numeric(uber_requests$Request.hour) - 1, 
                                      TimeSegment)
# Factors start at level 1. The above -1 is to account for 0 (12 AM). 

# Convert the new columns as factors with levels 
# (Early Morning < Morning peak ...< Late Night)
uber_requests$Time.segment <-  
 factor(uber_requests$Time.segment, 
         levels = c("Early Morning", "Morning peak", "Late Morning",
                        "Afternoon", "Evening peak", "Late Night"))


# Question 14 : How does status look for different time segments 
#               especially during peak hours? 

# Question 15 : How does the demand look like based on pickup point? 

plot2 <- ggplot(uber_requests, aes(x = Time.segment)) + plot_theme
  

plot2 +
 geom_bar(aes(fill = Status), position = "dodge") + 
  labs(x = "Time Segment", y = "Frequency of Status", 
       title = "Time Segment vs Frequency by Status") +
   scale_fill_manual(values = c("#F8766D","#619CFF", "#00BA38")) +
    facet_grid(factor(uber_requests$Pickup.point, 
                      levels = c("City", "Airport"))~.) 


# Challenge 1 : Morning peak hour window(4 AM to 10 AM) - 
#               Demand is higher in City. Cab Cancellations are higher.
# Challenge 2 : Evening peak hour window(5 PM to 10 PM) - 
#               Demand is higher in Airport. Cabs unavailability is higher.

# Looking at all in one. 
# Question 16: What is the frequency of Requests based on Status 
#             for different time segments and pickup points. 
plot2 + 
 geom_bar(aes(fill = Status), position = "dodge") + 
  labs(y = "Frequency of Status", 
      title = "Time Segment vs Status Frequency by Pickup point") +
   scale_fill_manual(values = c("#F8766D","#619CFF", "#00BA38")) +
    facet_grid(Request.wday~Pickup.point) +
     plot_theme +
     theme(axis.text.x  = element_text(size = 8))
         

# For the given data across all days :
### Morning  peak hour window, 
##               demand is more in City with high cancellations
### Evening  peak hour window,
##               demand is more in Airport with high unavailability of cars

###############################################################################
#                                                                             #
#                             Segmented Analysis                              #
#                                                                             #
###############################################################################

# Lets focus our analysis on the 2 peak hours time segment

Morning_peak_problem_set <- 
 uber_requests %>% 
  filter(Time.segment == "Morning peak")

Evening_peak_problem_set <- 
 uber_requests %>% 
  filter(Time.segment == "Evening peak")

plot3 <- ggplot() + plot_theme

plot3 + 
 geom_bar(data = Morning_peak_problem_set,
          aes(x = Status, fill = Pickup.point), 
          position = "dodge") + 
  labs(x = "Request Status", y = "Count", title = "Morning Challenge in City")
  
City_problem_set <-  Morning_peak_problem_set %>%
 filter(Pickup.point == "City")

plot3 + 
 geom_bar(data = Evening_peak_problem_set, 
          aes(x = Status, fill = Pickup.point),
          position = "dodge") + 
  labs(x = "Request Status", y = "Count", title = "Evening Challenge @ Airport")

Airport_problem_set <-  Evening_peak_problem_set %>%
 filter(Pickup.point == "Airport")

# Lets quantify the problems we have:

# Question 17: What does more (cancellations/unavailability of cars) mean? 
#              Can we quantify? How much are the numbers? 

# What is the proportion of Status during the 2 times slots?

percent(prop.table(table(City_problem_set$Status)),0)

percent(prop.table(table(Airport_problem_set$Status)),0)

# Question 18: What is the expected average & actual average # of trips 
#              per week(5 days) a driver takes in a week during 
#              the Morning/Evening peak hours?

# With the available 300 drivers what is the expected average # 
# of trips a driver should take for the city_problem_set?
round(nrow(City_problem_set)/300)

# What is the current average?
round(sum(City_problem_set$Status == "Trip Completed")/300)


# With the available 300 drivers what is the expected average# 
#  of trips a driver should take for the Airport_problem_set?
round(nrow(Airport_problem_set)/300)

# What is the current average?
round(sum(Airport_problem_set$Status == "Trip Completed")/300)

#  As pointed out earlier we would need additional cabs/drivers to take care 
#  of the peak demand. Also If we add say 100 additional cabs/drivers 
#  the expected average trips/week would come down from 7 to 5

round(nrow(Airport_problem_set)/(300 + 100))  # 5
round(nrow(City_problem_set)/(300 + 100))     # 5

# Issue 1:
#  No. of trips completed is only 28% and the cancellation is around 46%. 
#  Expected average trips during morning peak hours is 7 (or less if we add 
#  additional cabs/drivers) whereas average trips is only 2.

# Issue 2 :
#  No. of trips completed is only 22% and the cab unavailability is around 46%. 
#  Expected average trips during morning peak hours is 7 (or less if we add 
#  additional cabs/drivers) whereas average trips is only 2.

########################## -----End of File ----------#########################

