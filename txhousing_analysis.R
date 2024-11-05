# Name: Nam Nguyen
# Class: DATA 180


# Load in the libraries and data set that we are going to work with
library(ggplot2)
library(dplyr)
data(txhousing)
head(txhousing)

#------------------------------------------------------------------------------
# Excluding data from 2008 to 2009 due to the Great Depression
excl_rec <- txhousing %>%
  filter(year > 2009 | year < 2008)


# Here, I define the growth rate to be the additional sales volume gained
# through out the period form 2000 to 2015. In other words, it is the
# difference in the sales volume in 2015 and 2000 for each city.


# Since we are looking at the timeframe 2000 to 2015 and we are interested in 
# sales trend over this period, we will calculate the 
# total sales volume of the two years - 2000 and 2015 - for each city. Then, we can  
# compute the growth rate by subtracting sales volume of 2000 from 2015.


# We will calculate the total sales volume, mean, and median of 2000 and 2015 for each city.
group_data <- excl_rec %>% 
  filter(year == 2000 | year == 2015) %>% 
  group_by(city, year) %>% 
  summarize(
    total_vol_in_mil = sum(volume, na.rm = TRUE)/1000000
    )

# Compute growth rate, sort the values in descending order of growth rate so 
# the highest growing cities are at the top, and eliminate NA values arose from
# computation.
city_growth <- group_data %>%
  mutate(growth = total_vol_in_mil - lag(total_vol_in_mil)) %>%
  arrange(desc(growth)) %>%
  filter(is.na(growth) == FALSE)


# Top 10 cities with the highest growth rate experienced a grow of more
# than 400 million dollars in sales.
print(n = 10, city_growth)
top_cities <- city_growth %>% filter(growth >= 400)


# Get the names of those cities
cities <- top_cities$city


# Calculate the total sales value of each year for each city
top_cities_data <- excl_rec %>% 
  filter(city %in% cities) %>%
  group_by(city, year) %>% 
  summarize(total_vol_in_mil = sum(volume, na.rm=TRUE)/1000000)


# Visualize data using line graph as it is easy to interpret growth
ggplot(top_cities_data, aes(x = year, y = total_vol_in_mil, colour = city)) + 
  geom_line(size = .8) +
  labs(x = 'Year (2000-2015)', 
       y = "Total Sales Volume (in million Dollars)", 
       title = 'Top 10 cities with highest growth rate in sales volume')


# Customer wants to know whether the data is skewed by some outliers.

# We will approach this problem by calculating the mean house price and the 
# average median house price of the two years: 2000 and 2015. We can first filter out
# observations from 2000 and 2015. Then, we calculate the mean house price by
# dividing the total sales volume by number of sales within a year. Since we 
# do not know the exact median house price, we can use the average  median 
# house price as a replacement. To compute the average median house price, we 
# can take the average of all the median prices within a year. 
group_data_for_centr <- excl_rec %>% 
  filter(year == 2000 | year == 2015) %>% 
  group_by(city, year) %>% 
  summarize(
    mean_price = sum(volume) / sum(sales),
    avg_median_price = mean(median)
    ) %>%
  mutate(diff = mean_price - avg_median_price)


# Filters out the top cities that we are interested in. 
centr_growth <- group_data_for_centr %>%
  filter(city %in% cities) 

# Added percentage of change column to indicate how big the differences between
# mean house prices and average median house prices are.
  
percentage_data<- centr_growth %>% 
  mutate(percentage = 100*(diff/avg_median_price)) %>%
  select(city, year, percentage)

# Visualize data using bar chart

ggplot(percentage_data, aes(city, percentage, fill = year)) + 
  geom_bar(stat = "identity", position = position_dodge2()) + 
  labs(x = 'Cities', y = "Percentage of Difference")

print(percentage_data)

#------------------------------------------------------------------------------
# BEST TIME TO BUY HOUSE IN HISTORY


# Create a date column (year - month) to inform customers and add the average
# house price per month, since the customer wants to know the exact time
formatted_date <- excl_rec %>% 
  mutate(
    converted_year = as.character(year),
    converted_month = as.character(month), 
    time = paste(converted_year, converted_month, sep = "-")
    )


# We use median house price as the main indicator here, since median is 
# resistant to outliers. 

# Sort the median house price in ascending order to get the dates with the 
# lowest median house prices on top and filter out the top 20 best time to 
# buy a house, presuming customers prefer low prices
best_time_to_buy <- formatted_date %>% 
  select(city, time, median) %>%
  arrange(median) %>% 
  filter(median <= 59400)

print(n = 20, best_time_to_buy)
#------------------------------------------------------------------------------
# BEST TIME TO SELL HOUSE IN HISTORY


# Sort the average house price in the descending order to achieve the top dates
# with highest sales volume and filter out the top 20 best time to sell a house,
# presuming customers prefer selling houses at a high price
top_date_to_sell <- formatted_date %>%
  select(city, date, median) %>%
  arrange(desc(median)) %>%
  filter(median >= 270300)

print(top_date_to_sell)
#------------------------------------------------------------------------------
# RELATIONSHIP 

# We will filter out the NA values in the listings, and median columns. 
# Total exclusion turns out to be 1318 observations. The majority of the 
# exclusions coming from NA values in the listings column.
relationship <- excl_rec %>% 
  mutate(avg_house_price = volume/sales) %>%
  select(city, median, avg_house_price, listings) %>%
  filter(is.na(listings) == FALSE, 
         is.na(median) == FALSE
         )


# Visualize the relationship between median house price and active listings
ggplot(relationship, aes(x=median, y=listings, colour=city)) +
  geom_point() + 
  labs(
    x = "Median house price (in Dollars)",
    y = "Active listings"
  ) + 
  theme(
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 7)
    )

# Visualize the relationship between average house price and active listings
ggplot(relationship, aes(avg_house_price, listings,colour = city)) + 
  geom_point() + 
  labs(
    x = "Average house price (in Dollars)",
    y = "Active listings"
    ) + 
  theme(
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 7)
  )


