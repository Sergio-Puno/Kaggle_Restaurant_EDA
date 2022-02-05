## Aim of this project is to predict the future numbers of restaurant visitors
# This makes it a TIME SERIES FORECASTING problem

# Kaggle: https://www.kaggle.com/headsortails/be-my-guest-recruit-restaurant-eda/report

# Data is from Japanese restaurants

# data is form the Japanese equivalent of Yelp (Hot Pepper Gourmet a.k.a. hpg)
# and from the equivalent of Square (AirREGI/Restaurant board)

# spans from Jan 2016 to most of Apr 2017 -- training data
# spans from last week of APR plus May 2017 -- test data

# data data includes a holiday week and other days that had closures, these will
# be ignored for scoring. Training set omits days that restaurants were closed

# Load libraries
# general visualization
library('ggplot2')
library('scales')
library('grid')
library('gridExtra')
library('RColorBrewer')
library('corrplot')

# general data manipulation
library('dplyr')      # data manipulation
library('readr')      # input/output
library('data.table') # data manipulation
library('tibble')     # data wrangling
library('tidyr')      # data wrangling
library('stringr')    # string manipulation
library('forcats')    # factor manipulation

# specific manipulation
library('ggrepel')
library('ggridges')
library('ggExtra')
library('ggforce')
library('viridis')

# specific data manipulation
library('lazyeval') # data wrangling
library('broom')    # data wrangling
library('purrr')    # string manipulation

# data plus forecast
library('lubridate')
library('timeDate')
library('tseries')
library('forecast')
library('prophet')
library('timetk')

# maps / geospatial
library('geosphere')
library('leaflet')
library('leaflet.extras')
library('maps')

# Helper functions for compute binomial confidence intervals
# and multiplot function -R Cookbooks-

# Define multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

air_visits <- read_csv("./Data/air_visit_data.csv")
air_reserve <- read_csv("./Data/air_reserve.csv")
hpg_reserve <- read_csv("./Data/hpg_reserve.csv")
air_store <- read_csv("./Data/air_store_info.csv")
hpg_store <- read_csv("./Data/hpg_store_info.csv")
holidays <- read_csv("./Data/date_info.csv")
store_ids <- read_csv("./Data/store_id_relation.csv")
test <- read_csv("./Data/sample_submission.csv")

# overview of the data sets using summary() and glimpse() tools
### Air visits file
summary(air_visits)
glimpse(air_visits)

air_visits %>% distinct(air_store_id) %>% nrow() # 829 unique air store id's (visits)

### Air reservations file
summary(air_reserve)
glimpse(air_reserve)

air_reserve %>%  distinct(air_store_id) %>%  nrow() # 314 unique air store id's (reservations)

### HPG reservations file
summary(hpg_reserve)
glimpse(hpg_reserve)

hpg_reserve %>% distinct(hpg_store_id) %>%  nrow() # 13,325 unique store id's hpg reserve

### Air store information file
summary(air_store)
glimpse(air_store)

### HPG store information file
summary(hpg_store)
glimpse(hpg_store)

### Holidays file, named because this hase a binary flag for holiday or not per day
summary(holidays)
glimpse(holidays)

### Store ID's, relational data between hpg and air store id's, ONLY 150 PAIRS
summary(store_ids)
glimpse(store_ids)

## test data file
summary(test)
glimpse(test) # concatenated air_id+date

# Check for missing values
sum(is.na(air_visits))
sum(is.na(air_reserve))
sum(is.na(hpg_reserve))
sum(is.na(air_store))
sum(is.na(hpg_store))
sum(is.na(holidays))
sum(is.na(store_ids)) # no missing values in any file!

# Changed the formatting of the date/time features and also reformatted a few features
# to logical and factor variables for exploration purposes
air_visits <- air_visits %>% 
  mutate(visit_date=ymd(visit_date))

air_reserve <- air_reserve %>% 
  mutate(visit_datetime=ymd_hms(visit_datetime),
         reserve_datetime=ymd_hms(reserve_datetime))

hpg_reserve <- hpg_reserve %>% 
  mutate(visit_datetime=ymd_hms(visit_datetime),
         reserve_datetime=ymd_hms(reserve_datetime))

air_store <- air_store %>% 
  mutate(air_genre_name=as.factor(air_genre_name),
         air_area_name=as.factor(air_area_name))

hpg_store <- hpg_store %>% 
  mutate(hpg_genre_name=as.factor(hpg_genre_name),
         hpg_area_name=as.factor(hpg_area_name))

holidays <- holidays %>% 
  mutate(holiday_flg=as.logical(holiday_flg),
         date=ymd(calendar_date),
         calendar_date=as.character(calendar_date))


# Individual feature visualizations
# distributions of features in the individual files before combining them

# AIR VISITS
# First, looking at the visits to the Air restaurants. Plotting the total number
# of visitors per day over the full training time range together with the median
# visitors per day of the week and month of the year
p1 <- air_visits %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors=sum(visitors)) %>% 
  ggplot(aes(x=visit_date, y=all_visitors))+
  geom_line(col='blue')+
  labs(x='All visitors', y='Date')

p2 <- air_visits %>% 
  ggplot(aes(x=visitors))+
  geom_histogram(fill='blue', bins=30)+
  geom_vline(xintercept=20, color='orange', size=1.2)
  scale_x_log10()

p3 <- air_visits %>% 
  mutate(wday=wday(visit_date, label=TRUE, week_start=1)) %>% 
  group_by(wday) %>% 
  summarise(visits=median(visitors)) %>% 
  ggplot(aes(x=wday, y=visits, fill=wday))+
  geom_col()+
  theme(legend.position='none', axis.text.x=element_text(angle=45, hjust=1, vjust=0.9))+
  labs(x='Day of the week', y='Median visitors')+
  scale_fill_hue()

p4 <- air_visits %>% 
  mutate(month=month(visit_date, label=TRUE)) %>% 
  group_by(month) %>% 
  summarise(visits=median(visitors)) %>% 
  ggplot(aes(x=month, y=visits, fill=month))+
  geom_col()+
  theme(legend.position='none')+
  labs(x='Month', y='Median visitors')

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout = layout) # didn't work for whatever reason

# task for this dataset is to forecast the last week of April plus all of May 2017
# lets look at this time range in our 2016 training data

air_visits %>% 
  filter(visit_date > ymd('2016-4-15') & visit_date < ymd('2016-06-15')) %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(visitors)) %>% 
  ggplot(aes(x=visit_date, y=all_visitors))+
  geom_line()+ geom_smooth(method='loess', color='blue', span=1/7)+
  labs(x='Date', y='All visitors')

# with the graphs we have the black line as the visitor count and the blue line
# as the smoothing fit with the grey confidence area. We can see the weekly cycling
# of guest counts as well as the impact of 'Golden Week' from Apr 29 to May 5

# now taking a look at the reservations compared to actual visitor numbers
foo <- air_reserve %>% 
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label= TRUE, week_start = 1),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label= TRUE, week_start = 1),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = 'hour'),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = 'day')
  )

p1 <- foo %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(x=visit_date, y=all_visitors))+
  geom_line() + labs(x='air visit date')

p2 <- foo %>% 
  group_by(visit_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(x=visit_hour, y=all_visitors))+
  geom_col(fill='blue')

p3 <- foo %>% 
  filter(diff_hour < 24*5) %>% 
  group_by(diff_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(x=diff_hour, y=all_visitors))+
  geom_col(fill='blue') + labs(x='Time from reservation to visit (hrs)')

layout <- matrix(c(1,1,2,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout=layout)

# Findings:
# p1: much fewer reservations made in 2016 using the Air system, and noe at all for a 
# decent time frame (end of July to end of 2016)
# there is a dip at the end of the first quarter of 2017 may be due in part of the 
# reservations being at the end of the training time frame

# p2: reservations are typically made for the dinner hours in the evening

# p3: shows the typical strategy of making a reservation a few hours before the visit
# but if they are made in advance then it seems to be a booking in the evening for 
# one of the next evenings. chart is truncated but there are some data points with very
# long reservation lead times, more than a year in advance

foo %>% 
  arrange(desc(diff_day)) %>% 
  select(reserve_datetime, visit_datetime, diff_day, air_store_id) %>% 
  head(5)

# above are the top 5 longest reservation to visit gaps, only 2 store id's so
# either very fancy and limited places or an input error on the year entries

## We can do the same 3 chart layout for the HPG reservations:
doe <- hpg_reserve %>% 
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label= TRUE, week_start = 1),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label= TRUE, week_start = 1),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = 'hour'),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = 'day')
  )

p1 <- doe %>% 
  group_by(visit_date) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(x=visit_date, y=all_visitors))+
  geom_line() + labs(x='hpg visit date')

p2 <- doe %>% 
  group_by(visit_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(x=visit_hour, y=all_visitors))+
  geom_col(fill='red')

p3 <- doe %>% 
  dplyr::filter(diff_hour < 24*5) %>% 
  group_by(diff_hour) %>% 
  summarise(all_visitors = sum(reserve_visitors)) %>% 
  ggplot(aes(x=diff_hour, y=all_visitors))+
  geom_col(fill='red') + labs(x='Time from reservation to visit (hrs)')

layout <- matrix(c(1,1,2,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout=layout)

# notable findings:
# p1: similar but more orderly pattern (more data available) with a huge spike in
# Dec 2016. Also similar to the Air data, there is the drop off at the end of the 
# available time frame due to the test data cutoff

# p2: this chart shows the same, most reservations are for dinner hours

# p3: 24 hour pattern is visible again but it is worth noting that the section for the 
# few hours before the visit are not significantly higher like it was the Air data, all 
# the peaks are similar and the 48/72 hr peaks are actually higher

# switching from temporal data to spatial data analysis
# the latitudes and longitudes provided are just the area in which the restaurant
# belongs, as they do not want us identifying specific restaurant names
leaflet(air_store) %>% 
  addTiles() %>% 
  addProviderTiles('CartoDB.Positron') %>%
  addMarkers(~longitude, ~latitude,
             popup= ~air_store_id, label= ~air_genre_name,
             clusterOptions= markerClusterOptions())

# now we can plot the different types of cuisine as well as count of restaurants in
# the area names provided

p1 <- air_store %>% 
  group_by(air_genre_name) %>% 
  count() %>% 
  ggplot(aes(reorder(air_genre_name, n, FUN= min), n, fill= air_genre_name))+
  geom_col() + coord_flip() + theme(legend.position= 'none')+
  labs(x='Type of cuisine', y= 'Number of air restaurants')

p2 <- air_store %>% 
  group_by(air_area_name) %>% 
  count() %>% 
  ungroup() %>% 
  top_n(15,n) %>% 
  ggplot(aes(reorder(air_area_name, n, FUN= min), n, fill= air_area_name))+
  geom_col() + coord_flip() + theme(legend.position= 'none')+
  labs(x='Top 15 areas', y= 'Number of air restaurants')

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1,p2,layout=layout)

# lots of Izakaya's with cafe's being second. 
# Fukuoka has the largest number of air restaurants followed by mostly Tokyo locations

# we'll do the same but for the hpg_store data
leaflet(hpg_store) %>% 
  addTiles() %>% 
  addProviderTiles('CartoDB.Positron') %>%
  addMarkers(~longitude, ~latitude,
             popup= ~hpg_store_id, label= ~hpg_genre_name,
             clusterOptions= markerClusterOptions())

p1 <- hpg_store %>% 
  group_by(hpg_genre_name) %>% 
  count() %>% 
  ggplot(aes(reorder(hpg_genre_name, n, FUN= min), n, fill= hpg_genre_name))+
  geom_col() + coord_flip() + theme(legend.position= 'none')+
  labs(x='Type of cuisine', y= 'Number of hpg restaurants')

p2 <- hpg_store %>% 
  mutate(area= str_sub(hpg_area_name, 1,20)) %>% 
  group_by(area) %>% 
  count() %>% 
  ungroup() %>% 
  top_n(15,n) %>% 
  ggplot(aes(reorder(area, n, FUN= min), n, fill= area))+
  geom_col() + coord_flip() + theme(legend.position= 'none')+
  labs(x='Top 15 areas', y= 'Number of hpg restaurants')

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1,p2,layout=layout)

# hpg has more variety is cuisine genres than the air data. Japanese style 
# comes up much more so they are more specifically categorized than in the air set
# tokyo is again prominent for total number of restaurants, as well as Osaka

# looking now to the holidays, how many are there and how are they distributed
# among our prediction time range in 2017 and in 2016 for the same time range

foo <- holidays %>% 
  mutate(wday= wday(date, week_start = 1))

p1 <- foo %>% 
  ggplot(aes(x=holiday_flg, fill=holiday_flg))+
  geom_bar() + theme(legend.position = 'none')

p2 <- foo %>% 
  dplyr::filter(date > ymd('2016-04-15') & date < ymd('2016-06-01')) %>% 
  ggplot(aes(x= date, y= holiday_flg, color= holiday_flg))+
  geom_point(size=2)+
  theme(legend.position='none') + labs(x='2016 date')

p3 <- foo %>% 
  dplyr::filter(date > ymd('2017-04-15') & date < ymd('2017-06-01')) %>% 
  ggplot(aes(x= date, y= holiday_flg, color= holiday_flg))+
  geom_point(size=2)+
  theme(legend.position='none') + labs(x='2017 date')

layout <- matrix(c(1,1,2,3),2,2,byrow=FALSE)
multiplot(p1,p2,p3,layout=layout)

holidays %>%  summarise(frac=mean(holiday_flg))

# looks like the same days were holidays in 2016 vs 2017
# total holiday days is about 7%

# lets plots out visually the test vs training sets date ranges
foo <- air_visits %>% 
  dplyr::rename(date= visit_date) %>% 
  distinct(date) %>% 
  mutate(dset= 'train')

bar <- test %>% 
  separate(id,c('foo', 'bar', 'date'), sep='_') %>% 
  mutate(date= ymd(date)) %>% 
  distinct(date) %>% 
  mutate(dset='test')

foo <- foo %>% 
  bind_rows(bar) %>% 
  mutate(year=year(date))

# the goal was to have all the data on the same 'year' for the full date col but
# maintain the specific year col as the original to have them show up on the same
# point chart but then categorize them as diff based on the dset and year cols
year(foo$date) <- 2017

foo %>% 
  dplyr::filter(!is.na(date)) %>% 
  mutate(year= fct_relevel(as.factor(year), c('2017', '2016'))) %>% 
  ggplot(aes(x=date, y=year, color=dset))+
  geom_point(shape='|', size=10)+
  scale_x_date(date_labels= '%B', date_breaks= '1 month')+
  # scale_y_reverse()+
  theme(legend.position= 'bottom', axis.text.x= element_text(angle=45, hjust=1,
                                                             vjust=0.9))+
  labs(color='Data set')+
  guides(color= guide_legend(override.aes= list(size= 4, pch= 15)))

# now we move to feature relations
foo <- air_visits %>% 
  left_join(air_store, by= 'air_store_id')

foo %>% 
  group_by(visit_date, air_genre_name) %>% 
  summarise(mean_visitors= mean(visitors)) %>% 
  ungroup() %>% 
  ggplot(aes(x= visit_date, y= mean_visitors, color=air_genre_name))+
  geom_line()+
  labs(x='Date', y='Average number of visitors (Air)')+
  theme(legend.position='none')+
  scale_y_log10()+
  facet_wrap(~air_genre_name)

# we can relate this data back to our charts on the genre types and the total counts
# in order to understand why, for example Karaoke and Asian cuisine types have
# very chaotic charts (low counts in Air restaurant) but are still relatively 
# popular (either in spite of or because of the rarity)

# overall we can see that visitors average out between 10 and 100 per genre per day
# upward trends seem to be with creative cuisine and okonomiyaki

# there is a standard weekly fluctuation (or trend) in each of the genres, we can
# look at this more in depth at the mean visitors per week day and genre

p1 <- foo %>% 
  mutate(wday= wday(visit_date, label=TRUE, week_start = 1)) %>% 
  group_by(wday, air_genre_name) %>% 
  summarise(mean_visitors = mean(visitors)) %>% 
  ggplot(aes(x= air_genre_name, y= mean_visitors, color= wday))+
  geom_point(size=4)+
  theme(legend.position = 'left', axis.text.y= element_blank(),
        plot.title = element_text(size=14))+
  coord_flip()+
  labs(x='') + scale_x_discrete(position='top')+
  ggtitle('air_genre_name')+
  scale_color_hue()

p2 <- foo %>% 
  ggplot(aes(x=visitors, y=air_genre_name, fill=air_genre_name))+
  geom_density_ridges(bandwidth=0.1)+
  scale_x_log10()+
  theme(legend.position = 'none')+
  labs(y='')+
  scale_fill_cyclical(values=c('blue', 'red'))

layout <- matrix(c(1,1,2,2,2),1,5, byrow=TRUE)
multiplot(p1,p2,layout=layout)

# for this plot the y labeling is shared in the middle of the two plots
# biggest difference in the weekday plot is for karaoke which has significantly 
# higher visits on sat and then higher on sunday; similar to this is int'l cuisine

# most genres show the higher attendance fri/sat/sun vs. the weekdays with two 
# having substantial fridays yakiniku and izakaya

# bars/cocktails seem to be unpopular overall

# for the density plot, we can see that 'Asian' restaurants rarely have less than 10
# visits per day whereas karaoke has a broad distribution due to the impact of weekends

# Impact of holidays
foo <- air_visits %>% 
  mutate(calendar_date=as.character(visit_date)) %>% 
  left_join(holidays, by='calendar_date')

p1 <- foo %>% 
  ggplot(aes(x=holiday_flg, y=visitors, color=holiday_flg))+
  geom_boxplot()+
  scale_y_log10()+
  theme(legend.position='none')

p2 <- foo %>%
  mutate(wday=wday(date, label=TRUE, week_start=1)) %>% 
  group_by(wday, holiday_flg) %>% 
  summarise(mean_visitors=mean(visitors)) %>% 
  ggplot(aes(x=wday, y=mean_visitors, color=holiday_flg))+
  geom_point(size=4) + theme(legend.position='none')+
  labs(y='Average number of visitors')

layout <- matrix(c(1,2), 1,2, byrow=TRUE)
multiplot(p1,p2,layout=layout)

# overall i doesnt appear that holidays affect average visitor numbers (left panel)
# although it does seem that the higher peaks of visitors come on the non-holiday days

# based on the right panel, we can see that monday and tesday (and more generally all
# weekdays) holidays increase the average visitors significantly, with limited effect
# on the saturdays/sundays; the effect is actually negative on saturdays

# Restaurants per are and the effect on visitor numbers
# we will look at the number of restaurants of a certain genre per area and their
# impact on visitor numbers

# Air data
air_store %>% 
  mutate(area=str_sub(air_area_name, 1, 12)) %>%  # truncate the full area name
  ggplot(aes(x=area, y=air_genre_name))+
  geom_count(color='dodgerblue')+
  theme(legend.position='bottom', axis.text.x=element_text(angle=90, hjust=1, vjust=0.9))

# HPG data
hpg_store %>% 
  mutate(area=str_sub(hpg_area_name, 1, 12)) %>%  # truncate the full area name
  ggplot(aes(x=area, y=hpg_genre_name))+
  geom_count(color='firebrick1')+
  theme(legend.position='bottom', axis.text.x=element_text(angle=90, hjust=1, vjust=0.9))

# now we can look at the distribution in detail using boxplots and overlayed
# jitter plots. 

# Air data
air_store %>% 
  group_by(air_genre_name, air_area_name) %>% 
  count() %>% 
  ggplot(aes(reorder(air_genre_name, n, FUN=mean), n))+
  geom_boxplot()+
  geom_jitter(color='dodgerblue')+
  scale_y_log10()+
  coord_flip()+
  labs(x='Air genre', y='Occurences per air area')

# Findings: only a few genres have medians of more than 2 restaurants per area
# e.g. Italian/French and Dining Bar are more likely to be found in grps of > 2 per area

# Most genres have their distributions firmly clustered at 2, with some scatter
# towards high numbers; Cafe/Sweets has the higher # of occurrences in a single area (26)
# no value lower than 2 for the count, checked below:

tmp <- air_store %>% 
  group_by(air_genre_name, air_area_name) %>% 
  count()

min(tmp$n)

# this means there is no Air restaurant that is the only one of its genre in any area
# can take a deeper dive into the data to check out one of these occurrences
# looking at a pair of 'Dining Bar' restaurants and their visit data
air_store %>% 
  dplyr::filter(air_store_id=='air_b5598d12d1b84890' | air_store_id=='air_bbe1c1a47e09f161')

# these two are the same long and lat and genres, thus same area name as well
air_visits %>% 
  dplyr::filter(air_store_id=='air_b5598d12d1b84890' | air_store_id=='air_bbe1c1a47e09f161') %>% 
  arrange(visit_date) %>% 
  head(10)

# they have different visitor data for the same days so it is not an issue of duplicated
# information

# we can perform the same boxplot for HPG restaurants
hpg_store %>% 
  group_by(hpg_genre_name, hpg_area_name) %>% 
  count() %>% 
  ggplot(aes(reorder(hpg_genre_name, n, FUN=mean), n))+
  geom_boxplot()+
  geom_jitter(color='firebrick1')+
  scale_y_log10()+
  coord_flip()+
  labs(x='HPG genre', y='Occurences per air area')

# Findings: HPG has data with minimums of 1 genere per area, and a wider variety in 
# medians as there is a larger amount of data (and thus overall higher numbers)

# biggest example is the Japanese style genre, just over 10 median per area.

# now we can show the 'crowdedness' of our data set. first we can plit the overall 
# distribution of the air and hpg data points

foo <- air_visits %>% 
  left_join(air_store, by='air_store_id')

bar <- air_store %>% 
  group_by(air_genre_name, air_area_name) %>% 
  count()

foobar <- hpg_store %>% 
  group_by(hpg_genre_name, hpg_area_name) %>% 
  count()

p1 <- bar %>% 
  ggplot(aes(n))+
  geom_histogram(fill='dodgerblue', binwidth=1)+
  labs(x='Air genres per area')

p2 <- foobar %>% 
  ggplot(aes(n))+
  geom_histogram(fill='firebrick1', binwidth=1)+
  labs(x='HPG genres per area')

p3 <- foo %>% 
  group_by(air_genre_name, air_area_name) %>% 
  summarise(mean_log_visit=mean(log1p(visitors))) %>% 
  left_join(bar, by=c('air_genre_name', 'air_area_name')) %>% 
  group_by(n) %>% 
  summarise(mean_mlv=mean(mean_log_visit),
            sd_mlv=sd(mean_log_visit)) %>% 
  replace_na(list(sd_mlv=0)) %>% 
  ggplot(aes(n, mean_mlv))+
  geom_point(color='dodgerblue', size=4)+
  geom_errorbar(aes(ymin= mean_mlv-sd_mlv, ymax= mean_mlv+sd_mlv), width=0.5, size=0.7,
                color='dodgerblue')+
  labs(x='Cases of identical Air genres per area', y='Mean +/- SD of\n mean log1p visitors')

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(p1,p2,p3,layout=layout)

# writing the findings down to understand it myself
# for the upper panels:
# the number of cases of identical genres in the same area drops quickly (2 and 1 as the
# maxima for the air and hpg sets respectively, as found from the previous plots)
# there is a longer tail towards the larger case numbers

# for the bottom panel:
# the log1p means of visitor numbers show relatively large spread and are consistent in
# the range from 2 cases to 9 (points with whispers visible). From 10 cases and up we do
# not have the numbers to measure a spread. The scatter for these points >9 is fairly low
# (aside from one single value way out there which is the cafe 26 we found earlier i am
# assuming) and they lie notably higher than the means of the over <=9 points.
# One possibility is because of the lower available information, these high visitor means
# are not being brought down by small/less busy occurrences in other areas.

# this is interesting now that i am processing it, since the bottom plot is charting 
# cases of identical genres per area it is possible that the outlier points
# are
tmp <- foo %>% 
  group_by(air_genre_name, air_area_name) %>% 
  summarise(mean_log_visit=mean(log1p(visitors))) %>% 
  left_join(bar, by=c('air_genre_name', 'air_area_name'))

View(tmp)

# Reservations vs. visits
# this is a lot so take it slow

# note for this section we are using the visitor datetime and not the reservation
# datetime
foo <- air_reserve %>% 
  mutate(visit_date=date(visit_datetime)) %>% 
  group_by(air_store_id, visit_date) %>% 
  summarise(reserve_visitors_air=sum(reserve_visitors))

bar <- hpg_reserve %>% 
  mutate(visit_date=date(visit_datetime)) %>% 
  group_by(hpg_store_id, visit_date) %>% 
  summarise(reserve_visitors_hpg=sum(reserve_visitors)) %>% 
  inner_join(store_ids, by='hpg_store_id')

# now we use the visits file as the baseline as it has the 'results' aka the real visitors
all_reserve <- air_visits %>% 
  inner_join(foo, by=c('air_store_id', 'visit_date')) %>% 
  inner_join(bar, by=c('air_store_id', 'visit_date')) %>% 
  mutate(reserve_visitors = reserve_visitors_air + reserve_visitors_hpg)

p <- all_reserve %>% 
  dplyr::filter(reserve_visitors < 120) %>% # not sure about why we are limiting this set
  ggplot(aes(x=reserve_visitors, y=visitors))+
  geom_point(color='black', alpha=0.5)+
  geom_abline(slope=1, intercept=0, color='grey60')+
  geom_smooth(method='lm', color='dodgerblue')

ggMarginal(p, type='histogram', fill='dodgerblue', bins=50)

# Findings: the reserve_visitors and visitors numbers peak below 20, most are confined
# to below 100

# the points largely fall above the line of identity, indicating more visitors than
# reservations. This is due to a % of people being walk in customers

# even with the prior statement, there are still points that fell below this line,
# which means there are people who reserved but then changed their minds and didnt go

# the grey line shows the line of identity (x == y); blue is the linear fit
# based on the line of identity, the assumption is that 100 reservations == 100 visitors
# but if we look at the linear fit, we see that 100 reservations == 75 visitors on avg
# so since we are expecting the 100 visitors but only on avg get the 75, the larger the
# reserve_visitors value, the more likely to under estimate the eventual visitor numbers

# e.g if we predicted based on this simple plot, when we get 100 reservations we'd plan
# for 75 but may end up with 100 + walks ins 

# this is likely because with larger reservation #'s there is a higher chance that a
# reservation that is large gets cancelled vs. the change of a large group coming in as 
# a walk in

## Now we will look at breaking down the discrepancy between visitors and reservations
# over time

p1 <- all_reserve %>% 
  ggplot(aes(x=visitors - reserve_visitors))+
  geom_histogram(binwidth=5, fill='grey18')+
  coord_flip()+
  labs(x='')

p2 <- all_reserve %>% 
  ggplot(aes(x=visitors - reserve_visitors_air))+
  geom_histogram(binwidth=5, fill='dodgerblue')+
  coord_flip()+
  labs(x='')

p3 <- all_reserve %>% 
  ggplot(aes(x=visitors - reserve_visitors_hpg))+
  geom_histogram(binwidth=5, fill='firebrick1')+
  coord_flip()+
  labs(x='')

p4 <- all_reserve %>% 
  ggplot(aes(x= visit_date, y= visitors-reserve_visitors))+
  geom_hline(yintercept = c(150,0,-250))+
  geom_line()+
  geom_line(aes(x=visit_date, y= visitors-reserve_visitors_air + 150), color='dodgerblue')+
  geom_line(aes(x=visit_date, y= visitors-reserve_visitors_hpg - 250), color='firebrick1')+
  ggtitle('Visitors vs Reserved: All(Black), Air(Blue), HPG(Red)')

layout <- matrix(c(4,4,2,4,4,1,4,4,3), 3, 3, byrow=TRUE)
multiplot(p1,p2,p3,p4,layout=layout)

## Findings:
# the time series (left) shows a lot of scatter throughout the training time range
# both the air and hpg data sets are above the baseline (i.e. more visitors than reserv's)
# combining the two data sets bring the mean close to the zero line, can also be seen
# in histograms on the right

#  there is again a gap in the air dataset where there are no reservations (previous
# finding in section 4.2). assumption is that the trends would be the same.

# the histograms on the right, show how the distributions are skewed more towards larger 
# visitor numbers than reserve_visitor numbers. might be a mix of two distributions:
# a mostly normal spread due to cancellations plus a tail from walk in visitors

# quick look at the impact of holidays on the discrepancy between reservations and visitors
# using overlapping density plots
all_reserve %>% 
  mutate(date=visit_date) %>% 
  left_join(holidays, by='date') %>% 
  ggplot(aes(x=visitors-reserve_visitors, fill=holiday_flg))+
  geom_density(alpha=0.5)

# Findings: for holidays, there are somewhat higher numbers of visitors compare to
# reservations

# the peaks are nearly identical but we see a small yet clear difference towards the
# larger difference values


