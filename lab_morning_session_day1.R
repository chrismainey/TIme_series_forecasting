library(fpp3)
library(readr)

#------- Lab Session 1---------------

ae_uk_original <- read_csv("data/ae_uk.csv", 
                           col_types = cols( 
                             arrival_time=col_datetime(format = "%d/%m/%Y %H:%M"),
                             gender=col_character(),
                             type_injury=col_character()))

  ae_uk_original %>% duplicated() %>% sum()#check duplicates
ae_wd <- ae_uk_original %>% distinct(.)# remove duplicates
# nrow(ae_uk_original)-nrow(ae_wd)
ae_tsb <- ae_wd %>% #create tsibble
  as_tsibble(key = c(gender,type_injury), index = arrival_time, regular=FALSE)

#create a new tsibble
ae_hourly <- ae_tsb %>% group_by(gender,type_injury) %>% 
  index_by(arrival_1h = lubridate::floor_date(arrival_time, "1 hour")) %>% 
  summarise(n_attendance=n())

# check implicit NA 
has_gaps(ae_hourly)#check gaps
scan_gaps(ae_hourly)# show mw gaps
count_gaps(ae_hourly)# coun gaps
# fill gaps with zero
ae_hourly <- ae_tsb %>% group_by(gender, type_injury) %>% 
  index_by(arrival_1h = lubridate::floor_date(arrival_time, "1 hour")) %>% 
  summarise(n_attendance=n()) %>% fill_gaps(n_attendance=0L) %>% ungroup()

#you can use `index_by()` and `summarise()` to regularise index

# -------Lab Session 2------------

ae_hourly %>% autoplot(n_attendance)+ facet_wrap(gender~type_injury)
ae_daily <- ae_hourly %>% 
  index_by(year_day=as_date(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))
ae_daily %>% autoplot(n_attendance)
# ae_daily %>% filter_index("2016-02") %>% autoplot(n_attendance)

#ae_daily %>% filter_index("2016-01") %>% autoplot(n_attendance)
# ae_daily %>% filter_index("2016-3") %>% autoplot(n_attendance)+
#   scale_x_date(date_breaks = "1 day", date_labels = "%a")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ae_daily %>% tail(365) %>% autoplot(n_attendance)
# ae_monthly <- ae_hourly %>% 
#   index_by(year_month=yearmonth(arrival_1h)) %>% 
#   summarise(n_attendance=sum(n_attendance))
# 
# ae_monthly %>%
#   autoplot(n_attendance) +
#   labs(y = "attendances", x="Month",
#        title = "Monthly A&E attendance",
#        subtitle = "UK hospital")

#---- Lab Session 3--------

# ae_hourly_total <- 
#   ae_hourly %>% index_by(arrival_1h) %>% 
#   summarise(n_attendance=sum(n_attendance))

ae_hourly %>% gg_season(n_attendance)
ae_hourly %>% gg_season(n_attendance,period = "day")
ae_hourly %>% gg_season(n_attendance,period = "week")

ae_hourly %>% gg_subseries(n_attendance)


# ae_hourly %>% filter_index("2014"~ "2015") %>% gg_season(n_attendance)+theme(legend.position = "")
# ae_uk_monthly <- 
#   ae_uk_hourly %>% index_by(year_month=yearmonth(arrival_1h)) %>% 
#   summarise(n_attendance=sum(n_attendance))
# gg_season(ae_uk_monthly)
# gg_subseries(ae_uk_monthly)

# -------------------------Lab Session 4-------------------------

ae_daily <- ae_hourly %>% 
  index_by(year_day=as_date(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_daily %>% gg_lag(n_attendance, lags = c(1:14), geom = "point")
ae_daily %>% ACF(lag_max = 14)
ae_daily %>% ACF(n_attendance, lag_max = 14) %>% autoplot()

ae_daily %>% gg_tsdisplay()

ae_daily %>% features(n_attendance, ljung_box, dof=0)

# ae_hourly_total <- 
#      ae_hourly %>% index_by(arrival_1h) %>% 
#      summarise(n_attendance=sum(n_attendance))
# ae_hourly_total %>% gg_lag(n_attendance, lags = c(1, 2, 24, 48, 168,336))
# ae_uk_monthly %>% gg_tsdisplay()