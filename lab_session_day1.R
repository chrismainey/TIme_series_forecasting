library(fpp3)
library(readr)

#-------First  day-------
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

#--------Lab Session 3--------

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

# -------Lab Session 4-------------------------

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

# -------Lab Session 5--------------------------------
ae_daily <- ae_hourly %>% 
  index_by(year_day=as_date(arrival_1h)) %>% 
  summarise(n_attendance=sum(n_attendance))

ae_fit <- ae_daily %>%
  model(
    mean = MEAN(n_attendance),
    naive = NAIVE(n_attendance),
    snaive = SNAIVE(n_attendance),
    drift = RW(n_attendance ~ drift())
  )

af_fc <- ae_fit %>% forecast(h="42 days")
af_fc %>% autoplot(filter_index(ae_daily,"2016"~.), level=NULL)

ae_fit %>% augment() %>% filter(.model=="snaive")%>% select(.fitted)
ae_fit %>% augment() %>% filter(.model=="mean")%>% select(.resid)

#-------second  day------------
#-----Lab session 6-----

# traditional approach: train-test
f_horizon <- 42# forecast horizon
test <- ae_daily %>% slice((n()-(f_horizon-1)):n())# test data set
train <- ae_daily %>% slice(1:(n()-f_horizon))# train data set
nrow(ae_daily)==(nrow(test)+nrow(train))# check if split is correct
ae_model <- train %>% model(SNAIVE(n_attendance))# specify and train model
ae_model %>% forecast(test) %>% # forecast and visualise it
  autoplot(filter_index(ae_daily,"2016"))

ae_model %>% gg_tsresiduals()# check residual
augment(ae_model) %>% features(.resid, ljung_box, lag=14,dof=0)# ljung_box test for residual

# time series cross validation
train_tr <- train %>% # split data into folds with increasing size
  slice(1:(n()-42)) %>%
  stretch_tsibble(.init = 4*365, .step = 1)

# train models for 
ae_mode_tr <- train_tr %>%
  model(
    mean = MEAN(n_attendance),
    naive = NAIVE(n_attendance),
    drift = RW(n_attendance ~ drift()),
    snaive = SNAIVE(n_attendance)
  )
ae_mode_tr# observe mable
# eg: .id is related to training data for each fold
# tidy(ae_mode_tr)
# glance(ae_mode_tr)
# Produce forecast for h=42 or 42 days
ae_fc_tr <- ae_mode_tr %>% forecast(h=42)

ae_fc_tr#observe fable
View(ae_fc_tr[1:43,])# each .id is the forecast for each fold

ae_fc <- ae_fc_tr %>% 
  group_by(.id,.model) %>% 
  mutate(h=row_number()) %>% ungroup()
View(ae_fc[1:43,])

fc_accuracy <- ae_fc %>% accuracy(train,measures = list(
  point_accuracy_measures,
  interval_accuracy_measures
)) 
fc_accuracy %>% group_by(.model) %>% 
  summarise(RMSE=mean(RMSE),
            MAE=mean(MAE),
            winkler=mean(winkler))

#-----Lab Session 7------------------
ets_model <- train %>%
  model(
    ets = ETS(n_attendance),
    ses = ETS(n_attendance ~ error("A")+trend("N")+season("N")),
    holt_winter = ETS(n_attendance ~ error("A")+trend("A")+season("A"))
  )

glance(ets_model)
ets_model %>% select(ets) %>% tidy()

fcst_ets <- ets_model %>%
  forecast(h = "42 days") 

fcst_ets%>%
  autoplot(filter_index(ae_daily,"2016-02"~.), level = NULL)

fcst_ets %>% accuracy(test) %>% select(.model,RMSE,MAE)


