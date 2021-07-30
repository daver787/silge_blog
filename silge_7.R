library(tidyverse)
library(lubridate)
library(RSocrata)

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

crash <- crash_raw %>%
  transmute(injuries=if_else(injuries_total>0,"injuries",
                             "none"),
            crash_date,
            crash_hour,
            report_type=if_else(report_type=="","UNKNOWN",report_type),
            num_units,
            posted_speed_limit,
            weather_condition,
            lighting_condition,
            roadway_surface_cond,
            first_crash_type,
            trafficway_type,
            prim_contributory_cause,
            latitude,
            longitude)%>%
  na.omit()

crash %>%
  mutate(crash_date =floor_date(crash_date,unit="week"))%>%
  count(crash_date,injuries)%>%
  filter(crash_date !=last(crash_date),crash_date !=first(crash_date))%>%
  group_by(crash_date)%>%
  mutate(percent_injury=n/sum(n))%>%
  ungroup() %>%
  filter(injuries=="injuries")%>%
  ggplot(aes(crash_date,percent_injury,color=injuries))+
  geom_line(size=1.5,alpha=0.7)+
  scale_y_continuous(limits=c(0,NA),labels=scales::percent_format())



crash %>%
  mutate(crash_date =wday(crash_date,label=TRUE))%>%
  count(crash_date,injuries)%>%
  group_by(injuries)%>%
  mutate(percent=n/sum(n))%>%
  ungroup()%>%
  ggplot(aes(percent,crash_date,fill=injuries))+
  geom_col(position="dodge",alpha=0.8)+
  scale_x_continuous()





crash %>%
  count(first_crash_type,injuries)%>%
  mutate(first_crash_type=fct_reorder(first_crash_type,n))%>%
  group_by(injuries)%>%
  mutate(percent=n/sum(n))%>%
  ungroup()%>%
  group_by(first_crash_type)%>%
  filter(sum(n)>1e4)%>%
  ungroup()%>%
  ggplot(aes(percent,first_crash_type,fill=injuries))+
  geom_col(position="dodge",alpha=0.8)+
  scale_x_continuous()

crash %>%
  filter(latitude>0)%>%
  ggplot(aes(longitude,latitude,color=injuries))+
  geom_point(size=0.5,alpha=0.3)+
  labs(color=NULL)+
  scale_color_manual(values=c("deeppink","gray80"))

library(tidymodels)
set.seed(2021)
crash_split <- initial_split(crash,strata=injuries)
crash_train <- training(crash_split)
crash_test <- testing(crash_split)

set.seed(123)
crash_folds <- vfold_cv(crash_train,strata=injuries)


library(baguette)
library(themis)

crash_rec <- recipe(injuries ~.,data=crash_train)%>%
  step_date(crash_date)%>%
  step_rm(crash_date)%>%
  step_other(weather_condition,first_crash_type,trafficway_type,prim_contributory_cause,other="OTHER")%>%
  step_downsample(injuries)


bag_spec <-bag_tree(min_n=10)%>%
  set_engine("rpart",times=25)%>%
  set_mode("classification")


crash_wf <- workflow()%>%
  add_recipe(crash_rec)%>%
  add_model(bag_spec)


doParallel::registerDoParallel()

crash_res <-fit_resamples(crash_wf,
                          crash_folds)

collect_metrics(crash_res)
crash_fit<- last_fit(crash_wf,crash_split)
collect_metrics(crash_fit)

crash_imp <- crash_fit$.workflow[[1]]%>%
  pull_workflow_fit()

crash_imp$fit$imp %>%
  slice_max(value,n=10)%>%
  ggplot(aes(value,fct_reorder(term,value)))+
  geom_col(alpha=0.8,fill="midnightblue")


collect_predictions(crash_fit)%>%
  roc_curve(injuries,.pred_injuries)%>%
  autoplot()


crash_wf_model <- butcher::butcher(crash_fit$.workflow[[1]])
predict(crash_fit$.workflow[[1]],crash_test[222,])
predict(crash_wf_model,crash_test[222,])


write_rds(crash_wf_model,here::here("crash_wf_model.rds"))  
collect_metrics(crash_res)%>%
  write_csv(here::here("crash_model_metrics.csv"))
                                   