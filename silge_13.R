library(tidyverse)
train_raw<- read_csv("train_2.csv")

library(lubridate)
train_raw %>%
  mutate(age_upon_outcome=as.period(as.Date(datetime)-date_of_birth),
         age_upon_outcome=time_length(age_upon_outcome,unit="weeks"))%>%
  ggplot(aes(age_upon_outcome,after_stat(density),fill=outcome_type))+
  geom_histogram(bins=15,alpha=0.5,position="identity")+
  labs(x="age in weeks",fill=NULL)

train_raw%>%
  mutate(outcome_type=outcome_type=="adoption")%>%
  group_by(week=week(datetime),
           wday=wday(datetime))%>%
  summarise(outcome_type=mean(outcome_type))%>%
  ggplot(aes(week,wday,fill=outcome_type))+
  geom_tile(alpha=0.8)+
  scale_fill_viridis_c(labels=scales::percent)+
  labs(fill="% adoption rate",x="week of the year",y="day of the week")

library(tidymodels)

set.seed(123)
shelter_split <- train_raw %>%
  mutate(age_upon_outcome=as.period(as.Date(datetime)-date_of_birth),
         age_upon_outcome=time_length(age_upon_outcome,unit="weeks"))%>%
  initial_split(strata=outcome_type)

shelter_train <- training(shelter_split)
shelter_test <- testing(shelter_split)

set.seed(234)
shelter_folds <- vfold_cv(shelter_train,strata=outcome_type)
shelter_metrics <-metric_set(accuracy,roc_auc,mn_log_loss)

shelter_rec <- recipe(outcome_type ~ age_upon_outcome + animal_type + datetime + sex + spay_neuter,data = shelter_train)%>%
               step_date(datetime,features=c("dow","week","year"),keep_original_cols = FALSE)%>%
               step_dummy(all_nominal_predictors(),one_hot=TRUE)%>%
               step_zv(all_predictors())
#prep(shelter_rec)

stopping_spec <- boost_tree(
  trees = 500,
  mtry = tune(),
  learn_rate = tune(),
  stop_iter = tune()
)%>%
  set_engine("xgboost",validation=0.2)%>%
  set_mode("classification")

stopping_grid <-
  grid_latin_hypercube(
    mtry(range = c(5L, 20L)), ## depends on number of columns in data
    learn_rate(range = c(-5, -1)), ## keep pretty big
    stop_iter(range = c(10L, 50L)), ## bigger than default
    size = 10
  )


early_stop_wf <-workflow(shelter_rec,stopping_spec)
doParallel::registerDoParallel()

set.seed(789)
stopping_rs <- 
  tune_grid(
    early_stop_wf,
    shelter_folds,
    grid=stopping_grid,
    metrics=shelter_metrics
  )
  
autoplot(stopping_rs)+
  theme_light(base_family="IBMPLexSans")

show_best(stopping_rs,metric="mn_log_loss")

stopping_fit <- early_stop_wf%>%
  finalize_workflow(select_best(stopping_rs,"mn_log_loss"))%>%
  last_fit(shelter_split)


collect_metrics(stopping_fit)


library(vip)

extract_workflow(stopping_fit)%>%
  extract_fit_parsnip()%>%
  vip(num_features=15,geom="point")

collect_predictions(stopping_fit)%>%
  roc_curve(outcome_type,.pred_adoption:.pred_transfer)%>%
  autoplot()

collect_predictions(stopping_fit)%>%
  conf_mat(outcome_type,.pred_class)%>%
  autoplot()