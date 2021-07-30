library(tidyverse)
train_raw <- read_csv("train.csv")

train_raw %>%
  ggplot(aes(x=plate_x,y=plate_z,z=is_home_run))+
  stat_summary_hex(alpha=0.8,bins=15)+
  scale_fill_viridis_c(labels=scales::percent)+
  labs(fill="% home runs")


train_raw %>%
  ggplot(aes(x=launch_angle,y=launch_speed,z=is_home_run))+
  stat_summary_hex(alpha=0.8,bins=15)+
  scale_fill_viridis_c(labels=scales::percent)+
  labs(fill="% home runs")

train_raw %>%
  mutate(is_home_run=if_else(as.logical(is_home_run),"yes","no"))%>%
  select(is_home_run,balls,strikes,inning)%>%
  pivot_longer(balls:inning)%>%
  mutate(name=fct_inorder(name))%>%
  ggplot(aes(value,after_stat(density),fill=is_home_run))+
  geom_histogram(alpha=0.5,binwidth=1,position="identity")+
  facet_wrap(vars(name),scales="free")+
  labs(x=NULL,fill="Home run?")
  
  
library(tidymodels)

bb_split <- train_raw%>%
  mutate(is_home_run=if_else(as.logical(is_home_run),"HR","no"),
         is_home_run=factor(is_home_run))%>%
  initial_split(strata=is_home_run)
bb_train <- training(bb_split)
bb_test <- testing(bb_split)
set.seed(234)
bb_folds <- vfold_cv(bb_train,strata=is_home_run)


bb_rec<- recipe(is_home_run ~ launch_angle +launch_speed +plate_x+plate_z+inning+balls+strikes+game_date
       + bearing+pitch_mph+is_pitcher_lefty+is_batter_lefty,data=bb_train)%>%
  step_date(game_date,features=c("week"),keep_original_cols = FALSE)%>%
  step_unknown(all_nominal_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=TRUE)%>%
  step_impute_median(all_numeric_predictors(),-launch_angle,-launch_speed)%>%
  step_impute_linear(launch_angle,launch_speed,impute_with=imp_vars(plate_x,plate_z,pitch_mph))%>%
  step_nzv(all_predictors())

xgb_spec <- boost_tree(
                        trees= tune(),
                        min_n=tune(),
                        mtry=tune(),
                        learn_rate=0.01
                      )%>%
          set_engine("xgboost")%>%
          set_mode("classification")

xgb_wf <- workflow(bb_rec,xgb_spec)

library(finetune)
xgb_rs <-tune_race_anova(
      xgb_wf,
      bb_folds,
      grid=15,
     metrics=yardstick::metric_set(mn_log_loss),
      control=control_race(verbose_elim=TRUE)
)

plot_race(xgb_rs)
show_best(xgb_rs)

xgb_last <- xgb_wf %>%
  finalize_workflow(select_best(xgb_rs,"mn_log_loss"))%>%
  last_fit(bb_split)


collect_metrics(xgb_last)
collect_predictions(xgb_last)%>%
  mn_log_loss(is_home_run,.pred_HR)

library(vip)

extract_workflow(xgb_last)%>%
  extract_fit_parsnip()%>%
  vip(geom="point",num_features=15)

