library(tidyverse)

records <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv")

records %>%
  ggplot(aes(date,time,color=track))+
  geom_point(alpha=0.5,show.legend=FALSE)+
  facet_grid(rows=vars(type),cols=vars(shortcut),scales="free_y")

library(tidymodels)

set.seed(123)
mario_split <- records %>%
  select(shortcut,track,type,date,time)%>%
  mutate_if(is.character,factor)%>%
  initial_split(strata=shortcut)

mario_train <- training(mario_split)
mario_test <- testing(mario_split)

set.seed(234)
mario_folds <- bootstraps(mario_train,strata=shortcut)


tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth=tune())%>%
  set_engine("rpart")%>%
  set_mode("classification")

tree_grid <- grid_regular(cost_complexity(),tree_depth(),levels=7)

mario_wf <- workflow()%>%
  add_model(tree_spec)%>%
  add_formula(shortcut~.)

doParallel::registerDoParallel()

tree_res<- tune_grid(
  mario_wf,
  resamples=mario_folds,
  grid=tree_grid,
  control=control_grid(save_pred=TRUE))

collect_metrics(tree_res)
show_best(tree_res,metric="roc_auc")
select_by_one_std_err(tree_res,-cost_complexity)

#collect_predictions(tree_res)%>%
  #group_by(id)%>%
  #roc_curve(shortcut,.pred_No)%>%
  #autoplot()

choose_tree<- select_best(tree_res,metric="accuracy")

final_result <- mario_wf %>%
  finalize_workflow(choose_tree)%>%
  last_fit(mario_split)

final_fitted <- final_result$.workflow[[1]]
predict(final_fitted,mario_test[10:12,])


library(DALEXtra)

mario_explainer <- explain_tidymodels(
  final_fitted,
  data=dplyr::select(mario_train,-shortcut),
  y=as.integer(mario_train$shortcut)
  )
  
pdp_time <- model_profile(mario_explainer,variables="time",N=NULL,groups="type")  


as_tibble(pdp_time$agr_profiles) %>%
  mutate(`_label_`=str_remove(`_label_`,"workflow_"))%>%
  ggplot(aes(`_x_`,`_yhat_`,color=`_label_`))+
  geom_line(size=1.2,alpha=0.8)+
  labs(x="Time to complete track",
       y="Predicted probability of shortcut",
       color=NULL,
       title="Partial dependence plot for Mario Kart world records",
       subtitle="Predictions from a decision tree model")

