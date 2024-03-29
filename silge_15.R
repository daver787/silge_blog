library(tidyverse)
computer_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv")

computer_raw %>%
  distinct(value_id,.keep_all=TRUE)%>%
  count(char_type)

library(tidytext)
library(tidylo)

computer_counts <- computer_raw %>%
  distinct(value_id,.keep_all = TRUE) %>%
  unnest_tokens(word,interaction) %>%
  count(char_type,word,sort=TRUE)

computer_counts %>%
  bind_log_odds(char_type,word,n)%>%
  filter(n>10) %>%
  group_by(char_type)%>%
  slice_max(log_odds_weighted,n=10)%>%
  ungroup() %>%
  ggplot(aes(log_odds_weighted,fct_reorder(word,log_odds_weighted),fill= char_type))+
  geom_col(alpha=0.8,show.legend=FALSE)+
  facet_wrap(vars(char_type),scales="free_y")+
  labs(y=NULL)
  

library(tidymodels)

comp_split <- computer_raw %>%
  distinct(value_id,.keep_all=TRUE)%>%
  select(char_type,interaction)%>%
  initial_split(prop=0.8,strata=char_type)

comp_train <- training(comp_split)
comp_test <- testing(comp_split)

set.seed(234)
comp_folds <- bootstraps(comp_train,strata=char_type)


library(textrecipes)
library(themis)

rec_all <- recipe(char_type ~interaction,data=comp_train)%>%
           step_tokenize(interaction)%>%
           step_tokenfilter(interaction, max_tokens=80)%>%
           step_tfidf(interaction)

rec_all_norm <- 
  rec_all%>%
  step_normalize(all_predictors())

rec_all_smote <-
  rec_all_norm %>%
  step_smote(char_type)



rec_stop <- recipe(char_type ~interaction,data=comp_train)%>%
  step_tokenize(interaction)%>%
  step_stopwords(interaction)%>%
  step_tokenfilter(interaction, max_tokens=80)%>%
  step_tfidf(interaction)

rec_stop_norm <- 
  rec_stop%>%
  step_normalize(all_predictors())

rec_stop_smote <-
  rec_stop_norm %>%
  step_smote(char_type)


svm_spec <- svm_linear()%>%
  set_mode("classification")%>%
  set_engine("LiblineaR")


library(discrim)

nb_spec<- naive_Bayes()%>%
  set_mode("classification")%>%
  set_engine("naivebayes")



comp_models <- workflow_set(
  preproc=list(all = rec_all,
               all_norm = rec_all_norm,
               stop = rec_stop,
               stop_norm = rec_stop_norm,
               stop_smote = rec_stop_smote),
  models= list(nb = nb_spec,svm_spec),
  cross=TRUE
)

doParallel::registerDoParallel()

set.seed(123)
comp_rs <- comp_models%>%
  workflow_map(
    "fit_resamples",
    resamples = comp_folds,
    metrics = metric_set(accuracy,sensitivity,specificity)
  )

autoplot(comp_rs)

rank_results(comp_rs)%>%filter(.metric=="accuracy")

comp_wf <- workflow(rec_all,svm_spec)
comp_fitted <- last_fit(
  comp_wf,
  comp_split,
  metrics=metric_set(accuracy,sensitivity,specificity)
)

collect_metrics(comp_fitted)
collect_predictions(comp_fitted)%>%
  conf_mat(char_type,.pred_class)%>%
  autoplot()

extract_workflow(comp_fitted)%>%
  tidy()%>%
  group_by(estimate>0)%>%
  slice_max(abs(estimate),n=10)%>%
  ungroup()%>%
  mutate(term = str_remove(term,"tfidf_interaction_"))%>%
  ggplot(aes(estimate,fct_reorder(term,estimate),fill=estimate>0))+
  geom_col(alpha=0.8)+
  scale_fill_discrete(labels=c("people","computer"))+
  labs(y=NULL)