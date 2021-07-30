library(tidyverse)
library(tvthemes)
theme_set(theme_avatar(title.font="Slayer",
                       text.font="Slayer",
                       title.size=14))
avatar_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv")


avatar_raw %>% count(character, sort= TRUE)

library(tidytext)

avatar_raw %>%
  filter(!is.na(character_words))%>%
  mutate(book = fct_inorder(book),
         character= fct_lump_n(character,10))%>%
  count(book,character)%>%
  mutate(character = reorder_within(character,n,book))%>%
  ggplot(aes(n, character, fill= book))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~book, scales = "free")+
  scale_y_reordered()+
  scale_fill_manual(values= c(avatar_pal("WaterTribe")(1),
                              avatar_pal("EarthKingdom")(1),
                              avatar_pal("FireNation")(1)))+
  labs(y=NULL)


avatar <- avatar_raw %>%
  filter(!is.na(character_words))%>%
  mutate(aang=if_else(character=="Aang","Aang","Other"))%>%
  select(aang,book,text=character_words)



avatar %>%
  filter(aang =="Aang")%>%
  sample_n(10)%>%
  pull(text)


library(tidylo)
avatar_lo <- avatar %>%
  unnest_tokens(word,text)%>%
  count(aang,word)%>%
  bind_log_odds(aang,word,n)%>%
  arrange(-log_odds_weighted)


avatar_lo %>%
  group_by(aang)%>%
  slice_max(log_odds_weighted,n=15)%>%
  ungroup()%>%
  mutate(word=reorder(word,log_odds_weighted))%>%
  ggplot(aes(log_odds_weighted,word,fill=aang))+
  geom_col(alpha=0.8,show.legend=FALSE)+
  facet_wrap(~aang,scales="free")+
  scale_fill_avatar("AirNomads")+
  labs(y=NULL)

library(textfeatures)
tf <-textfeatures(avatar,sentiment=FALSE,word_dims=0,normalize=FALSE)


tf %>%
  bind_cols(avatar)%>%
  group_by(aang)%>%
  summarise(across(starts_with("n_"),mean))%>%
  pivot_longer(starts_with("n_"),names_to="text_feature")%>%
  filter(value>0.01)%>%
  mutate(text_feature=fct_reorder(text_feature,-value))%>%
  ggplot(aes(aang,value,fill=aang))+
  geom_col(position="dodge",alpha=0.8,show.legend=FALSE)+
  facet_wrap(~text_feature,scales="free",ncol=6)+
  scale_fill_avatar("AirNomads")+
  labs(x=NULL,y="Mean text features per spoken line")



library(tidymodels)
library(themis)
library(textrecipes)
set.seed(123)
avatar_split <-initial_split(avatar,strata=aang)
avatar_train<- training(avatar_split)
avatar_test<- testing(avatar_split)


set.seed(234)
avatar_folds <- vfold_cv(avatar_train,stara= aang)


avatar_rec<- recipe(aang~text,data=avatar_train)%>%
  step_downsample(aang)%>%
  step_textfeature(text)%>%
  step_zv(all_predictors())%>%
  step_normalize(all_predictors())

  
avatar_prec <- prep(avatar_rec)
juice(avatar_prec)


rf_spec <- rand_forest(trees=1000)%>%
  set_engine("ranger")%>%
  set_mode("classification")


svm_spec <- svm_rbf(cost=0.5)%>%
  set_engine("kernlab")%>%
  set_mode("classification")


avatar_wf <- workflow() %>%
  add_recipe(avatar_rec)



doParallel::registerDoParallel()

rf_rs <- avatar_wf%>%
  add_model(rf_spec)%>%
  fit_resamples(resamples=avatar_folds,
                metrics=metric_set(roc_auc,accuracy,sens,spec),
                control=control_resamples(save_pred=TRUE))

collect_metrics(rf_rs)
conf_mat_resampled(rf_rs)
  
  
svm_rs <- avatar_wf%>%
  add_model(svm_spec)%>%
  fit_resamples(resamples=avatar_folds,
                metrics=metric_set(roc_auc,accuracy,sens,spec),
                control=control_resamples(save_pred=TRUE))

collect_metrics(svm_rs)
conf_mat_resampled(svm_rs)


svm_rs %>%
  collect_predictions()%>%
  group_by(id)%>%
  roc_curve(aang,.pred_Aang)%>%
  ggplot(aes(1-specificity,sensitivity,color=id))+
  geom_abline(lty=2,alpha=0.8,size=1.5)+
  geom_path(show.legend=FALSE,alpha=0.6,size=1.2)+
  scale_color_avatar("EarthKingdom")+
  coord_equal()