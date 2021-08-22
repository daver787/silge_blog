library(tidyverse)
train_raw <- read_csv("train_3.csv")

price_plot <- train_raw %>%
  mutate(priceRange=parse_number(priceRange)+100000)%>%
  ggplot(aes(longitude,latitude,z=priceRange))+
  stat_summary_hex(alpha=0.8,bins=30)+
  scale_fill_viridis_c()+
  labs(fill="mean",title="Price")


library(patchwork)
plot_austin <- function(var,title){
  train_raw %>%
    mutate(priceRange=parse_number(priceRange)+100000)%>%
    ggplot(aes(longitude,latitude,z={{var}}))+
    stat_summary_hex(alpha=0.8,bins=30)+
    scale_fill_viridis_c()+
    labs(fill="mean",title=title)
  
}

(price_plot + plot_austin(avgSchoolRating,"School rating"))/
  (plot_austin(yearBuilt,"Year built") + plot_austin(log(lotSizeSqFt),"Lot size"))

library(tidytext)
austin_tidy <-train_raw%>%
  mutate(priceRange=parse_number(priceRange)+100000)%>%
  unnest_tokens(word,description)%>%
  anti_join(get_stopwords())

top_words <- austin_tidy%>%
  count(word, sort=TRUE)%>%
  filter(!word %in% as.character(1:5))%>%
  slice_max(n,n=100)%>%
  pull(word)

word_freqs <- austin_tidy %>%
  count(word,priceRange)%>%
  complete(word,priceRange,fill=list(n=0))%>%
  group_by(priceRange)%>%
  mutate(price_total=sum(n),
         proportion=n/price_total)%>%
  ungroup()%>%
  filter(word %in% top_words)

word_mods <- word_freqs %>%
  nest(data=-word)%>%
  mutate(model=map(data,~glm(cbind(n,price_total)~priceRange,.,family="binomial")),
         model = map(model,tidy))%>%
  unnest(model)%>%
  filter(term=="priceRange")%>%
  mutate(p.value=p.adjust(p.value))%>%
  arrange(-estimate)

library(ggrepel)

word_mods %>%
  ggplot(aes(estimate,p.value))+
  geom_vline(xintercept = 0,lty=2,alpha=0.7,color="gray50")+
  geom_point(color="midnightblue",alpha=0.8,size=1.5)+
  scale_y_log10()+
  geom_text_repel(aes(label=word),family="IBMPlexSans")
  

higher_words <- 
  word_mods %>%
  filter(p.value<0.05)%>%
  slice_max(estimate,n=12)%>%
  pull(word)

lower_words <- word_mods %>%
  filter(p.value<0.05)%>%
  slice_max(-estimate,n=12)%>%
  pull(word)
  

word_freqs %>%
  filter(word %in% higher_words)%>%
  ggplot(aes(priceRange, proportion,color=word))+
  geom_line(size=2.5,alpha=0.7,show.legend=FALSE)+
  facet_wrap(vars(word),scales="free_y")+
  scale_x_continuous(labels=scales::dollar)+
  scale_y_continuous(labels=scales::percent,limits=c(0,NA))

#finally starting the modeling
library(tidymodels)
set.seed(123)

austin_split <- train_raw%>%
  select(-city)%>%
  mutate(description=str_to_lower(description))%>%
  initial_split(strata=priceRange)

austin_train <- training(austin_split)
austin_test <-testing(austin_split) 

set.seed(234)
austin_folds<- vfold_cv(austin_train,v=5,strata=priceRange)

higher_pat<- glue::glue_collapse(higher_words,sep='|')
lower_pat<- glue::glue_collapse(lower_words,sep='')


austin_rec <- recipe(priceRange~.,data=austin_train)%>%
  update_role(uid,new_role="uid")%>%
  step_regex(description,pattern=higher_pat,result="high_price_words")%>%
  step_regex(description,pattern=lower_pat,result="low_price_words")%>%
  step_rm(description)%>%
  step_novel(homeType)%>%
  step_unknown(homeType)%>%
  step_other(homeType,threshold=0.02)%>%
  step_dummy(all_nominal_predictors(),one_hot=TRUE)%>%
  step_nzv(all_predictors())

xgb_spec <- 
  boost_tree(
    trees=1000,
    tree_depth=tune(),
    min_n=tune(),
    mtry=tune(),
    sample_size=tune(),
    learn_rate=tune()
  )%>%
  set_engine("xgboost")%>%
  set_mode("classification")

xgb_word_workflow <- workflow(austin_rec,xgb_spec)


set.seed(123)

xgb_grid<- grid_max_entropy(
  tree_depth(c(5L,10L)),
  min_n(c(10L,40L)),
  mtry(c(5L,10L)),
  sample_prop(c(0.5,1.0)),
  learn_rate(c(-2,-1)),
  size=20
  )

library(finetune)
doParallel::registerDoParallel()

set.seed(234)
xgb_word_rs <-
  tune_race_anova(
    xgb_word_workflow,
    austin_folds,
    grid=xgb_grid,
    metrics=metric_set(mn_log_loss),
    control=control_race(verbose_elim=TRUE)
  )
  
plot_race(xgb_word_rs)
show_best(xgb_word_rs)

xgb_last <- xgb_word_workflow %>%
  finalize_workflow(select_best(xgb_word_rs,"mn_log_loss"))%>%
  last_fit(austin_split)

collect_predictions(xgb_last)%>%
  mn_log_loss(priceRange,`.pred_0-250000`:`.pred_650000+`)

collect_predictions(xgb_last)%>%
  conf_mat(priceRange,.pred_class)%>%
  autopplot()

collect_predictions(xgb_last)%>%
  roc_curve(priceRange,`.pred_0-250000`:`.pred_650000+`)%>%
  ggplot(aes(1-specificity,sensitivity,color=.level))+
  geom_path(alpha=0.8,size=1.2)+
  coord_equal()+
  labs(color=NULL)

library(vip)

extract_workflow(xgb_last)%>%
  extract_fit_parsnip()%>%
  vip(geom="point",num_features=15)
  
  