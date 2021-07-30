library(tidyverse)
library(tidytext)
library(tidymodels)
library(textrecipes)
artwork <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv")
glimpse(artwork)

artwork %>%
  count(medium,sort =TRUE)

artwork %>%
  ggplot(aes(year))+
  geom_histogram(alpha=0.8,fill="midnightblue")


tate_df <- artwork %>%
  filter(year>1750)%>%
  select(year,medium)%>%
  na.omit()%>%
  arrange(year)

tate_df %>%
  unnest_tokens(word,medium)%>%
  count(word,sort=TRUE)

set.seed(123)
art_split <- initial_split(tate_df,strata=year)
art_train <- training(art_split)
art_test <- testing(art_split)

set.seed(234)
art_folds <- vfold_cv(art_train,strata=year)


art_rec <- recipe(year~medium,data=art_train)%>%
  step_tokenize(medium)%>%
  step_stopwords(medium)%>%
  step_tokenfilter(medium,max_tokens=500)%>%
  step_tf(medium)%>%
  step_normalize(all_predictors())


sparse_bp <- hardhat::default_recipe_blueprint(composition = "dgCMatrix")

lasso_spec <- linear_reg(penalty=tune(),mixture=1)%>%
  set_engine("glmnet")

art_wf <- workflow() %>%
  add_recipe(art_rec,blueprint=sparse_bp)%>%
  add_model(lasso_spec)


#doParallel::registerDoParallel()  

lambda_grid <- grid_regular(penalty(range=c(-3,0)),levels=20)  


lasso_rs <- tune_grid(
  art_wf,
  resamples=art_folds,
  grid=lambda_grid
)