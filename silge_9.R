library(tidyverse)
train_raw <- read_csv("train.csv")

train_raw %>%
  ggplot(aes(price,fill=neighbourhood_group))+
  geom_histogram(position="identity",alpha=0.5,bins=20)+
  scale_x_log10(labels=scales::dollar_format())+
  labs(fill=NULL,x="price per night")


train_raw%>%
  ggplot(aes(longitude,latitude,z=log(price)))+
  stat_summary_hex(alpha=0.8,bins=70)+
  scale_fill_viridis_c()+
  labs(fill="Mean price (log)")

library(tidymodels)
set.seed(123)
nyc_split <-train_raw %>%
  mutate(price=log(price+1))%>%
  initial_split(strata=price)

nyc_train <- training(nyc_split)
nyc_test <- testing(nyc_split)

set.seed(234)
nyc_folds <- vfold_cv(nyc_train,v=5,strata=price)


library(textrecipes)
nyc_rec <- recipe(price ~latitude+longitude+neighbourhood+room_type
                  +minimum_nights+number_of_reviews+availability_365+name,data=nyc_train)%>%
          step_novel(neighbourhood)%>%
          step_other(neighbourhood,threshold=0.01)%>%
          step_tokenize(name)%>%
          step_stopwords(name)%>%
          step_tokenfilter(name,max_tokens=30)%>%
          step_tf(name)

library(baguette)

bag_spec <- bag_tree(min_n=10)%>%
            set_engine("rpart",times=25)%>%
            set_mode("regression")


bag_wf <- 
  workflow()%>%
  add_recipe(nyc_rec)%>%
  add_model(bag_spec)

set.seed(123)
bag_fit <- fit(bag_wf,data=nyc_train) 

doParallel::registerDoParallel()
bag_rs <- fit_resamples(bag_wf,nyc_folds)
collect_metrics(bag_rs)

test_rs <- augment(bag_fit,nyc_test)

test_rs %>%
  ggplot(aes(exp(price),exp(.pred),color=neighbourhood_group))+
  geom_abline(slope=1,lty=2,color="gray50",alpha=0.5)+
  geom_point(alpha=0.2)+
  scale_x_log10(labels=scales::dollar_format())+
  scale_y_log10(labels=scales::dollar_format())+
  labs(color=NULL,x="True price",y="Predicted price")

          
library(rlang) 
rmsle_vec <- function(truth,estimate,na_rm=TRUE,...){
  
  rmsle_impl <- function(truth, estimate) {
    sqrt(mean((log(truth+1) - log(estimate+1))^2))
  }
  metric_vec_template(metric_impl = rmsle_impl, truth = truth, 
                      estimate = estimate, na_rm = na_rm, cls = "numeric",...)
  
  
}


rmsle <- function(data,..){
  UseMethod("rmsle")
}

rmsle <- new_numeric_metric(rmsle,direction="minimize")

rmsle.data.frame <- function(data,truth,estimate,na_rm=TRUE,...){
  metric_summarizer(
    metric_nm="rmsle",
    metric_fn=rmsle_vec,
    data=data,
    truth=!! enquo(truth),
    estimate=!!enquo(estimate),
    na_rm = na_rm,
    ...)
}

test_rs %>%
       mutate(across(c(price,.pred),exp))%>%rmsle(price,.pred)

