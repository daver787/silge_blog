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

lower_words <- 
  

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
