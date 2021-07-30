---
title: "NFL Attendance"
author: "David Espinola"
date: "5/2/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, echo = TRUE, fig.width = 8, fig.height = 5)
library(tidyverse)
library(knitr)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r read_data}
attendance <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv")
standings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv")

attendance_joined <- attendance %>%
  left_join(standings,by = c("year","team_name","team"))
```

## Including Plots

You can also embed plots, for example:

```{r eda, echo=FALSE}
attendance_joined %>%
  filter(!is.na(weekly_attendance))%>%
  ggplot(aes(fct_reorder(team_name,weekly_attendance),
         weekly_attendance,
         fill = playoffs))+
  geom_boxplot(outlier.alpha =0.5)+
  coord_flip()
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r}
attendance_joined %>%
  distinct(team_name, year, margin_of_victory, playoffs) %>%
  ggplot(aes(margin_of_victory,fill=playoffs))+
  geom_histogram( position= "identity",alpha= 0.7)
```
```{r}
attendance_joined %>%
  mutate(week=factor(week)) %>%
  ggplot(aes(week, weekly_attendance, fill= week))+
  geom_boxplot(show.legend= FALSE,outlier.alpha=0.4)
```

```{r}
attendance_df <- attendance_joined %>%
  filter(!is.na(weekly_attendance))%>%
  select(weekly_attendance, team_name, year, week,margin_of_victory, strength_of_schedule, playoffs)

attendance_df
```

```{r}
library(tidymodels)

attendance_split <- attendance_df %>%
  initial_split(strata=playoffs)

nfl_train <- training(attendance_split)
nfl_test <- testing(attendance_split)
```




```{r}
lm_spec <- linear_reg() %>%
  set_engine(engine = "lm")%>%
  set_mode("regression")


lm_fit <- lm_spec %>%
  fit(weekly_attendance ~.,data=nfl_train)
```

```{r}
rf_spec <- rand_forest(mode="regression") %>%
  set_engine("ranger")
  
rf_fit <- rf_spec %>% 
  fit(weekly_attendance ~., data = nfl_train)
```


```{r}
results_train <- lm_fit %>%
  predict(new_data= nfl_train )%>%
  mutate(truth= nfl_train$weekly_attendance,model="lm")%>%
  bind_rows(rf_fit %>%
              predict(new_data = nfl_train)%>%
              mutate(truth = nfl_train$weekly_attendance, model = "rf"))


results_test <- lm_fit %>%
  predict(new_data= nfl_test )%>%
  mutate(truth= nfl_test$weekly_attendance,model="lm")%>%
  bind_rows(rf_fit %>%
              predict(new_data = nfl_test)%>%
              mutate(truth = nfl_test$weekly_attendance, model = "rf"))

```

