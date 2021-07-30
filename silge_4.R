library(tidyverse)
student_debt <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv")


student_debt %>%
  ggplot(aes(year,loan_debt_pct,color=race))+
  geom_point(size=3,alpha=0.8)+
  geom_smooth(method="lm",se=FALSE)
  labs(x=NULL,y="% of families with student loan debt",color= NULL)
  
  
library(tidymodels)
lm_spec <- linear_reg()%>%
  set_engine("lm")

lm_fit <- lm_spec %>%
  fit(loan_debt_pct~year*race,data=student_debt)

tidy(lm_fit)


new_points <-crossing(race=c("Black","Hispanic","White"),year=1990:2020)
augment(lm_fit,new_data=new_points)%>%
  ggplot(aes(year,.pred,color=race))+
  geom_line(size=1.2,alpha=0.7)+
  geom_smooth(method="lm",se=FALSE)
labs(x=NULL,y="% of families with student loan debt",color= NULL)