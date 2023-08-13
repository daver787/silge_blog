library(tidyverse)
library(infer)

airmen_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv")

airmen_raw %>% View()

airmen_raw %>% count(pilot_type)

airmen <- airmen_raw %>%
  mutate(pilot_type = str_replace_all(pilot_type, "Liason", "Liaison") ) %>%
  mutate(rank_at_graduation = str_replace_all(rank_at_graduation, "Captain", "Capt") ) 

airmen %>%
  ggplot(aes(x = pilot_type, fill =  rank_at_graduation)) +
  geom_bar(stat = "count")

aircraft <- c("Single engine", "Twin engine")
ranks    <- c("Flight Officer", "2nd Lt")

pilot_vs_rank <- 
airmen %>%
  filter(pilot_type %in% aircraft, rank_at_graduation %in% ranks) %>%
  specify(pilot_type ~ rank_at_graduation, success = "Twin engine")


set.seed(123)
bootstrapped <- pilot_vs_rank %>%
  generate(reps = 1000, type = "bootstrap")

bootstrapped

set.seed(234)
permuted <- pilot_vs_rank %>%
  hypothesise(null = "independence") %>%
  generate(reps = 1000, type = "permute")

permuted

observed <- pilot_vs_rank %>%
  calculate(stat = "chisq", order = ranks)

airmen_chisq <- bootstrapped %>%
  calculate(stat = "chisq", order = ranks)

get_ci(airmen_chisq)

visualize(airmen_chisq) +
  shade_ci(get_ci(airmen_chisq),
          fill  = "midnightblue",
          color = "midnightblue",
          lty   = 2)


bootstrapped %>%
  calculate(stat = "odds ratio", order = ranks) %>%
  visualize() +
  labs(title = "Bootsrap distribution of Tuskegee airmen's
        rank by aircraft type",
        x    = "Odds ratio of twin vs single engines (for flight officers compared to 2nd Lts.)")
  
