library(tidyverse)
library(tidymodels)
library(tidytext)
library(here)
library(nnet)

df_train <- read_csv(here("ep3", "train.csv")) %>%
  janitor::clean_names()

df_test <- read_csv(here("ep3", "test.csv")) %>%
  janitor::clean_names()

glimpse(df_train)




# do some EPA -------------------------------------------------------------


# each rank appears 64 times - 63 times
df_train %>% 
  count(rank, sort = TRUE) %>%
  tail()

df_train %>%
  count(game, sort = TRUE)


df_train %>%
  group_by(month) %>%
  summarize(hours_watched_month =  sum(hours_watched)) %>%
  ggplot(aes(month, hours_watched_month)) +
  geom_point() +
  geom_smooth()

df_train %>%
  group_by(month) %>%
  summarize(hours_watched_month =  sum(hours_watched)) %>%
  ggplot(aes(month, hours_watched_month)) +
  geom_point() +
  geom_smooth() +
  theme_light()


df_train %>%
  group_by(month) %>%
  summarize(hours_streamed_month =  sum(hours_streamed )) %>%
  ggplot(aes(month, hours_streamed_month)) +
  geom_point() +
  geom_smooth() +
  theme_light()



df_train %>%
  group_by(month) %>%
  summarize(peak_viewers_month =  sum(peak_viewers )) %>%
  ggplot(aes(month, peak_viewers_month)) +
  geom_point() +
  geom_smooth() +
  theme_light()

df_train %>%
  group_by(month) %>%
  summarize(peak_channels_month =  sum(peak_channels)) %>%
  ggplot(aes(month, peak_channels_month)) +
  geom_point() +
  geom_smooth() +
  theme_light()

df_train %>%
  group_by(month) %>%
  summarize(streamers_month =  sum(streamers)) %>%
  ggplot(aes(month, streamers_month)) +
  geom_point() +
  geom_smooth() +
  theme_light()


df_train %>%
  group_by(month) %>%
  summarize(avg_viewer_ratio_month =  sum(avg_viewer_ratio)) %>%
  ggplot(aes(month, avg_viewer_ratio_month)) +
  geom_point() +
  geom_smooth() +
  theme_light()



df_train %>%
  mutate(rank = as.integer(rank)) %>%
  filter(rank < 4) %>%
  ggplot(aes(year, rank, color = game)) +
  geom_point() +
  geom_line()


df_train %>%
  group_by(year, month) %>%
  summarize(sum_viewer_ratio_month =  sum(avg_viewer_ratio)) 

df_train %>%
  group_by(game) %>%
  summarise(avg_viewer_ratio_month =  sum(avg_viewer_ratio),
            sum_rank = sum(rank)) %>%
  arrange(sum_rank)


str(df_train)

df_train %>%
  select(where(is.numeric)) %>%
  cor() %>%
  heatmap()


colSums(is.na(df_train))

df_train <- df_train %>%
  mutate(game = case_when(is.na(game) ~ "Unknown Game NA",
                          TRUE ~ game),
         rank = factor(rank))

df_train <- df_train %>%
  select(-hours_watched)




# Xgboost -----------------------------------------------------------------


xgb_spec <-
  boost_tree() %>%
  set_engine('xgboost') %>%
  set_mode('classification')


xgb_rec <- recipe(rank ~ ., data = df_train) %>%
  step_mutate(month = factor(month), year = factor(year)) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(xgb_rec)



xg_fit <- xgb_wf %>%
  fit(data = df_train)
  

df_train %>%
  cbind(., predict(xg_fit, ., type = "class")) %>%
  accuracy(rank, .pred_class)

df_test %>%
  cbind(., predict(xg_fit, ., type = "class")) %>%
  select(Game = game, Rank = .pred_class) %>%
  arrange(Rank) %>%
  write_csv("second_sub.csv")


# random forrest ----------------------------------------------------------------

library(baguette)



rec <- recipe(rank ~ ., data = df_train) %>%
  step_mutate(month = factor(month), year = factor(year)) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes())

bag_spec <-
  bag_tree(min_n = 10) %>%
  set_engine("rpart", times = 25) %>%
  set_mode("classification")


bag_wf <- workflow() %>%
  add_model(bag_spec) %>%
  add_recipe(rec)

set.seed(123)
bag_fit <- fit(bag_wf, data = df_train)
bag_fit


df_test %>%
  cbind(., predict(bag_fit, ., type = "class")) %>%
  select(Game = game, Rank = .pred_class) %>%
  arrange(Rank) %>%
  write_csv("first_sub.csv")



## Placed 19th
























