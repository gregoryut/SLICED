library(tidyverse)
library(tidymodels)
library(tidytext)
library(here)
library(doParallel)
library(parallel)
theme_set(theme_minimal())


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
  count(rank, sort = TRUE)



df_train %>%
  count(game, sort = TRUE)


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
         rank = factor(rank),
         month = factor(month), 
         year = factor(year))

df_test <- df_test %>%
  mutate(game = case_when(is.na(game) ~ "Unknown Game NA",
                          TRUE ~ game),
         month = factor(month), 
         year = factor(year))




glimpse(df_train)




# updated model after wathing Nick's stream -------------------------------

xgb_spec <-
  boost_tree(mtry = tune(),
             learn_rate = tune(),
             trees = tune(),
             tree_depth = tune(),
             min_n = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')


xgb_rec <- recipe(hours_watched ~ year + hours_streamed + peak_viewers + peak_channels +
                    streamers + avg_viewer_ratio, data = df_train) %>%
  step_dummy(all_nominal(), one_hot = TRUE)




xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(xgb_rec)

folds <- df_train %>%
  vfold_cv(v=10)



library(finetune)
library(tictoc)


cores <- parallel::detectCores(logical = FALSE)
cores

cl <- makePSOCKcluster(cores - 1)
registerDoParallel(cl)



tic()
xg_fit_reg <- tune_grid(xgb_wf,
                        folds,
                        grid = 10,
                        metrics = metric_set(rmse),
                        control = control_grid(verbose = TRUE, 
                                               parallel_over = "everything"))
toc()


autoplot(xg_fit_reg)


xg_fit_reg %>%
  collect_metrics() %>%
  arrange(desc(mean))


xgb_best <- xgb_wf %>%
  finalize_workflow(select_best(xg_fit_reg))

xgb_best_fit <- xgb_best %>%
  fit(df_train)


xgb_best_fit %>%
  augment(df_test) %>%
  arrange(desc(.pred)) %>%
  mutate(Rank = 1:200) %>%
  select(Game = game, Rank) %>%
  write_csv("sub_rank.csv")



tic()
xg_fit <- xgb_wf %>%
  tune_race_anova(folds,
                  grid = 10,
                  metrics = metric_set(rmse),
                  control = control_race(verbose_elim = TRUE))
toc()

plot_race(xg_fit)

xg_fit %>% 
  collect_metrics() %>%
  arrange(desc(mean))


xg_fit_best <- xgb_wf %>%
  finalize_workflow(select_best(xg_fit))


xg_fit_best <- xg_fit_best %>%
  fit(df_train)

xg_fit_best %>%
  augment(df_test) %>%
  arrange(desc(.pred)) %>%
  mutate(Rank = 1:200) %>%
  select(Game = game, Rank) %>%
  write_csv("sub_rank2.csv")



## Private score = 0.24747

















