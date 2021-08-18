library(tidyverse)
library(tidymodels)
library(here)


train_raw <- read_csv(here("train.csv")) %>%
  select(-Name, -Zip, -NAICS)

glimpse(train_raw)


train_raw %>%
  count(Sector)

train_raw %>%
  count(NewExist)

train_raw %>%
  count(UrbanRural)

train_raw %>%
  count(default_amount)

train_raw %>%
  count(City, sort = TRUE)

train_raw %>%
  count(City, sort = TRUE)

train_raw %>%
  count(State, sort = TRUE)

train_raw %>%
  count(Bank, sort = TRUE)

train <- train_raw %>%
  mutate(Sector = fct_lump(Sector, 17),
         City = fct_lump(City , 15),
         State = fct_lump(State, 15),
         Bank = fct_lump(Bank, 15),
         BankState = fct_lump(BankState, 15),
         NewExist = factor(ifelse(is.na(NewExist), "None", NewExist)),
         UrbanRural = factor(UrbanRural))

glimpse(train)


train %>%
  mutate(default_flag = ifelse(default_amount > 0, 1, 0)) %>%
  summarise(mean_flag = mean(default_flag))


xgb_spec <- boost_tree(
  mode = "regression",
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost")

rec <- recipe(default_amount ~ ., data = train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_normalize(all_numeric_predictors())

prep(rec) %>% bake(train)


folds <- train %>%
  vfold_cv(v = 3)

xgb_wf <- workflow(rec, xgb_spec)

colSums(is.na(train))


xgb_tuned <- tune_grid(
  xgb_wf,
  folds,
  metrics = metric_set(mae),
  grid = 10,
  control = control_grid(verbose = TRUE)
)



autoplot(xgb_tuned)

xgb_tuned %>%
  collect_metrics() %>%
  arrange(mean)

xgb_best <- xgb_wf %>%
  finalize_workflow(select_best(xgb_tuned, "mae")) %>%
  fit(train)


test <- read_csv(here("test.csv"))

test <- test %>% 
  mutate(Sector = fct_lump(Sector, 17),
         City = fct_lump(City , 15),
         State = fct_lump(State, 15),
         Bank = fct_lump(Bank, 15),
         BankState = fct_lump(BankState, 15),
         NewExist = factor(ifelse(is.na(NewExist), "None", NewExist)),
         UrbanRural = factor(UrbanRural))

# sub
xgb_best %>%
  augment(test) %>%
  select(LoanNr_ChkDgt, default_amount = .pred) %>%
  write_csv('sub_final.csv')






