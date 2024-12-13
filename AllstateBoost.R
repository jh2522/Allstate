library(vroom)
library(tidyverse)
library(tidymodels)
library(bonsai)
library(embed)
library(DataExplorer)
test <- "test.csv"
train <- "train.csv"
test1 <- vroom(test)
train1 <- vroom(train)

plot_correlation(train1)
#This had to be done in parts due to the large number of columns in the dataset

mycleandata <- train1 %>% 
  select(cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat10,
         cat11,cat12,cat13,cat16,cat23,cat28,cat36,cat38,
         cat40,cat50,cat57,cat72,cat73,cat76,cat79,cat80,
         cat81,cat82,cat87,cat90,cat100,cat101,cat112,
         cat114,cat115,cont2,cont3,cont7,cont11,cont12,loss) %>% 
  mutate(loss=log(loss))

mytestdata <- test1 %>% 
  select(cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat10,
         cat11,cat12,cat13,cat16,cat23,cat28,cat36,cat38,
         cat40,cat50,cat57,cat72,cat73,cat76,cat79,cat80,
         cat81,cat82,cat87,cat90,cat100,cat101,cat112,
         cat114,cat115,cont2,cont3,cont7,cont11,cont12)

my_recipe <- recipe(loss~., data=mycleandata) %>% 
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(loss)) %>% 
  step_normalize(all_numeric_predictors()) 

boost_model <- boost_tree(tree_depth=tune(), trees=400,learn_rate = tune()) %>% 
  set_engine("lightgbm") %>% 
  set_mode("regression")

boost_wf <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(boost_model)


tuning_grid_boost <- grid_regular(learn_rate(), tree_depth(), levels=5)

folds_boost <- vfold_cv(mycleandata, v = 5, repeats=1)

CV_results_boost <- boost_wf %>% 
  tune_grid(resamples=folds_boost, grid=tuning_grid_boost, metrics=metric_set(mae))
CV_results_boost
bestTune_boost <- CV_results_boost %>% 
  select_best(metric="mae")

final_wf_boost <- 
  boost_wf %>% 
  finalize_workflow(bestTune_boost) %>% 
  fit(data=mycleandata)

predict_boost <- final_wf_boost %>% 
  predict(new_data=mytestdata)
predict_boost <- exp(predict_boost)

kaggle_submission <- predict_boost %>% 
  bind_cols(., test1) %>% 
  select(id, .pred) %>% 
  rename(loss=.pred)
vroom_write(x=kaggle_submission, file="./Allstateboost.csv", delim=",") #upload a csv file

