library(tidyverse)
library(tidymodels)
library(parsnip)
library(bart)
library(dbarts)
library(stacks)
library(ranger)
library(rpart)
library(glmnet)
library(poissonreg)
library(patchwork)
library(GGally)
library(skimr)
library(DataExplorer)
library(vroom)
sample <- "sampleSubmission.csv"
test <- "test.csv"
train <- "train.csv"
sample1 <- vroom(sample)
test1 <- vroom(test)
train1 <- vroom(train)
train1
glimpse(test1)
skim(test1)
plot_intro(train1)
plot_correlation(train1)
colnames(train1)
trainer <- data.frame()
mycleandata1 <- train1 %>% 
  select(id,cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9,cat10,loss)
plot_correlation(mycleandata1)
mycleandata2 <- train1 %>% 
  select(cat11,cat12,cat13,cat14,cat15,cat16,cat17,cat18,cat19,cat20,loss)
plot_correlation(mycleandata2)
mycleandata3 <- train1 %>% 
  select(cat21,cat22,cat23,cat24,cat25,cat26,cat27,cat28,cat29,cat30,loss)
plot_correlation(mycleandata3)
mycleandata4 <- train1 %>% 
  select(cat31,cat32,cat33,cat34,cat35,cat36,cat37,cat38,cat39,cat40,loss)
plot_correlation(mycleandata4)
mycleandata5 <- train1 %>% 
  select(cat41,cat42,cat43,cat44,cat45,cat46,cat47,cat48,cat49,cat50,loss)
plot_correlation(mycleandata5)
mycleandata6 <- train1 %>% 
  select(cat51,cat52,cat53,cat54,cat55,cat56,cat57,cat58,cat59,cat60,loss)
plot_correlation(mycleandata6)
mycleandata7 <- train1 %>% 
  select(cat61,cat62,cat63,cat64,cat65,cat66,cat67,cat68,cat69,cat70,loss)
plot_correlation(mycleandata7)
mycleandata8 <- train1 %>% 
  select(cat71,cat72,cat73,cat74,cat75,cat76,cat77,cat78,cat79,cat80,loss)
plot_correlation(mycleandata8)
mycleandata9 <- train1 %>% 
  select(cat81,cat82,cat83,cat84,cat85,cat86,cat87,cat88,cat89,cat90,loss)
plot_correlation(mycleandata9)
mycleandata1 <- train1 %>% 
  select(cat116,loss)
plot_correlation(mycleandata1,maxcat=200)
mycleandata10 <- train1 %>% 
  select(cat91,cat92,cat93,cat94,cat95,cat96,cat97,cat98,cat99,cat100,loss)
plot_correlation(mycleandata10)
mycleandata11 <- train1 %>% 
  select(cat101,cat102,cat103,cat104,cat105,cat106,cat107,cat108,cat109,cat110,loss)
plot_correlation(mycleandata11)
mycleandata12 <- train1 %>% 
  select(cat111,cat112,cat113,cat114,cat115,cat116,loss)
plot_correlation(mycleandata12)
mycleandata13 <- train1 %>% 
  select(cont1,cont2,cont3,cont4,cont5,cont6,cont7,cont8,loss)
plot_correlation(mycleandata13)
mycleandata14 <- train1 %>% 
  select(cont9,cont10,cont11,cont12,cont13,cont14,loss)
plot_correlation(mycleandata14)
mycleandata <- train1 %>% 
  select(cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat10,cat11,cat12,cat13,cat16,cat23,cat28,cat36,cat38,cat40,cat50,cat57,cat72,cat73,cat76,cat79,cat80,cat81,cat82,cat87,cat90,cat100,cat101,cat112,cat114,cat115,cont2,cont3,cont7,cont11,cont12,loss)
mycleandata
mytestdata <- test1 %>% 
  select(cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat10,cat11,cat12,cat13,cat16,cat23,cat28,cat36,cat38,cat40,cat50,cat57,cat72,cat73,cat76,cat79,cat80,cat81,cat82,cat87,cat90,cat100,cat101,cat112,cat114,cat115,cont2,cont3,cont7,cont11,cont12)

colnames(mycleandata)
colnames(mytestdata)
my_linear_model <- linear_reg() %>% #create linear model
  set_engine("lm") %>% 
  set_mode("regression") %>% 
  fit(formula=loss ~ .,data=mycleandata)
my_linear_model
bike_predictions <- predict(my_linear_model,
                            new_data=mytestdata)
