library(tidyverse)
library(ISLR)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(glmnet)
library(rstudioapi)
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))



data_set <- edges %>% 
  filter(Domain == "성적경고",
         Domain == "성적경고해제") %>% 
  group_by(Domain, Num_year_term) %>% 
  full_join(student_info_by_semester, by = c("Source" = "student_code", 
                                             "Num_year_term")) %>% 
  ungroup() %>% 
  mutate(참여여부 = replace_na(참여여부, 0),
         참여여부 = as.factor(참여여부)) %>% 
  select_if(~sum(is.na(.))==0) %>%  # NA포함한 열 제거 
  mutate_if(~is.character(.), as.factor)
  select(-Target, -Domain, -Label, -(식별자:수료학기), 
         -생년월일, -학과코드, -학적상태, 
         -ent_year_term, -grd_year_term)

writexl::write_xlsx(data_set, "data_set.xlsx")
data_set <- readxl::read_xlsx("data_set.xlsx")   

data_set <- data_set %>% 
  filter(Domain == "성적경고",
         Domain == "성적경고해제") %>% 
  mutate_if(~is.character(.), as.factor) %>% 
  mutate(국적 = fct_lump(국적, n = 52))
  

#library
library(h2o) #gbm

#spin up h2o
h2o.init(nthreads = -1) #use this computer to the max
h2o.removeAll() # Clean slate - just in case the cluster was already running


mydata <- as.h2o(data_set %>% 
                   rename(y = 참여여부,
                          nationality = 국적))

splits <- h2o.splitFrame(mydata, c(0.6, 0.2),
                         seed = 1234)


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "valid.hex") 

## take a look at the first few rows of the data set
train[1:5,9]   ## rows 1-5, all columns

## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=9,                        ## the predictor columns, by column index
  y=3,                          ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1000000)                ## Set the random seed so that this can be
##  reproduced.

summary(rf1)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

rf1@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(rf1,valid = T)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Now we will try GBM. 
## First we will use all default settings, and then make some changes,
##  where the parameters and defaults are described.

gbm1 <- h2o.gbm(
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=9,                        ## the predictor columns, by column index
  y=3,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

###############################################################################
summary(gbm1)                   ## View information about the model.

h2o.hit_ratio_table(gbm1,valid = T)[1,2]
## Overall accuracy.

## This default GBM is much worse than our original random forest.
## The GBM is far from converging, so there are three primary knobs to adjust
##  to get our performance up if we want to keep a similar run time.
## 1: Adding trees will help. The default is 50.
## 2: Increasing the learning rate will also help. The contribution of each
##  tree will be stronger, so the model will move further away from the
##  overall mean.
## 3: Increasing the depth will help. This is the parameter that is the least
##  straightforward. Tuning trees and learning rate both have direct impact
##  that is easy to understand. Changing the depth means you are adjusting
##  the "weakness" of each learner. Adding depth makes each tree fit the data
##  closer. 
##
## The first configuration will attack depth the most, since we've seen the
##  random forest focus on a continuous variable (elevation) and 40-class factor
##  (soil type) the most.
##
## Also we will take a look at how to review a model while it is running.

###############################################################################
gbm2 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = valid,   ##
  x=9,                     ##
  y=3,                       ## 
  ntrees = 20,                ## decrease the trees, mostly to allow for run time
  ##  (from 50)
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 10,             ## increase the depth (from 5)
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType2",  ##
  seed = 2000000)             ##

#### While this is running, we can actually look at the model.
#### To do this we simply need a new connection to H2O.
#### This R console will run the model, so we need either another R console
####   or the web browser (or python, etc.).
#### In the demo, we will use Flow in our web browser
####  http://localhost:54321
#### And the focus will be to look at model performance, since we are using R to 
####  control H2O. So we can simply type in:
####  getModel "gbm_covType2"
###############################################################################

summary(gbm2)
h2o.hit_ratio_table(gbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]    ## review the new model's accuracy
###############################################################################

## This has moved us in the right direction, but still lower accuracy 
##  than the random forest.
## And it still has not converged, so we can make it more aggressive.
## We can now add the stochastic nature of random forest into the GBM
##  using some of the new H2O settings. This will help generalize 
##  and also provide a quicker runtime, so we can add a few more trees.

gbm3 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = valid,   ##
  x=1:12,                     ##
  y=13,                       ## 
  ntrees = 30,                ## add a few trees (from 20, though default is 50)
  learn_rate = 0.3,           ## increase the learning rate even further
  max_depth = 10,             ## 
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType3",  ##
  seed = 2000000)             ##
###############################################################################

summary(gbm3)
h2o.hit_ratio_table(rf1,valid = T)[1,2]     ## review the random forest accuracy
h2o.hit_ratio_table(gbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(gbm2,valid = T)[1,2]    ## review the second model's accuracy
h2o.hit_ratio_table(gbm3,valid = T)[1,2]    ## review the newest model's accuracy
###############################################################################

## Now the GBM is close to the initial random forest.
## However, we used a default random forest. 
## Random forest's primary strength is how well it runs with standard
##  parameters. And while there are only a few parameters to tune, we can 
##  experiment with those to see if it will make a difference.
## The main parameters to tune are the tree depth and the mtries, which
##  is the number of predictors to use.
## The default depth of trees is 20. It is common to increase this number,
##  to the point that in some implementations, the depth is unlimited.
##  We will increase ours from 20 to 30.
## Note that the default mtries depends on whether classification or regression
##  is being run. The default for classification is one-third of the columns.
##  The default for regression is the square root of the number of columns.

rf2 <- h2o.randomForest(        ##
  training_frame = train,       ##
  validation_frame = valid,     ##
  x=9,                     ##
  y=3,                       ## 
  model_id = "rf_covType2",     ## 
  ntrees = 200,                 ##
  max_depth = 30,               ## Increase depth, from 20
  stopping_rounds = 2,          ##
  stopping_tolerance = 1e-2,    ##
  score_each_iteration = T,     ##
  seed=3000000)                 ##
###############################################################################
summary(rf2)
h2o.hit_ratio_table(gbm3,valid = T)[1,2]    ## review the newest GBM accuracy
h2o.hit_ratio_table(rf1,valid = T)[1,2]     ## original random forest accuracy
h2o.hit_ratio_table(rf2,valid = T)[1,2]     ## newest random forest accuracy
###############################################################################

## So we now have our accuracy up beyond 95%. 
## We have witheld an extra test set to ensure that after all the parameter
##  tuning we have done, repeatedly applied to the validation data, that our
##  model produces similar results against the third data set. 

## Create predictions using our latest RF model against the test set.
finalRf_predictions<-h2o.predict(
  object = rf2
  ,newdata = test)

## Glance at what that prediction set looks like
## We see a final prediction in the "predict" column,
##  and then the predicted probabilities per class.
finalRf_predictions

## Compare these predictions to the accuracy we got from our experimentation
h2o.hit_ratio_table(rf2,valid = T)[1,2]             ## validation set accuracy
mean(finalRf_predictions$predict==test$Cover_Type)  ## test set accuracy

## We have very similar error rates on both sets, so it would not seem
##  that we have overfit the validation set through our experimentation.
##
## This concludes the demo, but what might we try next, if we were to continue?
##
## We could further experiment with deeper trees or a higher percentage of
##  columns used (mtries).
## Also we could experiment with the nbins and nbins_cats settings to control
##  the H2O splitting.
## The general guidance is to lower the number to increase generalization
##  (avoid overfitting), increase to better fit the distribution.
## A good example of adjusting this value is for nbins_cats to be increased
##  to match the number of values in a category. Though usually unnecessary,
##  if a problem has a very important categorical predictor, this can 
##  improve performance.
##
## Also, we can tune our GBM more and surely get better performance.
## The GBM will converge a little slower for optimal accuracy, so if we 
##  were to relax our runtime requirements a little bit, we could balance
##  the learn rate and number of trees used.
## In a production setting where fine-grain accuracy is beneficial, it is 
##  common to set the learn rate to a very small number, such as 0.01 or less,
##  and add trees to match. Use of early stopping is very powerful to allow 
##  the setting of a low learning rate and then building as many trees as 
##  needed until the desired convergence is met.
## As with random forest, we can also adjust nbins and nbins_cats.


### All done, shutdown H2O    
h2o.shutdown(prompt=FALSE)