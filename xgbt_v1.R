library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(ggplot2)

root = "/Users/yuhw/bjetml/"
setwd(root);
theme_set(theme_bw())
set.seed(1987)

load_my_data = function(x) {
  data = read.csv(x, header = FALSE)
  colnames(data) = c("Label"
                     , "jetpt", "ntracks"
                     , "DCA1", "DCA2", "DCA3", "DCA4", "DCA5"
                     , "SDCA1", "SDCA2", "SDCA3", "SDCA4", "SDCA5"
                     , "pt1", "pt2", "pt3", "pt4", "pt5"
  )
  
  data = data[, c("Label"
                  , "jetpt", "ntracks"
                  , "DCA1", "DCA2", "DCA3", "DCA4", "DCA5"
                  , "SDCA1", "SDCA2", "SDCA3", "SDCA4", "SDCA5")]
  
  # data[,1] = factor(data[,1], levels = c(5, -5, 4, -4, 0), labels = c(5, -5, 4, -4, 0))
  # levels(data[,1]) = c(1, 1, 0, 0, 0)
  
  data$Label[abs(data$Label)!=5] = 0
  data$Label[abs(data$Label)==5] = 1
  
  data
}

data = load_my_data("pp.csv")

ind = sample(2, nrow(data), replace = T, prob = c(.5, .5))
train = data[ind==1,]
test = data[ind==2,]

# Create matrix - One-Hot Encoding for Factor variables
train_matrix = xgb.DMatrix(data = as.matrix(train[,-1]), label = train[,1])
test_matrix = xgb.DMatrix(data = as.matrix(test[,-1]), label = test[,1])

# Parameters
nc = length(unique(train[,1]))
xgb_params = list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class" = nc)
watchlist = list(train = train_matrix, test = test_matrix)

# eXtreme Gradient Boosting Model
bst_model = xgb.train(params = xgb_params,
                      data = train_matrix,
                      nrounds = 100,
                      watchlist = watchlist,
                      eta = 0.3,
                      max.depth = 6,
                      gamma = 0,
                      subsample = 1,
                      colsample_bytree = 1,
                      missing = NA)

# 
e = data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')

#
imp = xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction & confusion matrix - test data
p = predict(bst_model, newdata = test_matrix)
pred = matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test[,1], max_prob = max.col(., "last")-1)

table(Prediction = pred$max_prob, Actual = pred$label)
  
  
  
  
  
  