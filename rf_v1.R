library(randomForest)
library(pROC)
library(tree)
library(reprtree)
library(plyr)
library(e1071)
library(caret)
library(PRROC)
library(ggplot2)
library(latex2exp)

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
  data[,1] = factor(data[,1], levels = c(5, -5, 4, -4, 0), labels = c(5, -5, 4, -4, 0))
  
  data = data[, c("Label"
                  , "jetpt", "ntracks"
                  , "DCA1", "DCA2", "DCA3", "DCA4", "DCA5"
                  , "SDCA1", "SDCA2", "SDCA3", "SDCA4", "SDCA5")]
  
  levels(data[,1]) = c(1, 1, 0, 0, 0)
  
  data
}

train_model = function(data) {
  model = randomForest(Label ~ ., data = data
                    , ntree = 200
                    , importance = T
                    #, proximity =T
                    )
  
  # reprtree:::plot.getTree(model, 1)
  
  varImpPlot(model)
  
  # MDSplot(model, data$Label, 2)
  
  model
}

test_model = function(model, test) {
  p_test = predict(model, test, type = 'prob')
  
  roc = roc(test$Label, p_test[,1])
  plot(roc,
       type = "S",
       main = paste("ROC, AUC = ", format(roc$auc,digits=3)),
       print.thres = c(0.5),
       print.thres.pattern = " Thrs = %.2f (Spec = %.4f, Sens = %.4f)", 
       print.thres.cex = 0.8)
  auc = auc(roc)
  print(auc)
  
  pr = pr.curve(scores.class0 = p_test[test$Label==1,1], scores.class1 = p_test[test$Label==0,1], curve = TRUE)
  #plot(pr)
  
  p = ggplot(data = data.frame(pr$curve), aes(x=X1,y=X2,color=X3))+
    geom_point(size = 3)+
    labs(x="b-jet Efficiency",y="b-jet Purity",
         title=paste("PR, AUC = ", format(pr$auc.integral,digits=3)),
         color="Threshold") +
    scale_colour_gradientn(colours=rainbow(4)) +
    coord_cartesian(ylim=c(0,1)) +
    theme(text = element_text(size=20))
  print(p)
  
  p = ggplot(data = data.frame(pr$curve[pr$curve[,3]>0,]), aes(x=X3,y=X2,color=X1))+
    geom_point(size = 3)+
    labs(x="Threshold",y="b-jet Purity",
         title=paste("b-jet Purity vs. Threshold"),
         color="b-jet Efficiency") +
    scale_colour_gradientn(colours=rainbow(4)) +
    theme(text = element_text(size=20))
  print(p)
  
  p = ggplot(data = data.frame(pr$curve), aes(x=X3,y=X1,color=X2))+
    geom_point(size = 3)+
    labs(x="Threshold",y="b-jet Efficiency",
         title=paste("b-jet Efficiency vs. Threshold"),
         color="b-jet Purity") +
    scale_colour_gradientn(colours=rainbow(4)) +
    theme(text = element_text(size=20))
  print(p)
  
  pr
}

data = load_my_data("pp.csv")
data.pos = data[data$Label == 1,]
data.neg = data[data$Label == 0,]
ind.pos = sample(2, nrow(data.pos), replace = T, prob = c(.5, .5))
ind.neg = sample(2, nrow(data.neg), replace = T, prob = c(.5, .5))
train = rbind(data.pos[ind.pos == 1,], data.neg[ind.neg == 1,])
test  = rbind(data.pos[ind.pos == 2,], data.neg[ind.neg == 2,])
#test  = data

system.time( model <- train_model(train) )

pr = test_model(model, test)

# tune = tuneRF(train[,-1], train[,1],
#        stepFactor = 0.5,
#        plot = T,
#        ntreeTry = 300,
#        trace = T,
#        improve = 0.05)







