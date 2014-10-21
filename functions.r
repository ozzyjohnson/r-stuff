f.getBestIter <- function(model){
  
  best.iter <- gbm.perf(model, method="OOB", plot.it=TRUE)
  print(paste('Best OOB Iteration: ', best.iter))
  
  best.iter <- gbm.perf(model, method="test", plot.it=FALSE)
  print(paste('Best Test Set Iteration: ', best.iter))
  
  best.iter <- gbm.perf(model, method="cv", oobag.curve=TRUE, overlay==TRUE, plot.it=FALSE)
  print(paste('Best CV Iteration: ', best.iter))
  
  return(best.iter)
}

f.modelNow.gbm <- function(df, gbm.n.trees, folds=1, bag=0.5, gbm.shrinkage=0.0056, gbm.n.cores=8, verbose=FALSE){
  fmla <- as.formula(paste('Happy'," ~", '.'))
  model = gbm(fmla,
              data=df,
              n.trees=gbm.n.trees,
              shrinkage=gbm.shrinkage,
              n.cores=gbm.n.cores,
              cv.folds=folds,
              bag.fraction=bag,
              interaction.depth=1,
              verbose=verbose)
  return(model)
}

f.modelNow.glm <- function(df, dependent, independent){
  fmla <- as.formula(paste(dependent," ~", paste(independent, collapse="+")))
  model = glm(fmla,
              data=df)
  return(model)
}

f.miceNormal <- function(data, m, i, s){
  return(complete(mice(data, m = m, maxit=i, MaxNWts = 1500, seed = s,
                       method = vector("character", length = ncol(data)),
                       predictorMatrix = (1 - diag(1, ncol(data))),
                       visitSequence = 'monotone',
                       form = vector("character", length = ncol(data)),
                       post = vector("character", length = ncol(data)), 
                       defaultMethod = c("pmm","logreg","polyreg","polr"),),))
}

f.modelPerf <- function(train, test, trees, folds, bag, shrink, mode='perf'){
  predictions.list <- list()
  performance.list <- list()
  if (mode == 'perf') {
    model.type = 'gbm'
    if (model.type == 'gbm'){
      model <- f.modelNow.gbm(train, trees, folds, bag, shrink)
      model.prob = predict.gbm(model, test, n.trees=f.getBestIter(model), type="response")

      table(test[,7], model.prob > 0.5)
      model.pred = prediction(model.prob, test[,7])
      model.perf = performance(model.pred, "tpr", "fpr")
      plot(model.perf, colorize=TRUE)
      
      model.auc.perf = performance(model.pred, "auc")@y.values
      print(model.auc.perf)
      
    } else {
      model <- f.modelNow.glm(train, colnames.df.complete.train[[i]], list.topInfluences[[1]][,1])
      model.pred = predict.glm(model, newdata = test, type="response")
      performance.list[[i]] <-  confusionMatrix(model.pred, test[,i])
    }
  }
  return(model.auc.perf)
}

f.compareResuls <- function(df, a.trees=3000, a.folds=5, a.bag, a.shrink=0.0056, a.split=0.5){
  df[,1] <- NULL
  
  spl = sample.split(df, a.split)
  df.train = subset(df, spl == TRUE)
  df.test = subset(df, spl == FALSE)
  
  results <- f.modelPerf(f.dataMutate(df.train), f.dataMutate(df.test), a.trees, a.folds, a.bag, a.shrink)
  control <- f.modelPerf(df.train, df.test, a.trees, a.folds, a.bag, a.shrink)
  
  print(paste('Results: ', results))
  print(paste('Control: ', control))
}

f.columnNegative <- function(df, dependent){
  colnames = colnames(df)
  for (v in dependent){
    colnames = colnames[which(!colnames==v)]
  }
  return(colnames)  
}