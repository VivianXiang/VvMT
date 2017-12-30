#' @title RegVarImp
#'
#' @description The function RegVarImp.
#'
#' @param data a dataframe
#' @param var a variable name
#' @param round.num exact digits
#' @param pdf Boolean value, whether to save as a pdf file, with default FALSE
#' @param savedir the pdf file save directory, like "D:/test/"
#'





RegVarImp <- function(data, yvar, xvars = "all", method = c("Lm", "SRTree", "MRTree"),
                      rtreecontrol = rpart.control(minsplit = 2, cp = 0.0001, xval = 10, maxdepth = 30),
                      round.num = 5) {
  Lm.result <- NULL
  SRTree.result <- NULL
  MRTree.result <- NULL
  realy = data[ ,colnames(data) == yvar]

  if (xvars == "all"){modeldata = data[ , colnames(data) != yvar]}
  else if (mode(xvars) == "numeric") {modeldata = data[ ,xvars]}
  else if (mode(xvars) == "character") {modeldata = data[ ,colnames(data) %in% xvars]}
  else {print("xvars??")}

  if (method == "Lm"){
    AdjRsqured <- NULL
    Mse <- NULL
    i = 1
    for (var in colnames(modeldata)){
      eval(parse(text = paste0("onevarmodel.fit <- lm(realy ~ modeldata$", var, ")")))
      AdjRsqured[i] = round(summary(onevarmodel.fit)$adj.r.squared, round.num)
      Mse[i] = round(mean((realy - predict(onevarmodel.fit, data))^2), round.num)
      rm(onevarmodel.fit)
      i = i + 1
    }
    Lm.result = data.frame(var.name = colnames(modeldata), AdjRsqured, Mse)
    return(Lm.result)
  }
  else if (method == "SRTree"){
    Mse <- NULL
    i = 1
    for (var in colnames(modeldata)){
      eval(parse(text = paste0("onevarmodel.fit <- rpart(realy ~ modeldata$", var, ", method = 'anova', control = rtreecontrol)")))
      Mse[i] = round(mean((realy - predict(onevarmodel.fit, data))^2), round.num)
      rm(onevarmodel.fit)
      i = i + 1
    }
    SRTree.result = data.frame(var.name = colnames(modeldata), Mse)
    return(SRTree.result)
  }
  else if (method == "MRTree"){
    xs = paste0(colnames(modeldata), collapse = ' + ')
    eval(parse(text = paste0("mrtreemodel <- rpart(", yvar, " ~ ", xs, ", data = data, method = 'anova', control = rtreecontrol)")))
    MRTree.result = data.frame(mrtreemodel$variable.importance)
    return(MRTree.result)
  }
}
