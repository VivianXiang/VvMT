#' @title VarGroupText
#'
#' @description The function VarGroupText is used to group the numeric vector.
#'
#' @param vector a vector
#' @param method "bin" or "quantile"
#' @param bins bins
#' @param nametype the data type of group name, "factor" or "numeric"
#' @param valuetype group representation value, "midpoint" or "lower" or "upper"
#'
#' @return VarGroupText returns a dataframe.
#'
#' @examples
#' VarGroupText(iris$Sepal.Length, "bin", bins = 10, nametype = "factor")
#' VarGroupText(iris$Sepal.Length, "bin", bins = 10, nametype = "numeric", valuetype = "midpoint")

VarGroupText <- function(vector, method = c("bin", "quantile"), bins = 10, nametype = c("factor", "numeric"), valuetype = c("midpoint", "lower", "upper")){
  grouptext <- NULL
  condition <- NULL
  groupname <- NULL
  if (method == "bin") {cutoffs = seq(range(vector)[1], range(vector)[2], length.out = bins + 1)}
  else if (method == "quantile") {cutoffs = quantile(vector, probs = seq(0, 1, length.out = bins + 1))}
  condition[1] = paste0("vector < ", cutoffs[2])
  if (nametype == "factor") {groupname[1] = paste0("(-Inf, ", cutoffs[2],"]")}
  else if (nametype == "numeric") {
    if (valuetype == "midpoint") {groupname[1] = mean(cutoffs[1:2])}
    else if (valuetype == "lower") {groupname[1] = cutoffs[1]}
    else if (valuetype == "upper") {groupname[1] = cutoffs[2]}
  }
  k <- 2
  while (k <= (bins - 1)) {
    condition[k] = paste0("vector >= ", cutoffs[k]," & vector < ", cutoffs[k + 1])
    if (nametype == "factor") {groupname[k] = paste0("[",cutoffs[k], ", ", cutoffs[k + 1], ")")}
    else if (nametype == "numeric") {
      if (valuetype == "midpoint") {groupname[k] = mean(cutoffs[c(k, k + 1)])}
      else if (valuetype == "lower") {groupname[k] = cutoffs[k]}
      else if (valuetype == "upper") {groupname[k] = cutoffs[k + 1]}
    }
    k = k + 1
  }
  condition[k] = paste0("vector >= ", cutoffs[k])
  if (nametype == "factor") {groupname[k] = paste0("[", cutoffs[k], ", Inf)")}
  else if (nametype == "numeric") {
    if (valuetype == "midpoint") {groupname[k] = mean(cutoffs[c(k-1,k)])}
    else if (valuetype == "lower") {groupname[k] = cutoffs[k-1]}
    else if (valuetype == "upper") {groupname[k] = cutoffs[k]}
  }
  grouptext = data.frame(condition = condition, groupname = groupname, stringsAsFactors = F)
  return(grouptext)
}

