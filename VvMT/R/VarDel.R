#' @title VarDel
#'
#' @description The function VarDel is used to delete the variables in the dataframe.
#'
#' @param data a dataframe
#' @param vars a vector of variable names, which need to be deleted
#'
#' @return VarDel returns a dataframe.
#'
#' @examples
#' VarDel(iris, "Species")





VarDel <- function(data, vars){
  col.index = aaply(vars, 1, function(x)which(x == colnames(data)))
  newdata = data[,-col.index]
  return(newdata)
}
