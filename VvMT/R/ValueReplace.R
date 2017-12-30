#' @title ValueReplace
#'
#' @description The function ValueReplace is used to replace the variable value.
#'
#' @param data a dataframe
#' @param vars a vector of variable names, which need to replace value
#' @param oris a vector of original values
#' @param new a new value
#' @param rename Boolean value, whether to rename the variable name and create a new variable, with default FALSE
#'
#' @return ValueReplace returns a dataframe.
#'
#' @examples
#' ValueReplace(iris, 'Sepal.Length', ori = 2, 1 )
#' ValueReplace(iris, 'Sepal.Length', ori = c(1.7,5.1), 1,TRUE)



ValueReplace <- function(data, vars, oris, new, rename = FALSE){
  for (var in vars){
    if (rename){
      eval(parse(text = paste0("if (sum(data$", var, " %in% ", oris, ") > 0) {data$",var,".new = data$",var,";data$", var, "[data$", var, " %in% ", oris, "] = ", new,"} else {print('", var," do not have the original values')}")))
    } else {
      eval(parse(text = paste0("if (sum(data$", var, " %in% ", oris, ") > 0) {data$", var, "[data$", var, " %in% ", oris, "] = ", new,"} else {print('", var," do not have the original values')}")))
    }
  }
  return(data)
}

