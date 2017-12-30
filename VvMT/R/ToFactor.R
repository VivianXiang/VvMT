#' @title ToFactor
#'
#' @description The function ToFactor is used to convert variables to factor type variables.
#'
#' @param data a dataframe
#' @param vars a vector of variable names, which need to convert to factor type
#' @param rename Boolean value, whether to rename the variable name and create a new variable (add ".factor' suffix), with default FALSE
#'
#' @return NumToChar returns a dataframe.
#' @examples
#' str(NumToChar(iris, 'Sepal.Length'))
#' str(NumToChar(iris, c('Sepal.Length', 'Sepal.Width'), rename = TRUE))


ToFactor <-  function(data, vars, rename = FALSE){
  for (var in vars){
    if (rename){
      eval(parse(text = paste0("data$", var, ".factor <- as.factor(data$", var, ")")))
    } else{
      eval(parse(text = paste0("data$", var, " <- as.factor(data$", var, ")")))
    }
  }
  return(data)
}
