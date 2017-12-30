#' @title CharToNum
#'
#' @description The function CharToNum is used to convert character variables to numeric type variables.
#'
#' @param data a dataframe
#' @param vars a vector of variable names, which are character variables in the dataframe need to convert to numeric type
#' @param rename Boolean value, whether to rename the variable name and create a new variable (add ".numeric' suffix), with default FALSE
#'
#' @return NumToChar returns a dataframe.
#' @examples
#' str(NumToChar(iris, 'Sepal.Length'))
#' str(NumToChar(iris, c('Sepal.Length', 'Sepal.Width'), rename = TRUE))




CharToNum <- function(data, vars, rename = FALSE){
  for (var in vars){
    if (rename){
      eval(parse(text = paste0("data$", var, ".numeric <- as.numeric(data$", var, ")")))
    } else {
      eval(parse(text = paste0("data$", var, " <- as.numeric(data$", var, ")")))
    }
  }
  return(data)
}
