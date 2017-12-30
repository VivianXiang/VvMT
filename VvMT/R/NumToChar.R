#' @title NumToChar
#'
#' @description The function NumToChar is used to convert numeric variables to character type variables.
#'
#' @param data a dataframe
#' @param vars a vector of variable names, which are numeric variables in the dataframe need to convert to character type
#' @param rename Boolean value, whether to rename the variable name and create a new variable (add ".char' suffix), with default FALSE
#'
#' @return NumToChar returns a dataframe.
#'
#' @examples
#' str(NumToChar(iris, 'Sepal.Length'))
#' str(NumToChar(iris, c('Sepal.Length', 'Sepal.Width'), rename = TRUE))

NumToChar <- function(data, vars, rename = FALSE){
  for (var in vars){
    if (rename) {
      eval(parse(text = paste0("data$", var, ".char <- as.character(data$", var, ")")))
    } else {
      eval(parse(text = paste0("data$", var, " <- as.character(data$", var, ")")))
    }
  }
  return(data)
}
