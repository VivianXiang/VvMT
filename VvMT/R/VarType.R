#' @title VarType
#'
#' @description The function VarType is used to determine the data type of each variable in the dataframe.
#'
#' @param data a dataframe to be determined
#' @param dtypes candidate data type, with default parameter c("integer", "numeric", "character", "factor")
#' @param threshold a threshold that determines whether a variable may be a discrete or category variable, with default 50
#' @param showtxt Boolean value, whether to output text conclusions, with default FALSE
#'
#' @return VarType returns a dataframe of the data type determination result.
#'
#' An object of class "VarType" is a dataframe containing at least the following components:
#'
#' @param dtype Boolean value, determination result of whether the variable belongs to each data type of the "dtypes" parameter
#' @param value.num the number of different values of the variable
#' @param value.num.prop the ratio of the number of different values of the variable to the total number of samples
#' @param note note
#' @param type variable data type summary results
#'
#' @examples
#' VarType(iris)


VarType <- function(data, dtypes = c("integer", "numeric", "character", "factor"), threshold = 50, showtxt = FALSE){
  var_type <- NULL
  for (tp in dtypes){
    eval(parse(text = paste0("var.type.", tp, " <- data.frame(sapply(data, is.", tp, ", USE.NAMES = T));")))
    eval(parse(text = paste0("var.type.", tp, " <- data.frame(var.name = row.names(var.type.", tp, "), ", tp ," = var.type.", tp, "[,1]);")))
    ifelse(is.null(var_type), eval(parse(text = paste0 ("var_type <- var.type.", tp))), eval(parse(text = paste0 ("var_type <- join(var_type, var.type.", tp, ", by = 'var.name')"))))
  }
  var_type$value.num <- sapply(data, function(x)(length(unique(x))))
  var_type$value.num.prop <- round(sapply(data, function(x)(length(unique(x))*100/nrow(data))),2)
  var_type$note <- ""
  if (sum((var_type$character == 1 | var_type$integer == 1) & var_type$value.num == nrow(data)) > 0) {var_type$note[(var_type$character == 1 | var_type$integer == 1) & var_type$value.num == nrow(data)] <- "maybe ID"}
  if (sum((var_type$character != 1) & (var_type$value.num <= threshold)) > 0) {var_type$note[(var_type$character != 1) & (var_type$value.num <= threshold)] <- "maybe discrete or category"}
  if (sum((var_type$value.num == 2)) > 0) {var_type$note[(var_type$value.num == 2)] <- "maybe dummy variable"}
  var_type$var.name <- as.character(var_type$var.name)
  var_type$type[var_type$numeric == 1] = "numeric"
  var_type$type[var_type$integer == 1] = "integer"
  var_type$type[var_type$character == 1] = "character"
  var_type$type[var_type$factor == 1] = "factor"
  if (showtxt) {
    cat(cat("DF Shape:", nrow(data),"*", ncol(data),"\t\t",
            "numeric variables:", sum(var_type$numeric),"\t\t",
            "integer variables:", sum(var_type$integer),"\t\t",
            "character variables:",sum(var_type$character),"\t\t",
            "factor variables:",sum(var_type$factor)),
        cat("\n\n"),
        cat("maybe ID:\n"),
        cat(cat("  "), var_type$var.name[var_type$note == "maybe ID"], sep = "\t"),
        cat("\n\n"),
        cat("maybe discrete or category:\n"),
        cat(cat("  "), var_type$var.name[var_type$note == "maybe discrete or category"], sep = "\t"),
        cat("\n\n"),
        cat("maybe dummy variable:\n"),
        cat(cat("  "), var_type$var.name[var_type$note == "maybe dummy variable"], sep = "\t")) }
  return(var_type)
}
