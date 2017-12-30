#' @title VarStat
#'
#' @description The function VarStat is used to calculate the variable description statistics.
#'
#' @param data a dataframe
#' @param var_type a result returned by the VarType function
#' @param nstat a vector of the descriptive statistics of numerical variables, you can choose c("Miss", "MissRate", "N", "Mean", "Sd", "Cv", "Skewness", "Kurtosis","Min", "Max", "Median", "Num", "Outlier") one or more
#' @param cstat a vector of the descriptive statistics of non-numerical variables, you can choose c("Miss", "MissRate", "N", "Num", "Mode") one or more
#' @param round.num exact digits
#'
#' @return VarStat returns two dataframe.
#'
#' @seealso VarStat
#'
#' @examples
#' VarStat(iris,VarType(iris))



VarStat <- function(data,
                    var_type,
                    nstat = c("Miss", "MissRate", "N", "Mean", "Sd", "Cv", "Skewness", "Kurtosis","Min", "Max", "Median", "Num", "Outlier"),
                    cstat = c("Miss", "MissRate", "N", "Num", "Mode"),
                    round.num = 2) {
  num_var_value <- NULL
  num_data <- subset(data, select = which(var_type$numeric == 1))
  for (stat in nstat){
    if (stat == "Miss") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)any(is.na(x))))")))}
    else if (stat == "MissRate") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)round(sum(is.na(x))/nrow(num_data), round.num)))")))}
    else if (stat == "N") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)sum(is.na(x) == 0), USE.NAMES = T))")))}
    else if (stat == "Mean") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)round(mean(x, na.rm = T), round.num), USE.NAMES = T))")))}
    else if (stat == "Sd") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)round(sd(x, na.rm = T), round.num), USE.NAMES = T))")))}
    else if (stat == "Cv") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)ifelse(mean(x, na.rm = T) != 0, round(sd(x, na.rm = T)/mean(x, na.rm = T), round.num), 'Inf'), USE.NAMES = T))")))}
    else if (stat == "Skewness") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)round(rowSkewness(t(x), na.rm = T), round.num), USE.NAMES = T))")))}
    else if (stat == "Kurtosis") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)round(rowKurtosis(t(x), na.rm = T), round.num), USE.NAMES = T))")))}
    else if (stat == "Min") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)round(min(x, na.rm = T), round.num), USE.NAMES = T))")))}
    else if (stat == "Max") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)round(max(x, na.rm = T), round.num), USE.NAMES = T))")))}
    else if (stat == "Median") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)round(median(x, na.rm = T), round.num), USE.NAMES = T))")))}
    else if (stat == "Num") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)length(unique(x)), USE.NAMES = T))")))}
    else if (stat == "Outlier") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(num_data, function(x)ifelse(sd(x, na.rm = T) != 0, any((x-mean(x, na.rm = T))/sd(x, na.rm = T) >= 3), 'NA')))")))}
    eval(parse(text = paste0("var.value.", stat, " <- data.frame(Var = row.names(var.value.", stat, "), ", stat ," = var.value.", stat, "[,1], stringsAsFactors = F)")))
    ifelse(is.null(num_var_value), eval(parse(text = paste0 ("num_var_value <- var.value.", stat))), eval(parse(text = paste0 ("num_var_value <- join(num_var_value, var.value.", stat, ", by = 'Var')"))))
  }
  char_var_value <- NULL
  char_data <- subset(data, select = which(var_type$numeric != 1))
  for (stat in cstat){
    if (stat == "Miss") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(char_data, function(x)any(is.na(x))))")))}
    else if (stat == "MissRate") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(char_data, function(x)round(sum(is.na(x))/nrow(char_data), round.num)))")))}
    else if (stat == "N") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(char_data, function(x)sum(is.na(x) == 0), USE.NAMES = T))")))}
    else if (stat == "Num") {eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(char_data, function(x)length(unique(x)), USE.NAMES = T))")))}
    else if (stat == "Mode") {
      eval(parse(text = paste0("var.value.", stat, " <- data.frame(sapply(char_data, function(x)(names(table(x))[which.max(table(x))]), USE.NAMES = T))")))
      eval(parse(text = paste0("var.value.", stat, "Num <- data.frame(sapply(char_data, function(x)(sum(table(x) == max(table(x)))), USE.NAMES = T))")))
      eval(parse(text = paste0("var.value.", stat, "Prop <- data.frame(sapply(char_data, function(x)(round(max(table(x))/length(x), round.num)), USE.NAMES = T))")))
    }
    eval(parse(text = paste0("var.value.", stat, " <- data.frame(Var = row.names(var.value.", stat, "), ", stat ," = var.value.", stat, "[,1], stringsAsFactors = F)")))
    ifelse(is.null(char_var_value), eval(parse(text = paste0 ("char_var_value <- var.value.", stat))), eval(parse(text = paste0 ("char_var_value <- join(char_var_value, var.value.", stat, ", by = 'Var')"))))
  }
  if ("Mode" %in% cstat){
    char_var_value$ModeNum = var.value.ModeNum[,1]
    char_var_value$ModeProp = var.value.ModeProp[,1]
  }
  return(list(num_var_value = num_var_value, char_var_value = char_var_value))
}
