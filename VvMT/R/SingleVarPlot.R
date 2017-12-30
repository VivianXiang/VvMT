#' @title SingleVarPlot
#'
#' @description The function SingleVarPlot is used to group the vector.
#'
#' @param data a dataframe
#' @param var a variable name
#' @param round.num exact digits
#' @param pdf Boolean value, whether to save as a pdf file, with default FALSE
#' @param savedir the pdf file save directory, like "D:/test/"
#'
#' @return VarGroup returns a dataframe.
#'
#' @seealso VarGroupText
#'
#' @examples
#' SingleVarPlot(data = iris, var = "Species")
#' SingleVarPlot(data = iris, var = "Sepal.Length", round.num = 2)
#' SingleVarPlot(data = iris, var = "Sepal.Length", round.num = 2, pdf = TRUE, savedir = "D:/")






SingleVarPlot <- function(data, var, round.num = 3, pdf = FALSE, savedir){
  eval(parse(text = paste0("var.type = is.numeric(data$", var,")")))
  if (var.type) {
    if (pdf) {
      eval(parse(text = paste0("pdf('", savedir, var,".pdf', width = 9, height = 4)")))
      eval(parse(text = paste0("grid.arrange(
                                  ggplot(data, aes(x = ",var,", y =..density..)) + geom_histogram(fill = '#0099CC', color = '#CCCCCC', bins = 30) + geom_line(stat = 'density', size = 1.5, color = '#333333'),
                                  tableGrob(data.frame(
                                                'Stat' = c('MissRate', 'N', 'Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', 'Sd', 'Cv','Skewness', 'Kurtosis'),
                                                'Value' = round(c(sum(is.na(data$",var,"))/length(data$",var,"), length(data$",var,"),
                                                                  min(data$",var,", na.rm = T), quantile(data$",var,", na.rm = T, 0.25), median(data$",var,", na.rm = T), mean(data$",var,", na.rm = T), quantile(data$",var,", na.rm = T, 0.75), max(data$",var,", na.rm = T),
                                                                  sd(data$",var,", na.rm = T),
                                                                  ifelse(mean(data$",var,", na.rm = T) != 0, sd(data$",var,", na.rm = T)/mean(data$",var,", na.rm = T), 0),
                                                                  rowSkewness(t(data$",var,"), na.rm = T), rowKurtosis(t(data$",var,"), na.rm = T)), ", round.num, ")),
                                            theme = ttheme_minimal()),
                                  ggplot(data, aes(x = '",var,"', y = ",var,")) + geom_boxplot(color = '#333333'),
                                  ncol = 3)")))
      dev.off()
    } else {
      eval(parse(text = paste0("grid.arrange(
                                  ggplot(data, aes(x = ",var,", y =..density..)) + geom_histogram(fill = '#0099CC', color = '#CCCCCC', bins = 30) + geom_line(stat = 'density', size = 1.5, color = '#333333'),
                                  tableGrob(data.frame(
                                                'Stat' = c('MissRate', 'N', 'Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', 'Sd', 'Cv','Skewness', 'Kurtosis'),
                                                'Value' = round(c(sum(is.na(data$",var,"))/length(data$",var,"), length(data$",var,"),
                                                                  min(data$",var,", na.rm = T), quantile(data$",var,", na.rm = T, 0.25), median(data$",var,", na.rm = T), mean(data$",var,", na.rm = T), quantile(data$",var,", na.rm = T, 0.75), max(data$",var,", na.rm = T),
                                                                  sd(data$",var,", na.rm = T),
                                                                  ifelse(mean(data$",var,", na.rm = T) != 0, sd(data$",var,", na.rm = T)/mean(data$",var,", na.rm = T), 0),
                                                                  rowSkewness(t(data$",var,"), na.rm = T), rowKurtosis(t(data$",var,"), na.rm = T)), ", round.num, ")),
                                            theme = ttheme_minimal()),
                                  ggplot(data, aes(x = '",var,"', y = ",var,")) + geom_boxplot(color = '#333333'),
                                  ncol = 3)")))
      }
    }
  else {
    if (pdf) {
      eval(parse(text = paste0("pdf('", savedir, var,".pdf', width = 9, height = 4)")))
      eval(parse(text = paste0("grid.arrange(
                                  ggplot(data, aes(x = ",var,")) + geom_bar(fill = '#0099CC', color = '#CCCCCC'),
                                  tableGrob(data.frame(
                                                'Stat' = c('MissRate', 'N', 'ValueNum', 'ModeNum', 'Mode', 'ModeProp'),
                                                'Value' = c(sum(is.na(data$", var,")) / length(data$", var,"),
                                                            length(data$", var, "),
                                                            length(unique(data$", var, ")),
                                                            sum(table(data$", var, ") == max(table(data$", var, "))),
                                                            names(table(data$", var ,")[which.max(table(data$", var, "))]),
                                                            round(max(table(data$", var, ")) /length(data$", var, "), ", round.num, "))),
                                            theme = ttheme_minimal()),
                                  ncol = 2)")))
      dev.off()
    } else {
      eval(parse(text = paste0("grid.arrange(
                                  ggplot(data, aes(x = ",var,")) + geom_bar(fill = '#0099CC', color = '#CCCCCC'),
                                  tableGrob(data.frame(
                                                'Stat' = c('MissRate', 'N', 'ValueNum', 'ModeNum', 'Mode', 'ModeProp'),
                                                'Value' = c(sum(is.na(data$", var,")) / length(data$", var,"),
                                                            length(data$", var, "),
                                                            length(unique(data$", var, ")),
                                                            sum(table(data$", var, ") == max(table(data$", var, "))),
                                                            names(table(data$", var ,")[which.max(table(data$", var, "))]),
                                                            round(max(table(data$", var, ")) /length(data$", var, "), ", round.num, "))),
                                          theme = ttheme_minimal()),
                                  ncol = 2)")))
      }
    }
}
