#' @title RShipPlot
#'
#' @description The function RShipPlot is used to group the vector.
#'
#' @param data a dataframe
#' @param xvar the x-axis variable name
#' @param yvar the y-axis variable name
#' @param Nalpha numeric variable alpha, with default 0.1
#' @param Falpha numeric variable alpha, with default 0.1
#' @param palette
#' @param bins
#' @param lowerlimits
#' @param upperlimits
#' @param round.num exact digits
#' @param pdf Boolean value, whether to save as a pdf file, with default FALSE
#' @param savedir the pdf file save directory, like "D:/test/"
#'
#' @return RShipPlot returns a dataframe.
#'
#' @seealso VarGroupText
#'
#' @examples
#' RShipPlot(data = iris,xvar = "Species", yvar = "Sepal.Width")
#' RShipPlot(data = iris,xvar = "Sepal.Length", yvar = "Sepal.Width", Nalpha = 0.5, bins = 10)




RShipPlot <- function(data, xvar, yvar, Nalpha = .1, Falpha = .8, palette = "Set3", bins = 50, lowerlimits = 0, upperlimits = 100, round.num = 3, pdf = FALSE, savedir){
  eval(parse(text = paste0("var.type = is.numeric(data$", xvar,")")))
  if (var.type) {
    if (pdf) {
      eval(parse(text = paste0("pdf('", savedir, xvar,"_Reg.pdf', width = 9, height = 4)")))
      eval(parse(text = paste0("
                               grid.arrange(
                                 tableGrob(data.frame(Stat = c('N', 'Cor.Coef', 'Reg.Coef', 'Reg.Tvalue', 'Reg.Pvalue', 'Reg.AdjRsqure'),
                                                      Value = round(c(length(data$", yvar, "),
                                                                      cor(data$", yvar, ", data$", xvar, "),
                                                                      summary(lm(data$", yvar, " ~ data$", xvar, "))$coefficients[2, 1],
                                                                      summary(lm(data$", yvar, " ~ data$", xvar, "))$coefficients[2, 3],
                                                                      summary(lm(data$", yvar, " ~ data$", xvar, "))$coefficients[2, 4],
                                                                      summary(lm(data$", yvar, " ~ data$", xvar, "))$adj.r.squared), 3)),
                                   theme = ttheme_minimal()),
                                 ggplot(data = data, aes(x = ", xvar, ", y = ", yvar, ")) + stat_bin2d(bins = ", bins, ") + scale_fill_gradient(low = 'lightblue', high = 'red', limits = c(", lowerlimits, ",", upperlimits, ")),
                                ncol = 2)")))
      dev.off()
    } else {
      eval(parse(text = paste0("
                               grid.arrange(
                                 tableGrob(data.frame(Stat = c('N', 'Cor.Coef', 'Reg.Coef', 'Reg.Tvalue', 'Reg.Pvalue', 'Reg.AdjRsqure'),
                                                      Value = round(c(length(data$", yvar, "),
                                                                    cor(data$", yvar, ", data$", xvar, "),
                                                                    summary(lm(data$", yvar, " ~ data$", xvar, "))$coefficients[2, 1],
                                                                    summary(lm(data$", yvar, " ~ data$", xvar, "))$coefficients[2, 3],
                                                                    summary(lm(data$", yvar, " ~ data$", xvar, "))$coefficients[2, 4],
                                                                    summary(lm(data$", yvar, " ~ data$", xvar, "))$adj.r.squared), 3)),
                                   theme = ttheme_minimal()),
                                 ggplot(data = data, aes(x = ", xvar, ", y = ", yvar, ")) + stat_bin2d(bins = ", bins, ") + scale_fill_gradient(low = 'lightblue', high = 'red', limits = c(", lowerlimits, ",", upperlimits, ")),
                                 ncol = 2)")))
    }
    } else {
      if (pdf) {
        eval(parse(text = paste0("pdf('", savedir, xvar,"_Reg.pdf', width = 14, height = 4)")))
        eval(parse(text = paste0("
                                 grid.arrange(
                                   ggplot(data, aes(fill = ", xvar, ", x = ", yvar, ")) + geom_density(alpha = ", Falpha, ") + scale_fill_brewer(palette = '", palette, "'),
                                   tableGrob(cbind(ddply(data, .(", xvar, "), summarise, N = length(", yvar, ")),NProp = round(ddply(data, .(", xvar, "), summarise, N = length(", yvar, "))[,2]/nrow(data), ", round.num, ")),theme = ttheme_minimal()),
                                   ggplot(data, aes(x = ", xvar, ", y = ", yvar, ")) + geom_boxplot(),
                                   ncol = 3)")))
        dev.off()
      } else {
        eval(parse(text = paste0("
                                 grid.arrange(
                                   ggplot(data, aes(fill = ", xvar, ", x = ", yvar, ")) + geom_density(alpha = ", Falpha, ") + scale_fill_brewer(palette = '", palette, "'),
                                   tableGrob(cbind(ddply(data, .(", xvar, "), summarise, N = length(", yvar, ")),NProp = round(ddply(data, .(", xvar, "), summarise, N = length(", yvar, "))[,2]/nrow(data), ", round.num, ")),theme = ttheme_minimal()),
                                   ggplot(data, aes(x = ", xvar, ", y = ", yvar, ")) + geom_boxplot(),
                                   ncol = 3)")))
      }
    }
  }
