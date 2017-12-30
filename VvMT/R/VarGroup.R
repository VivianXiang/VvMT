#' @title VarGroup
#'
#' @description The function VarGroup is used to group the vector.
#'
#' @param vector a vector
#' @param gcondition a vector of group conditions, matches gname
#' @param gname a vector of group names, matches gcondition
#'
#' @return VarGroup returns a dataframe.
#'
#' @seealso VarGroupText
#'
#' @examples
#'#' #(1)factor
#' basedata = VarGroupText(iris$Sepal.Length, "bin", bins = 10, nametype = "factor")
#' VarGroup(iris$Sepal.Length, basedata$condition, basedata$groupname)
#'
#' #(2)numeric
#' basedata = VarGroupText(iris$Sepal.Length, "bin", bins = 10, nametype = "numeric", valuetype = "midpoint")
#' VarGroup(iris$Sepal.Length, basedata$condition, basedata$groupname)

VarGroup <- function(vector, gcondition, gname){
  var_group <- data.frame(vector)
  for (i in 1:length(gcondition)){
    var_group$group[eval(parse(text = gcondition[i]))] = gname[i]
  }
  if (!is.numeric(gname)) {var_group$group = factor(var_group$group, levels = unique(gname))}
  return(var_group)
}

