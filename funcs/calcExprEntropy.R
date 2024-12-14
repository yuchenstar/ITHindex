


#' Title
#' ref: https://www.pnas.org/doi/full/10.1073/pnas.0803479105
#'
#' @param data A matrix (usually an expression matrix), in which genes is rowname and samples in columns.
#' @param norm Logical value, whether the specificity should be normalized by .
#'
#' @return
#' @export
#'
#' @examples
calcExprEntropy <- function(data, norm=FALSE) {
  div <- apply(data, 2L, diversity)
  if(norm) div <- div/log2(nrow(data))
  return(round(div,4))
}


diversity <- function(x) {
  rel <- x/sum(x, na.rm=TRUE)
  ent <- -sum(entropy(rel))
  return(ent)
}

entropy <- function(x) ifelse(x==0, 0, x*log2(x))
