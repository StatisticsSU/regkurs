#' Compute pair-wise correlations and hypothesis test
#'
#' Computes pair-wise correlations between variables in a dataframe df
#' Tests H0: rho = 0 vs H1: rho!=0 by computing p-values.
#' @param df dataframe
#' @return list with two tables: corrs (correlations), pvals (p-values)
#' @export
#' @examples
#' library(regkurs)
#' corr.matrix(bike[,c("temp","hum","windspeed")])
corr.matrix <- function(df){
  p = dim(df)[2]
  corrs = matrix(NA, p, p)
  pvals = matrix(NA, p, p)
  for (i in 1:(p-1)){
    for (j in (i+1):p){
      corr_res = cor.test(df[,i], df[,j])
      corrs[i,j] = corr_res$estimate
      pvals[i,j] = corr_res$p.value
    }
  }
  colnames(corrs) <- rownames(corrs) <- names(df)
  colnames(pvals) <- rownames(pvals) <- names(df)
  #corrs[lower.tri(corrs, diag = F)] = t(corrs[upper.tri(corrs, diag = F)])
  #pvals[lower.tri(pvals, diag = F)] = t(pvals[upper.tri(pvals, diag = F)])
  {cat("\nCorrelations\n------------------------------------------------\n");
    print(corrs, digits = 5, na.print = "")}
  {cat("\np-values\n------------------------------------------------\n");
    print(pvals, digits = 5, na.print = "")}
  return(invisible(list(corrs = corrs, pvals = pvals)))
}
