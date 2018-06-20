make_qdf <- function(sf,probs = c(0.25, 0.5, 0.75)){
  if(is.null(sf$strata)){
    out <-as.data.frame(t(quantile(sf,probs = probs)$quantile))
    colnames(out) <- paste0("q",colnames(out))
  }else{
    out <-as.data.frame(quantile(sf,probs = probs)$quantile)
    colnames(out) <- paste0("q",colnames(out))
    out$strata=row.names(out)
    row.names(out) <- NULL
  }
  return(out)
}
