#' @import survival
#' @export
tidysurv <- function(sf){
  if(is.null(sf$strata)){
    sfdf <- data.frame(time=sf$time,survival=sf$surv,
                       lower=sf$lower,upper=sf$upper,
                       stringsAsFactors = FALSE)
  }else{
    sfdf <- data.frame(time=sf$time,survival=sf$surv,
                       strata=rep(names(sf$strata),sf$strata),
                       lower=sf$lower,upper=sf$upper,
                       stringsAsFactors = FALSE)
  }
  return(sfdf)
}
