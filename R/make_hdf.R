#' @import dplyr
#' @export
make_hdf <-function(sf,bw){
  nb <- ceiling(max(sf$time)/bw)
  if(is.null(sf$strata)){
    sfdf <- data.frame(time=sf$time,survival=sf$surv,
                       stringsAsFactors = FALSE)
    df4hist <- sfdf %>%
      dplyr::mutate(time=cut(time,0:nb*bw)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarise(TP=diff(range(survival))) %>%
      dplyr::mutate(density=TP/bw) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(midtime=(as.integer(time)-1/2)*bw)
  }else{
    sfdf <- data.frame(time=sf$time,survival=sf$surv,
                       strata=rep(names(sf$strata),sf$strata),
                       stringsAsFactors = FALSE)
    df4hist <- sfdf %>%
      dplyr::mutate(time=cut(time,0:nb*bw)) %>%
      dplyr::group_by(strata,time) %>%
      dplyr::summarise(TP=diff(range(survival))) %>%
      dplyr::mutate(density=TP/bw) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(midtime=(as.integer(time)-1/2)*bw)
    tmp <- do.call("rbind",strsplit(df4hist$strata,", |="))
    n_strata <- ncol(tmp)/2
    values=tmp[,2*1:n_strata,drop=FALSE]
    colnames(values)<-tmp[1,2*1:n_strata-1]
    df4hist <- data.frame(df4hist,values,stringsAsFactors = FALSE)
  }
  return(df4hist)
}
