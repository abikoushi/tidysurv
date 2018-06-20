#' @import ggplot2
#' @export
geom_cd_pointrange <- function(mapping = NULL, data = NULL,
                               stat = "identity",position ="identity",
                               ...,
                               fatten = 4,
                               censored = 4,
                               binwidth = NULL,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCDpointrange,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      fatten = fatten,
      censored = censored,
      ...
    )
  )
}

GeomCDpointrange <- ggplot2::ggproto("GeomCDpointrange", ggplot2::Geom,
                                     default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1, shape = 19,
                                                       fill = NA, alpha = NA, stroke = 1),
                                     required_aes = c("x", "y","ymin","ymax"),
                                     setup_data = function(data, params) {
                                       data$censored=is.na(data$y)|is.na(data$ymin)|is.na(data$ymax)
                                       transform(data,
                                                 y = ifelse(is.na(y),max(ymax,na.rm = TRUE),y),
                                                 ymin = ifelse(is.na(ymin),max(ymax,na.rm = TRUE),ymin),
                                                 ymax = ifelse(is.na(ymax),max(ymax,na.rm = TRUE),ymax))
                                     },
                                     draw_panel = function(data, panel_params, coord, fatten = 4, censored = 4) {
                                       data2 <- data
                                       data2$y <- data$ymax
                                       data2$shape <- ifelse(data$censored,censored,NA)
                                       data2$size <- data2$size * 4
                                       grid::gTree(children = grid::gList(
                                         ggplot2::GeomPointrange$draw_panel(data, panel_params, coord),
                                         ggplot2::GeomPoint$draw_panel(data2, panel_params, coord)
                                       ))
                                     })
