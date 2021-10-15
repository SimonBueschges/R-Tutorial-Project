#' Add schulung theme to ggplot chart
#'
#' This function allows you to add the schulung theme to your ggplot graphics.
#' @keywords schulung_style
#' @export
#' @examples
#' line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
#'   geom_line(colour = "#007f7f", size = 1) +
#'   geom_hline(yintercept = 0, size = 1, colour = "#333333") +
#'   schulungs_style()
schulungs_style <- function() {
  font <- "Helvetica"

  ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = font,
      size = 22,
      face = "bold",
      color = "##2875DD"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 18,
      margin = ggplot2::margin(5, 0, 5, 0),
      color = "#c0c0c0"
    ),
    plot.caption = ggplot2::element_blank(),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = font,
      size = 18,
      color = "#000000"
    ),
    axis.title = ggplot2::element_text(
      family = font,
      size = 12,
      color = "#000000"
    ),
    axis.text = ggplot2::element_text(
      family = font,
      size = 12,
      color = "#000000"
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
    panel.background = ggplot2::element_blank(),
  )
}