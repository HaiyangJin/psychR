
#' Plot Receiver Operating Characteristic (ROC) curve
#'
#' @param d a vector of sensitivity d' (default to c(1,2,3)).
#'
#' @return a ROC curve plot.
#' @export
#'
#' @examples
#' plotroc()
plotroc <- function(d = c(1,2,3)) {

  # "all" hit rates
  hits <- seq(0, 1, by = 0.01)

  # custom function to calculate the false positive rate
  calfa <- function(thisd, hits) {

    thisfas <- c(0, pnorm((qnorm(hits[2:(length(hits)-1)]) - thisd)), 1)

    # Create a data frame for plotting
    df_roc <- data.frame(d = thisd, hit = hits, fa = thisfas)

    return(df_roc)
  }

  # make the ROC df
  df_roc <- purrr::map_dfr(d, calfa, hits=hits)

  # Plot the ROC curve
  df_roc |>
    dplyr::mutate(d = as.factor(d)) |>
    ggplot2::ggplot(ggplot2::aes(x = fa, y = .data$hit, group=d)) +
    ggplot2::geom_line(ggplot2::aes(color = d), linewidth=.7) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = seq(0, 1, by=.2),
                                limits = c(0, 1)) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                breaks = seq(0, 1, by=.2),
                                limits = c(0, 1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "False positive", y = "Hit rate",
                  color=expression("Sensitivity"~italic(d)*"'")) +
    ggplot2::guides(x.sec = "axis", y.sec = "axis") +
    ggplot2::theme(axis.line = ggplot2::element_line(color = "black", linewidth = 1),
                   axis.text.y.right = ggplot2::element_text(color="white"),  # Remove y-axis text
                   axis.ticks.y.right = ggplot2::element_blank(),  # Remove y-axis ticks
                   axis.title.y.right = ggplot2::element_blank(),  # Remove y-axis title
                   axis.text.x.top = ggplot2::element_text(color="white"),  # Remove y-axis text
                   axis.ticks.x.top = ggplot2::element_blank(),  # Remove y-axis ticks
                   axis.title.x.top = ggplot2::element_blank(),  # Remove y-axis title
                   panel.background = ggplot2::element_rect(fill = "white"),
                   text = ggplot2::element_text(size=20)) +
    NULL
}
