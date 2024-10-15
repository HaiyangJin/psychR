
#' Plot Signal Detection Theory (SDT) functions
#'
#' @param d sensitivity d' (default to 2).
#' @param c_x criterion on x-axis (it is neither beta or C). It is simply where the criteria on x-axis. Default to d/2.
#' @param xlim x-axis limits. Default to c(-4, 6).
#' @param ymax y-axis limits. Default to .45.
#' @param hit hit rate. Default to NULL. If both hit and fa are provided, d is calculated as qnorm(hit) - qnorm(fa).
#' @param fa false alarm rate. Default to NULL. If both hit and fa are provided, d is calculated as qnorm(hit) - qnorm(fa).
#'
#' @return a list with three elements: `plot` (SDT plot),  `dv` (sensitivity, beta, and C) and `rates` (hit, miss, false alarm, and correct rejection).
#' @export
#'
#' @examples
#' sdt <- plotsdt()
plotsdt <- function(d = 2,
                    c_x = d/2,
                    xlim = c(-4, d+4),
                    ymax = NULL,
                    hit = NULL,
                    fa = NULL
){

  if (!is.null(hit) && !is.null(fa)) {
    d <- qnorm(hit) - qnorm(fa)
  }

  # output
  out <- list()

  # Create a grid of points
  x <- seq(xlim[1], xlim[2], length = 1000)

  ## Compute the value of the function at each point
  # noise distribution
  y_noise <- dnorm(x, mean = 0, sd = 1)
  # signal distribution
  y_signal <- dnorm(x, mean = d, sd = 1)

  # default ymax
  if (is.null(ymax)) {
    ymax <- max(c(y_noise, y_signal)) + .05
  }
  # x axis breaks
  xbreaks <- seq(-10, 10, by=2)

  ## make data.frame
  df_sdt <- data.frame(x = x,
                       noise = y_noise,
                       signal = y_signal) |>
    tidyr::pivot_longer(cols = c(.data$noise, .data$signal),
                        names_to = "condition",
                        values_to = "y") |>
    dplyr::mutate(condition = factor(.data$condition,
                                     levels = c("signal","noise")))

  # calculate SDT DV
  beta <- dnorm(c_x, mean = d, sd = 1) / dnorm(c_x, mean = 0, sd = 1)
  C <- -(dnorm(c_x, mean = d, sd = 1) + dnorm(c_x, mean = 0, sd = 1))/2
  out$dv <- data.frame(d = d, beta = beta, C = C, c_x = c_x)

  # calculate the corresponding rates
  rates <- data.frame(stim_signal = c(1-pnorm(c_x, mean=d, sd=1), # hit
                                      pnorm(c_x, mean=d, sd=1)), # miss
                      stim_noise = c(1-pnorm(c_x, mean=0, sd=1), # false alarm
                                     pnorm(c_x, mean=0, sd=1)), # correct rejection
                      row.names = c("report_signal", "report_noise"))
  out$rates <- rates

  plot_sdt <- df_sdt |>
    ggplot2::ggplot(ggplot2::aes(x=x, y=.data$y,
                                 color=.data$condition, linetype=.data$condition)) +
    ggplot2::geom_ribbon(data=df_sdt |> dplyr::filter(x > c_x),
                         ggplot2::aes(ymin=0, ymax=.data$y, fill=.data$condition),
                         alpha=0.4, show.legend=FALSE) +
    ggplot2::scale_fill_manual(values=c("#D55E00", "#56B4E9")) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values=c("black", "gray35")) +
    ggplot2::scale_linetype_manual(values=c("solid", "dashed")) +
    ggplot2::scale_y_continuous(limits=c(0, ymax), expand=c(0,0)) +
    ggplot2::scale_x_continuous(limits=xlim, breaks=seq(-10, 10, by=2)) +
    ggplot2::geom_vline(xintercept = c_x, linetype="longdash") +
    ggplot2::labs(color=NULL, linetype=NULL, fill=NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Remove y-axis text
                   axis.ticks.y = ggplot2::element_blank(),  # Remove y-axis ticks
                   axis.title.y = ggplot2::element_blank(),  # Remove y-axis title
                   axis.title.x = ggplot2::element_blank(),  # Remove y-axis title
                   axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.5),  # Add x-axis line
                   panel.background = ggplot2::element_rect(fill = "white"),
                   text = ggplot2::element_text(size=16))

  out$plot <- plot_sdt

  return(out)
}
