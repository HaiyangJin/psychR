
#' Plot Signal Detection Theory (SDT) functions
#'
#' @param d sensitivity d' (default to 2).
#' @param c_x criterion on x-axis (it is neither beta or C). It is simply where the criteria on x-axis. Default to d/2.
#' @param xlim x-axis limits. Default to c(-4, 6).
#' @param ymax y-axis limits. Default to .45.
#' @param hit hit rate. Default to NULL. If both hit and fa are provided, d is calculated as qnorm(hit) - qnorm(fa).
#' @param fa false alarm rate. Default to NULL. If both hit and fa are provided, d is calculated as qnorm(hit) - qnorm(fa).
#' @param showd whether show d' value on the plot. Default to FALSE. If TRUE, d' is shown in red. If a color is provided, d' is shown in that color.
#' @param showc whether show criterion C on the plot. Default to FALSE. If TRUE, C is shown in darkgreen. If a color is provided, C is shown in that color.
#' @param showb whether show beta value on the plot. Default to FALSE. If TRUE, beta is shown in blue. If a color is provided, beta is shown in that color.
#'
#' @return a list with three elements: `plot` (SDT plot),  `dv` (sensitivity, beta, and C) and `rates` (hit, miss, false alarm, and correct rejection).
#' @export
#'
#' @examples
#' sdt <- plotsdt()
plotsdt <- function(d = 2,
                    c_x = d/2-0.75,
                    xlim = c(-4, d+4),
                    ymax = NULL,
                    hit = NULL,
                    fa = NULL,
                    showd = FALSE,
                    showc = FALSE,
                    showb = FALSE
){
  # calculate d' if hit and fa are provided
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
    ymax <- max(c(y_noise, y_signal)) + .1
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
                                     levels = c("signal","noise"))) |>
    dplyr::mutate(sdttype =
                    dplyr::case_when(
                      .data$x < c_x & .data$condition=="signal" ~ "Miss",
                      .data$x > c_x & .data$condition=="signal" ~ "Hit",
                      .data$x < c_x & .data$condition=="noise" ~ "Correct rejection",
                      .data$x > c_x & .data$condition=="noise" ~ "False alarm",
                      .default = "NA"),
                  sdttype = factor(.data$sdttype,
                                   levels = c("Hit", "Miss", "Correct rejection", "False alarm")))

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
    dplyr::mutate(condition = forcats::fct_recode(.data$condition,
                                                  "signal(+noise)"="signal", "noise"="noise")) |>
    ggplot2::ggplot(ggplot2::aes(x=x)) +
    ggplot2::geom_ribbon(data=df_sdt |>
                           dplyr::filter(x > c_x) |>
                           dplyr::select(-condition),
                         ggplot2::aes(ymin=0, ymax=.data$y, fill=.data$sdttype),
                         alpha=0.4) +
    ggplot2::scale_fill_manual(values=c("#D55E00", "#56B4E9")) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(ggplot2::aes(y=.data$y,
                                    color=.data$condition,
                                    linetype=.data$condition)) +
    ggplot2::scale_color_manual(values=c("black", "gray35")) +
    ggplot2::scale_linetype_manual(values=c("solid", "longdash")) +
    ggplot2::scale_y_continuous(limits=c(0, ymax), expand=c(0,0)) +
    ggplot2::scale_x_continuous(limits=xlim, breaks=seq(-10, 10, by=2)) +
    ggplot2::geom_vline(xintercept = c_x, linewidth=1) +
    ggplot2::geom_vline(xintercept = d/2, linetype="dashed") +
    ggplot2::labs(color=NULL, linetype=NULL, fill=NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),  # Remove y-axis text
                   axis.ticks.y = ggplot2::element_blank(),  # Remove y-axis ticks
                   axis.title.y = ggplot2::element_blank(),  # Remove y-axis title
                   axis.title.x = ggplot2::element_blank(),  # Remove y-axis title
                   axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.5),  # Add x-axis line
                   panel.background = ggplot2::element_rect(fill = "white"),
                   text = ggplot2::element_text(size=20))

  if (showd != FALSE) {

    if (showd == TRUE) {
      dcolor = "red"
    } else {
      dcolor = showd
    }

    # add label for sensitivity d'
    plot_sdt <- plot_sdt +
      ggplot2::geom_label(label='italic("d")*"\'"', x = d/2, y = max(df_sdt$y),
                          parse = TRUE, #
                          color=dcolor, size=20, size.unit = "pt",
                          vjust = -0.25, hjust = 0.5,
                          label.padding = ggplot2::unit(0.5, "lines")) +
      ggplot2::geom_segment(x = 0, y =  dnorm(0, mean = 0, sd = 1),
                            xend = d, yend = dnorm(d, mean = d, sd = 1),
                            color=dcolor,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "lines"), ends="both")) +
      ggplot2::geom_segment(x = 0, y = 0,
                            xend = 0, yend = dnorm(0, mean = 0, sd = 1),
                            color="gray30", linetype = "dashed") +
      ggplot2::geom_segment(x = d, y = 0,
                            xend = d, yend = dnorm(d, mean = d, sd = 1),
                            color="gray30", linetype = "dashed")
  }

  if (showc != FALSE) {

    if (showc == TRUE) {
      ccolor = "darkgreen"
    } else {
      ccolor = showc
    }

    # add label for criteria C
    plot_sdt <- plot_sdt +
      ggplot2::geom_label(label="italic(c)", x = (c_x+d/2)/2, y = .3,
                          parse = TRUE,
                          color=ccolor, size=20, size.unit = "pt",
                          vjust = -0.25, hjust = 0.5,
                          label.padding = ggplot2::unit(0.5, "lines")) +
      ggplot2::geom_segment(x = c_x, y = .3, xend = d/2, yend = .3,
                            color=ccolor,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "lines")))
  }

  if (showb != FALSE){

    if (showb == TRUE) {
      bcolor = "blue"
    } else {
      bcolor = showb
    }

    # add label for beta
    plot_sdt <- plot_sdt +
      ggplot2::geom_label(label="beta==frac(italic(l)[Hit],italic(l)[FA])",
                          x = -2.8, y = .25,
                          color=bcolor, size=24, size.unit = "pt",
                          vjust = -0.25, hjust = 0.5,
                          parse = TRUE,
                          label.padding = ggplot2::unit(0.5, "lines")) +
      ggplot2::geom_segment(x = c_x-.6, y = 0,
                            xend = c_x-.6, yend = dnorm(c_x, mean = d, sd = 1),
                            color=bcolor,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "lines"), ends="both")) +
      ggplot2::geom_segment(x = c_x-.6, y = dnorm(c_x, mean = d, sd = 1),
                            xend = c_x, yend = dnorm(c_x, mean = d, sd = 1),
                            linetype="dashed",
                            color=bcolor) +
      ggplot2::geom_segment(x = c_x-1.2, y = 0,
                            xend = c_x-1.2, yend = dnorm(c_x, mean = 0, sd = 1),
                            color=bcolor,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "lines"), ends="both")) +
      ggplot2::geom_segment(x = c_x-1.2, y = dnorm(c_x, mean = 0, sd = 1),
                            xend = c_x, yend = dnorm(c_x, mean = 0, sd = 1),
                            linetype="dashed",
                            color=bcolor) +
      ggplot2::geom_label(label="italic(l)[Hit]",
                          x = c_x-.6, y = dnorm(c_x, mean = d, sd = 1)/2,
                          color=bcolor, size=18, size.unit = "pt",
                          vjust = 0.5, hjust = 0.5,
                          parse = TRUE,
                          label.padding = ggplot2::unit(0.5, "lines")) +
      ggplot2::geom_label(label="italic(l)[FA]",
                          x = c_x-1.2, y = dnorm(c_x, mean = 0, sd = 1)/2,
                          color=bcolor, size=18, size.unit = "pt",
                          vjust = 0.25, hjust = 0.5,
                          parse = TRUE,
                          label.padding = ggplot2::unit(0.5, "lines"))
  }

  out$plot <- plot_sdt

  return(out)
}
