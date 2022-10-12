
#' Plot hypothetical scenarios for NHST and Equivalence Tests
#' @description Plot the hypothetical scenarios for NHST and Equivalence Test.
#'
#' @param delta_null one value or a vector of two values. One value will be used
#' for one-sided tests and two values will be used as two-sided tests.
#' @param linecustom a custom group of situations to be displayed.
#' @param xrange the range of the x-axis.
#' @param StrScenario the string to be displayed on y-axis. Default to "Scenario".
#' @param linecolor the three colors to be used for plotting NHST significant,
#' ET significant, and inconclusive results. Default to red, blue, gray.
#' @param jitters some jitters to show the intervals. Default to .2.
#' @param label label of x-axis. Default to "Correlation coefficient".
#' @param seed seed for generating the jitters.
#'
#' @return a plot (so far).
#' @export
#'
#' @examples
#' equi_plot(c(-.15, .15))
#' equi_plot(-.3)
#' equi_plot(.2)
#' linecustom <- matrix(c(0.10, -0.23, -0.13, -0.04, -0.19, -0.17,
#'                        0.30, -0.03,  0.07,  0.16,  0.01,  0.17), nrow=2, byrow=TRUE)
#' equi_plot(c(-.15, .15), linecustom=linecustom)
equi_plot <- function(delta_null,
                      linecustom = NULL,
                      xrange = NULL,
                      StrScenario="Scenario",
                      linecolor=c("#D55E00", "#56B4E9", "gray30"), # NHST, ET, nothing
                      jitters = .2,
                      label="Correlation coefficient", seed=1215) {

  set.seed(seed)

  if (length(delta_null)==1){
    # one-sided
    delta_lr <- min(delta_null, 0)
    delta_ur <- max(delta_null, 0)

    if (delta_lr<0){
      alt <- "smaller"
      delta_ur <- -delta_lr # flip it temporarily
    } else if (delta_ur>0) {
      alt <- "greater"
    }

  } else if (length(delta_null)==2){
    alt <- "two-sided"
    # two-sided
    delta_lr <- min(delta_null)
    delta_ur <- max(delta_null)
  }

  # generate equivalence intervals
  if (!is.null(linecustom)){
    # the first and second rows of linecustom will be the lower and upper boundaries
    df_equi <- data.frame(
      CI_low = linecustom[1, ],
      CI_upp = linecustom[2, ])

  } else if (length(delta_null)==1){
    message(delta_null)

    df_equi <- data.frame(
      CI_low = c(delta_ur*(1+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*jitters*stats::runif(1,.5,1.1),
                 delta_ur*jitters*stats::runif(1,.5,1.1),
                 -delta_ur*(jitters*stats::runif(1,.5,1.1)),
                 -delta_ur*(.5+jitters*stats::runif(1,.5,1.1)),
                 -delta_ur*(.5+jitters*stats::runif(1,.5,1.1)),
                 -delta_ur*(jitters*stats::runif(1,.5,1.1))),
      CI_upp = c(delta_ur*(1.8+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*(1.3+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*(.7+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*(.7+jitters*stats::runif(1,.5,1.1)),
                 -delta_ur*(jitters*stats::runif(1,.5,1.1)),
                 delta_ur*(.7+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*(1.3+jitters*stats::runif(1,.5,1.1)))
    )

  } else if (length(delta_null)==2){
    message(delta_null)
    df_equi <- data.frame(
      CI_low = c(delta_lr*(1+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*jitters*stats::runif(1,.5,1.1),
                 delta_lr*(.6+jitters*stats::runif(1,.5,1.1)),
                 delta_lr*(1+jitters/2*stats::runif(1,.5,1.1)),
                 delta_lr*jitters*stats::runif(1,.5,1.1),
                 delta_lr*(1+jitters/2*stats::runif(1,.5,1.1))),
      CI_upp = c(delta_lr*jitters*stats::runif(1,.5,1.1),
                 delta_ur*(1+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*(.6+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*jitters*stats::runif(1,.5,1.1),
                 delta_ur*(1+jitters*stats::runif(1,.5,1.1)),
                 delta_ur*(1+jitters*stats::runif(1,.5,1.1)))
    )
  }

  df_equi <- df_equi |>
    dplyr::mutate(Scenarios = paste(StrScenario, 1:nrow(df_equi)),
                  Scenarios = as.factor(.data$Scenarios),
                  Scenarios = factor(.data$Scenarios,
                                     levels = rev(levels(.data$Scenarios))))

  if (alt == "two-sided") {
    df_equi <- df_equi |>
      dplyr::mutate(sigNHST = .data$CI_upp<0 | .data$CI_low>0,
                    sigET = .data$CI_low >= delta_lr & .data$CI_upp <= delta_ur,
                    incon = !.data$sigNHST & !.data$sigET,
                    result = ifelse(.data$sigNHST, "NHST",
                                    ifelse(.data$sigET, "ET", "Incon")),
                    result = factor(.data$result, levels=c("NHST", "ET", "Incon")))

    if (is.null(xrange)){
      themax <- min(max(abs(df_equi$CI_low),abs(df_equi$CI_upp))+.1,1)
      xrange <- c(-themax, themax)
    }

  } else {
    df_equi <- df_equi |>
      dplyr::mutate(sigNHST = .data$CI_low>0,
                    sigET = .data$CI_upp <= delta_ur,
                    incon = !.data$sigNHST & !.data$sigET,
                    result = ifelse(.data$sigNHST, "NHST",
                                    ifelse(.data$sigET, "ET", "Incon")),
                    result = factor(.data$result, levels=c("NHST", "ET", "Incon")))

    if (is.null(xrange)){
      themax <- min(max(df_equi$CI_upp)+.1,1)
      themin <- max(min(df_equi$CI_low)-.1,-1)
      xrange <- c(themin, themax)
    }

    if (alt == "smaller"){
      delta_ur <- 0
      xrange <- sort(-xrange)
      df_equi <- df_equi |>
        dplyr::mutate(CI_low = -.data$CI_low,
                      CI_upp = -.data$CI_upp)
    }
  }

  # break values
  break_values <- sort(unique(c(delta_lr,delta_ur,0,-delta_lr,-delta_ur)))
  # vertical lines to plot
  vline_values <- unique(c(delta_lr,delta_ur))
  vline_values <- vline_values[vline_values !=0]

  plot_equi <- ggplot2::ggplot(df_equi,
                               ggplot2::aes(xmin = .data$CI_low, xmax = .data$CI_upp,
                                            y = .data$Scenarios, color = .data$result)) +
    ggplot2::geom_errorbarh(height = .4, size = 1) +
    ggplot2::scale_color_manual(values = linecolor) +
    ggplot2::geom_vline(xintercept = 0, linetype = "longdash") +
    ggplot2::geom_vline(xintercept = vline_values, linetype = "dashed", color = "gray30") +
    ggplot2::scale_x_continuous(limits = xrange, breaks = break_values) +
    ggplot2::xlab("label") +
    papaja::theme_apa() +
    ggplot2::theme(legend.position = "none") +
    NULL

  # ggsave("equivalence_test.png", plot_equi, width = 8, height = 5)
  return(plot_equi)

}
