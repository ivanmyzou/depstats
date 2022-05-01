#' Plot Power Curves
#'
#'
#' @description This function is used to produce power plots
#' @param power_data data table of the powers.
#' @param title a vector of titles.
#' @name plotpowers

plotpowers <- function(power_data, title){
  modelpowers <- power_data %>% melt(
    id.vars = c("sample_size", "scenario"),
    measure.vars = names(power_data)[1:22] #columns for competitor tests
  ) #the column for powers is value

  modelpowers[, is_model := (variable %in% c("combined", "score", "image"))]

  ggplot(modelpowers, aes(y = value, x = sample_size, color = variable,
                          alpha = is_model, size = is_model)) +
    geom_line() +
    theme_minimal() +
    scale_color_manual(values = c("combined" = "royalblue2",
                                  "score" = "salmon2",
                                  "image" = "green3")) +
    scale_alpha_manual(values = c(0.5, 0.9)) +
    scale_size_manual(values = c(0.5, 0.75)) +
    theme(legend.position="none") + xlab('sample size') + ylab('power') +
    ylim(0, 1) +
    scale_x_continuous(breaks = sample_size) + ggtitle(title)
}
