#' Plot PhecodeX PheWAS results
#' @param data data of PheWAS results
#' @param beta_var name of column containing beta values
#' @param log10p_var name of column containing -log10(p) values
#' @param color_var name of column containing color values
#' @param phe_var name of column containing phecode values
#' @param threshold_color color of dashed line at p = 0.05
#' @param suggestive_threshold p-value of suggestive threshold if desired; NULL otherwise
#' @param suggestive_color color of suggestive threshold
#' @param order_reset reset ordering of phecodes
#' @param group_space space between groups
#' @param label_top number of top phecodes to label
#' @param title title of plot
#' @param genetic_offset offset for genetic phecodes
#' @param color_dot_pt size of color dots
#' @param color_dot_symbol symbol for color dots
#' @param label_size size of labels
#' @return a Manhattan plot of PheWAS results
#' @import ggplot2
#' @import data.table
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggtext element_markdown
#' @importFrom ms theme_ms
#' @export
#' @examples
#' \dontrun{
#' ## TO DO
#' }

plot_phewasx <- function(
    data,
    beta_var             = "beta",
    log10p_var           = "log10p",
    color_var            = "color",
    phe_var              = "exposure",
    threshold_color      = "red",
    suggestive_threshold = 0.05,
    suggestive_color     = "orange",
    order_reset          = TRUE,
    group_space          = 20,
    label_top            = 5,
    title                = NULL,
    genetic_offset       = 15,
    color_dot_pt         = 15,
    color_dot_symbol     = "\u25CF",
    label_size           = 3
) {
  # prep
  if (!data.table::is.data.table(data)) data <- data.table::as.data.table(data)
  data2 <- data.table::copy(data.table::as.data.table(data))

  if (beta_var %in% names(data2)) {
    phewas <- data2[, direction := data.table::fifelse(get(beta_var) > 0, "Positive", "Negative")]
  } else {
    phewas <- data2[, direction := "None"]
  }
  if (!data.table::is.data.table(phewas)) phewas <- data.table::as.data.table(phewas)

  plot_data <- data.table::merge.data.table(
    phewas,
    glp1::pheinfox[, .(
      phecode,
      group,
      groupnum,
      description,
      order
    )],
    by.x = phe_var,
    by.y = "phecode"
  )[order(order), ]
  if (!data.table::is.data.table(plot_data)) plot_data <- data.table::as.data.table(plot_data)

  # reset ordering, if requested
  if (order_reset) {
    plot_data[order(order), order := 1:.N]
  }

  # add spacing, if requested
  if (group_space > 0) {
    plot_data <- plot_data[, order := order + ((groupnum - 1) * group_space)]
  }
  if (genetic_offset > 0) {
    plot_data <- plot_data[group == "Genetic", order := order + genetic_offset]
  }

  vars              <- c("group", color_var)
  plot_data_mean    <- plot_data[, .(mean = mean(order, na.rm = TRUE)), by = group]
  plot_data_mean    <- data.table::merge.data.table(plot_data_mean, unique(glp1::pheinfox[, ..vars]), by = "group")
  phe_colors        <- plot_data_mean[[color_var]]
  names(phe_colors) <- plot_data_mean[, group]

  # plot
  plot <- plot_data |>
    ggplot2::ggplot(aes(x = order, y = -get(log10p_var), fill = group, color = group)) +
    ggplot2::geom_point(ggplot2::aes(shape = direction), size = 2, alpha = 0.5, show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = -log10(0.05 / phewas[!is.na(get(log10p_var)), .N]), linewidth = 1, linetype = "dashed", color = threshold_color) +
    (\() if (!is.null(suggestive_threshold))
      ggplot2::geom_hline(yintercept = -log10(0.05), linewidth = 1, linetype = "dashed", color = suggestive_color)
    )() +
    (\() if (!is.null(color_dot_symbol)) {
      ggplot2::scale_x_continuous(
        breaks = plot_data_mean[, mean],
        labels = paste0(
          "<span style=\"color: ", plot_data_mean[, color], "\"><span style=\"font-size: ", color_dot_pt, "pt\">", color_dot_symbol, "</span></span> ",
          plot_data_mean[, group]
        )
      )
    } else {
      ggplot2::scale_x_continuous(
        breaks = plot_data_mean[, mean],
        labels = paste0(
          "<span style=\"color: ", plot_data_mean[, color], "\">",
          plot_data_mean[, group], "</span>"
        )
      )
    })() +
    ggplot2::labs(
      x = "",
      y = "-log10(p-value)"
    ) +
    # scale_x_continuous(breaks = plot_data_mean[, mean], labels = plot_data_mean[, group]) +
    ggplot2::scale_fill_manual(values = phe_colors) +
    ggplot2::scale_color_manual(values = phe_colors) +
    ggplot2::scale_shape_manual(values = c("Positive" = 24, "Negative" = 25, "None" = 19)) +
    ms::theme_ms(show_grid_lines = FALSE) +
    ggplot2::theme(
      axis.text.x = ggtext::element_markdown(angle = -45, hjust = 0)
    )

  if (!is.null(label_top)) {
    plot <- plot +
      ggrepel::geom_label_repel(
        data = plot_data[order(get(log10p_var))][1:label_top, ],
        ggplot2::aes(x = order, y = -get(log10p_var), label = description),
        color = "black",
        show.legend = FALSE,
        inherit.aes = FALSE,
        size        = label_size,
        max.time    = 3,
        max.iter    = 100000
      )
  }

  if (!is.null(title)) {
    plot <- plot + ggplot2::labs(title = title)
  }

  return(plot)
}
