# For publication
my_ggwithinstats2 <- function(data, title, x, y, outlier.label, xlab, ylab, 
                              outlier.tagging = FALSE, results.subtitle = FALSE, 
                              centrality.label.args = TRUE, point.path = FALSE,
                              ...) {  # ... for limits and breaks
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  outlier.label <- rlang::enquo(outlier.label)
  
  if(centrality.label.args){
    centrality.label.args <- list(size = 3, nudge_x = 0.2, segment.linetype = 5, fill = "#FFF8E7")
  }else{
    centrality.label.args <- list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0) # very hacky way of not showing label
  }
  
  data %>%
    ggstatsplot::ggwithinstats(
      x = !!x,
      y = !!y,
      title = title,
      xlab = xlab,
      ylab = ylab,
      outlier.tagging = outlier.tagging,                    # whether outlines need to be tagged
      outlier.label = !!outlier.label,                      # variable to be used for tagging outliers
      outlier.coef = 2,
      pairwise.comparisons = TRUE,
      pairwise.display = "all",
      results.subtitle = results.subtitle,
      type = "parametric",
      bf.message = FALSE, 
      p.adjust.method = "none",
      point.path = point.path,
      ggtheme = ggprism::theme_prism(),
      # package = "RColorBrewer",  # "ggsci",
      # palette = "Dark",         # "default_jco",
      violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
      centrality.plotting = TRUE,
      centrality.type = "parameteric",
      centrality.point.args = list(size = 5, color = "darkred"),
      centrality.label.args = centrality.label.args,
      ggplot.component = list(
        theme(
          plot.title = element_text(hjust = 0, size = 16),
          plot.subtitle = element_text(hjust = 0, size = 12), 
          plot.caption = element_text(hjust = 0, size = 12), 
          text = element_text(size = 14)
        ))
    ) + scale_colour_grey(start = 0.2, end = 0.2) +  # hacky way to change point color
    scale_y_continuous(...)
}

# Fast ggsave - saves plot with filename of R plot object
fast_ggsave <- function(plot, device = "png", path = NULL,
                        units = "in", dpi = 300, width = 5, height = 5, ...){ 
  plot_name <- deparse(substitute(plot))
  ggplot2::ggsave(filename = paste0(plot_name, ".", device), plot = plot,
                  device = device, path = path,
                  units = units, dpi = dpi,
                  width = width, height = height,
                  ...
  )
  
} # use: fast_ggsave(jrad_ox_p, path = savefolder)



###### 
# OX
jrad_ox_p <- 
  jrad_df %>%
  dplyr::select(ID, OX_pre, OX_post) %>%
  tidyr::pivot_longer(cols = c(OX_pre, OX_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "JRAD",
    ylab = "OXT",
    centrality.label.args = FALSE,
    breaks = seq(from = .8, to = 1.3, by = .1),
    limits = c(.75, 1.32)
  )
fast_ggsave(jrad_ox_p, path = savefolder)

es_df %>%
  rstatix::get_summary_stats(OX_pre, OX_post, type = "common")

es_ox_p <- 
  es_df %>%
  dplyr::select(ID, OX_pre, OX_post) %>%
  tidyr::pivot_longer(cols = c(OX_pre, OX_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "ES",
    ylab = "OXT",
    centrality.label.args = FALSE,
    breaks = seq(from = .8, to = 1.3, by = .1),
    limits = c(.75, 1.32)
  )
fast_ggsave(es_ox_p, path = savefolder)


# STAI
jrad_stai_p <- 
  jrad_df %>%
  dplyr::select(ID, StaiS_pre, StaiS_post) %>%
  tidyr::pivot_longer(cols = c(StaiS_pre, StaiS_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "JRAD",
    ylab = "Anxiety",
    centrality.label.args = FALSE,
    breaks = seq(from = 20, to = 65, by = 5),
    limits = c(19, 65)
  )
jrad_stai_p
fast_ggsave(jrad_stai_p, path = savefolder)


es_stai_p <-
  es_df %>%
  dplyr::select(ID, StaiS_pre, StaiS_post) %>%
  tidyr::pivot_longer(cols = c(StaiS_pre, StaiS_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "ES",
    ylab = "Anxiety",
    centrality.label.args = FALSE,
    breaks = seq(from = 20, to = 65, by = 5),
    limits = c(19, 65)
  )
es_stai_p
fast_ggsave(es_stai_p, path = savefolder)

# PA
jrad_pa_p <- 
  jrad_df %>%
  dplyr::select(ID, PA_pre, PA_post) %>%
  tidyr::pivot_longer(cols = c(PA_pre, PA_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "JRAD",
    ylab = "Positive Affect",
    centrality.label.args = FALSE,
    breaks = seq(from = 20, to = 50, by = 5),
    limits = c(17, 51)
  )
jrad_pa_p
fast_ggsave(jrad_pa_p, path = savefolder)


es_pa_p <- 
  es_df %>%
  dplyr::select(ID, PA_pre, PA_post) %>%
  tidyr::pivot_longer(cols = c(PA_pre, PA_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "ES",
    ylab = "Positive Affect",
    centrality.label.args = FALSE,
    breaks = seq(from = 20, to = 50, by = 5),
    limits = c(17, 51)
  )
es_pa_p
fast_ggsave(es_pa_p, path = savefolder)


# NA  
jrad_na_p <- 
  jrad_df %>%
  dplyr::select(ID, NA_pre, NA_post) %>%
  tidyr::pivot_longer(cols = c(NA_pre, NA_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "JRAD",
    ylab = "Negative Affect",
    centrality.label.args = FALSE,
    breaks = seq(from = 10, to = 30, by = 5),
    limits = c(8, 30)
  ) 
jrad_na_p
fast_ggsave(jrad_na_p, path = savefolder)


es_na_p <- 
  es_df %>%
  dplyr::select(ID, NA_pre, NA_post) %>%
  tidyr::pivot_longer(cols = c(NA_pre, NA_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "ES",
    ylab = "Negative Affect",
    centrality.label.args = FALSE,
    breaks = seq(from = 10, to = 30, by = 5),
    limits = c(8, 30)
  )
es_na_p
fast_ggsave(es_na_p, path = savefolder)

# APS
jrad_aps_p <- 
  jrad_df %>%
  dplyr::select(ID, APS_pre, APS_post) %>%
  tidyr::pivot_longer(cols = c(APS_pre, APS_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "JRAD",
    ylab = "Prosocial Attitudes",
    centrality.label.args = FALSE,
    breaks = seq(from = 20, to = 80, by = 10),
    limits = c(22, 80)
  ) 
jrad_aps_p
fast_ggsave(jrad_aps_p, path = savefolder)


es_aps_p <- 
  es_df %>%                                        # id 28 is a clear outlier
  dplyr::select(ID, APS_pre, APS_post) %>%
  tidyr::pivot_longer(cols = c(APS_pre, APS_post), names_to = "Time", values_to = "value") %>%
  dplyr::mutate(
    Time = stringr::str_replace(Time, "pre", "Pre"),
    Time = stringr::str_replace(Time, "post", "Post"), 
    Time = stringr::str_remove(Time, ".*_"),
    Time = stringr::str_remove(Time, "_"), 
    Time = factor(Time, levels = c("Pre", "Post"))) %>%
  group_by(ID) %>%
  my_ggwithinstats2(
    x = Time,
    y = value,
    outlier.label = ID, 
    title = "",
    xlab = "ES",
    ylab = "Prosocial Attitudes",
    centrality.label.args = FALSE,
    breaks = seq(from = 20, to = 80, by = 10),
    limits = c(22, 80)
  ) 
es_aps_p
fast_ggsave(es_aps_p, path = savefolder)

