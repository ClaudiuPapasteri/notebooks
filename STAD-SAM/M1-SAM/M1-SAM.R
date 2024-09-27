if (!require("pacman")) install.packages("pacman", dependencies = TRUE)
packages <- c(
  "papaja",
  "here", "fs",
  "conflicted",
  "rio",
  "tidyverse", 
  "psych",          
  "rstatix", "ggstatsplot",
  "ggplot2", "ggpubr", "scales",
  "report"
  # , ...
)
pacman::p_load(char = packages)

# Set here to Rnotebook directory
here::set_here()
unloadNamespace("here")                   # need new R session or unload namespace for .here file to take precedence over .Rproj
notebook_name <- fs::path_file(here::here())

# Solve conflicts in favor of tidyverse
conflicted::conflict_prefer("filter", winner = "dplyr")
conflicted::conflict_prefer("select", winner = "dplyr")
conflicted::conflict_prefer("slice", winner = "dplyr")
conflicted::conflict_prefer("rename", winner = "dplyr")
conflicted::conflict_prefer("count", winner = "dplyr")

# Set kintr options including root.dir pointing to the .here file in Rnotebook directory
knitr::opts_chunk$set(
  root.dir = here::here(),
  #fig.width = 5, fig.asp = 1/3, 
  comment = "#",
  collapse = TRUE,
  echo = TRUE, warning = TRUE, message = TRUE, cache = TRUE       # echo = False for github_document, but will be folded in html_notebook
)

# Themes for ggplot2 plotting (here used APA style)
theme_set(papaja::theme_apa())


# -------------------------------------------------------------------------

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
  
} # use: fast_ggsave(plot_1, path = graphs_out_folder)


# -------------------------------------------------------------------------

## Read files
m1_file <- "Date Complete M1 v.13 siPPGGSRamilaza.sav"
sam_file <- "df_memo_SAM.RDS"
folder <- r'-(C:\Users\User\c\Github\R Notebooks\notebooks\STAD-SAM\M1-SAM)-' 

m1_df <- rio::import(file.path(folder, m1_file))
sam_df <- readRDS(file.path(folder, sam_file))

m1_df <-
  m1_df %>%
  dplyr::filter(P == 3) %>%
  rowwise() %>%
  mutate(
    Valence =  mean(c_across(starts_with("s1.")), na.rm = TRUE),
    Vividness = mean(c_across(starts_with("s2.")), na.rm = TRUE),
    Relevance = mean(c_across(starts_with("s3.")), na.rm = TRUE)
  )

# check
all.equal(m1_df$Valence, m1_df$Media_s1); cbind(m1_df$Valence, m1_df$Media_s1) 
all.equal(m1_df$Vividness, m1_df$Media_s2); cbind(m1_df$Vividness, m1_df$Media_s2) 
all.equal(m1_df$Relevance, m1_df$Media_s3); cbind(m1_df$Relevance, m1_df$Media_s3)


df <- rbind(
  m1_df %>% 
    dplyr::mutate(Conditia = "M1") %>%
    dplyr::select(ID, Conditia, Varsta, Valence, Vividness, Relevance) %>%
    haven::zap_formats() %>% haven::zap_label(), 
  sam_df %>%
    dplyr::select(ID, Conditia, Varsta, Valence, Vividness, Relevance)
)

# -------------------------------------------------------------------------
plotval <- 
  df %>%
    ggstatsplot::ggbetweenstats(
      x = Conditia,
      y = Valence,
      outlier.label = ID,
      xlab = ""
    )

plotviv <-
  df %>%
    ggstatsplot::ggbetweenstats(
      x = Conditia,
      y = Vividness,
      outlier.label = ID,
      xlab = ""
    )

plotrelv <-
  df %>%
    ggstatsplot::ggbetweenstats(
      x = Conditia,
      y = Relevance,
      outlier.label = ID,
      xlab = ""
    )


plot_m1vrmiros_val <- 
  df %>%
  as.data.frame() %>%
  dplyr::filter(Conditia %in% c("M1", "VR miros")) %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Valence,
    outlier.label = ID,
    xlab = ""
  )

plot_m1vrmiros_viv <- 
  df %>%
  as.data.frame() %>%
  dplyr::filter(Conditia %in% c("M1", "VR miros")) %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Vividness,
    outlier.label = ID,
    xlab = ""
  )

plot_m1vrmiros_relv <- 
  df %>%
  as.data.frame() %>%
  dplyr::filter(Conditia %in% c("M1", "VR miros")) %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Relevance,
    outlier.label = ID,
    xlab = ""
  )

# fast_ggsave(plotval, width = 7, height = 5)
# fast_ggsave(plotviv, width = 7, height = 5)
# fast_ggsave(plotrelv, width = 7, height = 5)
# fast_ggsave(plot_m1vrmiros_val, width = 5, height = 5)
# fast_ggsave(plot_m1vrmiros_viv, width = 5, height = 5)
# fast_ggsave(plot_m1vrmiros_relv, width = 5, height = 5)

# -------------------------------------------------------------------------
plot_np_val <- 
  df %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Valence,
    outlier.label = ID,
    xlab = "",
    type = "np"
  )

plot_np_viv <-
  df %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Vividness,
    outlier.label = ID,
    xlab = "",
    type = "np"
  )

plot_np_relv <-
  df %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Relevance,
    outlier.label = ID,
    xlab = "",
    type = "np"
  )


plot_np__m1vrmiros_val <- 
  df %>%
  as.data.frame() %>%
  dplyr::filter(Conditia %in% c("M1", "VR miros")) %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Valence,
    outlier.label = ID,
    xlab = "",
    type = "np"
  )

plot_np__m1vrmiros_viv <- 
  df %>%
  as.data.frame() %>%
  dplyr::filter(Conditia %in% c("M1", "VR miros")) %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Vividness,
    outlier.label = ID,
    xlab = "",
    type = "np"
  )

plot_np__m1vrmiros_relv <- 
  df %>%
  as.data.frame() %>%
  dplyr::filter(Conditia %in% c("M1", "VR miros")) %>%
  ggstatsplot::ggbetweenstats(
    x = Conditia,
    y = Relevance,
    outlier.label = ID,
    xlab = "",
    type = "np"
  )

# fast_ggsave(plot_np_val, width = 7, height = 5)
# fast_ggsave(plot_np_viv, width = 7, height = 5)
# fast_ggsave(plot_np_relv, width = 7, height = 5)
# fast_ggsave(plot_np__m1vrmiros_val, width = 5, height = 5)
# fast_ggsave(plot_np__m1vrmiros_viv, width = 5, height = 5)
# fast_ggsave(plot_np__m1vrmiros_relv, width = 5, height = 5)

# -------------------------------------------------------------------------
df %>%
  as.data.frame() %>%
  dplyr::filter(Conditia %in% c("M1", "VR miros")) %>%
  lm(Valence ~ Conditia + Relevance, data = .) %>% 
  summary()

df %>%
  as.data.frame() %>%
  dplyr::select(Conditia, Valence, Relevance, Vividness) %>%
  GGally::ggpairs()
