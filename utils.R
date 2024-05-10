# Demo Data Distribution Plots ----

build_dist_plot <- function(data, title) {
  # Make survey design
  srvyr_design <- data %>%
    srvyr::as_survey_design(ids = 1, weights = weight)

  # Compute counts
  count_df <- srvyr_design %>%
    group_by(lrcat, alcat) %>%
    summarise(meanlr = survey_mean(lrScale, na.rm=T),
              meanal = survey_mean(alScale, na.rm=T),
              freq = survey_total(na.rm=T)) %>%
    ungroup() %>%
    filter(!is.na(lrcat) & !is.na(alcat))

  # Core plot
  plot <- count_df %>%
    mutate(share = freq / sum(freq, na.rm=T),
           alcat = as.character(alcat),
           alcat = case_when(alcat == "Con" ~ "Auth",
                             TRUE ~ alcat),
           label = paste(lrcat,alcat),
           label = replace(label, label == "Centre Centre","Centre"),
           label = replace(label, label == "Left Centre","Left"),
           label = replace(label, label == "Centre Lib","Lib"),
           label = replace(label, label == "Right Centre","Right"),
           label = replace(label, label == "Centre Auth","Auth")) %>%
    ggplot(aes(y=meanal, x=meanlr, size=share)) +
    geom_point(shape=21, fill=liteblu, color="black") +
    scale_y_continuous(limits=c(0,4)) +
    scale_x_continuous(limits=c(0,4)) +
    labs(y = "Libertarian-Authoritarian", x = "Left-Right") +
    theme_bw() +
    theme(aspect.ratio=1) +
    guides(size="none")

  # Top histogram
  hist_top <- as.data.frame(svytable(~ lrScale, design=srvyr_design)) %>%
    ggplot(aes(y=Freq, x=lrScale)) +
    geom_bar(stat="identity", fill=liteblu, color="black") +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(axis.ticks=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank())

  # Right histogram
  hist_right <- as.data.frame(svytable(~ alScale, design=srvyr_design)) %>%
    ggplot(aes(y=Freq, x=alScale)) +
    geom_bar(stat="identity", fill=liteblu, color="black") +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    coord_flip() +
    theme(axis.ticks=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank())

  # Putting together the plot
  g <- ggplotGrob(plot)
  panel_id <- g$layout[g$layout$name == "panel",c("t","l")]
  g <- gtable_add_cols(g, unit(1,"in"))
  g <- gtable_add_grob(g, ggplotGrob(hist_right),
                       t = panel_id$t, l = ncol(g))
  g <- gtable_add_rows(g, unit(1,"in"), 0)
  g <- gtable_add_grob(g, ggplotGrob(hist_top),
                       t = 1, l = panel_id$l)
  grid.newpage()

  # Output object
  joint_plot <- grid.arrange(g, top=title)
  return(joint_plot)
}



# Demo Coef Plots ----

# Fn for extracting df from list of regression results
prepare_demo_data <- function(data, plot) {
  # Labels
  datasets <- c("BSA", "BES")

  # Check which one we want
  if (plot == 1) {
    nums <- 1:2
    b <- 0
  }
  if (plot == 2) {
    nums <- 3:4
    b <- 2
  }

  # Shape data depending on which regressions are desired
  demo_plot_data <- map_dfr(nums, function(x) {
    broom::tidy(data[[x]], conf.int=T) %>%
      mutate(dataset=datasets[x-b])
  }) %>%
    filter(term != "(Intercept)") %>%
    mutate(term = factor(term, levels=c("edlevelGCSE/equiv",
                                        "edlevelA-level/equiv",
                                        "edlevelUndergrad",
                                        "edlevelPostgrad")))

  return(demo_plot_data)
}

# Fn for making coefficient plots
make_demo_coef_plot <- function(data, plot) {
  # Prepare data
  data <- prepare_demo_data(data, plot)

  # Position dodge value repeated
  pd <- position_dodge(width = 0.4)

  # Set title based on plot
  if (plot == 1) title <- "Left-Right"
  if (plot == 2) title <- "Libertarian-Authoritarian"

  # Make Plot
  p <- ggplot(data, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high, label=round(estimate,2), color=dataset, shape=dataset)) +
    geom_vline(xintercept=0) +
    geom_point(position=pd, size=2) +
    geom_errorbar(width=0,
                  size=1,
                  position=pd) +
    geom_text_repel(color="black", force=2) +
    scale_x_continuous(limits=c(-1,1))  +
    scale_y_discrete(limits=rev, labels = c("Postrgrad", "Undergrad", "A-level/Equiv", "GSCE/Equiv")) +
    scale_color_discrete(labels=c("BES", "BSA"), name="Dataset") +
    scale_shape("Dataset") +
    labs(title = title, y="Variable", x="Estimate") +
    theme_bw() +
    theme(aspect.ratio=1,
          plot.title=element_text(hjust=0.5))

  # Return plot
  return(p)
}
















