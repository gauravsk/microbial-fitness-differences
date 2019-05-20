# defines functions used to make the plots on the two-species pane

twospecies_cone <- function(df) {
  cone <- coex_cone_truncated + 
    geom_point(data = df, aes(x = nd, y = fd, fill = factor(which), size = which),
               pch = 21, stroke = 1.2) + 
    scale_fill_manual(values = c("#CC79A7", "#CC79A7", "#F0E442")) +
    scale_size_manual(values = c(3,3,6)) + 
    geom_text(data = df, aes(x = nd +.01, y = fd, label = which, color = outcome), hjust = 0,
              size = c(7,4,4), fontface = "bold.italic") +
    scale_color_manual(values = c("white", "black")) + 
    theme(legend.position = "none")
  
  if (all(df$outcome == "coexist")) {
    cone <- cone + scale_color_manual(values = "white")
  }

  if (all(df$outcome == "exclusion")) {
    cone <- cone + scale_color_manual(values = "black")
  }
  return(cone)
}

twospecies_trajectory <- function(df) {
  yn1 <- filter(df, species == "Ni") %>% 
    select(size) %>% tail(., n = 1) %>% unlist %>% unname
  yn2 <- filter(df, species == "Nj") %>% 
    select(size) %>% tail(., n = 1) %>% unlist %>% unname
  plot_trajectories(df) + 
    annotate("text", x = 120, y = yn1+5, label = latex2exp::TeX("N_1"), size = 6) + 
    annotate("text", x = 90, y = yn2+5, label = latex2exp::TeX("N_2"), size = 6) +
    labs(title = "Trajectory for Net outcome") + 
    theme(title = element_text(size = 14))
}