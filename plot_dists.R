library(tidyverse)
library(ggdist)

colors2 <- rev(c("#A7A9AC", "#AD1E53", "#EF3F23", "#FBAB22"))



ttz <- read_csv("data/source_data/fig4a.csv")


plot_dist_data <- function(thisdf){
  thisdf %>%
    mutate(age = paste(age, "M", sep = "")) %>%
    ggplot() +
    stat_slab(aes(as.factor(age), (rf), fill = as.factor(tx)),
              .width = 0, alpha = 0.4, n = 20,normalize = "groups",
              height = 10, adjust = 3) + 
    stat_summary(fun.y = mean,
                 aes(x = as.factor(age),color = as.factor(tx),
                     y = rf, group = factor(tx)),
                 geom = "line", size = 1.5, alpha = 0.2) +
    facet_grid(.~tx, scales = "free", space = "free") + 
    stat_summary(aes(x = as.factor(age),color = as.factor(tx),
                     y = rf), 
                 fun.data = mean_sdl,
                 shape = 18, size = 0.6)  +
    scale_color_manual(values = rev(colors2)) +
    scale_fill_manual(values = rev(colors2)) +
    mytheme() #+
  #theme(aspect.ratio = 1)
}

ttz %>%
  plot_dist_data() + 
  scale_y_sqrt() +
  labs(y = "time to zero (s)")
