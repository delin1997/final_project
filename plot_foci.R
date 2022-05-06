library(ggplot2)
library(ggalt)
plot_foci <- function(foci_female, foci_male, ind = c(1, 1)){
  data <- data.frame(codec = foci_female$stepT, 
                     names = gsub('[_new]', '', foci_female$selectedVar$names))
  data$names <- factor(data$names, levels = data$names)
  p <- ggplot(data = data, mapping = aes(x = names, y = codec)) + geom_point()
  plot_female <- p + 
    theme(panel.grid=element_blank(),panel.border=element_blank(),
          axis.line=element_line(size=0.5,colour="black")) + 
    theme(legend.position="none") +
    labs(x = element_blank(), y = element_blank()) + 
    geom_hline(yintercept=foci_female$stepT[ind[1]], linetype = 2) + 
    geom_vline(xintercept=gsub('[_new]', '', foci_female$selectedVar$names[ind[1]]), linetype = 2)
  
  data <- data.frame(codec = foci_male$stepT, 
                     names = gsub('[_new]', '', foci_male$selectedVar$names))
  data$names <- factor(data$names, levels = data$names)
  p <- ggplot(data = data, mapping = aes(x = names, y = codec)) + geom_point()
  plot_male <- p + 
    theme(panel.grid=element_blank(),panel.border=element_blank(),
          axis.line=element_line(size=0.5,colour="black")) + 
    theme(legend.position="none") +
    labs(x = element_blank(), y = element_blank()) + 
    geom_hline(yintercept=foci_male$stepT[ind[2]], linetype = 2) + 
    geom_vline(xintercept=gsub('[_new]', '', foci_male$selectedVar$names[ind[2]]), linetype = 2)
  
  ggarrange(plot_female, plot_male, ncol = 1, nrow = 2)
}

