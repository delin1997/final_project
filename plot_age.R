library(ggplot2)
library(ggalt)
plot_age <- function(CA_female, BA_female, CA_male, BA_male){
  data <- data.frame(CA = CA_female, BA = BA_female)
  p <- ggplot(data = data, mapping = aes(x = CA, y = BA)) + geom_point(alpha = 0.3) +
    #geom_jitter(height = 1, width = 1, alpha = 0.3) + 
    geom_smooth(method='lm', formula= y~x)
  plot_female <- p + 
    theme(panel.grid=element_blank(),panel.border=element_blank(),
          axis.line=element_line(size=0.5,colour="black")) + 
    labs(x = element_blank(), y = element_blank()) + 
    theme(legend.position="none")
  
  data <- data.frame(CA = CA_male, BA = BA_male)
  p <- ggplot(data = data, mapping = aes(x = CA, y = BA)) + geom_point(alpha = 0.3) +
    #geom_jitter(height = 1, width = 1, alpha = 0.3) + 
    geom_smooth(method='lm', formula= y~x)
  plot_male <- p + 
    theme(panel.grid=element_blank(),panel.border=element_blank(),
          axis.line=element_line(size=0.5,colour="black")) + 
    labs(x = element_blank(), y = element_blank()) + 
    theme(legend.position="none")
  
  ggarrange(plot_female, plot_male, ncol = 1, nrow = 2)
}



