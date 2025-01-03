library(ggplot2)
library(plyr)
library(grid)
library(gridExtra) 
library(cowplot)
library(ggrepel)
library(scales)


##########################################################################
######                                                              ######
######         Scatterplot for CTmax, CTmin and Bio                 ######
######         vs Elevation and Bio 1                               ######
######         uses "New_PGLS_CTmax_Sept24.csv"                     ######
######         uses "New_PGLS_CTmin_Sept24.csv"                     ######
######                                                              ######
##########################################################################

setwd("C:/Users/jhanc/Box/PhD/Talks/SICB_2024")

#######################
####               ####
#### For Elevation ####
####               ####
#######################

###############
####       ####
#### CTmax ####
####       ####
###############
data_ctmax <- read.csv("New_PGLS_CTmax_Sept24.csv", header=1)
attach(data_ctmax)
head(data_ctmax)


## CTmax vs Elevation
ele_ctmax <- ggplot(data_ctmax, aes(x = mid.ele, y = ctmax, color = clade)) +
  geom_point(aes(fill = clade), size =30, alpha = 0.7, stroke = 3, color = "black", shape = 21) +
  geom_abline(intercept = 33.71600, slope = -0.00275, linewidth=8, linetype="dashed", alpha = 0.7) + #pgls.SEy regression results
  geom_abline(intercept = 34.21586, slope = -0.00422, linewidth=8, color = "#084c61", alpha = 0.7) + #Draconura
  geom_abline(intercept = 33.73064, slope = -0.00239, linewidth=8, color = "#323031", alpha = 0.7) + #Dactyloa

  scale_x_continuous(breaks = seq(200, 2600, 800), limit = c(200, 2600)) +
  scale_y_continuous(breaks = seq(26, 34, 2), limit = c(26, 34)) +
  
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("#323031", "#084c61"), name  = "Clade") +
  scale_fill_manual(values=c("#323031", "#084c61"), name  = "Clade") +
  
  theme(panel.background = element_rect(fill = NA, colour = NA), 
        plot.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = NA, colour = NA), 
        legend.background = element_rect(fill = NA, colour = NA),
        legend.text = element_text(family="serif", size = 40),
        legend.position = c(.82, .85),
        legend.title = element_text(family="serif", size = 45, face = "bold"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  
  
  theme(legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"), 

        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
  

  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(Mean~elevation~(m))) +
  ylab(expression(CT[max]~(degree*C))) +

  theme(axis.text.x = element_text(family="serif", size = 50, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 50, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=55)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=55))


###############
####       ####
#### CTmin ####
####       ####
###############
data_ctmin = read.csv(file="New_PGLS_CTmin_Sept24.csv", header=TRUE)
attach(data_ctmin)
head(data_ctmin)

# CTmin vs Elevation
ele_ctmin <- ggplot(data_ctmin, aes(x = mid.ele, y = ctmin, color = clade)) +
  geom_point(aes(fill = clade),  size = 30, alpha = 0.7, stroke = 3, color = "black", shape = 21) +
  geom_abline(intercept = 16.940582, slope = -0.002407, linewidth=8, linetype="dashed", alpha = 0.7) + #pgls.SEy regression results
  geom_abline(intercept = 16.183112, slope = -0.001695, linewidth=8, color = "#084c61", alpha = 0.7) + #Draconura
  geom_abline(intercept = 18.892566, slope =  -0.003549, linewidth=8, color = "#323031", alpha = 0.7) + #Dactyloa
  
  scale_x_continuous(breaks = seq(200, 2600, 800), limit = c(200, 2600)) +
  scale_y_continuous(breaks = seq(6, 22, 4), limit = c(6, 22)) +
  theme_classic() +
  
  #guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("#323031", "#084c61"), name  = "Clade") +
  scale_fill_manual(values=c("#323031", "#084c61"), name  = "Clade") +

  theme(panel.background = element_rect(fill = NA, colour = NA), 
        plot.background = element_rect(fill = NA, colour = NA),
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
  
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(Mean~elevation~(m))) +
  ylab(expression(CT[min]~(degree*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 50, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 50, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=55)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=55))





################################
####                        ####
####  Let's save our plot   ####
####                        ####
################################

##############
## Let's save our plot

# Adjust the theme of each plot to remove the background



png("forPoster_Elevation_Dec02.png", height = 12, width = 26, units = "in", res = 300, bg = "transparent")

plot_grid(ele_ctmin, ele_ctmax,
          ncol = 2, nrow = 1)

dev.off()






###################
####           ####
#### For Bio 1 ####
####           ####
###################

###############
####       ####
#### CTmax ####
####       ####
###############
## CTmax vs Elevation
bio1_ctmax <- ggplot(data_ctmax, aes(x = bio1, y = ctmax, color = clade)) +
  geom_point(aes(fill = clade), size =30, alpha = 0.7, stroke = 3, color = "black", shape = 21) +
  geom_abline(intercept = 17.870792, slope = 0.612534, linewidth=8, linetype="dashed", alpha = 0.7) + # pgls.SEy regression result
  geom_abline(intercept = 14.214171 , slope = 0.743273, linewidth=8, color = "#084c61", alpha = 0.7) + #Draconura
  geom_abline(intercept = 19.974116, slope = 0.520007, linewidth=8, color = "#323031", alpha = 0.7) + #Dactyloa
  
  scale_x_continuous(breaks = seq(16, 24, 2), limit = c(16, 24)) +
  scale_y_continuous(breaks = seq(26, 34, 2), limit = c(26, 34)) +
  
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("#323031", "#084c61"), name  = "Clade") +
  scale_fill_manual(values=c("#323031", "#084c61"), name  = "Clade") +
  
  theme(panel.background = element_rect(fill = NA, colour = NA), 
        plot.background = element_rect(fill = NA, colour = NA),
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
  
  
  theme(legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"), 
        
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
  
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(Mean~annual~temperature~(degree*C))) +
  ylab(expression(CT[max]~(degree*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 50, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 50, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=55)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=55))


###############
####       ####
#### CTmin ####
####       ####
###############
# CTmin vs Elevation
bio1_ctmin <- ggplot(data_ctmin, aes(x = bio1, y = ctmin, color = clade)) +
  geom_point(aes(fill = clade),  size = 30, alpha = 0.7, stroke = 3, color = "black", shape = 21) +
  geom_abline(intercept = 2.5443051, slope = 0.5664828, linewidth=8, linetype="dashed", alpha = 0.7) + #pgls.SEy regression results
  geom_abline(intercept = 5.961357, slope = 0.409415, linewidth=8, color = "#084c61", alpha = 0.7) + #Draconura
  geom_abline(intercept = -2.8124277, slope = 0.8464473, linewidth=8, color = "#323031", alpha = 0.7) + #Dactyloa
  
  scale_x_continuous(breaks = seq(16, 24, 2), limit = c(16, 24)) +
  scale_y_continuous(breaks = seq(6, 22, 4), limit = c(6, 22)) +
  theme_classic() +
  
  #guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("#323031", "#084c61"), name  = "Clade") +
  scale_fill_manual(values=c("#323031", "#084c61"), name  = "Clade") +
  
  theme(panel.background = element_rect(fill = NA, colour = NA), 
        plot.background = element_rect(fill = NA, colour = NA),
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
  
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(Mean~annual~temperature~(degree*C))) +
  ylab(expression(CT[min]~(degree*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 50, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 50, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=55)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=55))





################################
####                        ####
####  Let's save our plot   ####
####                        ####
################################

##############
## Let's save our plot
# Adjust the theme of each plot to remove the background



png("forPoster_Bio1_Dec02.png", height = 12, width = 26, units = "in", res = 300, bg = "transparent")

plot_grid(bio1_ctmin, bio1_ctmax,
          ncol = 2, nrow = 1)

dev.off()
