library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(cowplot)
library(ggrepel)

setwd("C:/Users/jhanc/Box/Investigacion/Investigaciones/Side projects/Anolis/CT/Datos_Ult/NewBoxPlot")
data<-read.csv("BoxPlotCTmax2.csv", header=TRUE)
attach(data)
head(data)

##########################################################################
######                                                              ######
######         Box plot for CTmax, CTmin                            ######
######         uses "New_PGLS_CTmax_Sept24.csv"                     ######
######         uses "New_PGLS_CTmin_Sept24.csv"                     ######
######                                                              ######
##########################################################################


###############
####       ####
#### CTmax ####
####       ####
###############

png("BoxPlotCTmax_Hor_Dec02.png", width = 20, height = 12, units = "in", res = 300, type = "cairo")

ggplot(data, aes(x = Sp, y = Valor, fill = Clade)) +
  geom_boxplot(aes(color = Clade), size = 0.75, width = 0.8, alpha = 0.7) +
  geom_jitter(color = "black", size = 8, alpha = 0.3, position = position_jitter(0.1)) +
  
  scale_color_manual(values = c("black", "black"),
                     name = "Clade",
                     breaks = c("Draconura", "Dactyloa"),
                     labels = c("Draconura", "Dactyloa")) +

  scale_fill_manual(values = c("#084c61", "#323031"),
                    name = "Clade",
                    breaks = c("Draconura", "Dactyloa"),
                    labels = c("Draconura", "Dactyloa")) +
  
  scale_x_discrete(limits = c("antonii", "maculiventris", "notopholis", "chloris", "ventrimaculatus", 
                              "danieli", "chocuorum", "maculigula", "princeps", "calimae", "heterodermus"),
                   labels = c("anto", "macv", "noto", "chlr", "vent", "dani", "purp", "macg", "prin", 
                              "cali", "hetr")) +

  scale_y_continuous(breaks = seq(21, 39, 3), limits = c(21, 39)) +
  
  labs( = "", y = expression(CT[max] ~ (degree*C))) +
  
  theme_classic() +
  theme(legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"), 
        legend.text = element_text(family = "serif", size = 40),
        legend.position = c(0.92, 0.90),
        legend.title = element_blank(),
        axis.text.x = element_text(family = "serif", size = 40, lineheight = 0.9, vjust = 1, face = "italic"),
        axis.text.y = element_text(family = "serif", size = 40, lineheight = 0.9, vjust = 1),
        axis.title.y = element_text(family = "serif", vjust = 1.2, size = 50),
        axis.title.x = element_text(family = "serif", vjust = -0.5, size = 50))


detach(data)
dev.off()





###############
####       ####
#### CTmin ####
####       ####
###############

data2<-read.csv("BoxPlotCTmin.csv", header=TRUE)
attach(data2)
head(data2)

png("BoxPlotCTmin_Hor_Dec02.png", width = 20, height = 12, units = "in", res = 300, type = "cairo")

ggplot(data2, aes(x = Sp, y = Valor, fill = Clade)) +
  geom_boxplot(aes(color = Clade), size = 0.75, width = 0.8, alpha = 0.7) +
  geom_jitter(alpha = 0.3, color = "black", size = 8, position = position_jitter(0.1)) +
  
  scale_color_manual(values = c("black", "black"),
                     name = "Clade",
                     breaks = c("Draconura", "Dactyloa"),
                     labels = c("Draconura", "Dactyloa")) +

  scale_fill_manual(values = c("#084c61", "#323031"),
                    name = "Clade",
                    breaks = c("Draconura", "Dactyloa"),
                    labels = c("Draconura", "Dactyloa")) +
  
  scale_x_discrete(limits = c("antonii", "maculiventris", "granuliceps", "notopholis", 
                              "chloris", "ventrimaculatus", "danieli", "chocuorum", "maculigula",
                              "princeps", "calimae", "heterodermus"),
                    labels = c("anto", "macv", "gran", "noto", "chlr", "vent", "dani", 
                               "purp", "macg", "prin", "cali", "hetr")) +

  scale_y_continuous(breaks = seq(3, 21, 3), limits = c(3, 21)) +
  
  labs(x = "", y = expression(CT[min] ~ (degree*C))) +
  
  theme_classic() +
  theme(legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"), 
        legend.text = element_text(family = "serif", size = 40),
        legend.position = c(0.92, 0.90),
        legend.title = element_blank(),
        axis.text.x = element_text(family = "serif", size = 40, lineheight = 0.9, vjust = 1, face = "italic"),
        axis.text.y = element_text(family = "serif", size = 40, lineheight = 0.9, vjust = 1),
        axis.title.y = element_text(family = "serif", vjust = 1.2, size = 50),
        axis.title.x = element_text(family = "serif", vjust = -0.5, size = 50))

deatach(data2)
dev.off()
