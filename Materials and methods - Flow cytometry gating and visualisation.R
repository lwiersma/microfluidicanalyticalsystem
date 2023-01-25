library(readr)
library(dplyr)
library(stringr)
library(flowCore)

setwd("~/Documents/Academia/MSc Industrial Engineering and Management/Design Project/Results/20230105")
filename <- 'C5.fcs'
flowframe <- read.FCS(filename)

threshold <- 110000

#FL6-H is B525-H (green fluorescence intensity)
#SSC_1-H is Vilet SSC-H (side scatter, cell complexity)

x_axis <- data.frame(flowframe$"FL6-H"@exprs)
y_axis <- data.frame(flowframe$"SSC_1-H"@exprs)

rialtje <- flowframe$"SSC_1-H"@exprs


dataset <- cbind(x_axis, 
                 y_axis)
colnames(dataset) <- c('x_axis', 
                       'y_axis')

secs <- as.numeric(data.frame(difftime(strptime(flowframe@description$`$ETIM`, 
                                               '%H:%M:%S'), 
                                      strptime(flowframe@description$`$BTIM`, 
                                               '%H:%M:%S'), 
                                      units = 'secs')))
concentration <- format(signif(round((N_bacteria * 1000) / (30 * secs), 
                                     digits = 0), 
                               digits = 3), 
                        big.mark = ",")

N_bacteria <- sum(x_axis > threshold, 
                  na.rm = TRUE)

library(ggplot2)
library(scales)
library(factoextra)
library(ggsci)

visual <- ggplot(dataset, aes(x_axis, 
                              y_axis)) +
  geom_point(size=0.250,
             alpha=0.750,
             colour = '#0070B6') +
  theme(legend.position = c(.97,
                            .025),
        legend.direction = 'vertical',
        legend.justification = c("right",
                                 "bottom"),
        legend.box.just = "bottom",
        legend.margin = margin(6, 
                               6, 
                               6, 
                               6)) +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon",
                  colour = 'white',
                  size=0.250,
                  show.legend = FALSE,
                  alpha=0.100,
                  bins=8) +
  scale_x_log10(breaks = c(100, 
                           1000, 
                           10000, 
                           100000, 
                           1000000, 
                           10000000),
                labels = c(expression("10"^"2"), 
                           expression("10"^"3"), 
                           expression("10"^"4"),
                           expression("10"^"5"),
                           expression("10"^"6"),
                           expression("10"^"7")),
                limits = c(100, 
                           10000000)) +
  scale_y_log10(breaks = c(100, 
                           1000, 
                           10000, 
                           100000, 
                           1000000, 
                           10000000),
                labels = c(expression("10"^"2"), 
                           expression("10"^"3"), 
                           expression("10"^"4"),
                           expression("10"^"5"),
                           expression("10"^"6"),
                           expression("10"^"7")),
                limits = c(100, 
                           10000000)) +
  labs(x="Green fluorescence intensity",
       y="Side scatter") +
  annotate("rect", 
           xmin = threshold, 
           xmax = 10000000, 
           ymin = 100, 
           ymax = 10000000,
           alpha = 0.2) +
  theme(axis.title.x = element_text(size = 19), 
        axis.title.y = element_text(size = 19), 
        axis.text.x = element_text(size = 19), 
        axis.text.y = element_text(size = 19)) +
  annotate("text",
           size = 6,
           x=1000000, 
           y=250, 
           label= bquote(.(concentration) ~ "cells · mL"**{-1}))


visual

ggsave(file = paste("~/Desktop/",
                    filename,
                    ".png", 
                    sep=""), 
       dpi=300, 
       width=10, 
       height=10)
