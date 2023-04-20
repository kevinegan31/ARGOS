rm(list = ls())
library(tidyr)
library(tidyverse)
library(scales)
library(latex2exp)
library(cowplot)
library(ggh4x)
library(ggpubr)
library(gridExtra)
library(patchwork)
library(reticulate)
library(RColorBrewer)
library(stringr)
library(gridExtra)
file_wd <- paste(
  "/Users/kevinegan/Documents/GitHub.nosync/ARGOS/",
  # "~//GitHub/ARGOS/",
  sep = ""
)
# file_wd <- "D:/GitHub/ARGOS/" # github path
file_wd2 <- paste(file_wd, "Data/Linear3D/stacked_bar_csv/", sep = "")
setwd(file_wd2)
# setwd('C:/Users/cfzh32/Documents/GitHub/ARGOS/Data/Linear2D/stacked_bar_csv')
## never changed variables ------------------------
snr_seq <- c(seq(1, 61, 3), Inf)
snr_seq_desired <- c(1, 5, 9, 13, 17, 21, 22)

colors0 <- c(
  "#de815d","#3f64d9","#93bd2e","#8c5bd5","#3cc755","#cd6de3","#74c34d",
  "#a53ba5","#41bf6c","#e453b2","#459731","#777ee5","#bebb3c","#694ca7",
  "#e1aa34","#4f8dde","#e17c2e","#52a5d5","#e95a36","#3abec8","#ba3124",
  "#60caae","#df3f79","#5cc287","#b63d84","#8da637","#ac7dd5","#35772f",
  "#d88ad2","#5b7920","#854f96","#95b86b","#da3b50","#3d9c7e","#ab3c54",
  "#52975e","#e183ab","#2b6e3f","#df6d74","#277257","#ab4d24","#5565a6",
  "#a28e2c","#a09cdf","#b47924","#9b5686","#89914c","#994e60","#4e5d20",
  "#e49486","#69773d","#9a5738","#c8b275","#6c6214","#dda55c","#916f38"
)

ggplot_data_snr <- function(algorithm, xdot_linear3d, ydot_linear3d, zdot_linear3d, threshold=40){
  # xdot_linear3d = xdot_linear3d_STLS;ydot_linear3d = ydot_linear3d_STLS;zdot_linear3d = zdot_linear3d_STLS;algorithm='SINDy with AIC'
  ## x_dot
  xdot_linear3d_reg_list <- lapply(seq_along(snr_seq), function(i){
    xdot_linear3d[((i-1)*100+1):((i-1)*100+1+99),]
  })
  x_dot_linear3d_terms_data = sapply(xdot_linear3d_reg_list, function(x) apply(x,2,function(c) sum(c!=0)))
  rownames(x_dot_linear3d_terms_data)[1] <- "1"
  plot_data_xdot <- cbind.data.frame(eq = 'xdot', names = rownames(x_dot_linear3d_terms_data),
      values = x_dot_linear3d_terms_data[, snr_seq_desired]
    )
  
  ## y_dot
  ydot_linear3d_reg_list <- lapply(seq_along(snr_seq), function(i){
    ydot_linear3d[((i-1)*100+1):((i-1)*100+1+99),]
  })
  y_dot_linear3d_terms_data = sapply(ydot_linear3d_reg_list, function(x) apply(x,2,function(c) sum(c!=0)))
  rownames(y_dot_linear3d_terms_data)[1] <- "1"
  plot_data_ydot <- cbind.data.frame(eq = 'ydot', names = rownames(y_dot_linear3d_terms_data),
      values = y_dot_linear3d_terms_data[, snr_seq_desired]
    )
  
  ## z_dot
  zdot_linear3d_reg_list <- lapply(seq_along(snr_seq), function(i){
    zdot_linear3d[((i-1)*100+1):((i-1)*100+1+99),]
  })
  z_dot_linear3d_terms_data = sapply(zdot_linear3d_reg_list, function(x) apply(x,2,function(c) sum(c!=0)))
  rownames(z_dot_linear3d_terms_data)[1] <- "1"
  plot_data_zdot <- cbind.data.frame(eq = 'zdot', names = rownames(z_dot_linear3d_terms_data),
      values = z_dot_linear3d_terms_data[, snr_seq_desired]
    )
  plot_data <- rbind(plot_data_xdot,plot_data_ydot,plot_data_zdot)
  
  ## plot_data 
  plot_data1 <- gather(plot_data, "snr", "value", -names, -eq)
  
  ## correct terms and incorrect terms frequency > threshold
  plot_data1 <- rbind(dplyr::filter(plot_data1, value>threshold|names=='x'|names=='y') %>% dplyr::filter(eq=='xdot'),
                      dplyr::filter(plot_data1, value>threshold|names=='x'|names=='y') %>% dplyr::filter(eq=='ydot'),
                      dplyr::filter(plot_data1, value>threshold|names=='z') %>% dplyr::filter(eq=='zdot'))
  ## incorrect terms, frequency <= threshold, and sum them
  others_values_xdot <- apply(subset(plot_data,plot_data[,1]=='xdot')[,-c(1,2)], 2, function(x){
    terms <- subset(plot_data,plot_data[,1]=='xdot')[,2]
    index <- which(x<=threshold & terms!='x'& terms!='y')
    sum(x[index])
  })
  others_values_ydot <- apply(subset(plot_data,plot_data[,1]=='ydot')[,-c(1,2)], 2, function(x){
    terms <- subset(plot_data,plot_data[,1]=='ydot')[,2]
    index <- which(x<=threshold & terms!='x'& terms!='y')
    sum(x[index])
  })
  others_values_zdot <- apply(subset(plot_data,plot_data[,1]=='zdot')[,-c(1,2)], 2, function(x){
    terms <- subset(plot_data,plot_data[,1]=='zdot')[,2]
    index <- which(x<=threshold & terms!='z')
    sum(x[index])
  })
  others_values <- cbind(xdot=others_values_xdot,ydot=others_values_ydot,zdot=others_values_zdot)
  others <- cbind.data.frame(snr=names(plot_data)[-c(1,2)],others_values) %>% 
    reshape2::melt('snr',variable.name = 'eq') %>% cbind(names='others')
  plot_data2 <- rbind(plot_data1,others) # the main plot data in ggplot
  rownames(plot_data2) <- 1:nrow(plot_data2)
  ## Add factors for variable order in Legend
  plot_data2$snr <-
    factor(plot_data2$snr,
           levels = unique(plot_data2$snr))
  plot_data2$names <-
    factor(plot_data2$names,
           levels =
             c(rownames(x_dot_linear3d_terms_data),'others'))
  
  ## need to replace x,y,z,... to x_1,x_2,x_3,...
  terms_show <- as.character(unique(sort(plot_data2$names)))
  # terms_show <- subset(terms_show, terms_show!='others')
  plot_data2$names <-
    factor(plot_data2$names,
           levels = terms_show)
             # c('others',terms_show[length(terms_show):1]))
  colors <- c(colors0[match(terms_show[-length(terms_show)],rownames(x_dot_linear3d_terms_data))], "#6c6c6c")
  
  ## terms frequency label 
  plot_data2$label <- plot_data2$value
  try(plot_data2[which(plot_data2$value<=threshold),]$label <- NA, T)
  ## replace x,y,z to x_1,x_2,x_3
  plot_data2$names2 <- str_replace_all(plot_data2$names,c('x'='x_1','y'='x_2','z'='x_3'))
  plot_data2$names2 <- 
    str_replace_all(plot_data2$names2,c('x_1x_1x_1x_1'='x_1^4','x_2x_2x_2x_2'='x_2^4','x_3x_3x_3x_3'='x_3^4',
                                        'x_1x_1x_1'='x_1^3','x_2x_2x_2'='x_2^3','x_3x_3x_3'='x_3^3',
                                        'x_1x_1'='x_1^2','x_2x_2'='x_2^2','x_3x_3'='x_3^2'))
  
  ## white break in the plot to show which equation 
  eq_breaks <- cbind(eq=rep(c('xdot','ydot','zdot'),each=length(snr_seq_desired)),
                     names='break',snr=rep(unique(plot_data1$snr),3),value=50,label=NA,names2='break')
  plot_data2 <- rbind.data.frame(plot_data2,eq_breaks)
  plot_data2$value <- as.numeric(plot_data2$value)
  plot_data2$snr <-
    factor(plot_data2$snr,
           levels = unique(plot_data2$snr))
  terms_legend_break <- sort(unique(plot_data2$names))
  terms_legend_break <- terms_legend_break[-match(c('others','break'),terms_legend_break)]
  terms_legend_lab <- c(unname(TeX(paste0('$',terms_legend_break %>%
                        str_replace_all(c('x'='x_1','y'='x_2','z'='x_3')),'$'))),'Others','')
  ## split each equations intop three bars
  x_axis <- rbind((1:length(colnames(plot_data)[-c(1,2)])-1)*3,
                  (1:length(colnames(plot_data)[-c(1,2)])-1)*3+1,
                  (1:length(colnames(plot_data)[-c(1,2)])-1)*3+2)
  snr_value <- colnames(plot_data)[-c(1,2)]
  eqs <- unique(plot_data$eq)
  plot_data2$x_axis <- NA
  for(i in 1:length(colnames(plot_data)[-c(1,2)])){
    for(j in 1:length(unique(plot_data$eq))){
      plot_data2$x_axis[which(plot_data2$eq==eqs[j] & plot_data2$snr==snr_value[i])] <- x_axis[j,i]
    }
  }
  plot_data2$x_axis <- as.factor(plot_data2$x_axis)
  plot_data2$correct <- NA
  correct_terms <- c(which(plot_data2$eq=='xdot' & plot_data2$names=='x'),
                     which(plot_data2$eq=='xdot' & plot_data2$names=='y'),
                     which(plot_data2$eq=='ydot' & plot_data2$names=='x'),
                     which(plot_data2$eq=='ydot' & plot_data2$names=='y'),
                     which(plot_data2$eq=='zdot' & plot_data2$names=='z'))
  plot_data2$correct[correct_terms] <- 1
  plot_data2$correct[-correct_terms] <- 0
  plot_data2$correct <- as.factor(plot_data2$correct)
  plot_data2$correct_color <- as.numeric(plot_data2$correct)
  plot_data2$correct_color[which(plot_data2$correct_color == 2)] <- 'grey80'
  plot_data2$correct_color[which(plot_data2$correct_color == 1)] <- 'white'
  
  ## plot_data2 with a colors column
  plot_data4 <- plot_data2
  plot_data4$color <- NA
  terms_legend_break2 <- c(as.character(terms_legend_break),'others','break')
  for(i in 1:length(terms_legend_break2)){
    ind <- which(plot_data4$names == terms_legend_break2[i])
    plot_data4$color[ind] <- c(colors,'white')[i]
  }
  plot_data4$label[which(!is.na(plot_data4$label))] <- "         "
  
  ## another label to show variables names
  plot_data4$label2 <- as.character(plot_data4$names2)
  plot_data4[which(plot_data4$label2=='others'),]$label2 <- 'Others'
  try(plot_data4[which(plot_data4$value<=threshold),]$label2 <- NA, T)
  latex_index <- which(plot_data4$label2!='Others' & !is.na(plot_data4$label2) & plot_data4$label2!='break')
  if(length(latex_index)!=0){plot_data4[latex_index,]$label2 <- TeX(paste0('$',plot_data4[latex_index,]$label2,'$'))}
  plot_data4$label2[which(plot_data4$eq=='xdot'&plot_data4$names=='break')] <- TeX('$\\dot{x}_1$')
  plot_data4$label2[which(plot_data4$eq=='ydot'&plot_data4$names=='break')] <- TeX('$\\dot{x}_2$')
  plot_data4$label2[which(plot_data4$eq=='zdot'&plot_data4$names=='break')] <- TeX('$\\dot{x}_3$')
  snr_levels <- levels(plot_data4$snr)
  names_levels <- levels(plot_data4$names)
  plot_data2$label2 <- plot_data4$label
  plot_data2$label2[which(plot_data2$names=='break')] <- "         "
  plot_data2$label2[which(plot_data2$names=='others'&!is.na(plot_data2$label2))] <- "            "
  
  x_labels <- snr_seq[snr_seq_desired]
  x_labels[length(x_labels)] <- TeX("$\\infty$")
  ## plot xdot data
  plot_xdot_data_index <- which(plot_data4$eq=='xdot' & plot_data4$names2 != 'breaks')
  plot_xdot_data <- plot_data4[plot_xdot_data_index,]
  plot_xdot_data$y_axis <- NA
  for(i in snr_levels){
    snr_index <- which(plot_xdot_data$snr==i)
    plot_xdot_data$y_axis[snr_index] <- cumsum(plot_xdot_data$value[snr_index])-plot_xdot_data$value[snr_index]/2
  }
  plot_xdot_data <- plot_xdot_data[which(plot_xdot_data$names!='break'),]
  plot_xdot_data$label2[which(is.na(plot_xdot_data$label))] <- '  '
  plot_xdot_data$correct_color[plot_xdot_data$correct==1] <- '#f8766d66'
  
  ## plot ydot data
  plot_ydot_data_index <- which(plot_data4$eq=='ydot' & plot_data4$names2 != 'breaks')
  plot_ydot_data <- plot_data4[plot_ydot_data_index,]
  plot_ydot_data$y_axis <- NA
  for(i in snr_levels){
    snr_index <- which(plot_ydot_data$snr==i)
    plot_ydot_data$y_axis[snr_index] <- cumsum(plot_ydot_data$value[snr_index])-plot_ydot_data$value[snr_index]/2
  }
  plot_ydot_data <- plot_ydot_data[which(plot_ydot_data$names!='break'),]
  plot_ydot_data$label2[which(is.na(plot_ydot_data$label))] <- '  '
  plot_ydot_data$correct_color[plot_ydot_data$correct==1] <- '#00ba3844'
  
  ## plot zdot data
  plot_zdot_data_index <- which(plot_data4$eq=='zdot' & plot_data4$names2 != 'breaks')
  plot_zdot_data <- plot_data4[plot_zdot_data_index,]
  plot_zdot_data$y_axis <- NA
  for(i in snr_levels){
    snr_index <- which(plot_zdot_data$snr==i)
    plot_zdot_data$y_axis[snr_index] <- cumsum(plot_zdot_data$value[snr_index])-plot_zdot_data$value[snr_index]/2
  }
  plot_zdot_data <- plot_zdot_data[which(plot_zdot_data$names!='break'),]
  plot_zdot_data$label2[which(is.na(plot_zdot_data$label))] <- '  '
  plot_zdot_data$correct_color[plot_zdot_data$correct==1] <- '#619CFF44'
  plot_data_xyz_dot <- rbind(plot_xdot_data, plot_ydot_data, plot_zdot_data)
  plot_data_xyz_dot$eq <- as.factor(plot_data_xyz_dot$eq)
  levels(plot_data_xyz_dot$eq) <- c(TeX('$\\dot{x}_1$'),TeX('$\\dot{x}_2$'),TeX('$\\dot{x}_3$'))
  annotate_data <- plot_data_xyz_dot %>% select('snr','y_axis','label2','eq')
  names(annotate_data)[2] <- 'value'
  
  original_ggplot <- ggplot(plot_data_xyz_dot,aes(x=snr,y=value))+
    geom_bar(aes(colour=eq), fill=plot_data_xyz_dot$correct_color, position="stack", stat="identity",lwd=1.5) +
    facet_grid(rows = vars(eq), labeller = label_parsed) +
    geom_text(data = annotate_data, label = annotate_data$label2, color=1, size=5) +
    labs(x = TeX("$SNR(dB)$"),
         y = "Frequency",
         title=algorithm,
         # title = TeX('$\\dot{x}_2$')
    ) +
    # scale_x_discrete(drop = FALSE, breaks=x_axis[3,],labels = x_labels) +
    scale_x_discrete(drop = FALSE, labels = x_labels) +
    theme(
      plot.title = element_text(size = 25),
      strip.text = element_text(size = 25),
      # axis.line.x = element_line(colour = "black"),
      axis.line = element_line(colour = "black"),
      axis.ticks.length = unit(.25, "cm"),
      # axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 16),
      # Changed for legend
      legend.title = element_text(face = "bold", size = 16),
      # legend.key = element_rect(fill = "lightblue", color = NA),
      axis.text = element_text(size = 14),
      # axis.text.x = element_text(size = 14),
      # axis.text.y = element_blank(),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(
        size = 20,
        angle = 90,
        vjust = 0.5
      ),
      legend.text.align = 0
    ) +
    scale_colour_manual(breaks=levels(plot_data_xyz_dot$eq),labels=unname(TeX(c('$\\dot{x}_1$', '$\\dot{x}_2$', '$\\dot{x}_3$'))),name='Equations',
                        values = hue_pal()(3))+
    # scale_alpha_manual(values = c(0.5,1))+
    guides(color = guide_legend(override.aes = list(fill = NA)),
           linetype = guide_legend(override.aes = list(fill = NA)),
           alpha='none')
  return(original_ggplot)
}

## lasso --------------
xdot_linear3d_lasso <- read.csv("SNR/linear3d_inc_snr_xdot_lasso_pred_models_new_sg.csv")[,-1]
ydot_linear3d_lasso <- read.csv("SNR/linear3d_inc_snr_ydot_lasso_pred_models_new_sg.csv")[,-1]
zdot_linear3d_lasso <- read.csv("SNR/linear3d_inc_snr_zdot_lasso_pred_models_new_sg.csv")[,-1]
lasso_plot_snr <- ggplot_data_snr('ARGOS-Lasso', xdot_linear3d_lasso, ydot_linear3d_lasso, zdot_linear3d_lasso)
lasso_plot2_snr <- lasso_plot_snr+theme(legend.position='none')
lasso_plot2_snr <- lasso_plot_snr+theme(legend.position='none')+
  annotate("rect", xmin = 3.5, xmax = 7.5, ymin = -10, ymax = 250,
           alpha = 0, color= "purple",lwd=1)
## alasso ------------------
xdot_linear3d_alasso <- read.csv("SNR/linear3d_inc_snr_xdot_alasso_pred_models_new_sg.csv")[,-1]
ydot_linear3d_alasso <- read.csv("SNR/linear3d_inc_snr_ydot_alasso_pred_models_new_sg.csv")[,-1]
zdot_linear3d_alasso <- read.csv("SNR/linear3d_inc_snr_zdot_alasso_pred_models_new_sg.csv")[,-1]
alasso_plot_snr <- ggplot_data_snr('ARGOS-Adaptive Lasso', xdot_linear3d_alasso, ydot_linear3d_alasso, zdot_linear3d_alasso)
alasso_plot2_snr <- alasso_plot_snr+theme(legend.position='none')
alasso_plot2_snr <- alasso_plot_snr+theme(legend.position='none')+
  annotate("rect", xmin = 4.5, xmax = 6.5, ymin = -10, ymax = 250,
           alpha = 0, color= "purple",lwd=1)
## STLS -------------------------
xdot_linear3d_STLS <- read.csv("SNR/linear3d_inc_snr_xdot_sindy_pred_models_new_sg.csv")[,-1]
ydot_linear3d_STLS <- read.csv("SNR/linear3d_inc_snr_ydot_sindy_pred_models_new_sg.csv")[,-1]
zdot_linear3d_STLS <- read.csv("SNR/linear3d_inc_snr_zdot_sindy_pred_models_new_sg.csv")[,-1]
STLS_plot_snr <- ggplot_data_snr('SINDy with AIC', xdot_linear3d_STLS, ydot_linear3d_STLS, zdot_linear3d_STLS,100)
STLS_plot2_snr <- STLS_plot_snr+theme(legend.position='none')
STLS_plot2_snr <- STLS_plot_snr+theme(legend.position='none')+
  annotate("rect", xmin = 6.5, xmax = 7.5, ymin = -10, ymax = 250,
           alpha = 0, color= "purple",lwd=1)

legend <- get_legend(STLS_plot2_snr+theme(legend.position='bottom'))
ggplot_legend <- cowplot::plot_grid(legend)

## compose -----------------
library(gridExtra)
layout_matrix <- matrix(c(0,rep(1,20),rep(2,20),rep(3,20)), ncol=1)
stacked_linear3d_snr <-
  grid.arrange(
    ggplot() + labs(tag = expression(bold('b'))) + theme(
      text = element_text(size = 35),
      plot.background = element_blank(),
      panel.background = element_blank()
    ) ,
    lasso_plot2_snr,
    alasso_plot2_snr,
    STLS_plot2_snr,
    # ggplot_legend,
    nrow = 4,
    ncol = 1,
    # layout_matrix = layout_matrix
    heights = c(1,10,10,10)
  )
file_wd <- paste(
  "/Users/kevinegan/Documents/GitHub.nosync/PrivateAutomaticSparseRegression/",
  # "~//GitHub/ARGOS/",
  sep = ""
)
setwd(file_wd)
ggsave(stacked_linear3d_snr, filename = './Figures/stacked_linear3d_snr.pdf', width = 10, height = 15, dpi = 300)
ggsave(stacked_linear3d_snr, filename = './Figures/stacked_linear3d_snr.png', width = 10, height = 15, dpi = 300)
