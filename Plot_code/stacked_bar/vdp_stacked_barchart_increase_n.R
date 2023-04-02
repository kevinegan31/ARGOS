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
file_wd <- paste(
  "/Users/kevinegan/Documents/GitHub.nosync/ARGOS/",
  # "~//GitHub/ARGOS/",
  sep = ""
)
# file_wd <- "D:/GitHub/ARGOS/" # github path
file_wd2 <- paste(file_wd, "Data/Vdp/stacked_bar_csv/", sep = "")
setwd(file_wd2)
# setwd('C:/Users/cfzh32/Documents/GitHub/ARGOS/Data/Vdp/stacked_bar_csv')
## never changed variables ------------------------
n_init <- 2
n_final <- 5
n_seq <- round(seq(n_init, n_final, length = (n_final - n_init)*10 + 1), 1)
n_seq_total <- rep(n_seq, each = 100)
n_seq_desired <- c(1, 6, 11, 16, 21, 26, 31)

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

ggplot_data_n <- function(algorithm, xdot_vdp, ydot_vdp, threshold=40){
  # xdot_vdp = xdot_vdp_lasso_reg_df;ydot_vdp = ydot_vdp_lasso_reg_df;algorithm='lasso'
  ## x_dot
  xdot_vdp_reg_list <- lapply(seq_along(n_seq), function(i){
    xdot_vdp[((i-1)*100+1):((i-1)*100+1+99),]
  })
  x_dot_vdp_terms_data = sapply(xdot_vdp_reg_list, function(x) apply(x,2,function(c) sum(c!=0)))
  rownames(x_dot_vdp_terms_data)[1] <- "1"
  plot_data_xdot <- cbind.data.frame(eq = 'xdot',names = rownames(x_dot_vdp_terms_data),
      values = x_dot_vdp_terms_data[, n_seq_desired])
  
  ## y_dot
  ydot_vdp_reg_list <- lapply(seq_along(n_seq), function(i){
    ydot_vdp[((i-1)*100+1):((i-1)*100+1+99),]
  })
  y_dot_vdp_terms_data = sapply(ydot_vdp_reg_list, function(x) apply(x,2,function(c) sum(c!=0)))
  rownames(y_dot_vdp_terms_data)[1] <- "1"
  plot_data_ydot <- cbind.data.frame(eq = 'ydot',names = rownames(y_dot_vdp_terms_data),
      values = y_dot_vdp_terms_data[, n_seq_desired])
  plot_data <- rbind(plot_data_xdot,plot_data_ydot)
  
  ## plot_data 
  plot_data1 <- gather(plot_data,"n","value", -names, -eq)
  
  ## correct terms and incorrect terms frequency > threshold
  plot_data1 <- rbind(dplyr::filter(plot_data1, value>threshold|names=='y') %>% dplyr::filter(eq=='xdot'),
                      dplyr::filter(plot_data1, value>threshold|names=='x'|names=='y'|names=='xxy') %>% dplyr::filter(eq=='ydot'))
  ## incorrect terms, frequency <= threshold, and sum them
  others_values_xdot <- apply(as.data.frame(subset(plot_data,plot_data[,1]=='xdot')[,-c(1,2)]), 2, function(x){
    terms <- subset(plot_data,plot_data[,1]=='xdot')[,2]
    index <- which(x<=threshold & terms!='y')
    sum(x[index])
  })
  others_values_ydot <- apply(as.data.frame(subset(plot_data,plot_data[,1]=='ydot')[,-c(1,2)]), 2, function(x){
    terms <- subset(plot_data,plot_data[,1]=='ydot')[,2]
    index <- which(x<=threshold & terms!='x'& terms!='y'&terms!='xxy')
    sum(x[index])
  })
  others_values <- cbind(xdot=others_values_xdot,ydot=others_values_ydot)
  others <- cbind.data.frame(n=names(plot_data)[-c(1,2)],others_values) %>% 
    reshape2::melt('n',variable.name = 'eq') %>% cbind(names='others')
  plot_data2 <- rbind(plot_data1,others) # the main plot data in ggplot
  rownames(plot_data2) <- 1:nrow(plot_data2)
  ## Add factors for variable order in Legend
  plot_data2$n <-
    factor(plot_data2$n,
           levels = unique(plot_data2$n))
  
  plot_data2$names <-
    factor(plot_data2$names,
           levels =
             c(rownames(x_dot_vdp_terms_data),'others'))
  ## need to replace x,y,z,... to x_1,x_2,x_3,...
  terms_show <- as.character(unique(sort(plot_data2$names)))
  # terms_show <- subset(terms_show, terms_show!='others')
  plot_data2$names <-
    factor(plot_data2$names,
           levels = terms_show)
  colors <- c(colors0[match(terms_show[-length(terms_show)],rownames(x_dot_vdp_terms_data))], "#6c6c6c")
  
  ## terms frequency label 
  plot_data2$label <- plot_data2$value
  try(plot_data2[which(plot_data2$value<=threshold),]$label <- NA, T)
  ## replace x,y,z to x_1,x_2,x_3
  plot_data2$names2 <- str_replace_all(plot_data2$names,c('x'='x_1','y'='x_2'))
  plot_data2$names2 <- 
    str_replace_all(plot_data2$names2,c('x_1x_1x_1x_1'='x_1^4','x_2x_2x_2x_2'='x_2^4',
                                        'x_1x_1x_1'='x_1^3','x_2x_2x_2'='x_2^3',
                                        'x_1x_1'='x_1^2','x_2x_2'='x_2^2'))
  
  ## white break in the plot to show which equation 
  eq_breaks <- cbind(eq=rep(c('xdot','ydot'),each=length(n_seq_desired)),
                     names='break',n=rep(unique(plot_data1$n),2),value=50,label=NA,names2='break')
  plot_data2 <- rbind.data.frame(plot_data2,eq_breaks)
  plot_data2$value <- as.numeric(plot_data2$value)
  plot_data2$n <-
    factor(plot_data2$n,
           levels = unique(plot_data2$n))
  
  terms_legend_break <- sort(unique(plot_data2$names))
  terms_legend_break <- terms_legend_break[-match(c('others','break'),terms_legend_break)]
  terms_legend_lab <- c(unname(TeX(paste0('$',terms_legend_break %>%
                                            str_replace_all(c('x'='x_1','y'='x_2')),'$'))),'Others','')
  
  ## split each equations intop three bars
  x_axis <- rbind((1:length(colnames(plot_data)[-c(1,2)])-1)*3,
                  (1:length(colnames(plot_data)[-c(1,2)])-1)*3+1)
  
  n_value <- colnames(plot_data)[-c(1,2)]
  eqs <- unique(plot_data$eq)
  plot_data2$x_axis <- NA
  for(i in 1:length(colnames(plot_data)[-c(1,2)])){
    for(j in 1:length(unique(plot_data$eq))){
      plot_data2$x_axis[which(plot_data2$eq==eqs[j] & plot_data2$n==n_value[i])] <- x_axis[j,i]
    }
  }
  plot_data2$x_axis <- as.factor(plot_data2$x_axis)
  plot_data2$correct <- NA
  correct_terms <- c(which(plot_data2$eq=='xdot' & plot_data2$names=='y'),
                     which(plot_data2$eq=='ydot' & plot_data2$names=='x'),
                     which(plot_data2$eq=='ydot' & plot_data2$names=='y'),
                     which(plot_data2$eq=='ydot' & plot_data2$names=='xxy'))
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
  n_levels <- levels(plot_data4$n)
  names_levels <- levels(plot_data4$names)
  
  # plot_data2$label2 <- plot_data4$label
  plot_data2$label2[which(plot_data2$names=='break')] <- "         "
  plot_data2$label2[which(plot_data2$names=='others'&!is.na(plot_data2$label2))] <- "            "
  x_labels <- unname(TeX(paste0('$','10^{',n_seq[n_seq_desired],'}','$')))
  
  ## plot xdot data
  plot_xdot_data_index <- which(plot_data4$eq=='xdot' & plot_data4$names2 != 'breaks')
  plot_xdot_data <- plot_data4[plot_xdot_data_index,]
  plot_xdot_data$y_axis <- NA
  for(i in n_levels){
    n_index <- which(plot_xdot_data$n==i)
    plot_xdot_data$y_axis[n_index] <- cumsum(plot_xdot_data$value[n_index])-plot_xdot_data$value[n_index]/2
  }
  plot_xdot_data <- plot_xdot_data[which(plot_xdot_data$names!='break'),]
  plot_xdot_data$label2[which(is.na(plot_xdot_data$label))] <- '  '
  plot_xdot_data$correct_color[plot_xdot_data$correct==1] <- '#f8766d66'
  
  ## plot ydot data
  plot_ydot_data_index <- which(plot_data4$eq=='ydot' & plot_data4$names2 != 'breaks')
  plot_ydot_data <- plot_data4[plot_ydot_data_index,]
  plot_ydot_data$y_axis <- NA
  for(i in n_levels){
    n_index <- which(plot_ydot_data$n==i)
    plot_ydot_data$y_axis[n_index] <- cumsum(plot_ydot_data$value[n_index])-plot_ydot_data$value[n_index]/2
  }
  plot_ydot_data <- plot_ydot_data[which(plot_ydot_data$names!='break'),]
  plot_ydot_data$label2[which(is.na(plot_ydot_data$label))] <- '  '
  plot_ydot_data$correct_color[plot_ydot_data$correct==1] <- '#00ba3844'
  plot_data_xy_dot <- rbind(plot_xdot_data, plot_ydot_data)
  plot_data_xy_dot$eq <- as.factor(plot_data_xy_dot$eq)
  levels(plot_data_xy_dot$eq) <- c(TeX('$\\dot{x}_1$'),TeX('$\\dot{x}_2$'))
  annotate_data <- plot_data_xy_dot %>% select('n','y_axis','label2','eq')
  names(annotate_data)[2] <- 'value'
  
  original_ggplot <- ggplot(plot_data_xy_dot,aes(x=n,y=value))+
    geom_bar(aes(colour=eq), fill=plot_data_xy_dot$correct_color, position="stack", stat="identity",lwd=1.5) +
    facet_grid(rows = vars(eq), labeller = label_parsed) +
    geom_text(data = annotate_data, label = annotate_data$label2, color=1, size=5) +
    labs(x = expression(italic("n")),
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
    scale_colour_manual(breaks=levels(plot_data_xy_dot$eq),labels=unname(TeX(c('$\\dot{x}_1$', '$\\dot{x}_2$'))),name='Equations',
                        values = hue_pal()(3))+
    # scale_alpha_manual(values = c(0.5,1))+
    guides(color = guide_legend(override.aes = list(fill = NA)),
           linetype = guide_legend(override.aes = list(fill = NA)),
           alpha='none')
  return(original_ggplot)
}

## lasso --------------
xdot_vdp_lasso_reg_df <- read.csv("N/vdp_inc_n_xdot_lasso_pred_models_new_sg.csv")[-1]
ydot_vdp_lasso_reg_df <- read.csv("N/vdp_inc_n_ydot_lasso_pred_models_new_sg.csv")[-1]
lasso_plot_n <- ggplot_data_n('ARGOS-Lasso', xdot_vdp_lasso_reg_df, ydot_vdp_lasso_reg_df)
lasso_plot2_n <- lasso_plot_n+theme(legend.position='none')
lasso_plot2_n <- lasso_plot_n+theme(legend.position='none')+
  annotate("rect", xmin = 2.5, xmax = 7.5, ymin = -10, ymax = 320,
           alpha = 0, color= "purple",lwd=1)
## alasso ------------------
xdot_vdp_alasso_reg_df <- read.csv("N/vdp_inc_n_xdot_alasso_pred_models_new_sg.csv")[-1]
ydot_vdp_alasso_reg_df <- read.csv("N/vdp_inc_n_ydot_alasso_pred_models_new_sg.csv")[-1]
alasso_plot_n <- ggplot_data_n('ARGOS-Adaptive Lasso', xdot_vdp_alasso_reg_df, ydot_vdp_alasso_reg_df)
alasso_plot2_n <- alasso_plot_n+theme(legend.position='none')
alasso_plot2_n <- alasso_plot_n+theme(legend.position='none')+
  annotate("rect", xmin = 2.5, xmax = 7.5, ymin = -10, ymax = 320,
           alpha = 0, color= "purple",lwd=1)
## STLS -------------------------
xdot_vdp_sindy_reg_df <- read.csv("N/vdp_inc_n_xdot_sindy_pred_models_new_sg.csv")[-1]
ydot_vdp_sindy_reg_df <- read.csv("N/vdp_inc_n_ydot_sindy_pred_models_new_sg.csv")[-1]
n_seq_desired = c(1, 6)
STLS_plot_n <- ggplot_data_n('SINDy with AIC', xdot_vdp_sindy_reg_df, ydot_vdp_sindy_reg_df,95)
STLS_plot2_n <- STLS_plot_n+theme(legend.position='none',  
                                  strip.background = element_blank(),
                                  strip.text = element_blank())

n_seq_desired <- c(11, 16, 21, 26, 31)
STLS_plot_n <- ggplot_data_n('   ', xdot_vdp_sindy_reg_df, ydot_vdp_sindy_reg_df,80)
STLS_plot2_n2 <- STLS_plot_n+theme(legend.position='none',axis.title.y = element_blank())
STLS_plot2_n2 <- STLS_plot_n+theme(legend.position='none',axis.title.y = element_blank())+
  annotate("rect", xmin = 0.5, xmax = 5.5, ymin = -10, ymax = 350,
           alpha = 0, color= "purple",lwd=1)
layout_matrix <- matrix(c(rep(1,1),rep(2,2)),nrow=1)
n_STLS <- grid.arrange(STLS_plot2_n, STLS_plot2_n2,layout_matrix = layout_matrix,nrow=1,ncol=2)




legend <- get_legend(STLS_plot2_n+theme(legend.position='bottom'))
ggplot_legend <- cowplot::plot_grid(legend)

## compose -----------------
library(gridExtra)
layout_matrix <- matrix(c(0,rep(1,20),rep(2,20),rep(3,20)), ncol=1)
stacked_vdp_n <-
  grid.arrange(
    ggplot() + labs(tag = expression(bold('a'))) + theme(
      text = element_text(size = 35),
      plot.background = element_blank(),
      panel.background = element_blank()
    ) ,
    lasso_plot2_n,
    alasso_plot2_n,
    n_STLS,
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
ggsave(stacked_vdp_n, filename = './Figures/stacked_vdp_n.pdf', width = 10, height = 15, dpi = 300)
ggsave(stacked_vdp_n, filename = './Figures/stacked_vdp_n.png', width = 10, height = 15, dpi = 300)
