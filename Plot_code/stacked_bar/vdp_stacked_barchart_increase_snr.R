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
# setwd('D:/Users/Administrator/OneDrive - Durham University/Proc_Royal_Soc_A/Data/Lorenz')
# setwd('/Users/kevinegan/OneDrive - Durham University/Proc_Royal_Soc_A/Data/Lorenz')
# xdot_vdp_lasso <- read.csv("Bernoulli_Prediction_Models/xdot_vdp_lasso_increasing_n_prediction_models_ordered.csv")[,-1]
file_wd <- paste(
  # "/Users/kevinegan/Documents/",
  "~/",
  sep = ""
)
# file_wd <- "D:/"
file_wd2 <- paste(file_wd, "GitHub/PrivateAutomaticSparseRegression/Data/vdp/Bernoulli_Prediction_Models/", sep = "")
setwd(file_wd2)
## never changed variables ------------------------
snr_seq <- c(seq(1, 61, 3), Inf)
snr_seq_desired <- c(1, 5, 9, 13, 17, 21, 22)

# colors
colors0 <- c(
  "#a84b20", "#5e79e8", "#6eb729", "#7051c7", "#4fc656", "#bf6fe6", "#a6bb31",
  "#ab3ca8", "#3d912e", "#c93c90", "#38c481", "#df3f79", "#86be59", "#e672c7",
  "#648a2e", "#6b56ac", "#e4a231", "#5b92e0", "#e38421", "#53a7d7", "#d04229",
  "#3abec8", "#e23c54", "#60caae", "#b33246", "#79bf80", "#ba87da", "#c4ad39",
  "#4e68a5", "#e47333", "#9f9add", "#777c1b", "#8e5393", "#949b43", "#b64f7d",
  "#47935a", "#dd6666", "#3d9c7e", "#e7835f", "#1a6447", "#d18abc", "#2f7038",
  "#e78a96", "#2f7b63", "#bc7f2a", "#8d4768", "#698e4e", "#a65356", "#4d661f",
  "#d7a468", "#62612c", "#afb873", "#9d613a", "#655d14", "#89834d", "#917127"
)

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

# colors0 <- c("#549c65", "#9660ca", "#6ab646", "#ce4b9d", "#a8993f",
#              "#6f81cb", "#c67640", "#4cbcce", "#ce474a", "#be6a8d", "#6c6c6c")
# colors0 <- c("#8661d0", "#80b73c", "#c858ac", "#45b470", "#c75c75", "#47b4c5",
#              "#c95938", "#7c7fc5", "#c59542", "#8eb172", "#5e7430")

ggplot_data_snr <- function(algorithm, xdot_vdp, ydot_vdp, threshold = 40){
  # xdot_vdp = xdot_vdp_lasso;ydot_vdp = ydot_vdp_lasso;algorithm='Lasso'
  ## x_dot
  xdot_vdp_reg_list <- lapply(seq_along(snr_seq), function(i){
    xdot_vdp[((i-1)*100+1):((i-1)*100+1+99),]
  })
  # xdot_vdp_reg_list <- split(xdot_vdp, seq_along(snr_seq))
  # apply(xdot_vdp_reg_list[[1]], 2, function(c) sum(c!=0))
  x_dot_vdp_terms_data = sapply(xdot_vdp_reg_list, function(x) apply(x,2,function(c) sum(c!=0)))
  rownames(x_dot_vdp_terms_data)[1] <- "1"
  # snr_seq[c(seq(1,30,5)[-6],22)]
  # index <- c(seq(1,30,5)[-6],22)
  plot_data_xdot <-
    cbind.data.frame(
      eq = 'xdot',
      names = rownames(x_dot_vdp_terms_data),
      values = x_dot_vdp_terms_data[, snr_seq_desired]
    )
  
  ## y_dot
  ydot_vdp_reg_list <- lapply(seq_along(snr_seq), function(i){
    ydot_vdp[((i-1)*100+1):((i-1)*100+1+99),]
  })
  # ydot_vdp_reg_list <- split(ydot_vdp, seq_along(snr_seq))
  # apply(ydot_vdp_reg_list[[1]], 2, function(c) sum(c!=0))
  y_dot_vdp_terms_data = sapply(ydot_vdp_reg_list, function(x) apply(x,2,function(c) sum(c!=0)))
  rownames(y_dot_vdp_terms_data)[1] <- "1"
  # snr_seq[c(seq(1,30,5)[-6],22)]
  # index <- c(seq(1,30,5)[-6],22)
  plot_data_ydot <-
    cbind.data.frame(
      eq = 'ydot',
      names = rownames(y_dot_vdp_terms_data),
      values = y_dot_vdp_terms_data[, snr_seq_desired]
    )
  
  plot_data <- rbind(plot_data_xdot,plot_data_ydot)
  
  ## plot_data 
  # library(reshape2)
  # plot_data2 = melt(plot_data,'names')
  plot_data1 <-
    gather(
      plot_data,
      "snr",
      "value",
      -names, -eq
    )
  # plot_data1 <- subset(plot_data1, plot_data1$value>20 | plot_data1$names=='x' | plot_data1$names=='y')
  # plot_data1 <- plot_data1[-which(plot_data1$value<20 & plot_data1$names!='x'& plot_data1$names!='y'),]
  # plot_data1$names[which(plot_data1$value<20 & plot_data1$names!='x'& plot_data1$names!='y')] <- 'others'
  
  ## correct terms and incorrect terms frequency > 20
  plot_data1 <- rbind(dplyr::filter(plot_data1, value>threshold|names=='y') %>% dplyr::filter(eq=='xdot'),
                      dplyr::filter(plot_data1, value>threshold|names=='x'|names=='y'|names=='xxy') %>% dplyr::filter(eq=='ydot'))
  ## incorrect terms, frequency <= threshold, and sum them
  others_values_xdot <- apply(subset(plot_data,plot_data[,1]=='xdot')[,-c(1,2)], 2, function(x){
    terms <- subset(plot_data,plot_data[,1]=='xdot')[,2]
    index <- which(x<=threshold & terms!='y')
    sum(x[index])
  })
  
  others_values_ydot <- apply(subset(plot_data,plot_data[,1]=='ydot')[,-c(1,2)], 2, function(x){
    terms <- subset(plot_data,plot_data[,1]=='ydot')[,2]
    index <- which(x<=threshold & terms!='x'& terms!='y'&terms!='xxy')
    sum(x[index])
  })
  
  others_values <- cbind(xdot=others_values_xdot,ydot=others_values_ydot)
  
  others <- cbind.data.frame(snr=names(plot_data)[-c(1,2)],others_values) %>% 
    reshape2::melt('snr',variable.name = 'eq') %>% cbind(names='others')
  
  # others <- cbind.data.frame(names=rep('others',nrow(others_values)),snr=names(plot_data)[-c(1,2)],value=others_values) %>% 
  #   gather("snr","value",-names,-snr)
  
  plot_data2 <- rbind(plot_data1,others) # the main plot data in ggplot
  # plot_data2 <- filter(plot_data2, snr!='values.22') # remove snr=Inf
  rownames(plot_data2) <- 1:nrow(plot_data2)
  
  # plot_data2 <- plot_data1
  ## Add factors for variable order in Legend
  plot_data2$snr <-
    factor(plot_data2$snr,
           levels = unique(plot_data2$snr))
  
  plot_data2$names <-
    factor(plot_data2$names,
           levels =
             c(rownames(x_dot_vdp_terms_data),'others'))
  
  # image(1 : length(colors0), 1, as.matrix(1 : length(colors0)), col = colors0 ,xlab =  "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  # color_data <- cbind.data.frame(color=c(colors0,"#917127"), terms=c(rownames(x_dot_vdp_terms_data),'others'))
  
  ## need to replace x,y,z,... to x_1,x_2,x_3,...
  # terms_show <- c('x','y','z',as.character(unique(sort(plot_data2$names))[-match(c('x','y','z','others'),unique(sort(plot_data2$names)))]))
  terms_show <- as.character(unique(sort(plot_data2$names)))
  # terms_show <- subset(terms_show, terms_show!='others')
  plot_data2$names <-
    factor(plot_data2$names,
           levels = terms_show)
  # c('others',terms_show[length(terms_show):1]))
  colors <- c(colors0[match(terms_show[-length(terms_show)],rownames(x_dot_vdp_terms_data))], "#6c6c6c")
  # colors <- colors0[c(match(terms_show[-length(terms_show)],rownames(x_dot_vdp_terms_data)),11)]
  # colors <- c(colors0[match(terms_show,rownames(x_dot_vdp_terms_data))],"#917127")
  # colors <- colors[length(colors):1]
  # colors <- c("#B3B3B3", brewer.pal(length(terms_show), "Set2"))
  # colors <- c(brewer.pal(length(terms_show), "Set2")[length(terms_show):1],"#B3B3B3")
  
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
  eq_breaks <- cbind(eq=rep(c('xdot','ydot'),each=length(snr_seq_desired)),
                     names='break',snr=rep(unique(plot_data1$snr),2),value=50,label=NA,names2='break')
  
  plot_data2 <- rbind.data.frame(plot_data2,eq_breaks)
  plot_data2$value <- as.numeric(plot_data2$value)
  plot_data2$snr <-
    factor(plot_data2$snr,
           levels = unique(plot_data2$snr))
  
  terms_legend_break <- sort(unique(plot_data2$names))
  terms_legend_break <- terms_legend_break[-match(c('others','break'),terms_legend_break)]
  terms_legend_lab <- c(unname(TeX(paste0('$',terms_legend_break %>%
                                            str_replace_all(c('x'='x_1','y'='x_2','z'='x_3')),'$'))),'Others','')
  # terms_legend_break <- terms_legend_break[-match(c('others'),terms_legend_break)]
  # terms_legend_lab <- c(unname(TeX(paste0('$',terms_legend_break %>%
  #                                           str_replace_all(c('x'='x_1','y'='x_2','z'='x_3')),'$'))),'Others')
  
  ## split each equations intop three bars
  x_axis <- rbind((1:length(colnames(plot_data)[-c(1,2)])-1)*3,
                  (1:length(colnames(plot_data)[-c(1,2)])-1)*3+1)
  
  snr_value <- colnames(plot_data)[-c(1,2)]
  eqs <- unique(plot_data$eq)
  plot_data2$x_axis <- NA
  for(i in 1:length(colnames(plot_data)[-c(1,2)])){
    for(j in 1:length(unique(plot_data$eq))){
      # print(which(plot_data2$eq==eqs[j] & plot_data2$snr==snr_value[i]))
      # print(x_aixs[j,i])
      plot_data2$x_axis[which(plot_data2$eq==eqs[j] & plot_data2$snr==snr_value[i])] <- x_axis[j,i]
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
  
  
  ## white break terms dataset replace labels with latex
  plot_data3 <- subset(plot_data2,names=='break')
  plot_data3$label[which(plot_data3$eq=='xdot')] <- TeX('$\\dot{x}_1$')
  plot_data3$label[which(plot_data3$eq=='ydot')] <- TeX('$\\dot{x}_2$')
  
  plot_data3$y_axis <- NA # define no stacked y-axis 
  for(i in 1:nrow(plot_data3)){
    plot_data3$y_axis[i] <- 
      sum(dplyr::filter(plot_data2, eq==plot_data3$eq[i] & snr==plot_data3$snr[i] & names!=plot_data3$names[i])$value)
  }
  
  plot_data3$y_axis2 <- NA # define stacked y-axis 
  for(i in unique(plot_data3$snr)){
    ind <- which(plot_data3$eq=='xdot' & plot_data3$snr==i)
    ind2 <- which(plot_data3$snr==i)
    plot_data3$y_axis2[ind] <- sum(plot_data3$y_axis[ind2])+25+50
    ind <- which(plot_data3$eq=='ydot' & plot_data3$snr==i)
    ind2 <- which(plot_data3$eq!='xdot' & plot_data3$snr==i)
    plot_data3$y_axis2[ind] <- plot_data3$y_axis[ind]+25
  }
  
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
  # plot_data4[which(plot_data4$names=='break'),]$label2 <- plot_data3$label
  # plot_data4 <- subset(plot_data4, !is.na(plot_data4$label)|plot_data4$names=='break')
  
  # plot_data2 <- filter(plot_data2, snr!='values.22') # remove snr=Inf
  # plot_data3 <- plot_data3[which(plot_data3$snr!='values.22'),] # remove snr=Inf
  # plot_data4 <- plot_data4[which(plot_data4$snr!='values.22'),] # remove snr=Inf
  
  snr_levels <- levels(plot_data4$snr)
  names_levels <- levels(plot_data4$names)
  
  plot_data5 <- plot_data4
  plot_data5$eq_ord <- NA
  plot_data5$eq_ord[which(plot_data5$eq=='xdot')] <- '2xdot'
  plot_data5$eq_ord[which(plot_data5$eq=='ydot')] <- '1ydot'
  plot_data5 <- plot_data5[with(plot_data5, order(snr, eq_ord, names)),]
  
  plot_data5$y_axis <- NA
  for(i in snr_levels){
    snr_index <- which(plot_data5$snr==i)
    plot_data5$y_axis[snr_index] <- cumsum(plot_data5$value[snr_index])-plot_data5$value[snr_index]/2
  }
  plot_data5 <- plot_data5[which(!is.na(plot_data5$label)|plot_data5$names=='break'),]
  plot_data5$color[which(plot_data5$names=='break')] <- 'black'
  
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
  
  
  plot_data_xy_dot <- rbind(plot_xdot_data, plot_ydot_data)
  plot_data_xy_dot$eq <- as.factor(plot_data_xy_dot$eq)
  levels(plot_data_xy_dot$eq) <- c(TeX('$\\dot{x}_1$'),TeX('$\\dot{x}_2$'))
  annotate_data <- plot_data_xy_dot %>% select('snr','y_axis','label2','eq')
  names(annotate_data)[2] <- 'value'
  
  original_ggplot <- ggplot(plot_data_xy_dot,aes(x=snr,y=value))+
    geom_bar(aes(colour=eq), fill=plot_data_xy_dot$correct_color, position="stack", stat="identity",lwd=1.5) +
    facet_grid(rows = vars(eq), labeller = label_parsed) +
    geom_text(data = annotate_data, label = annotate_data$label2, color=1, size=5) +
    labs(x = "SNR",
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
xdot_vdp_lasso <- read.csv("xdot_vdp_TAPER_lasso_increasing_snr_N5000_prediction_models_seq_new_sg.csv")[,-1]
ydot_vdp_lasso <- read.csv("ydot_vdp_TAPER_lasso_increasing_snr_N5000_prediction_models_seq_new_sg.csv")[,-1]
lasso_plot_snr <- ggplot_data_snr('ARGOS-Lasso', xdot_vdp_lasso, ydot_vdp_lasso)
lasso_plot2_snr <- lasso_plot_snr+theme(legend.position='none')
# legend <- get_legend(lasso_plot+theme(legend.position='bottom'))
# cowplot::plot_grid(legend)

## alasso ------------------
xdot_vdp_alasso <- read.csv("xdot_vdp_TAPER_alasso_increasing_snr_N5000_prediction_models_new_sg.csv")[,-1]
ydot_vdp_alasso <- read.csv("ydot_vdp_TAPER_alasso_increasing_snr_N5000_prediction_models_new_sg.csv")[,-1]
alasso_plot_snr <- ggplot_data_snr('ARGOS-Adaptive Lasso', xdot_vdp_alasso, ydot_vdp_alasso)
alasso_plot2_snr <- alasso_plot_snr+theme(legend.position='none')
# legend <- get_legend(alasso_plot+theme(legend.position='bottom'))
# cowplot::plot_grid(legend)

## STLS -------------------------
xdot_vdp_STLS <- read.csv("xdot_vdp_pysindy_increasing_snr_N5000_prediction_models_new_sg.csv")[,-1]
ydot_vdp_STLS <- read.csv("ydot_vdp_pysindy_increasing_snr_N5000_prediction_models_new_sg.csv")[,-1]
STLS_plot_snr <- ggplot_data_snr('SINDy with AIC', xdot_vdp_STLS, ydot_vdp_STLS)
STLS_plot2_snr <- STLS_plot_snr+theme(legend.position='none')
# legend <- get_legend(STLS_plot+theme(legend.position='bottom'))
# cowplot::plot_grid(legend)

# write.csv(x_dot_vdp_lasso_terms_data,file = 'D:/Users/Administrator/OneDrive - Durham University/Proc_Royal_Soc_A/Plots/Lorenz/x_dot_vdp_lasso_terms_data.csv')
# scale_fill_manual(values = colors, breaks = unique(plot_data2$names), 
#                   labels=unname(TeX(c("Other",'$\\dot{x}_1\\dot{x}_2$','$\\dot{x}_2\\dot{x}_3$','$\\dot{x}_1\\dot{x}_3$','$\\dot{x}_3$','$\\dot{x}_2$', '$\\dot{x}_1$'))), name='Terms') +

## legend -------------------
# x_legend <- xdot_vdp_lasso[,c(1:5,7,8,10)]
# x_legend <- xdot_vdp_lasso[,1:11]
# x_legend[,-1] <- 4
# legend_plot <- ggplot_data('STLS', x_legend, x_legend, x_legend)
legend <- get_legend(STLS_plot2_snr+theme(legend.position='bottom'))
ggplot_legend <- cowplot::plot_grid(legend)

## boxplot --------------------------
# source(paste0(
#   "D:/GitHub/",
#   "Iterative-Sparse-Regression/Plot_Code/Lorenz/vdp_boxplot_number_of_selected_variables_increasing_snr.R"))
## compose -----------------
library(gridExtra)
layout_matrix <- matrix(c(0,rep(1,20),rep(2,20),rep(3,20)), ncol=1)
stacked_vdp_snr <-
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
ggsave(stacked_vdp_snr, filename = '../../../Figures/stacked_vdp_snr.pdf', width = 10, height = 15, dpi = 300)
ggsave(stacked_vdp_snr, filename = '../../../Figures/stacked_vdp_snr.png', width = 10, height = 15, dpi = 300)
