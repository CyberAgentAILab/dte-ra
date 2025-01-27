rm(list = ls())

##------------------------------------------------------------------------------
## Load libraries and source files
##------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
require(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(cowplot)
library(patchwork)

cb_colors = brewer.pal(n = 8, name = "Dark2") # discrete colorblind palette


##------------------------------------------------------------------------------
##  Setup
##------------------------------------------------------------------------------
vec.rho = c(0.3, 0.5)
vec.n = c(300, 500,1000)
v = c("Simple", "OLS", "Logit")   


##------------------------------------------------------------------------------
## Main simulations
##------------------------------------------------------------------------------
for (dgp.n in 1:4){
  for(j.rho in 1:length(vec.rho)){
    
    rho = vec.rho[j.rho]
    
    df.bias = c()
    df.ratio = c()
    df.rmse = c()
    df.coverage = c()
    df.length = c()
    df.boot.coverage = c()
    df.boot.length = c()
    df.robust.boot.coverage = c()
    df.robust.boot.length = c()
    
    for(j in 1:length(vec.n)) { ## loop: sample size 
      n = vec.n[j] 
      file.info = paste0("dgp", dgp.n, "_rho", rho, "_n", n)
      
      df.bias.temp = read.csv(paste0("../result/", file.info, "_bias.csv"))
      df.bias = rbind(df.bias, df.bias.temp)
      
      df.rmse.temp = read.csv(paste0("../result/", file.info, "_rmse.csv"))
      df.rmse = rbind(df.rmse, df.rmse.temp)
      
      df.ratio.temp = read.csv(paste0("../result/", file.info, "_ratio.csv"))
      df.ratio = rbind(df.ratio, df.ratio.temp)
      
      # Analytic SE
      df.coverage.temp = read.csv( paste0('../result/', file.info, "_coverage.csv"))
      df.coverage = rbind(df.coverage, df.coverage.temp)
      
      df.length.temp = read.csv( paste0('../result/', file.info, "_averagelength.csv"))
      df.length = rbind(df.length, df.length.temp)
      
      # Bootstrap SE
      df.boot.coverage.temp = read.csv( paste0('../result/', file.info, "_boot_coverage.csv"))
      df.boot.coverage = rbind(df.boot.coverage, df.boot.coverage.temp)
      
      df.boot.length.temp = read.csv( paste0('../result/', file.info, "_boot_averagelength.csv"))
      df.boot.length = rbind(df.boot.length, df.boot.length.temp)
      
      # Robust bootstrap SE
      df.robust.boot.coverage.temp = read.csv( paste0('../result/', file.info, "_boot_robust_coverage.csv"))
      df.robust.boot.coverage = rbind(df.robust.boot.coverage, df.robust.boot.coverage.temp)
      
      df.robust.boot.length.temp = read.csv( paste0('../result/', file.info, "_boot_robust_averagelength.csv"))
      df.robust.boot.length = rbind(df.robust.boot.length, df.robust.boot.length.temp)
      
    }
    
    ##------------------------------------------------------------------------------
    ## Change data format
    ##------------------------------------------------------------------------------
    df.bias = df.bias  %>%
      select( c(v, n, location)) %>%
      pivot_longer(cols = v,
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.bias = transform(df.bias, est= factor(est, levels = v))
    
    df.rmse = df.rmse  %>%
      select( c(v, n, location)) %>%
      pivot_longer(cols = v,
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.rmse = transform(df.rmse, est= factor(est, levels = v))
    
    df.ratio = df.ratio  %>%
      pivot_longer(cols = c("OLS", "Logit"),
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.ratio = transform(df.ratio, est= factor(est, levels = c("OLS", "Logit")))
    
    df.coverage = df.coverage  %>%
      select( c(v, n, location)) %>%
      pivot_longer(cols = v,
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.coverage = transform(df.coverage, est= factor(est, levels = v))
    
    df.length = df.length  %>%
      select( c(v, n, location)) %>%
      pivot_longer(cols = v,
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.length = transform(df.length, est= factor(est, levels = v))
    
    # Bootstrap SE
    df.boot.coverage = df.boot.coverage  %>%
      select( c(v, n, location)) %>%
      pivot_longer(cols = v,
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.boot.coverage = transform(df.boot.coverage, est= factor(est, levels = v))
    
    df.boot.length = df.boot.length  %>%
      select( c(v, n, location)) %>%
      pivot_longer(cols = v,
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.boot.length = transform(df.boot.length, est= factor(est, levels = v))
    
    # Robust bootstrap SE
    df.robust.boot.coverage = df.robust.boot.coverage  %>%
      select( c(v, n, location)) %>%
      pivot_longer(cols = v,
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.robust.boot.coverage = transform(df.robust.boot.coverage, est= factor(est, levels = v))
    
    df.robust.boot.length = df.robust.boot.length  %>%
      select( c(v, n, location)) %>%
      pivot_longer(cols = v,
                   names_to = "est", 
                   values_to = "val") %>%
      mutate(est = as.factor(est) ) %>%   
      data.frame() 
    
    df.robust.boot.length = transform(df.robust.boot.length, est= factor(est, levels = v))
    
    
    
    g1 = ggplot(df.bias) +
      geom_boxplot(aes(x=factor(n), y=val, fill=factor(est) )) + 
      scale_fill_manual(values=cb_colors) +
      theme_bw() +
      ylab("Bias") +
      geom_hline(yintercept=0, color = "black", size=0.01, alpha = .3) +
      guides(fill=guide_legend(title="Estimator")) +
      theme(legend.position="bottom") +
      theme(text=element_text(size=17),
            legend.text=element_text(size=14),
            legend.title=element_text(size=15),
            axis.title.x=element_blank())+
      scale_x_discrete(labels=function(x) paste0("n=", x))
    
    
    g2 = ggplot(df.rmse) +
      geom_boxplot(aes(x=factor(n), y=val, fill=factor(est) )) + 
      scale_fill_manual(values=cb_colors) +
      theme_bw() +
      ylab("RMSE") +
      geom_hline(yintercept=0, color="black", size=0.01, alpha = .3) +
      guides(fill=guide_legend(title="Estimator")) +
      theme(legend.position="bottom") +
      theme(text=element_text(size=17),
            legend.text=element_text(size=14),
            legend.title=element_text(size=15),
            axis.title.x=element_blank())+
      scale_x_discrete(labels=function(x) paste0("n=", x))
    
    
    g3 = ggplot(df.length) +
      geom_boxplot(aes(x=factor(n), y=val, fill=factor(est) )) + 
      scale_fill_manual(values=cb_colors) +
      theme_bw() +
      ylab("Average length") +
      geom_hline(yintercept=0, color = "black", size=0.01, alpha = .3) +
      guides(fill=guide_legend(title="Estimator")) +
      theme(legend.position="bottom") +
      theme(text=element_text(size=17),
            legend.text=element_text(size=14),
            legend.title=element_text(size=15),
            axis.title.x=element_blank())+
      scale_x_discrete(labels=function(x) paste0("n=", x))
    
    
    g4= ggplot(df.coverage) +
      geom_boxplot(aes(x=factor(n), y=val, fill=factor(est))) + 
      scale_fill_manual(values=cb_colors) +
      theme_bw() +
      ylab("Coverage Probability") +
      guides(fill=guide_legend(title="Estimator")) +
      theme(legend.position="bottom") +
      theme(text=element_text(size=17),
            legend.text=element_text(size=14),
            legend.title=element_text(size=15),
            axis.title.x=element_blank())+
      scale_x_discrete(labels=function(x) paste0("n=", x))
    
    
    # Combine the plots with patchwork
    combined_plot = (g1 + g2) / (g3 + g4) + plot_layout(guides='collect') & theme(legend.position='bottom')
    
    # Print the combined plot
    print(combined_plot)
    
    # Save
    ggsave(paste0("../result/", "fig_", file.info, "_all.pdf"),
           width = 11, height = 8)
    
    # Bootstrap SE plots in the Appendix 
    g5 = ggplot(df.boot.length) +
      geom_boxplot(aes(x=factor(n), y=val, fill=factor(est) )) + 
      scale_fill_manual(values=cb_colors) +
      theme_bw() +
      ylab("Average length") +
      geom_hline(yintercept=0, color = "black", size=0.01, alpha = .3) +
      guides(fill=guide_legend(title="Estimator")) +
      theme(legend.position="bottom") +
      theme(text=element_text(size=17),
            legend.text=element_text(size=14),
            legend.title=element_text(size=15),
            axis.title.x=element_blank())+
      scale_x_discrete(labels=function(x) paste0("n=", x))+
      ggtitle("Bootstrap SD")
    
    g6= ggplot(df.boot.coverage) +
      geom_boxplot(aes(x=factor(n), y=val, fill=factor(est))) + 
      scale_fill_manual(values=cb_colors) +
      theme_bw() +
      ylab("Coverage Probability") +
      ylim(c(0.92, 0.97))+
      guides(fill=guide_legend(title="Estimator")) +
      theme(legend.position="bottom") +
      theme(text=element_text(size=17),
            legend.text=element_text(size=14),
            legend.title=element_text(size=15),
            axis.title.x=element_blank())+
      scale_x_discrete(labels=function(x) paste0("n=", x))
    
    g7 = ggplot(df.robust.boot.length) +
      geom_boxplot(aes(x=factor(n), y=val, fill=factor(est) )) + 
      scale_fill_manual(values=cb_colors) +
      theme_bw() +
      ylab("Average length") +
      geom_hline(yintercept=0, color = "black", size=0.01, alpha = .3) +
      guides(fill=guide_legend(title="Estimator")) +
      theme(legend.position="bottom") +
      theme(text=element_text(size=17),
            legend.text=element_text(size=14),
            legend.title=element_text(size=15),
            axis.title.x=element_blank())+
      scale_x_discrete(labels=function(x) paste0("n=", x))+
      ggtitle("Bootstrap IQR")
    
    g8 = ggplot(df.robust.boot.coverage) +
      geom_boxplot(aes(x=factor(n), y=val, fill=factor(est))) + 
      scale_fill_manual(values=cb_colors) +
      theme_bw() +
      ylab("Coverage Probability") +
      ylim(c(0.92, 0.97))+
      guides(fill=guide_legend(title="Estimator")) +
      theme(legend.position="bottom") +
      theme(text=element_text(size=17),
            legend.text=element_text(size=14),
            legend.title=element_text(size=15),
            axis.title.x=element_blank())+
      scale_x_discrete(labels=function(x) paste0("n=", x))
    
    
    # Combine the plots with patchwork
    combined_plot = (g5 + g6) / (g7 + g8) + plot_layout(guides='collect') & theme(legend.position='bottom')
    
    # Print the combined plot
    print(combined_plot)
    
    #Save
    ggsave(paste0("../experiment/", "fig_", file.info, "_bootstrap.pdf"),
           width = 11, height = 8)
  }
}

##------------------------------------------------------------------------------
## Appendix: simulations with covariate transformations
##------------------------------------------------------------------------------
vec.poly = c(2, 3) # degrees of polynomials to consider

for (j in 1:length(vec.poly)){
  num_poly = vec.poly[j]
  
  dgp.n = 1
  rho = 0.5
  
  df.bias = c()
  df.ratio = c()
  df.rmse = c()
  df.coverage = c()
  df.length = c()
  df.boot.coverage = c()
  df.boot.length = c()
  df.robust.boot.coverage = c()
  df.robust.boot.length = c()
  
  for(j in 1:length(vec.n)) { ## loop: sample size 
    n = vec.n[j] 
    file.info = paste0("dgp", dgp.n, "_rho", rho, "_poly", num_poly, "_n", n)
    
    df.bias.temp = read.csv(paste0("../result/", file.info, "_bias.csv"))
    df.bias = rbind(df.bias, df.bias.temp)
    
    df.rmse.temp = read.csv(paste0("../result/", file.info, "_rmse.csv"))
    df.rmse = rbind(df.rmse, df.rmse.temp)
    
    df.ratio.temp = read.csv(paste0("../result/", file.info, "_ratio.csv"))
    df.ratio = rbind(df.ratio, df.ratio.temp)
    
    # Analytic SE
    df.coverage.temp = read.csv( paste0('../result/', file.info, "_coverage.csv"))
    df.coverage = rbind(df.coverage, df.coverage.temp)
    
    df.length.temp = read.csv( paste0('../result/', file.info, "_averagelength.csv"))
    df.length = rbind(df.length, df.length.temp)
    
    # Bootstrap SE
    df.boot.coverage.temp = read.csv( paste0('../result/', file.info, "_boot_coverage.csv"))
    df.boot.coverage = rbind(df.boot.coverage, df.boot.coverage.temp)
    
    df.boot.length.temp = read.csv( paste0('../result/', file.info, "_boot_averagelength.csv"))
    df.boot.length = rbind(df.boot.length, df.boot.length.temp)
    
    # Robust bootstrap SE
    df.robust.boot.coverage.temp = read.csv( paste0('../result/', file.info, "_boot_robust_coverage.csv"))
    df.robust.boot.coverage = rbind(df.robust.boot.coverage, df.robust.boot.coverage.temp)
    
    df.robust.boot.length.temp = read.csv( paste0('../result/', file.info, "_boot_robust_averagelength.csv"))
    df.robust.boot.length = rbind(df.robust.boot.length, df.robust.boot.length.temp)
    
  }
  
  ##------------------------------------------------------------------------------
  ## Change data format
  ##------------------------------------------------------------------------------
  df.bias = df.bias  %>%
    select( c(v, n, location)) %>%
    pivot_longer(cols = v,
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.bias = transform(df.bias, est= factor(est, levels = v))
  
  df.rmse = df.rmse  %>%
    select( c(v, n, location)) %>%
    pivot_longer(cols = v,
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.rmse = transform(df.rmse, est= factor(est, levels = v))
  
  df.ratio = df.ratio  %>%
    pivot_longer(cols = c("OLS", "Logit"),
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.ratio = transform(df.ratio, est= factor(est, levels = c("OLS", "Logit")))
  
  df.coverage = df.coverage  %>%
    select( c(v, n, location)) %>%
    pivot_longer(cols = v,
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.coverage = transform(df.coverage, est= factor(est, levels = v))
  
  df.length = df.length  %>%
    select( c(v, n, location)) %>%
    pivot_longer(cols = v,
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.length = transform(df.length, est= factor(est, levels = v))
  
  # Bootstrap SE
  df.boot.coverage = df.boot.coverage  %>%
    select( c(v, n, location)) %>%
    pivot_longer(cols = v,
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.boot.coverage = transform(df.boot.coverage, est= factor(est, levels = v))
  
  df.boot.length = df.boot.length  %>%
    select( c(v, n, location)) %>%
    pivot_longer(cols = v,
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.boot.length = transform(df.boot.length, est= factor(est, levels = v))
  
  # Robust bootstrap SE
  df.robust.boot.coverage = df.robust.boot.coverage  %>%
    select( c(v, n, location)) %>%
    pivot_longer(cols = v,
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.robust.boot.coverage = transform(df.robust.boot.coverage, est= factor(est, levels = v))
  
  df.robust.boot.length = df.robust.boot.length  %>%
    select( c(v, n, location)) %>%
    pivot_longer(cols = v,
                 names_to = "est", 
                 values_to = "val") %>%
    mutate(est = as.factor(est) ) %>%   
    data.frame() 
  
  df.robust.boot.length = transform(df.robust.boot.length, est= factor(est, levels = v))
  
  
  g1 = ggplot(df.bias) +
    geom_boxplot(aes(x=factor(n), y=val, fill=factor(est) )) + 
    scale_fill_manual(values=cb_colors) +
    theme_bw() +
    ylab("Bias") +
    geom_hline(yintercept=0, color = "black", size=0.01, alpha = .3) +
    guides(fill=guide_legend(title="Estimator")) +
    theme(legend.position="bottom") +
    theme(text=element_text(size=17),
          legend.text=element_text(size=14),
          legend.title=element_text(size=15),
          axis.title.x=element_blank())+
    scale_x_discrete(labels=function(x) paste0("n=", x))
  
  
  g2 = ggplot(df.rmse) +
    geom_boxplot(aes(x=factor(n), y=val, fill=factor(est) )) + 
    scale_fill_manual(values=cb_colors) +
    theme_bw() +
    ylab("RMSE") +
    geom_hline(yintercept=0, color="black", size=0.01, alpha = .3) +
    guides(fill=guide_legend(title="Estimator")) +
    theme(legend.position="bottom") +
    theme(text=element_text(size=17),
          legend.text=element_text(size=14),
          legend.title=element_text(size=15),
          axis.title.x=element_blank())+
    scale_x_discrete(labels=function(x) paste0("n=", x))
  
  
  g3 = ggplot(df.boot.length) +
    geom_boxplot(aes(x=factor(n), y=val, fill=factor(est) )) + 
    scale_fill_manual(values=cb_colors) +
    theme_bw() +
    ylab("Average length") +
    geom_hline(yintercept=0, color = "black", size=0.01, alpha = .3) +
    guides(fill=guide_legend(title="Estimator")) +
    theme(legend.position="bottom") +
    theme(text=element_text(size=17),
          legend.text=element_text(size=14),
          legend.title=element_text(size=15),
          axis.title.x=element_blank())+
    scale_x_discrete(labels=function(x) paste0("n=", x))+
    ggtitle("Bootstrap SD")
  
  g4 = ggplot(df.boot.coverage) +
    geom_boxplot(aes(x=factor(n), y=val, fill=factor(est))) + 
    scale_fill_manual(values=cb_colors) +
    theme_bw() +
    ylab("Coverage Probability") +
    ylim(c(0.92, 0.97))+
    guides(fill=guide_legend(title="Estimator")) +
    theme(legend.position="bottom") +
    theme(text=element_text(size=17),
          legend.text=element_text(size=14),
          legend.title=element_text(size=15),
          axis.title.x=element_blank())+
    scale_x_discrete(labels=function(x) paste0("n=", x))
  
  # Combine the plots with patchwork
  combined_plot = (g1 + g2) / (g3 + g4) + plot_layout(guides='collect') & theme(legend.position='bottom')
  
  # Print the combined plot
  print(combined_plot)
  
  # Save
  ggsave(paste0("../result/", "fig_", file.info, "_all.pdf"),
         width = 11, height = 8)
}

