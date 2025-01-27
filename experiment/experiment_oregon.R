rm(list = ls())

##------------------------------------------------------------------------------
## Load libraries and source files
##------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(foreign)
library(fastglm)
library(ggpubr)
library(RColorBrewer)

# Call source files
source( "functions.R" )

##------------------------------------------------------------------------------
## Setup
##------------------------------------------------------------------------------
# Set seed
set.seed(12345)

# Color palette for figures
cb_colors = brewer.pal(n = 8, name = "Dark2") # discrete colorblind palette

##------------------------------------------------------------------------------
## Load data
##------------------------------------------------------------------------------
df.full = read.csv("../data/data_oregon_12m.csv")

df = df.full %>% 
  filter(numhh_list==1) %>% # subset to household size equals 1
  na.omit() 
summary(df)

# Select outcome variable to consider
df= df %>% mutate(Y=doc_num_mod_12m, W=treatment)
## select variables 
vec.y = as.numeric( as.matrix(df$Y) ) 
vec.d = as.numeric( as.matrix(df$W) )
mat.x = as.matrix(df[,2:11])
num_obs = length(vec.y)
ymin = min(vec.y)
ymax = max(vec.y)

##------------------------------------------------------------------------------
## Estimation setup
##------------------------------------------------------------------------------
# Locations for DTE
vec.loc = seq(ymin, ymax, by = 1) 
# h for PTE
h.pte   = 1 ## gap length for PTE
# number of bootstrap repetitions
B.size = 500

##------------------------------------------------------------------------------
## Regression-adjusted DTE and PTE
##------------------------------------------------------------------------------
start_time = Sys.time()
results = DTE.estimation(vec.y, vec.d, mat.x, vec.loc, "logistic_regression", n_boot= B.size)
end_time = Sys.time()
print(paste("Time spent:", end_time-start_time))

##------------------------------------------------------------------------------
## Plot PTE (simple)
##------------------------------------------------------------------------------
y.max = max(max(results$pte$pte+1.96*results$pte$se), max(results$pte.ra$pte+1.96*results$pte.ra$se)) + 1e-5
y.min = min(min(results$pte$pte-1.96*results$pte$se), min(results$pte$pte-1.96*results$pte$se)) - 1e-5

ggplot(results$pte, aes(vec.loc, pte) ) + 
  geom_bar( stat = "identity", color= cb_colors[4], fill=cb_colors[4]) +
  geom_errorbar(aes(ymin = results$pte$pte-1.96*results$pte$se, 
                    ymax = results$pte$pte-1.96*results$pte$se),
                width= 1) +
  ylim(y.min, y.max) + 
  geom_hline(yintercept=0, color="black", size=0.01, alpha = .7) +
  theme_bw() + 
  labs(x= "Number of doctor visits", y="PTE")  +
  scale_x_continuous(breaks = seq(0,15,by=1), limit=c(ymin-0.5,15+0.5)) +
  theme(axis.text.x = element_text(hjust=1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

ggsave("../result/Oregon_num_doc_PTE_simple.pdf", width=5, height =3)  ## save 


##------------------------------------------------------------------------------
## Plot PTE (adjusted)
##------------------------------------------------------------------------------
ggplot(results$pte.ra, aes(vec.loc, pte) ) + 
  geom_bar( stat = "identity", color= cb_colors[5], fill=cb_colors[5]) +
  geom_errorbar(aes(ymin = results$pte$pte-1.96*results$pte$se, 
                    ymax = results$pte$pte+1.96*results$pte$se),
                width= 1) +
  ylim(y.min, y.max) + 
  geom_hline(yintercept=0, color="black", size=0.01, alpha = .7) +
  theme_bw() + 
  labs(x= "Number of doctor visits", y="PTE")  +
  scale_x_continuous(breaks = seq(0,15,by=1), limit=c(ymin-0.5, 15+0.5))+
  theme(axis.text.x = element_text( hjust=1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

ggsave("../result/Oregon_num_doc_PTE_adj.pdf", width=5, height =3) ## save

##------------------------------------------------------------------------------
## Plot DTE (simple)
##------------------------------------------------------------------------------
ggplot() + 
  geom_line( aes(vec.loc, results$dte$dte - 1.96*results$dte$se), color= cb_colors[4], linetype=2) +
  geom_line( aes(vec.loc, results$dte$dte + 1.96*results$dte$se), color= cb_colors[4], linetype=2) +
  geom_ribbon(aes(x    = vec.loc, 
                  ymin = results$dte$dte - 1.96*results$dte$se, 
                  ymax = results$dte$dte + 1.96*results$dte$se), 
              fill = cb_colors[4], alpha = .3) +
  geom_line( aes(vec.loc, results$dte$dte), color = cb_colors[4]) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(0,ymax,by=2), limit=c(ymin-0.5,ymax+0.5)) +
  geom_hline(yintercept=0, color="black", size=0.1, alpha = .3) +
  labs(title = "", 
       x= "Number of doctor visits", y="DTE") +
  theme(text=element_text(size=17))+
  theme(axis.text.x = element_text(hjust = 1))+
  theme(axis.text.x = element_text(hjust=1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

ggsave("../result/Oregon_num_doc_DTE_simple.pdf", width=5, height =3)  ## save 


##------------------------------------------------------------------------------
## Plot DTE (adjusted)
##------------------------------------------------------------------------------
ggplot() + 
  geom_line( aes(vec.loc, results$dte.ra$dte - 1.96*results$dte.ra$se), color= cb_colors[5], linetype=2) +
  geom_line( aes(vec.loc, results$dte.ra$dte + 1.96*results$dte.ra$se), color= cb_colors[5], linetype=2) +
  geom_ribbon(aes(x = vec.loc, 
                  ymin = results$dte.ra$dte - 1.96*results$dte.ra$se, 
                  ymax = results$dte.ra$dte + 1.96*results$dte.ra$se), 
              fill = cb_colors[5], alpha = .4)+
  geom_line( aes(vec.loc, results$dte.ra$dte), color = cb_colors[5]) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(0,ymax,by=2), limit=c(ymin-0.5,ymax+0.5)) +
  geom_hline(yintercept=0, color="black", size=0.1, alpha = 0.3) +
  labs(title = "", 
       x= "Number of doctor visits", y="DTE") +
  theme(axis.text.x = element_text(hjust=1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

ggsave("../result/Oregon_num_doc_DTE_adj.pdf", width=5, height =3)  ## save 






