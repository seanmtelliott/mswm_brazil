# Brazil gas data

rm(list=ls())
options(stringsAsFactors = F)

setwd("~/Classes/ECO2401/Paper/")
plot_dir <- file.path(getwd(),"plots")
plot_dir_opt <- file.path(getwd(),"plots","opt")
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(depmixS4)
library(gridExtra)
library(MSwM)
source("utilities.R")

brazil_gas <- read.delim("~/Classes/ECO2401/Paper/2004-2019.tsv", stringsAsFactors=FALSE)

# Price: PREÇO.MÉDIO.REVENDA


data_regional <- brazil_gas %>%
  filter(PRODUTO=="ÓLEO DIESEL") %>%
  group_by(ESTADO) %>%
  mutate(av_price=PREÇO.MÉDIO.REVENDA,
         price_lag = lag(av_price,n=1),
         weekly_diff = av_price - price_lag) %>%
  dplyr::select(c("DATA.FINAL","ESTADO","av_price","weekly_diff"))

data_regional <- data_regional %>% 
  na.omit()  %>%
  mutate(DATA.FINAL = as.Date(DATA.FINAL),
         State = ESTADO)



# Plot all data series

all_history_plot <- ggplot(data=data_regional) + aes(x=as.Date(DATA.FINAL),y=av_price,colour=State) + geom_line() + xlab("Date") + ylab("Average Price") +
  theme(legend.position="bottom", legend.title=element_text(size=0)) 

data_date_filter <- data_regional %>% 
  filter(DATA.FINAL >= "2015-01-01") 

date_list <- unique(data_date_filter$DATA.FINAL)

model_period_plot <- ggplot(data=data_date_filter) + aes(x=as.Date(DATA.FINAL),y=av_price,colour=State) + geom_line() + xlab("Date") + ylab("") 

mylegend <- g_legend(all_history_plot)

combined_price_plot <- grid.arrange(arrangeGrob(all_history_plot + theme(legend.position="none"),
                               model_period_plot + theme(legend.position="none"),
                               nrow=1))

# Fit simple HMM with two states to illustrate how process works, here we can observe a clear break in the data
twostate_summary <- list()
j=0
for(i in unique(data_date_filter$ESTADO)){
j=j+1
hmm2st_probs <- fit_state_hmm(data_date_filter,estado=i,num_states=2)
plot_hmm_probs(data_date_filter,hmm2st_probs,estado=i,directory=plot_dir) 

break_date <- ifelse((hmm2st_probs %>% filter(State=="S1"))$pp[1] >= 0.5,
               as.character(min((hmm2st_probs %>% filter(State=="S1" & pp<=0.5))$date)),
               as.character(min((hmm2st_probs %>% filter(State=="S1" & pp>=0.5))$date)))

twostate_summary[[j]] <- c(j,i,break_date)

}

twostate_summary <- do.call(rbind.data.frame,twostate_summary)
names(twostate_summary) <- c("State","Break Date")



# Switch to using weekly changes since that is what we will model.
  
# Determine number of optimal states (1 or 2)

sample_states <- c("MINAS GERAIS","RIO GRANDE DO SUL","SAO PAULO")

data_date_filter <- data_date_filter %>% filter(ESTADO %in% sample_states)
  
set.seed(2019)
optimal_states <- state_determination(data_date_filter,max_states=3)

# Show plots for optimal number of states

optst_plotlist = list()
j=0
for(i in sample_states){

  j=j+1
  opt_states <- as.numeric((optimal_states %>% filter(region==i))$opt_states)
  hmm_optst_probs <- fit_state_hmm(data_date_filter,estado=i,num_states=opt_states,type="diff")
  hmm_optst_plot <- plot_hmm_probs(data_date_filter,hmm_optst_probs,estado=i,type="diff",directory=plot_dir_opt) 
  optst_plotlist[[j]] <- hmm_optst_plot

}

# Fit Markov switching models using first differences
set.seed(2020)
mswm_model_list = list()
aic_list = list()
j=0
for(i in sample_states){
j=j+1
sample_mswm <- fit_mswm_multiple(data_date_filter,max_ar = 4,model_region=i,opt_state=optimal_states)

# Select based on AIC
aic_model <- rapply(lapply(sample_mswm,function(x) AIC(x)),c)
aic_list[[j]] <- c(i,rapply(lapply(sample_mswm,function(x) AIC(x)),c))
selected_model <- which(aic_model==min(aic_model))

mswm_model_list[[j]] <- sample_mswm[[selected_model]]
}


# Select best model based on AIC

mg_model <- mswm_model_list[[1]]
rgds_model <- mswm_model_list[[2]]
sp_model <- mswm_model_list[[3]]


# Display model diagnostics and posterior probablities

par(mar=c(3,3,3,3))
plotProb(mswm_model_list[[1]],which=1)

plotDiag(mswm_model_list[[1]],which=2)

# ARMA models for benchmarking

# results_list <- list()
# k=0
# for(i in 1:4){
#     overall_model <- arima(test_region$weekly_diff,c(i,0,0))$bic
#     result <- c(paste("AR(",i,"),MA(",j,")",sep=""))
#     results_list[[k]] <-result
# }
