# Brazil gas data

rm(list=ls())

library(readr)
library(tidyr)
library(depmixS4)

brazil_gas <- read.delim("~/2004-2019.tsv", stringsAsFactors=FALSE)

test_region <- brazil_gas %>%
  filter(REGIÃO == "NORTE" & PRODUTO == "ÓLEO DIESEL") %>%
  group_by(REGIÃO, DATA.FINAL) %>%
  summarize(av_price=mean(PREÇO.MÉDIO.REVENDA)) %>%
  mutate(price_lag = lag(av_price,n=1),
         weekly_diff = av_price - price_lag) %>%
  filter(DATA.FINAL >= "2015-01-01")

test_region_oos <- test_region[c(780:785),]

test_region <- test_region[c(1:779),]

weekly_diff <- test_region$weekly_diff %>% na.omit()

dates <- test_region$DATA.FINAL
dates <- dates[2:length(dates)]

ggplot(data=test_region) + aes(x=as.Date(DATA.FINAL),y=av_price) + geom_line()

hmm <- depmix(weekly_diff ~ 1, family = gaussian(), nstates = 2, data=data.frame(weekly_diff=weekly_diff))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

plot(as.Date(dates),weekly_diff, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')

state1 <- test_region[which(post_probs$S1>0.5),]
state2 <- test_region[which(post_probs$S1<0.5),]

results_list <- list()
k=0
for(i in 1:4){
  for(j in 1:4){
    k=k+1
    state1_model <- arima(state1$weekly_diff,c(i,0,j))$aic
    state2_model <- arima(state2$weekly_diff,c(i,0,j))$aic
    overall_model <- arima(test_region$weekly_diff,c(i,0,j))$aic
    result <- c(paste("AR(",i,"),MA(",j,")",sep=""))
    results_list[[k]] <-result
  }
}
