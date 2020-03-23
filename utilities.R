#Metrics paper utilities

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

fit_state_hmm <- function(price_data,estado,num_states=2,type="level"){
  
  if(type=="level"){
  
  price <- (price_data %>% filter(State==estado))$av_price
  
  }else if(type=="diff"){
    
    price <- (price_data %>% filter(State==estado))$weekly_diff
    
  }
  
  hmm <- depmix(price ~ 1, family = gaussian(), nstates = num_states, data=data.frame(price=price))
  hmmfit <- fit(hmm, verbose = FALSE)
  
  getmodel(hmm,"response",2,1)
  post_probs <- posterior(hmmfit) 
  post_probs$date <- unique(price_data$DATA.FINAL)
  post_probs <- post_probs  %>% gather(key="State",value="pp",-state,-date)
  
  return(post_probs)
  
}

plot_hmm_probs <- function(price_data,post_probs,estado,type="level",directory){
  
  if(type=="level"){
  price_plot <- ggplot(data=price_data %>% filter(State==estado)) + aes(x=as.Date(DATA.FINAL),y=av_price) + geom_line() + xlab("") + ylab("Average Price (R$/l)") 
  }else if(type=="diff"){
    
    price_plot <- ggplot(data=price_data %>% filter(State==estado)) + aes(x=as.Date(DATA.FINAL),y=weekly_diff) + geom_line() + xlab("") + ylab("Weekly Diff (R$/l)") 
    
  }
  
  post_prob_plot <- ggplot(data=post_probs) + aes(x=as.Date(date),y=pp,color=State) + geom_line() + xlab("Date") + ylab("Posterior Probability") 
  
  combined_plot <- grid.arrange(arrangeGrob(price_plot + theme(legend.position="none"),
                                            post_prob_plot + theme(legend.position="none"),
                                            nrow=2))
  ggsave(file.path(directory,paste(estado,".png",sep="")),combined_plot)
  
  
}

state_determination <- function(price_data,max_states=1){

  estado <- unique(price_data$ESTADO)
  k=0
  opt_list <- list()
  for(i in estado){
  k=k+1

  price <- (price_data %>% filter(ESTADO == i))$weekly_diff
  g_state <- i

  bic_list <- c()
  aic_list <- c()

    for(j in 1:max_states){
      hmm <- depmix(price ~ 1, family = gaussian(), nstates = (j), data=data.frame(price=price))
      hmmfit <- fit(hmm, verbose = FALSE)
      bic_list[j] <- BIC(hmmfit)
      aic_list[j] <- AIC(hmmfit)
    }
  opt_states <- which(bic_list==min(bic_list)) 
  opt_list[[k]] <- t(c(i,opt_states,paste(bic_list,collapse=';'),paste(aic_list,collapse=';')))

  }
  
  opt_combined <- do.call(rbind.data.frame,opt_list)
  names(opt_combined) <- c("region","opt_states","BIC","AIC")
  
  return(opt_combined)
  
}

fit_mswm_multiple <- function(price_data,max_ar,model_region,opt_state){

    price <- (price_data %>% filter(ESTADO == model_region))
    model=lm(weekly_diff~1,price)
    mod_list <- list()
    
    opt_num_st <- as.numeric((opt_state %>% filter(region==model_region))$opt_states)
    
    for(j in 1:4){
      mod=msmFit(model,k=opt_num_st,p=j,sw=c(rep(TRUE,j),TRUE,TRUE),control=list(parallel=FALSE,trace=FALSE))
      mod_list[[j]] <- mod
    }
    
    return(mod_list)
    
  
}

fit_ar_multiple <- function(price_data,max_ar,region){
  
  
  price <- (price_data %>% filter(ESTADO == region))$weekly_diff

  mod_list <- list()
  
  for(j in 1:4){
    mod=arima(price,c(j,0,0))
    mod_list[[j]] <- mod
  }
  
  return(mod_list)
  
}

