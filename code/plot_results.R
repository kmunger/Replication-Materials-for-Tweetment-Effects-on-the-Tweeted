



#########################################################################;
#  File-Name: plot_results.R				                                    #;
#  Date: September 28, 2016				                                    	#;
#  Author:	KM							                                            #;
#  Purpose:	Plot results                                                #;
#########################################################################;

setwd("C:/Users/kevin/Documents/GitHub/Replication-Materials-for-Tweetment-Effects-on-the-Tweeted/")


##run read_in_data_fit_models.R first

#############################This is to run the models divided by anonymity models

models<-list(   wk2_rac_anon, wk2_rac_id, mon1_rac_anon, mon1_rac_id,  
              mon2_rac_anon, mon2_rac_id)

modelnames<-list(   "wk2_rac_anon", "wk2_rac_id", "mon1_rac_anon", "mon1_rac_id",  
                    "mon2_rac_anon", "mon2_rac_id")


##need to sepearate out the first ones to be the only ones that have labeled axes, for space
wk1_models<-list(wk1_rac_anon, wk1_rac_id)

wk1_modelnames<-list("wk1_rac_anon", "wk1_rac_id")



##############################This is to run the full models 

models<-list(   wk2_rac,  mon1_rac,  mon2_rac)
modelnames<-list(   "wk2_rac",   "mon1_rac",   "mon2_rac" )

##need to sepearate out the first ones to be the only ones that have labeled axes, for space

wk1_models<-list(wk1_rac)

wk1_modelnames<-list("wk1_rac")



##plot the wk1 models, with axis labels

for (i in 1:length(wk1_models)){
  ##change length, one fewer coefficient
  coefs <- t(t(c(wk1_models[[i]]$coefficients[2:6,1])))
  ses <- t(t(c(wk1_models[[i]]$coefficients[2:6,2])))
  y.axis <- c(1:5)
  #fix x axis to the max values any coefficients take for comparison
  min <- (-.65)
  max <- .55
  var.names <- c("In-Group/Low","Out-Group/Low","In-Group/High","Out-Group/High", "Log Followers")
  adjust <- 0
  ##########################
  ###change filename depending on the data source
  ##########################
  pdf(paste0(wk1_modelnames[i], "_conservative.pdf"), 5, 7)
  par(mar=c(4,6,1,1))
  
  plot(coefs[,1], y.axis, type = "p", axes = F, xlab = "Change in Average Daily Slur Use", ylab = "", pch = 19, cex = .8, 
       xlim=c(min,max),ylim = c(.5,5.5), main = "")
  rect(min,.5,max,1.5, col = c("grey97"), border="grey90", lty = 2)
  rect(min,1.5,max,2.5, col = c("grey95"), border="grey90", lty = 2)
  rect(min,2.5,max,3.5, col = c("grey97"), border="grey90", lty = 2)
  rect(min,3.5,max,4.5, col = c("grey95"), border="grey90", lty = 2)
  rect(min,4.5,max,5.5, col = c("grey97"), border="grey90", lty = 2)
  
  axis(1, tick = T,cex.axis = .75, mgp = c(2,.7,0))
  axis(2, at = y.axis, label = var.names, las = 1, tick = FALSE, cex.axis =.8, hadj=1)
  abline(h = y.axis, lty = 2, lwd = .5, col = "white")
  segments(coefs[,1]-qnorm(.975)*ses[,1], y.axis+2*adjust, coefs[,1]+qnorm(.975)*ses[,1], y.axis+2*adjust, lwd =  1)
  
  segments(coefs[,1]-qnorm(.95)*ses[,1], y.axis+2*adjust-.035, coefs[,1]-qnorm(.95)*ses[,1], y.axis+2*adjust+.035, lwd = .9)
  segments(coefs[,1]+qnorm(.95)*ses[,1], y.axis+2*adjust-.035, coefs[,1]+qnorm(.95)*ses[,1], y.axis+2*adjust+.035, lwd = .9)
  points(coefs[,1], y.axis+2*adjust,pch=21,cex=.8, bg="white")
  abline(v=0, lty = 2) # draw dotted line through 0 for reference line for null sign ificance hypothesis testing
  
  

  dev.off()
  
}

##plot the other models, without axis labels

for (i in 1:length(models)){
  ##change length, one fewer coefficient
  coefs <- t(t(c(models[[i]]$coefficients[2:6,1])))
  ses <- t(t(c(models[[i]]$coefficients[2:6,2])))
  y.axis <- c(1:5)
  #fix x axis to the max values any coefficients take for comparison
  min <- (-.65)
  max <- .55
  #var.names <- c("In-group/Low Followers","Out-group/Low Followers","In-group/High Followers","Out-group/High Followers", "Log Followers")
  adjust <- 0
  ##########################
  ###change filename depending on the data source
  ##########################
  pdf(paste0(modelnames[i], "_conservative.pdf"), 5, 7)
  par(mar=c(4,1,1,1))
  
  plot(coefs[,1], y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
       xlim=c(min,max),ylim = c(.5,5.5), main = "")
  rect(min,.5,max,1.5, col = c("grey97"), border="grey90", lty = 2)
  rect(min,1.5,max,2.5, col = c("grey95"), border="grey90", lty = 2)
  rect(min,2.5,max,3.5, col = c("grey97"), border="grey90", lty = 2)
  rect(min,3.5,max,4.5, col = c("grey95"), border="grey90", lty = 2)
  rect(min,4.5,max,5.5, col = c("grey97"), border="grey90", lty = 2)
  ?axis
  axis(1, tick = T,cex.axis = .75, mgp = c(2,.7,0))
  #axis(2, at = y.axis, label = var.names, las = 1, tick = FALSE, cex.axis =.8, hadj=1)
  abline(h = y.axis, lty = 2, lwd = .5, col = "white")
  segments(coefs[,1]-qnorm(.975)*ses[,1], y.axis+2*adjust, coefs[,1]+qnorm(.975)*ses[,1], y.axis+2*adjust, lwd =  1)
  
  segments(coefs[,1]-qnorm(.95)*ses[,1], y.axis+2*adjust-.035, coefs[,1]-qnorm(.95)*ses[,1], y.axis+2*adjust+.035, lwd = .9)
  segments(coefs[,1]+qnorm(.95)*ses[,1], y.axis+2*adjust-.035, coefs[,1]+qnorm(.95)*ses[,1], y.axis+2*adjust+.035, lwd = .9)
  points(coefs[,1], y.axis+2*adjust,pch=21,cex=.8, bg="white")
  abline(v=0, lty = 2) # draw dotted line through 0 for reference line for null sign ificance hypothesis testing
  
  
  dev.off()
  
}



