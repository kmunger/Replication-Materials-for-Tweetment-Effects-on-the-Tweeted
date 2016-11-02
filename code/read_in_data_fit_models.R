


#########################################################################;
#  File-Name: read_in_data_fit_models.R			                          	#;
#  Date: September 28, 2016				                                    	#;
#  Author:	KM						                                            	#;
#  Purpose:	Read in anonymized data and run models                      #;
#########################################################################;

library(dplyr)
library(foreign)

setwd("C:/Users/kevin/Documents/GitHub/Replication-Materials-for-Tweetment-Effects-on-the-Tweeted/")

##choose which assumption--the conservative assumption file replicates the analysis in the appendix

data<-read.csv(file = "data/data_anonymized_standard_assumption.csv")

data<-read.csv(file = "data/data_anonymized_conservative_assumption.csv")


##encode treatment varaible as a factor
data$treat.f<-as.factor(data$treat.f)




#1 week time period

##first model is the full analysis

wk1_rac<-summary(lm(racism.scores.post.1wk ~ treat.f + log.followers +  racism.scores.pre.2mon, data = data))

##second model looks only at the most anonymous users

wk1_rac_anon<-summary(lm(racism.scores.post.1wk ~ treat.f  + log.followers + racism.scores.pre.2mon, 
                         data = filter(data,anonymity>1) ))

##third model looks at non-anonymous users

wk1_rac_id<-summary(lm(racism.scores.post.1wk ~ treat.f  + log.followers + racism.scores.pre.2mon, 
                       data = filter(data,anonymity<2)))



#2weeks


wk2_rac<-summary(lm(racism.scores.post.2wk ~ treat.f+ log.followers +  racism.scores.pre.2mon, data = data))

wk2_rac_anon<-summary(lm(racism.scores.post.2wk ~ treat.f  + log.followers + racism.scores.pre.2mon, 
                         data = filter(data,anonymity>1)))

wk2_rac_id<-summary(lm(racism.scores.post.2wk ~ treat.f  + log.followers + racism.scores.pre.2mon, 
                       data = filter(data,anonymity<2)))


#1month

mon1_rac<-summary(lm(racism.scores.post.1mon ~ treat.f  + log.followers + racism.scores.pre.2mon, data = data))

mon1_rac_anon<-summary(lm(racism.scores.post.1mon ~ treat.f  + log.followers + racism.scores.pre.2mon, 
                          data = filter(data,anonymity>1)))

mon1_rac_id<-summary(lm(racism.scores.post.1mon ~ treat.f  + log.followers + racism.scores.pre.2mon, 
                        data = filter(data,anonymity<2)))





#2 months
mon2_rac<-summary(lm(racism.scores.post.2mon ~ treat.f  + log.followers +  racism.scores.pre.2mon, data = data))

mon2_rac_anon<-summary(lm(racism.scores.post.2mon ~ treat.f  + log.followers + racism.scores.pre.2mon, 
                          data = filter(data,anonymity>1)))

mon2_rac_id<-summary(lm(racism.scores.post.2mon ~ treat.f  + log.followers + racism.scores.pre.2mon, 
                        data = filter(data,anonymity<2)))





