#mono vs bi
Simon_M<-Simon[Simon$bilingual==0,]
Simon_B<-Simon[Simon$bilingual==1,]
#MONO
BFSimon_error_mono <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*responserepeat*Type+Participant.Private.ID, data=Simon_M, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFSimon_error_mono$BF<-ifelse(BFSimon_error_mono$BF>100,">100",BFSimon_error_mono$BF)
plot_error_mono<- aov_car(Incorrect ~ Nminus1*responserepeat*Type+ Error(Participant.Private.ID/Nminus1*responserepeat*Type), 
                          anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon_M)
#BI
BFSimon_error_bi <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*responserepeat*Type+Participant.Private.ID, data=Simon_B, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFSimon_error_bi$BF<-ifelse(BFSimon_error_bi$BF>100,">100",BFSimon_error_bi$BF)
plot_error_bi<- aov_car(Incorrect ~ Nminus1*responserepeat*Type+ Error(Participant.Private.ID/Nminus1*responserepeat*Type), 
                        anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon_B)
BF_err_M <- BFSimon_error_mono$BF
BF_err_B <- BFSimon_error_bi$BF
### EXCLUDE TRIALS WITH ERRORS FOR SUBSEQUENT ANALYSES
#Mono
Simon_M2 <- Simon_M[Simon_M$Incorrect==0,]
BFSimon_RT_mono <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*responserepeat*Type+Participant.Private.ID, data=Simon_M2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFSimon_RT_mono$BF<-ifelse(BFSimon_RT_mono$BF>100,">100",BFSimon_RT_mono$BF)
plot_RT_mono<- aov_car(Reaction.Time ~ Nminus1*responserepeat*Type+ Error(Participant.Private.ID/Nminus1*responserepeat*Type), 
                       anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon_M2)
#bi
Simon_B2 <- Simon_B[Simon_B$Incorrect==0,]
BFSimon_RT_bi <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*responserepeat*Type+Participant.Private.ID, data=Simon_B2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFSimon_RT_bi$BF<-ifelse(BFSimon_RT_bi$BF>100,">100",BFSimon_RT_bi$BF)
plot_RT_bi<- aov_car(Reaction.Time ~ Nminus1*responserepeat*Type+ Error(Participant.Private.ID/Nminus1*responserepeat*Type), 
                     anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon_B2)
BF_RT_M <- BFSimon_RT_mono$BF
BF_RT_B <- BFSimon_RT_bi$BF

plot_Simon_err_mono<-afex_plot(plot_error_mono,x="Nminus1",trace = "Type",panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_err_M[2],"\nNminus1: bf", BF_err_M[3],"\nNminus1*trial: bf", BF_err_M[4],"\nNminus1*trial*responseRep bf",BF_err_M[8]),
       y="Accuracy")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="bottom")

plot_Simon_err_bi<-afex_plot(plot_error_bi,x="Nminus1",trace = "Type",panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_err_B[2],"\nNminus1: bf", BF_err_B[3],"\nNminus1*trial: bf", BF_err_B[4],"\nNminus1*trial*responseRep bf",BF_err_B[8]),
       y="Accuracy")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="bottom")

plot_Simon_RT_mono<-afex_plot(plot_RT_mono,x="Nminus1",trace = "Type",panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf", BF_RT_M[2],"\nNminus1: bf",  BF_RT_M[3],"\nNminus1*trial: bf",  BF_RT_M[4],"\nNminus1*trial*responseRep bf", BF_RT_M[8]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="none")


plot_Simon_RT_bi<-afex_plot(plot_RT_bi,x="Nminus1",trace = "Type",panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT_B[2],"\nNminus1: bf", BF_RT_B[3],"\nNminus1*trial: bf", BF_RT_B[4],"\nNminus1*trial*responseRep bf",BF_RT_B[8]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="none")

plot_grid(plot_Simon_err_mono, plot_Simon_err_bi,
          labels = c("Simon_mono_err", "Simon_bi_err"),
          label_x = 0.4, label_y = 1,
          ncol = 2, nrow = 1)

plot_grid(plot_Simon_err_mono, plot_Simon_err_bi,plot_Simon_RT_mono, plot_Simon_RT_bi,
          labels = c("Simon_mono_err", "Simon_bi_err","Simon_mono_rt", "Simon_bi_rt"),
          label_x = 0.3, label_y = 1.01,
          ncol = 2, nrow = 2)
