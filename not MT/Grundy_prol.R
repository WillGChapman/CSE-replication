requires = c("readxl", "data.table","dplyr","ggplot2","afex", "emmeans","stringi", "tidyr","plyr","cowplot","tidyverse","bayestestR","BayesFactor","janitor")
installs = requires[!requires %in% installed.packages()[,"Package"]]
# If any are not yet installed, install them.
if(length(installs)) install.packages(installs)
# Load the packages
for (l in requires) require(l, character.only=TRUE)


#participant demographics
setwd("~/OneDrive - University of Bristol/replication of Grundy/data/demographics/ppt")
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
demo = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
head(demo,n=1)
demo<-demo[demo$Event.Index!="END OF FILE",]
demo<-demo[demo$Question.Key!="BEGIN QUESTIONNAIRE",]
### keep just the relevant columns
demo <- demo[,c("Participant.Private.ID","Question.Key", "Response",'Experiment.Version')]
#long to wide
demo <- demo %>%
  spread(Question.Key, Response)

# keep<-c("Participant.Private.ID", 'version', "Schedule.ID","%english.childhood","age","Gender","Gender.quantised", "number.of.languages","number.of.languages.quantised","mouse",
#         "mouse.quantised","drugs.alcohol.1","Self.education.quantised",  "SES.quantised", "mother.education.quantised")
# demo<-demo2[keep]
#for some reason have to reload
write.csv(demo,"demo.csv")
demo<-read.csv(file="demo.csv")
#Study information (i.e. mouse)
setwd("~/OneDrive - University of Bristol/replication of Grundy/data/demographics/study")
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
study = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
### keep just the relevant columns
study<-study[study$Event.Index!="END OF FILE",]
study<-study[study$Question.Key!="BEGIN QUESTIONNAIRE",]
study <- study[,c("Participant.Private.ID","Question.Key", "Response",'Experiment.Version')]
#long to wide
study <- study %>%
  spread(Question.Key, Response)
table(study$mouse) #30 used mouse

#Language information
setwd("~/OneDrive - University of Bristol/replication of Grundy/data/demographics/Language1")
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
Language1 = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
### keep just the relevant columns
Language1<-Language1[Language1$Question.Key!="BEGIN QUESTIONNAIRE",]
Language1<-Language1[Language1$Event.Index!="END OF FILE",]
Language1 <- Language1[,c("Participant.Private.ID","Question.Key", "Response",'Experiment.Version')]
#long to wide
Language1 <- Language1 %>%
  spread(Question.Key, Response)
setwd("C:/Users/cn19915/OneDrive - University of Bristol/replication of Grundy/data/demographics/Language2")
setwd("~/OneDrive - University of Bristol/replication of Grundy/data/demographics/Language2")
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
Language2 = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
### keep just the relevant columns
Language2<-Language2[Language2$Question.Key!="BEGIN QUESTIONNAIRE",]
head(Language2)
Language2<-Language2[Language2$Event.Index!="END OF FILE",]
Language2 <- Language2[,c("Participant.Private.ID","Question.Key", "Response",'Experiment.Version')]
#long to wide
Language2 <- Language2 %>%
  spread(Question.Key, Response)
head(Language2,n=1)
Language2$`age bilingual-quantised`[is.na(Language2$`age bilingual-quantised`)] <- 1
Language2$bilingual<-ifelse(Language2$`age bilingual-quantised`==1,0,1)
bilingual<-Language2[,c("Participant.Private.ID","bilingual")]
table(bilingual$bilingual) #27mono 18 bi
#####################


#####################
#Normal flanker
setwd("~/OneDrive - University of Bristol/replication of Grundy/data/prolific data/pilot")
setwd("C:/Users/cn19915/OneDrive - University of Bristol/replication of Grundy/data/prolific data/pilot")
files = list.files(pattern="38sw")
pilot= do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
pilot$lang<-"bilingual"
setwd("~/OneDrive - University of Bristol/replication of Grundy/data/prolific data/November")
setwd("C:/Users/cn19915/OneDrive - University of Bristol/replication of Grundy/data/prolific data/November")
files = list.files(pattern="38sw")
Nov = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
Nov$lang<-"bilingual"
setwd("~/OneDrive - University of Bristol/replication of Grundy/data/prolific data/monolingual")
setwd("C:/Users/cn19915/OneDrive - University of Bristol/replication of Grundy/data/prolific data/monolingual")
files = list.files(pattern="38sw")
mono= do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
mono$lang<-"monolingual"

Flanker_KP<-rbind(pilot,Nov,mono)
table(unique(Flanker_KP$lang))
#Makes names more managable for graphs
Flanker_KP<-Flanker_KP %>% 
  mutate(Type = str_replace(Type, "Incongruent", "Incong"))
Flanker_KP<-Flanker_KP %>% 
  mutate(Type = str_replace(Type, "Congruent", "Cong"))

#Look at how long each ppts took
time<-Flanker_KP[,c("Participant.Private.ID","Local.Timestamp","Local.Date","Trial.Number")]
time<-subset(time, Trial.Number== "BEGIN TASK" | Trial.Number == "END TASK")
time<-pivot_wider(data = time, 
                  id_cols = Participant.Private.ID, 
                  names_from = Trial.Number, 
                  values_from = c("Local.Timestamp","Local.Date"))
names(time)[names(time) == "Local.Timestamp_BEGIN TASK"] <- "beginTS"
names(time)[names(time) == "Local.Timestamp_END TASK"] <- "endTS"
time$taken<-time$endTS-time$beginTS
time$taken2<-format( as.POSIXct(Sys.Date())+time$taken/1000, "%M:%S") #longest time is 7 mins- so all good!

taken<-time[,c("Participant.Private.ID","taken2")]
#add time taken to falnker
Flanker_KP<-merge(Flanker_KP, taken, by="Participant.Private.ID")

### keep just the relevant rows
Flanker_KP<-Flanker_KP[Flanker_KP$Attempt %in% 1,]
Flanker_KP<-Flanker_KP[Flanker_KP$display!="PracticeTrial",]
Flanker_KP<-Flanker_KP[Flanker_KP$Type!="Neutral",]
head(Flanker_KP)
Flanker_KP <- Flanker_KP[,c("Participant.Private.ID","Incorrect", "Type","Reaction.Time", "Image","Trial.Number", "Answer","lang")]
### CODE VARIABLE "CONGRUENCY ON TRIAL N-1"
Nminus1 <- data.table::shift(Flanker_KP$Type, fill=NA)
tabyl(Nminus1) %>% adorn_pct_formatting() %>% knitr::kable()
Flanker_KP <- cbind(Flanker_KP, Nminus1)

### CODE VARIABLE "RESPONSEREPEAT"
respNminus1 <- data.table::shift(Flanker_KP$Answer, fill=NA)
responserepeat <- rep("Response_Repeat", dim(Flanker_KP)[1])
responserepeat[Flanker_KP$Answer != respNminus1] <- "Response_Switch"
tabyl(responserepeat) %>% adorn_pct_formatting() %>% knitr::kable()
Flanker_KP <- cbind(Flanker_KP, responserepeat)

##remove start/ 1st trials
str(Flanker_KP)
Flanker_KP$Reaction.Time<-as.numeric(Flanker_KP$Reaction.Time)
Flanker_KP$Trial.Number<-as.integer(Flanker_KP$Trial.Number)
Flanker_KP<-Flanker_KP[Flanker_KP$Trial.Number!=c(41,81,121,161,201),]

#average accuracy by ppt
Flanker_KP$Incorrect <- as.numeric(Flanker_KP$Incorrect)
av_data_ppt <- Flanker_KP %>% select(Participant.Private.ID, Incorrect, Reaction.Time) %>% group_by(Participant.Private.ID) %>% summarise_each(mean)
names(av_data_ppt)[names(av_data_ppt) == "Incorrect"] <- "Accuracy"
names(av_data_ppt)[names(av_data_ppt) == "Reaction.Time"] <- "AvRT"
Flanker_KP<-merge(Flanker_KP,av_data_ppt,by="Participant.Private.ID")
#remove bad ppts
length(unique(Flanker_KP$Participant.Private.ID))
Flanker_KP<-Flanker_KP[Flanker_KP$Accuracy<=0.25,] #(removal of 2 ppts)

#create efficiy
Flanker_KP$Accuracy2<-100-Flanker_KP$Accuracy
Flanker_KP$efficiency<-Flanker_KP$Reaction.Time/Flanker_KP$Accuracy2
#Data cleaing Cho and Kim <150 >1250 removed (18% in their data)
Flanker_KP2<-Flanker_KP[Flanker_KP$Reaction.Time>=150,]  
100-(2539/2539*100) #0% removal
Flanker_KP2<-Flanker_KP2[Flanker_KP2$Reaction.Time<=1250,]
100-(2493/2539*100)#1.81% removal 
100-(2493/2539*100)#1.81% removal overall
head(Flanker_KP2,n=1)

#why such large variation in incong response repeat?
Flanker_KP4<-Flanker_KP2
boxplot(Reaction.Time~Type+Nminus1,data=Flanker_KP4)
out<-boxplot(Reaction.Time~Type+Nminus1,data=Flanker_KP4)$out
Flanker_KP4<- Flanker_KP4[-which(Flanker_KP4$Reaction.Time %in% out),]
Flanker_KP3<-Flanker_KP2
#do analysis
Flanker_KP2$Participant.Private.ID <- paste("ppt", Flanker_KP2$Participant.Private.ID, sep="_")
Flanker_KP2$Participant.Private.ID<-as.factor(Flanker_KP2$Participant.Private.ID)
Flanker_KP2$Type<-as.factor(Flanker_KP2$Type)
Flanker_KP2$Nminus1<-as.factor(Flanker_KP2$Nminus1)
Flanker_KP2$lang<-as.factor(Flanker_KP2$lang)
Flanker_KP2$responserepeat<-as.factor(Flanker_KP2$responserepeat)
Flanker_KP2$Reaction.Time<-as.numeric(Flanker_KP2$Reaction.Time)
str(Flanker_KP2)
 
Flanker_KP3<-Flanker_KP
Flanker_KP<-Flanker_KP3
options(scipen=999)
BFFlanker_error <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*responserepeat*Type+Participant.Private.ID, data=Flanker_KP2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
#attempt at4 way
round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*responserepeat*Type*lang+Participant.Private.ID, data=Flanker_KP2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
# Participant.Private.ID               1.00         1.00             
# Type                                 0.11         0.00     9.79e+38
# lang                                 0.11         0.23        0.372
# lang:Type                            0.30         0.13        0.502
# Nminus1                              0.11         0.00     1.48e+08
# Nminus1:Type                         0.30         0.10     5.48e+11
# lang:Nminus1                         0.30         0.03        0.093
# lang:Nminus1:Type                    0.11         0.01         1.11
# responserepeat                       0.11         0.00        0.023
# responserepeat:Type                  0.30         0.02        11.82
# lang:responserepeat                  0.30         0.01        0.031
# lang:responserepeat:Type             0.11         0.00        0.167
# Nminus1:responserepeat               0.30         0.00        0.091
# Nminus1:responserepeat:Type          0.11         0.90       497.70
# lang:Nminus1:responserepeat          0.11         0.00        0.064
# lang:Nminus1:responserepeat:Type     0.01         0.00        0.092

BFFlanker_error$BF<-exp(BFFlanker_error$log_BF)
BFFlanker_error$BF <- format(round(BFFlanker_error$BF, 2), nsmall = 2)
BFFlanker_error$BF<-ifelse(BFFlanker_error$BF > 100,">100",BFFlanker_error$BF)

plot_error<- aov_car(Incorrect ~ Nminus1*responserepeat*Type+ Error(Participant.Private.ID/Nminus1*responserepeat*Type), 
                     anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP2)

### EXCLUDE TRIALS WITH ERRORS FOR SUBSEQUENT ANALYSES
Flanker_KP3 <- Flanker_KP2[Flanker_KP2$Incorrect==0,]
BFFlanker_RT <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*responserepeat*Type+Participant.Private.ID, data=Flanker_KP3, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFFlanker_RT$BF<-exp(BFFlanker_RT$log_BF)
BFFlanker_RT$BF <- format(round(BFFlanker_RT$BF, 2), nsmall = 2)
BFFlanker_RT$BF<-ifelse(BFFlanker_RT$BF>100,">100",BFFlanker_RT$BF)
str(BFFlanker_RT)

plot_RT<- aov_car(Reaction.Time ~ Nminus1*responserepeat*Type+ Error(Participant.Private.ID/Nminus1*responserepeat*Type), 
                  anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP3)
BF_err <- BFFlanker_error$BF
BF_err[4] <- ">100"
BF_err[3] <- ">100"
BF_RT <- BFFlanker_RT$BF
BF_RT [4] <- ">100"
BF_RT [8] <- ">100"

plot_Flanker_err<-afex_plot(plot_error,x="Nminus1",trace = "Type",panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_err[2],"\nNminus1*trial: bf", BF_err[4],"\nNminus1*trial*responseRep bf",BF_err[8]),
       y="Errors")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="bottom")

plot_Flanker_RT<-afex_plot(plot_RT,x="Nminus1",trace = "Type",panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT[2],"\nNminus1*trial: bf", BF_RT[4],"\nNminus1*trial*responseRep bf",BF_RT[8]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="none")

plot_grid(plot_Flanker_err, plot_Flanker_RT,
          labels = c("Flanker_accuracy", "Flanker_RT"),
          label_x = 0.4, label_y = 1,
          ncol = 2, nrow = 1)

#efficacy
BFFlanker_eff <-round(bayesfactor_inclusion(anovaBF(efficiency ~ Nminus1*responserepeat*Type+Participant.Private.ID, data=Flanker_KP2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
#attempt at 4way
round(bayesfactor_inclusion(anovaBF(efficiency ~ Nminus1*responserepeat*Type*lang+Participant.Private.ID, data=Flanker_KP2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)

#carrying on with 3 way
BFFlanker_eff$BF<-exp(BFFlanker_eff$log_BF)
BFFlanker_eff$BF <- format(round(BFFlanker_eff$BF, 2), nsmall = 2)
BFFlanker_eff$BF<-ifelse(BFFlanker_eff$BF>100,">100",BFFlanker_eff$BF)
plot_eff<- aov_car(efficiency ~ Nminus1*responserepeat*Type+ Error(Participant.Private.ID/Nminus1*responserepeat*Type), 
                     anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP)
BF_eff <- BFFlanker_eff$BF
BF_eff [8] <- ">100"
BF_eff [4] <- ">100"
BF_eff [5] <- ">100"
plot_Flanker_eff<-afex_plot(plot_eff,x="Nminus1",trace = "Type",panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_eff[2],"\nNminus1*trial: bf", BF_eff[4],"\nNminus1*trial*responseRep bf",BF_eff[8]),
       y="efficiency (RT/average correct)")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="bottom")
plot_Flanker_eff


#Look at response repeat and switch seperately to unpick interaction
head(Flanker_KP)
Flanker_KP_Repeat<-Flanker_KP[Flanker_KP$responserepeat=="Response_Repeat",]
Flanker_KP_Switch<-Flanker_KP[Flanker_KP$responserepeat=="Response_Switch",]
#MONO
BFFlanker_error_Repeat <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*Type+Participant.Private.ID, data=Flanker_KP_Repeat, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFFlanker_error_Repeat$BF<-ifelse(BFFlanker_error_Repeat$log_BF>100,">100",BFFlanker_error_Repeat$log_BF)
plot_error_Repeat<- aov_car(Incorrect ~ Nminus1*Type+ Error(Participant.Private.ID/Nminus1*Type), 
                            anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP_Repeat)
#BI
BFFlanker_error_switch <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*Type+Participant.Private.ID, data=Flanker_KP_Switch, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFFlanker_error_switch$BF<-ifelse(BFFlanker_error_switch$log_BF>100,">100",BFFlanker_error_switch$log_BF)
plot_error_switch<- aov_car(Incorrect ~ Nminus1*Type+ Error(Participant.Private.ID/Nminus1*Type), 
                            anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP_Switch)
BF_err_Repeat <- BFFlanker_error_Repeat$BF
BF_err_Switch <- BFFlanker_error_switch$BF
### EXCLUDE TRIALS WITH ERRORS FOR SUBSEQUENT ANALYSES
#Mono
Flanker_KP_Repeat2 <- Flanker_KP_Repeat[Flanker_KP_Repeat$Incorrect==0,]
BFFlanker_RT_Repeat <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*Type+Participant.Private.ID, data=Flanker_KP_Repeat2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFFlanker_RT_Repeat$log_BF<-ifelse(BFFlanker_RT_Repeat$BF>100,">100",BFFlanker_RT_Repeat$log_BF)
plot_RT_Repeat<- aov_car(Reaction.Time ~ Nminus1*Type+ Error(Participant.Private.ID/Nminus1*Type), 
                         anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP_Repeat2)
#bi
Flanker_KP_Switch2 <- Flanker_KP_Switch[Flanker_KP_Switch$Incorrect==0,]
BFFlanker_RT_switch <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*Type+Participant.Private.ID, data=Flanker_KP_Switch2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFFlanker_RT_switch$BF<-ifelse(BFFlanker_RT_switch$log_BF>100,">100",BFFlanker_RT_switch$log_BF)
plot_RT_switch<- aov_car(Reaction.Time ~ Nminus1*Type+ Error(Participant.Private.ID/Nminus1*Type), 
                         anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP_Switch2)
BF_RT_Repeat <- BFFlanker_RT_Repeat$BF
BF_RT_Switch <- BFFlanker_RT_switch$BF

plot_Flanker_err_Repeat<-afex_plot(plot_error_Repeat,x="Nminus1",trace = "Type", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_err_Repeat[2],"\nNminus1: bf", BF_err_Repeat[3],"\nNminus1*trial: bf", BF_err_Repeat[4]),
       y="Errors")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="bottom")

plot_Flanker_err_switch<-afex_plot(plot_error_switch,x="Nminus1",trace = "Type", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_err_Switch[2],"\nNminus1: bf", BF_err_Switch[3],"\nNminus1*trial: bf", BF_err_Switch[4]),
       y="Errors")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="bottom")

plot_Flanker_RT_Repeat<-afex_plot(plot_RT_Repeat,x="Nminus1",trace = "Type", error="within", mapping = c("linetype","color"), data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf", BF_RT_Repeat[2],"\nNminus1: bf",  BF_RT_Repeat[3],"\nNminus1*trial: bf",  BF_RT_Repeat[4]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="none")

plot_Flanker_RT_switch<-afex_plot(plot_RT_switch,x="Nminus1",trace = "Type", error="within", mapping = c("linetype","color"), data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT_Switch[2],"\nNminus1: bf", BF_RT_Switch[3],"\nNminus1*trial: bf", BF_RT_Switch[4]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="none")

plot_grid(plot_Flanker_err_Repeat, plot_Flanker_err_switch,
          labels = c("Flanker_Repeat_err", "Flanker_switch_err"),
          label_x = 0.4, label_y = 1,
          ncol = 2, nrow = 1)

plot_grid(plot_Flanker_err_Repeat, plot_Flanker_err_switch,plot_Flanker_RT_Repeat, plot_Flanker_RT_switch,
          labels = c("Flanker_Repeat_err", "Flanker_switch_err","Flanker_Repeat_rt", "Flanker_switch_rt"),
          label_x = 0.3, label_y = 1.01,
          ncol = 2, nrow = 2)



#####################
#Simon
setwd("C:/Users/cn19915/OneDrive - University of Bristol/replication of Grundy/data/prolific data/pilot")
files = list.files(pattern="*95tf")
pilot= do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
pilot$lang<-"bilingual"
setwd("C:/Users/cn19915/OneDrive - University of Bristol/replication of Grundy/data/prolific data/November")
files = list.files(pattern="*95tf")
Nov = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
Nov$lang<-"bilingual"
setwd("C:/Users/cn19915/OneDrive - University of Bristol/replication of Grundy/data/prolific data/monolingual")
files = list.files(pattern="*95tf")
Simon_mono= do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
Simon_mono$lang<-"monolingual"


Simon <- rbind(pilot,Nov,Simon_mono)
Simon_bi<-rbind(pilot,Nov)

#Makes names more managable for graphs
Simon<-Simon %>% 
  mutate(Type = str_replace(Type, "Incongruent", "Incong"))
Simon<-Simon %>% 
  mutate(Type = str_replace(Type, "Congruent", "Cong"))
#Look at how long each ppts took
time<-Simon[,c("Participant.Private.ID","Local.Timestamp","Local.Date","Trial.Number")]
time<-subset(time, Trial.Number== "BEGIN TASK" | Trial.Number == "END TASK")
time<-pivot_wider(data = time, 
                  id_cols = Participant.Private.ID, 
                  names_from = Trial.Number, 
                  values_from = c("Local.Timestamp","Local.Date"))
names(time)[names(time) == "Local.Timestamp_BEGIN TASK"] <- "beginTS"
names(time)[names(time) == "Local.Timestamp_END TASK"] <- "endTS"
time$taken<-time$endTS-time$beginTS
time$taken2<-format( as.POSIXct(Sys.Date())+time$taken/1000, "%M:%S") #no ppts over 5 mins all good
taken<-time[,c("Participant.Private.ID","taken2")]
#add time taken to colour
Simon<-merge(Simon, taken, by="Participant.Private.ID")

### keep just the relevant rows
Simon<-Simon[Simon$Attempt %in% 1,]
Simon<-Simon[Simon$display!="PracticeTrial",]
Simon<-Simon[Simon$Type!="Neutral",]
head(Simon)
Simon <- Simon[,c("Participant.Private.ID","Incorrect", "Type","Reaction.Time", "Image", "Trial.Number","Answer","lang")]
Simon$Trial.Number<-as.integer(Simon$Trial.Number)
Simon <- Simon %>% arrange(Participant.Private.ID, Trial.Number)

### CODE VARIABLE "CONGRUENCY ON TRIAL N-1"
Nminus1 <- data.table::shift(Simon$Type, fill=NA)
tabyl(Nminus1) %>% adorn_pct_formatting() %>% knitr::kable()
Simon <- cbind(Simon, Nminus1)

### CODE VARIABLE "RESPONSEREPEAT"
respNminus1 <- data.table::shift(Simon$Answer, fill=NA)
responserepeat <- rep("Response_Repeat", dim(Simon)[1])
responserepeat[Simon$Answer != respNminus1] <- "Response_Switch"
tabyl(responserepeat) %>% adorn_pct_formatting() %>% knitr::kable()
Simon <- cbind(Simon, responserepeat)

##remove start/ 1st trials
Simon$Trial.Number<-as.integer(Simon$Trial.Number)
Simon<-Simon[Simon$Trial.Number!=c(41,81,121,161),]
#average accuracy by ppt
Simon$Incorrect <- as.numeric(Simon$Incorrect)
av_data_ppt <- Simon %>% select(Participant.Private.ID, Incorrect, Reaction.Time) %>% group_by(Participant.Private.ID) %>% summarise_each(mean)
names(av_data_ppt)[names(av_data_ppt) == "Incorrect"] <- "Accuracy"
names(av_data_ppt)[names(av_data_ppt) == "Reaction.Time"] <- "AvRT"
Simon<-merge(Simon,av_data_ppt,by="Participant.Private.ID")
#remove bad ppts
length(unique(Simon$Participant.Private.ID))
Simon<-Simon[Simon$Accuracy<=0.25,] #(removal of 0 ppts)

#Data cleaing Cho and Kim <150 >1250 removed (18% in their data)
Simon<-Simon[Simon$Reaction.Time>=150,]  
100-(2861/2862*100) #0.03% removal
Simon<-Simon[Simon$Reaction.Time<=1250,]
100-(2833/2861*100)#0.98% removal 
100-(2833/2862*100)#0.01% removal overall

#create efficiy
Simon$Accuracy2<-100-Simon$Accuracy
Simon$efficiency<-Simon$Reaction.Time/Simon$Accuracy2

#do analysis
Simon$Participant.Private.ID<-as.factor(Simon$Participant.Private.ID)
Simon$responserepeat<-as.factor(Simon$responserepeat)
Simon$Nminus1<-as.factor(Simon$Nminus1)
Simon$Type<-as.factor(Simon$Type)
Simon$lang<-as.factor(Simon$lang)
str(Simon)

#attempting 4 way
BFsimon_error <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*Type*responserepeat*lang+Participant.Private.ID, data=Simon, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_error$BF<-exp(BFsimon_error$log_BF)
BFsimon_error$BF <- format(round(BFsimon_error$BF, 2), nsmall = 2)
BFsimon_error$BF<-ifelse(BFsimon_error$BF>100,">100",BFsimon_error$BF)
BF_err <- BFsimon_error$BF
BF_err[2]<- ">100"
BF_err[6]<- ">100"
BF_err[11]<- ">100"
BF_err[14]<- ">100"
BF_err[16]<- "0.07"
#BF_err[5]<- ">100"

#3way mono
Simon_mon_err<-Simon[Simon$lang=="monolingual",]
BFsimon_error_mon <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*Type*responserepeat+Participant.Private.ID, data=Simon_mon_err, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_error_mon$BF<-exp(BFsimon_error_mon$log_BF)
BFsimon_error_mon$BF <- format(round(BFsimon_error_mon$BF, 2), nsmall = 2)
BFsimon_error_mon$BF<-ifelse(BFsimon_error_mon$BF>100,">100",BFsimon_error_mon$BF)
BF_err_mon <- BFsimon_error_mon$BF

plot_error_mon<- aov_car(Incorrect ~ Nminus1*Type*responserepeat+ Error(Participant.Private.ID/Nminus1*Type*responserepeat), 
                         anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon_mon_err)

#3way bi
Simon_bi_err<-Simon[Simon$lang=="bilingual",]
BFsimon_error_bi <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*Type*responserepeat+Participant.Private.ID, data=Simon_bi_err, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_error_bi$BF<-exp(BFsimon_error_bi$log_BF)
BFsimon_error_bi$BF <- format(round(BFsimon_error_bi$BF, 2), nsmall = 2)
BFsimon_error_bi$BF<-ifelse(BFsimon_error_bi$BF>100,">100",BFsimon_error_bi$BF)
BF_err_bi <- BFsimon_error_bi$BF

plot_error_bi<- aov_car(Incorrect ~ Nminus1*Type*responserepeat+ Error(Participant.Private.ID/Nminus1*Type*responserepeat), 
                     anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon_bi_err)

### EXCLUDE TRIALS WITH ERRORS FOR SUBSEQUENT ANALYSES
Simon2 <- Simon[Simon$Incorrect==0,]
#attempting 4 way
BFsimon_RT <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*Type*responserepeat*lang+Participant.Private.ID, data=Simon2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_RT$BF<-exp(BFsimon_RT$log_BF)
BFsimon_RT$BF <- format(round(BFsimon_RT$BF, 2), nsmall = 2)
BFsimon_RT$BF<-ifelse(BFsimon_RT$BF>100,">100",BFsimon_RT$BF)
BF_RT <- BFsimon_RT$BF
BF_RT[2]<- ">100"
BF_RT[3]<- ">100"
BF_RT[6]<- ">100"
BF_RT[11]<- ">100"
BF_RT[13]<- ">100"
BF_RT[14]<- ">100"
BF_RT[16]<- "0.08"

##3way mono RT
Simon_mon_rt<-Simon2[Simon2$lang=="monolingual",]
BFsimon_RT_mono <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*Type*responserepeat+Participant.Private.ID, data=Simon_mon_rt, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_RT_mono$BF<-exp(BFsimon_RT_mono$log_BF)
BFsimon_RT_mono$BF <- format(round(BFsimon_RT_mono$BF, 2), nsmall = 2)
BFsimon_RT_mono$BF<-ifelse(BFsimon_RT_mono$BF>100,">100",BFsimon_RT_mono$BF)
BF_RT_mono <- BFsimon_RT_mono$BF


plot_RT_mon<- aov_car(Reaction.Time ~ Nminus1*Type*responserepeat+ Error(Participant.Private.ID/Nminus1*Type*responserepeat), 
                      anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon_mon_rt)



##3way bi RT
Simon_bi_rt<-Simon2[Simon2$lang=="bilingual",]
BFsimon_RT_bi <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*Type*responserepeat+Participant.Private.ID, data=Simon_bi_rt, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_RT_bi$BF<-exp(BFsimon_RT_bi$log_BF)
BFsimon_RT_bi$BF <- format(round(BFsimon_RT_bi$BF, 2), nsmall = 2)
BFsimon_RT_bi$BF<-ifelse(BFsimon_RT_bi$BF>100,">100",BFsimon_RT_bi$BF)
BF_RT_bi <- BFsimon_RT_bi$BF
BF_RT_bi[2]<- ">100"
BF_RT_bi[4]<- ">100"
BF_RT_bi[8]<- ">100"


plot_RT_bi<- aov_car(Reaction.Time ~ Nminus1*Type*responserepeat+ Error(Participant.Private.ID/Nminus1*Type*responserepeat), 
                  anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon_bi_rt)


plot_Simon_err<-afex_plot(plot_error,x="Nminus1",trace = "Type", panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_err[2],"\nNminus1*trial: bf", BF_err[6],"\nNminus1*trial*responseRep bf",BF_err[8]),
       y="Errors")+theme(legend.position="none")+theme(legend.position= "bottom")+scale_color_manual(values=c("blue","red"))

#seperate plots for mono bi
plot_Simon_err_mon<-afex_plot(plot_error_mon,x="Nminus1",trace = "Type", panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_err_mon[2],"\nNminus1*trial: bf", BF_err_mon[4],"\nNminus1*trial*responseRep bf",BF_err_mon[8],"\nNminus1*trial*responseRep*lang bf",BF_err[16]),
       y="Errors")+theme(legend.position="none")+theme(legend.position= "bottom")+scale_color_manual(values=c("blue","red"))
plot_Simon_err_bi<-afex_plot(plot_error_bi,x="Nminus1",trace = "Type", panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_err_bi[2],"\nNminus1*trial: bf",BF_err_bi[4],"\nNminus1*trial*responseRep bf",BF_err_bi[8],"\nNminus1*trial*responseRep*lang bf",BF_err[16]),
       y="Errors")+theme(legend.position="none")+theme(legend.position= "bottom")+scale_color_manual(values=c("blue","red"))



plot_Simon_RT<-afex_plot(plot_RT,x="Nminus1",trace = "Type",panel = "responserepeat",  error="within", mapping = c("linetype","color"), data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT[2],"\nNminus1*trial: bf", BF_RT[4],"\nNminus1*trial*responseRep bf",BF_RT[8]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+theme(legend.position="none")

#seperate plots for mono bi
plot_Simon_RT_mon<-afex_plot(plot_RT_mon,x="Nminus1",trace = "Type",panel = "responserepeat",  error="within", mapping = c("linetype","color"), data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT_mono[2],"\nNminus1*trial: bf", BF_RT_mono[4],"\nNminus1*trial*responseRep bf",BF_RT_mono[8],"\nNminus1*trial*responseRep*lang bf",BF_RT[16]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+theme(legend.position="none")
plot_Simon_RT_bi<-afex_plot(plot_RT_bi,x="Nminus1",trace = "Type",panel = "responserepeat",  error="within", mapping = c("linetype","color"), data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT_bi[2],"\nNminus1*trial: bf", BF_RT_bi[4],"\nNminus1*trial*responseRep bf",BF_RT_bi[8],"\nNminus1*trial*responseRep*lang bf",BF_RT[16]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+theme(legend.position="none")


plot_grid(plot_Simon_err_bi, plot_Simon_err_mon,
          labels = c("Simon_err_bi", "Simon_err_mon"),
          label_x = 0.4, label_y = 1,
          ncol = 2, nrow = 1)
plot_grid(plot_Simon_RT_bi, plot_Simon_RT_mon,
             labels = c("Simon_RT_bi", "Simon_RT_mon"),
             label_x = 0.4, label_y = 1,
             ncol = 2, nrow = 1)

BFsimon_eff <-round(bayesfactor_inclusion(anovaBF(efficiency ~ Nminus1*responserepeat*Type+Participant.Private.ID, data=Simon, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_eff$BF<-exp(BFsimon_eff$log_BF)
BFsimon_eff$BF <- format(round(BFsimon_eff$BF, 2), nsmall = 2)
BFsimon_eff$BF<-ifelse(BFsimon_eff$BF>100,">100",BFsimon_eff$BF)
plot_eff_Si<- aov_car(efficiency ~ Nminus1*responserepeat*Type+ Error(Participant.Private.ID/Nminus1*responserepeat*Type), 
                   anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon)
BF_eff__Si <- BFsimon_eff$BF
BF_eff__Si[2]<- ">100"
BF_eff__Si[4]<- ">100"
BF_eff__Si[5]<- ">100"
BF_eff__Si[8]<- ">100"
BF_eff__Si
plot_Si_eff<-afex_plot(plot_eff_Si,x="Nminus1",trace = "Type",panel = "responserepeat", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency")+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_eff__Si[2],"\nNminus1*trial: bf", BF_eff__Si[4],"\nNminus1*trial*responseRep bf",BF_eff__Si[8]),
       y="efficiency (RT/average correct)")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="bottom")
plot_Si_eff

############all together

plot_grid(plot_Flanker_err, plot_Flanker_RT,plot_Simon_err, plot_Simon_RT, 
          labels = c("Flanker", "Flanker","Simon", "Simon"),
          label_size = 10,
          label_x = 0.4, label_y = 1,
          ncol = 2, nrow = 2)

plot_grid(plot_Flanker_err,plot_Simon_err, plot_Flanker_RT, plot_Simon_RT, 
          labels = c("Colour", "Flanker","Simon","Colour", "Flanker","Simon"),
          label_size = 10,
          label_x = 0.45, label_y = 1.021,
          ncol = 3, nrow = 2)
plot_grid(plot_Flanker_err,plot_Simon_err,  plot_Flanker_RT, plot_Simon_RT, 
          labels = c( "Flanker","Simon", "Flanker","Simon"),
          label_size = 10,
          label_x = 0.45, label_y = 1.021,
          ncol = 2, nrow = 2)

theme_set(theme_classic() +
            theme(
              legend.position="none",
              axis.line = element_line(colour = "black"),
              axis.ticks = element_line(colour = "black"),
              axis.text = element_text(colour = "black", size = 10),
              axis.title = element_text(size = 10),
              panel.border = element_rect(colour = "black", fill=NA)
            ))

#removing response repeat
#Flanker
BFFlanker_error <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*Type+Participant.Private.ID, data=Flanker_KP, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFFlanker_error$BF<-ifelse(BFFlanker_error$BF>100,">100",BFFlanker_error$BF)
plot_error<- aov_car(Incorrect ~ Nminus1*Type+ Error(Participant.Private.ID/Nminus1*Type), 
                     anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP)
### EXCLUDE TRIALS WITH ERRORS FOR SUBSEQUENT ANALYSES
Flanker_KP2 <- Flanker_KP[Flanker_KP$Incorrect==0,]
BFFlanker_RT <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*Type+Participant.Private.ID, data=Flanker_KP2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFFlanker_RT$BF<-ifelse(BFFlanker_RT$BF>100,">100",BFFlanker_RT$BF)
plot_RT<- aov_car(Reaction.Time ~ Nminus1*Type+ Error(Participant.Private.ID/Nminus1*Type), 
                  anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Flanker_KP2)
BF_err <- BFFlanker_error$BF
BF_RT <- BFFlanker_RT$BF

plot_Flanker_err<-afex_plot(plot_error,x="Nminus1",trace = "Type", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency", dodge = 0.0)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT[2],"\nNminus1*trial: bf", BF_RT[4]),
       y="Errors")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="bottom")

plot_Flanker_RT<-afex_plot(plot_RT,x="Nminus1",trace = "Type", error="within", mapping = c("linetype","color"), dodge = 0.0, data_plot=FALSE)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT[2],"\nNminus1*trial: bf", BF_RT[4]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+
  theme(legend.position="none")

#Simon
BFsimon_error <-round(bayesfactor_inclusion(anovaBF(Incorrect ~ Nminus1*Type+Participant.Private.ID, data=Simon, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_error$BF<-ifelse(BFsimon_error$BF>100,">100",BFsimon_error$BF)
plot_error<- aov_car(Incorrect ~ Nminus1*Type+ Error(Participant.Private.ID/Nminus1*Type), 
                     anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon)
### EXCLUDE TRIALS WITH ERRORS FOR SUBSEQUENT ANALYSES
Simon2 <- Simon[Simon$Incorrect==0,]
BFsimon_RT <-round(bayesfactor_inclusion(anovaBF(Reaction.Time ~ Nminus1*Type+Participant.Private.ID, data=Simon2, whichRandom="Participant.Private.ID"), match_models=TRUE),digit=2)
BFsimon_RT$BF<-ifelse(BFsimon_RT$BF>100,">100",BFsimon_RT$BF)
plot_RT<- aov_car(Reaction.Time ~ Nminus1*Type+ Error(Participant.Private.ID/Nminus1*Type), 
                  anova_table = list(es = "pes", correction="none"), fun=mean,na.rm=T, data=Simon2)
BF_err <- BFsimon_error$BF
BF_RT <- BFsimon_RT$BF

plot_Simon_err<-afex_plot(plot_error,x="Nminus1",trace = "Type", error="within", mapping = c("linetype","color"), data_plot=FALSE,legend_title = "Trial Congruency", dodge = 0.0)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT[2],"\nNminus1*trial: bf", BF_RT[4]),
       y="Errors")+theme(legend.position="none")+theme(legend.position= "bottom")+scale_color_manual(values=c("blue","red"))

plot_Simon_RT<-afex_plot(plot_RT,x="Nminus1",trace = "Type",  error="within", mapping = c("linetype","color"), data_plot=FALSE, dodge = 0.0)+
  labs(x=paste("Congruency Trial N-1","\n\n Trial bf",BF_RT[2],"\nNminus1*trial: bf", BF_RT[4]),
       y="Reaction time")+scale_linetype_manual(values=c("twodash", "solid"))+scale_color_manual(values=c("blue","red"))+theme(legend.position="none")
