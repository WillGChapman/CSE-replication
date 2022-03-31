# Flanker
# analysis of CSE in a the flanker task replication.

setwd("~/CSE-rep-git/new_analysis")

# load in flanker data

dataDFfull <- read.csv(file="combined_flanker_data.csv")

# data clearning variables

processing_options <- list(lowRTcut = 150,
                           highRTcut = 1250,
                           performance_cut = 0.25,
                           remove_first_trials = TRUE)

# # calculate time taken by each ppt for task
# 
# time<-dataDF[,c("Participant.Private.ID","Local.Timestamp","Local.Date","Trial.Number")]
# time<-subset(time, Trial.Number== "BEGIN TASK" | Trial.Number == "END TASK")
# time<-pivot_wider(data = time, 
#                   id_cols = Participant.Private.ID, 
#                   names_from = Trial.Number, 
#                   values_from = c("Local.Timestamp","Local.Date"))
# names(time)[names(time) == "Local.Timestamp_BEGIN TASK"] <- "beginTS"
# names(time)[names(time) == "Local.Timestamp_END TASK"] <- "endTS"
# time$taken<-time$endTS-time$beginTS
# time$taken2<-format( as.POSIXct(Sys.Date())+time$taken/1000, "%M:%S") #longest time is 7 mins- so all good!
# taken<-time[,c("Participant.Private.ID","taken2")]
# 
# #add time taken to flanker data
# dataDF<-merge(dataDF, taken, by="Participant.Private.ID")
# 
# rm(list=c("taken","time"))

# filter out irrelevant rows, and select relevant columns

dataDF <- dataDFfull %>% 
  filter(Attempt==1) %>% # use just screen which actually records experiment
  filter(display!="PracticeTrial") %>%  # remove practice trials
  filter(Type!="Neutral") #%>% # remove neutral trials
  #select(Participant.Private.ID, Incorrect, Type, 
  #       Reaction.Time, Image, Trial.Number, Answer, lang)

# add previous trial congruency, and previous trial answer (left or right)

dataDF <- dataDF %>% 
  mutate(Nminus1 = lag(Type),
         posterror = lag(Incorrect),
         respminus1 = lag(Answer),
         respshift = ifelse(Answer!=respminus1, yes = "different", no = "same")) %>% 
  mutate(across(c(Type, Nminus1, posterror, respshift),as_factor))

# coerce RT, Incorrect flag and Trial number to numeric data

dataDF <- dataDF %>% 
  mutate(Reaction.Time = as.numeric(Reaction.Time),
         Trial.Number = as.numeric(Trial.Number),
         Incorrect = as.numeric(Incorrect))

# now time to remove the first trials after start or break (no N-1, for those, of course!)

if (processing_options$remove_first_trials==TRUE){
  dataDF <- dataDF %>% 
    filter(!(Trial.Number %in% c(41,81,121,161,201)))
}

# how many participants now? (polly code comments different)
length(unique(dataDF$Participant.Private.ID))

# compute accuracy scores for each ppt and remove from DF

dataDF <- dataDF %>% 
  group_by(Participant.Private.ID) %>% # group by participant
  mutate(accuracy = mean(Incorrect)) %>%  # add error rate, add to each row
  filter(accuracy <= processing_options$performance_cut)

#length(unique(dataDF$Participant.Private.ID))

# 4 removed for poor performance (fewer than polly, as 0.25 acc cutoff exact for one participant.) - changed to a value from processing_options

# remove trials with less than 150ms or more than 1250ms RT - now settable in processing_options
dataDF <- dataDF %>% 
  filter(Reaction.Time >= processing_options$lowRTcut & 
           Reaction.Time <= processing_options$highRTcut)

dataDF <- dataDF %>% ungroup() %>% 
  rename(CurrentTrial = Type,
         RT = Reaction.Time,
         Error = Incorrect,
         ParticipantID = Participant.Private.ID,
         Stimulus = Image,
         LanguageGroup = lang) %>% 
  mutate(ParticipantID = as_factor(ParticipantID),
         CurrentTrial = as_factor(CurrentTrial),
         Nminus1 = as_factor(Nminus1))

# crosstabs to check values
# xtabs(~dataDF$Answer)
# xtabs(~dataDF$Response)
# xtabs(~dataDF$Answer!=dataDF$Response)
# xtabs(~dataDF$ParticipantID+dataDF$CurrentTrial)

# final data wrangling to select only relevant variables

dataDF <- dataDF %>%
  select(ParticipantID, CurrentTrial, Stimulus, Answer, 
         Response, RT, Error, LanguageGroup, Nminus1, 
         posterror, respshift)

# exploratory plots of RT distributions
flankcurve <- ggplot(data=dataDF) 
flankcurve + 
  geom_density(aes(RT, colour = interaction(CurrentTrial, Nminus1)))+
  facet_wrap(~LanguageGroup)

flankboxplot <- ggplot(data=dataDF, mapping = aes(y=RT, colour = Nminus1))
flankboxplot+
  geom_boxplot(mapping = aes(x=Nminus1, colour = CurrentTrial))+
  facet_wrap(~LanguageGroup)

# conventional analysis, split by language group

monodata <- dataDF %>% filter(LanguageGroup=="monolingual")
bilidata <- dataDF %>% filter(LanguageGroup=="bilingual")

data_agg <- aggregate(RT ~ CurrentTrial*Nminus1*respshift*ParticipantID, 
                      mean, na.rm=T, data=monodata)

aov_RT <- aov_car(RT ~ CurrentTrial*Nminus1*respshift +
                    Error(ParticipantID/CurrentTrial*Nminus1*respshift),
                  data=data_agg)
knitr::kable(nice(aov_RT))

afex_plot(aov_RT, panel="respshift", x="Nminus1", trace="CurrentTrial", 
          error="within", data_plot=F)