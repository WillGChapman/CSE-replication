library(afex)
library(emmeans)
library(janitor)
library(ggplot2)
library(BayesFactor)
library(bayestestR)

data <- read.csv("data_exp_71111-v3_task-38sw.csv", header=T, stringsAsFactors=FALSE)
str(data)

data <- Flanker_KP

names(data)[names(data)=="Type"] <- "congruency"
names(data)[names(data)=="Reaction.Time"] <- "RT"
names(data)[names(data)=="Incorrect"] <- "Error"
names(data)[names(data)=="Participant.Private.ID"] <- "ParticipantID"
names(data)[names(data)=="Image"] <- "Stimulus"

data$ParticipantID <- as.factor(data$ParticipantID)
data$congruency <- as.factor(data$congruency)

### how many participants in this data set?
length(levels(as.factor(data$ParticipantID)))

### only retain lines which required a keyboard response
data <- data[data$Zone.Type=="response_keyboard",]

### delete practice trials
data <- data[data$display=="Trials",]

### variable "Answer" shows the correct (required) response
xtabs(~data$Answer)

### variable "Response" shows the actual response
xtabs(~data$Response)

### cases where the two differ are errors
xtabs(~data$Answer!=data$Response)

### show conditions per subject - all looks correct
xtabs(~data$ParticipantID+data$congruency)

### retain only the relevant columns
data <- data[,c("ParticipantID", "congruency", "Stimulus", "Answer", "Response", "RT", "Error", "lang")]
data$congruency <- droplevels(data$congruency)
data$RT <- as.numeric(data$RT)

str(data)

### overall RT by subject - all look ok
aggregate(RT ~ ParticipantID, mean, na.rm=T, data=data)

### overall error percentages by subject - one definitely looks dodgy w. 44% errors:
errorrates <- aggregate(Error ~ ParticipantID, mean, na.rm=T, data=data)

badppts <- errorrates %>% as_tibble() %>% arrange(desc(Error))

### take out
data <- subset(data, !(data$ParticipantID %in% badppts$ParticipantID[1:3]))

### remaining RT and error rate looks ok

mean(data$RT)
mean(100*data$Error)


### rudimentary trimming

### very few RTs above 1.5 sec - delete
tabyl(data$RT>1500) %>% adorn_pct_formatting() %>% knitr::kable()
data$RT[data$RT > 1500] <- NA

### very few RTs below 150 ms - delete
tabyl(data$RT<150) %>% adorn_pct_formatting() %>% knitr::kable()
data$RT[data$RT < 150] <- NA

hist(data$RT)

alldata <- data


#revert to mono vs bilingual

monodata <- alldata %>% filter(lang == "monolingual")
bilindata <- alldata %>% filter(lang == "bilingual")

data <- bilindata

### quick initial check

aov_RT <- aov_car(RT ~ congruency + Error(ParticipantID/congruency),
                  fun=mean, na.rm=T, data=data)
knitr::kable(nice(aov_RT))
emmeans(aov_RT, ~congruency)
pairs(emmeans(aov_RT, ~congruency))

aov_Error <- aov_car(100*Error ~ congruency + Error(ParticipantID/congruency),
                     fun=mean, na.rm=T, data=data)
knitr::kable(nice(aov_Error))
emmeans(aov_Error, ~congruency)
pairs(emmeans(aov_Error, ~congruency))

### delete neutral condition
data <- subset(data, data$congruency != "Neutral")
data$congruency <- droplevels(data$congruency)

### code congruency N-1
data$congruencyNminus1 <- data.table::shift(data$congruency)

### code whether response on trial N and N-1 is the same
data$responserepeat <- as.factor(ifelse(data$Answer== data.table::shift(data$Answer), 
                                        "same", "different"))

xtabs(~data$congruency+data$congruencyNminus1+data$responserepeat)


### CONVENTIONAL ANALYSIS

data_agg <- aggregate(RT~congruency*congruencyNminus1*responserepeat*ParticipantID, mean, na.rm=T, data=data)

### 3-way anova, showing a highly significant 3-way interaction

aov_RT <- aov_car(RT ~ congruency*congruencyNminus1*responserepeat + 
                    Error(ParticipantID/congruency*congruencyNminus1*responserepeat),
                  data=data_agg)
knitr::kable(nice(aov_RT))

### plot the pattern
theme_set(theme_classic() +
            theme(axis.line = element_line(colour = "black"),
                  axis.ticks = element_line(colour = "black"),
                  axis.text = element_text(colour = "black")))

afex_plot(aov_RT, panel="responserepeat", x="congruencyNminus1", trace="congruency", 
          error="within", data_plot=F)

### simple effects

### "different" responses
aov_RT <- aov_car(RT ~ congruency*congruencyNminus1 + 
                    Error(ParticipantID/congruency*congruencyNminus1),
                  data=subset(data_agg, data_agg$responserepeat=="different"))
knitr::kable(nice(aov_RT))

### "same" responses
aov_RT <- aov_car(RT ~ congruency*congruencyNminus1 + 
                    Error(ParticipantID/congruency*congruencyNminus1),
                  data=subset(data_agg, data_agg$responserepeat=="same"))
knitr::kable(nice(aov_RT))

### BAYESIAN ANALYSIS

bayesfactor_inclusion(anovaBF(RT ~ congruency*congruencyNminus1*responserepeat + ParticipantID, 
                              whichRandom="ParticipantID", data=data_agg), match_models=T)

### simple effects

bayesfactor_inclusion(anovaBF(RT ~ congruency*congruencyNminus1 + ParticipantID, 
                              whichRandom="ParticipantID", data=subset(data_agg, data_agg$responserepeat=="different")), 
                      match_models=T)

bayesfactor_inclusion(anovaBF(RT ~ congruency*congruencyNminus1 + ParticipantID, 
                              whichRandom="ParticipantID", data=subset(data_agg, data_agg$responserepeat=="same")),
                      match_models=T)