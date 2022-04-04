# Participant comparison script

rm(list=ls())

expdata <- read_csv("combined_flanker_data.csv")

PartIDfromdata <- base::unique(expdata$Participant.Private.ID, na.rm = TRUE)

PartIDsAndLang <- 

#read in language information from other Gorilla output

#demographic data from bilingual group may be b5v6 (LSBQ), x8ml (lang questions)

bilingLSBQ <- read_csv(file = here::here("not\ MT/November/data_exp_56184-v5_questionnaire-b5v6.csv"))

bilingLangQues <- read_csv(file = here::here("not\ MT/November/data_exp_56184-v5_questionnaire-x8ml.csv"))

#monolingual data demo in g8rn (LSBQ), o26x (lang questions)

monoLSBQ <- read_csv(file = here::here("not\ MT/monolingual/data_exp_71111-v3_questionnaire-g8rn.csv"))

monoLangQues <- read_csv(file = here::here("not\ MT/monolingual/data_exp_71111-v3_questionnaire-o26x.csv"))

