## this script imports and combines all the questionairre data about language exposure, language use and other multitasking variables.

##libraries, etc

# check required libraries are installed
requires  <-  c("readxl", 
                "data.table", 
                "afex",
                "emmeans",
                "cowplot",
                "tidyverse",
                "bayestestR",
                "BayesFactor",
                "janitor")

# installs required libraries (which aren't already installed)
installs  <-  requires[!requires %in% installed.packages()[,"Package"]]

# load required packages
for (l in requires) require(l, character.only=TRUE)

# clear variables
rm(list=c("installs", "l", "requires"))

# reset working directory to project root
setwd(here::here())

# Participant comparison script

rm(list=ls())

#function to replace known "na"-type values. Add to this list if it comes up again.
na_string_replace_with_NA <- function(x) {
  case_when(
    x %in% c("NA","/","Na","no","n/a","na","N/A", "hn/a","none", "NO", "N/a",
             "hardly use this", "I don't use it", "Not spoken") ~ NA_character_,
    TRUE ~ x)
}

na_string_replace_with_zero <- function(x) {
  case_when(
    x %in% c("NA","/","Na","no","n/a","na","N/A","none", "NO") ~ NA_character_,
    TRUE ~ x)
}



expdata <- read.csv("new_analysis/combined_flanker_data.csv")

#combine those with whether or not they're bilingual
PartIDsAndLang <- expdata %>% 
  filter(!is.na(Participant.Private.ID)) %>% 
  distinct(Participant.Private.ID, lang)

#read in language information from other Gorilla output

#demographic data from bilingual group may be b5v6 (LSBQ), x8ml (lang questions)

bilingLSBQ <- read_csv(file = here::here("not\ MT/November/data_exp_56184-v5_questionnaire-b5v6.csv")) %>% 
  clean_names() %>% 
  filter(question_key!="BEGIN QUESTIONNAIRE", question_key!="END QUESTIONNAIRE")

bilingppts <- bilingLSBQ %>% 
  select(participant_private_id, local_date, experiment_id, local_date, 
         participant_device, participant_browser, participant_monitor_size,
         participant_viewport_size) %>% 
  filter(!duplicated(participant_private_id))

# spread out the responses and export a list of gorilla response questions
# questionkey <- bilingLSBQ %>% 
#   select(participant_private_id, question_key, response) %>% 
#   pivot_wider(names_from = question_key, values_from = response) %>%
#   colnames()

# write.csv(questionkey, file="qkey.csv")

# read in LSBQ question key you painstakingly hand coded
question_key_lookup <- read.csv(file = "questions_key.csv", header = TRUE)

# spread out the question key and response columns
bilingLSBQ_wide <- bilingLSBQ %>% 
  select(participant_private_id, question_key, response) %>% 
  pivot_wider(names_from = question_key, values_from = response)

### replace column names with more meaningful list

# make a function to look up column names
renamegorilla <-function(x) {question_key_lookup[match(x,question_key_lookup[,"Gorilla_code"]), "question"]}
# below, what the above looks like as an anonymous function: (useful for later, perhaps)
# (\(x) question_key_lookup[match(x,question_key_lookup[,"Gorilla_code"]), "question"])("response-2-2")

# apply function to do the thing with created function, probably a tidy way to do this somewhere
colnames(bilingLSBQ_wide) <-  apply(as.matrix(colnames(bilingLSBQ_wide)), FUN = renamegorilla, MARGIN = 1)

# time for some real wrangling

# first reorder columns so everything makes more sense
bilingLSBQ_wide <- bilingLSBQ_wide[, intersect(question_key_lookup[,3], colnames(bilingLSBQ_wide))]

bilingLangQues <- read_csv(file = here::here("not\ MT/November/data_exp_56184-v5_questionnaire-x8ml.csv")) %>% 
  clean_names() %>% filter(question_key!="BEGIN QUESTIONNAIRE", question_key!="END QUESTIONNAIRE")

bilingLQs_wide <- bilingLangQues %>% 
  select(participant_private_id, question_key, response) %>% 
  pivot_wider(names_from = question_key, values_from = response) %>% 
  clean_names() %>% rename(work_dual_language4 = response_5,
                           work_dual_language4_quantised = response_5_quantised)

#bilingual other multitasking data

bilingOtherQues <- read_csv(file = here::here("not\ MT/November/data_exp_56184-v5_questionnaire-sifi.csv")) %>%
  clean_names() %>% filter(question_key!="BEGIN QUESTIONNAIRE", question_key!="END QUESTIONNAIRE")

bilingOtherQues_wide <- bilingOtherQues %>%
  select(participant_private_id, question_key, response, experiment_version) %>% 
  pivot_wider(names_from = question_key, values_from = response) %>% 
  clean_names() %>% rename(handedness = response_6_mixed)

bilingual_all_data <-  bilingppts %>% 
  left_join(bilingLSBQ_wide, by="participant_private_id") %>% 
  left_join(bilingLQs_wide, by="participant_private_id") %>%
  left_join(bilingOtherQues_wide, by="participant_private_id")

bilingual_all_data$language_group <- "bilingual"
#monolingual data demo in g8rn (LSBQ), o26x (lang questions)

monoLSBQ <- read_csv(file = here::here("not\ MT/monolingual/data_exp_71111-v3_questionnaire-g8rn.csv")) %>%
  clean_names() %>% filter(question_key!="BEGIN QUESTIONNAIRE", question_key!="END QUESTIONNAIRE")

monoppts <- monoLSBQ %>% 
  select(participant_private_id, local_date, experiment_id, local_date, 
         participant_device, participant_browser, participant_monitor_size,
         participant_viewport_size) %>% 
  filter(!duplicated(participant_private_id))

monoLSBQ_wide <- monoLSBQ %>% 
  select(participant_private_id, question_key, response) %>% 
  pivot_wider(names_from = question_key, values_from = response)

colnames(monoLSBQ_wide) <- apply(as.matrix(colnames(monoLSBQ_wide)), FUN = renamegorilla, MARGIN = 1)

monoLSBQ_wide <- monoLSBQ_wide[, intersect(question_key_lookup[,3], colnames(monoLSBQ_wide))]

# now for the dual use questionnaire

monoLangQues <- read_csv(file = here::here("not\ MT/monolingual/data_exp_71111-v3_questionnaire-o26x.csv")) %>% 
  clean_names() %>% filter(question_key!="BEGIN QUESTIONNAIRE", question_key!="END QUESTIONNAIRE")

monoLQs_wide <- monoLangQues %>% 
  select(participant_private_id, question_key, response) %>% 
  pivot_wider(names_from = question_key, values_from = response) %>% 
  clean_names()%>% rename(work_dual_language4 = response_5,
                          work_dual_language4_quantised = response_5_quantised)

#monolingual other multitasking data (ojo9)

monoOtherQues <- read_csv(file = here::here("not\ MT/monolingual/data_exp_71111-v3_questionnaire-ojo9.csv")) %>%
  clean_names() %>% filter(question_key!="BEGIN QUESTIONNAIRE", question_key!="END QUESTIONNAIRE")

monoOtherQues_wide <- monoOtherQues %>%
  select(participant_private_id, question_key, response) %>% 
  pivot_wider(names_from = question_key, values_from = response) %>% 
  clean_names() %>% rename(handedness = response_6_mixed)

# check that all the participants appear in both data frames
paste("common ppt number", length(union(monoLSBQ_wide$participant_private_id,monoLQs_wide$participant_private_id)))
paste("uncommon ppt number", length(setdiff(monoLSBQ_wide$participant_private_id,monoLQs_wide$participant_private_id)))

mono_all_data <- monoppts %>% 
  left_join(monoLSBQ_wide, by="participant_private_id") %>% 
  left_join(monoLQs_wide, by="participant_private_id") %>%
  left_join(monoOtherQues_wide, by="participant_private_id")

## label and combine data

mono_all_data$language_group <- "monolingual"

all_data <- bind_rows(bilingual = bilingual_all_data, monolingual = mono_all_data)

# function to return mean of digits
quantised_rescue <- function(x) sapply(x, function(z) ifelse(is.na(z), NA, sum(floor(z / 10^(0:(nchar(z) - 1L))) %% 10)/nchar(z)))

#ditch some "NA"s 
all_data_prof_combined <- all_data %>% 
  mutate(across(c(maternal_job, maternal_L1,maternal_L2, maternal_L3,
                  paternal_job, paternal_L1, paternal_L2, paternal_L3, 
                  bornUK_text, L2, L3, L4, L5, L1_AoA, L2_AoA, L3_AoA,
                  L4_AoA, L5_AoA, age_start_double_language_use),
                na_string_replace_with_NA))

#LSBQ demog info
all_data_prof_combined <- all_data_prof_combined %>% 
  unite(col = gender, c(male, female), na.rm = TRUE) %>% 
  mutate(age_in_years = as.numeric(DOB_year)) %>% 
  unite(col = handedness2, c(left_handed, right_handed), na.rm = TRUE)%>% 
  unite(col = headinjury, c(headinjury_no, headinjury_text), na.rm = TRUE) %>%
  unite(col = neuroimpair, c(neuroimpair_no, neuroimpair_text), na.rm = TRUE) %>% 
  unite(col = psychmeds, c(psychmeds_no, psychmeds_text), na.rm = TRUE) %>% 
  mutate(videogame_hours = replace_na(FPS_hours_per_week, "0"), # fill in blank with zero char
         videogame_hours = parse_number(videogame_hours)) %>%  #convert char field to numeric
  mutate(vision_prob_glasses_or_contacts = replace_na(vision_prob_glasses_or_contacts, "2020"),
         vision_prob_glasses_or_contacts = gsub("no", "2020", vision_prob_glasses_or_contacts)) %>% 
  unite(col = bornUK, c(bornUK_yes, bornUK_text), na.rm = TRUE) %>% 
  unite(col = live_away_anglosphere, c(lived_not_anglo_no,
                                       lived_not_anglo_text), na.rm = TRUE)

#LSBQ usage time
all_data_prof_combined <- all_data_prof_combined  %>% 
  unite(col = L1_speaking_time, c(L1_speaking_time_little_quantised, 
                                  L1_speaking_time_some_quantised,
                                  L1_speaking_time_most_quantised,
                                  L1_speaking_time_all_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(L1_speaking_time = as.numeric(L1_speaking_time)) %>% 
  mutate(L1_speaking_time = quantised_rescue(L1_speaking_time)) %>% 
  unite(col = L1_listening_time, c(L1_listening_time_little_quantised,
                                  L1_listening_time_some_quantised,
                                  L1_listening_time_most_quantised,
                                  L1_listening_time_all_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(L1_listening_time = as.numeric(L1_listening_time)) %>% 
  mutate(L1_listening_time = quantised_rescue(L1_listening_time)) %>% 
  unite(col = L1_reading_time, c(L1_reading_time_none_quantised,
                                L1_reading_time_little_quantised,
                                L1_reading_time_some_quantised,
                                L1_reading_time_most_quantised,
                                L1_reading_time_all_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(L1_reading_time = as.numeric(L1_reading_time)) %>% 
  mutate(L1_reading_time = quantised_rescue(L1_reading_time)) %>% 
  unite(col = L1_writing_time, c(L1_writing_time_none_quantised,
                                      L1_writing_time_little_quantised,
                                      L1_writing_time_some_quantised,
                                      L1_writing_time_most_quantised,
                                      L1_writing_time_all_quantised), sep = "", na.rm = TRUE) %>%
  mutate(L1_writing_time = as.numeric(L1_writing_time)) %>% 
  mutate(L1_writing_time = quantised_rescue(L1_writing_time)) %>% 
  mutate(L2_speaking_time_little_quantised = if_else(L2_speaking_time_little=="Little", 2, NA_real_),
         L2_speaking_time_some_quantised = if_else(L2_speaking_time_some=="Some", 3, NA_real_),
         L2_speaking_time_most_quantised = if_else(L2_speaking_time_most=="Most", 4, NA_real_),
         L2_speaking_time_all_quantised = if_else(L2_speaking_time_all=="All", 5, NA_real_)) %>% 
  unite(col = L2_speaking_time, c(L2_speaking_time_little_quantised, 
                                              L2_speaking_time_some_quantised,
                                              L2_speaking_time_most_quantised,
                                              L2_speaking_time_all_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(L2_speaking_time = as.numeric(L2_speaking_time)) %>% 
  mutate(L2_speaking_time = quantised_rescue(L2_speaking_time)) %>% 
  mutate(L2_listening_time_little_quantised = if_else(L2_listening_time_little=="Little", 2, NA_real_),
         L2_listening_time_some_quantised = if_else(L2_listening_time_some=="Some", 3, NA_real_),
         L2_listening_time_most_quantised = if_else(L2_listening_time_most=="Most", 4, NA_real_),
         L2_listening_time_all_quantised = if_else(L2_listening_time_all=="All", 5, NA_real_)) %>% 
  unite(col = L2_listening_time, c(L2_listening_time_little_quantised,
                                               L2_listening_time_some_quantised,
                                               L2_listening_time_most_quantised,
                                               L2_listening_time_all_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(L2_listening_time = as.numeric(L2_listening_time)) %>% 
  mutate(L2_listening_time = quantised_rescue(L2_listening_time)) %>% 
  mutate(L2_reading_time_none_quantised = if_else(L2_reading_time_none=="None", 1, NA_real_),
         L2_reading_time_little_quantised = if_else(L2_reading_time_little=="Little", 2, NA_real_),
         L2_reading_time_some_quantised = if_else(L2_reading_time_some=="Some", 3, NA_real_),
         L2_reading_time_most_quantised = if_else(L2_reading_time_most=="Most", 4, NA_real_),
         L2_reading_time_all_quantised = if_else(L2_reading_time_all=="All", 5, NA_real_)) %>% 
  unite(col = L2_reading_time, c(L2_reading_time_none_quantised,
                                             L2_reading_time_little_quantised,
                                             L2_reading_time_some_quantised,
                                             L2_reading_time_most_quantised,
                                             L2_reading_time_all_quantised), sep = "", na.rm = TRUE) %>% 
          mutate(L2_reading_time = as.numeric(L2_reading_time)) %>% 
          mutate(L2_reading_time = quantised_rescue(L2_reading_time)) %>% 
  mutate(L2_writing_time_none_quantised = if_else(L2_writing_time_none=="None", 1, NA_real_),
         L2_writing_time_little_quantised = if_else(L2_writing_time_little=="Little", 2, NA_real_),
         L2_writing_time_some_quantised = if_else(L2_writing_time_some=="Some", 3, NA_real_),
         L2_writing_time_most_quantised = if_else(L2_writing_time_most=="Most", 4, NA_real_),
         L2_writing_time_all_quantised = if_else(L2_writing_time_all=="All", 5, NA_real_)) %>% 
  unite(col = L2_writing_time, c(L2_writing_time_none_quantised,
                                             L2_writing_time_little_quantised,
                                             L2_writing_time_some_quantised,
                                             L2_writing_time_most_quantised,
                                             L2_writing_time_all_quantised), sep = "", na.rm = TRUE) %>%
  mutate(L2_writing_time = as.numeric(L2_writing_time)) %>% 
  mutate(L2_writing_time = quantised_rescue(L2_writing_time))



#get parental education information
all_data_prof_combined <- all_data_prof_combined %>% 
    mutate(maternaleducation_1 = if_else(maternaleducation_1=="No high school diploma", 1, NA_real_),
         maternaleducation_2 = if_else(maternaleducation_2=="High school diploma", 2, NA_real_),
         maternaleducation_3 = if_else(maternaleducation_3=="Some post-secondary education", 3, NA_real_),
         maternaleducation_4 = if_else(maternaleducation_4=="Post-secondary degree or diploma", 4, NA_real_),
         maternaleducation_5 = if_else(maternaleducation_5=="Graduate or professional degree", 5, NA_real_)) %>% 
  unite(col = maternal_education, 
        c(maternaleducation_1, maternaleducation_2, maternaleducation_3, 
          maternaleducation_4, maternaleducation_5), sep = "", na.rm = TRUE) %>% 
  mutate(maternal_education = as.numeric(maternal_education), 
           maternal_education = quantised_rescue(maternal_education)) %>%
  mutate(paternaleducation_1 = if_else(paternaleducation_1=="No high school diploma", 1, NA_real_),
         paternaleducation_2 = if_else(paternaleducation_2=="High school diploma", 2, NA_real_),
         paternaleducation_3 = if_else(paternaleducation_3=="Some post-secondary education", 3, NA_real_),
         paternaleducation_4 = if_else(paternaleducation_4=="Post-secondary degree or diploma", 4, NA_real_),
         paternaleducation_5 = if_else(paternaleducation_5=="Graduate or professional degree", 5, NA_real_)) %>% 
  unite(col = paternal_education, 
        c(paternaleducation_1, paternaleducation_2, paternaleducation_3, 
          paternaleducation_4, paternaleducation_5), sep = "", na.rm = TRUE) %>% 
  mutate(paternal_education = as.numeric(paternal_education), 
         paternal_education = quantised_rescue(paternal_education))


#add the social dual use
all_data_prof_combined <- all_data_prof_combined %>% 
  mutate(people_parents_all_L1 = if_else(people_parents_all_L1=="All Language 1", 1, NA_real_),
         people_parents_mostly_L1 = if_else(people_parents_mostly_L1=="Mostly Language 1", 2, NA_real_),
         people_parents_half_and_half = if_else(people_parents_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         people_parents_mostly_L2 = if_else(people_parents_mostly_L2=="Mostly the other language", 4, NA_real_),
         people_parents_all_L2 = if_else(people_parents_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_use_parents_score, 
        c(people_parents_all_L1, people_parents_mostly_L1, people_parents_half_and_half,
          people_parents_mostly_L2, people_parents_all_L2), sep = "", na.rm = TRUE) %>% 
  mutate(dual_use_parents_score = as.numeric(dual_use_parents_score),
         dual_use_parents_score = quantised_rescue(dual_use_parents_score)) %>% 
  mutate(people_siblings_all_L1 = if_else(people_siblings_all_L1=="All Language 1", 1, NA_real_),
         people_siblings_mostly_L1 = if_else(people_siblings_mostly_L1=="Mostly Language 1", 2, NA_real_),
         people_siblings_half_and_half = if_else(people_siblings_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         people_siblings_mostly_L2 = if_else(people_siblings_mostly_L2=="Mostly the other language", 4, NA_real_),
         people_siblings_all_L2 = if_else(people_siblings_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_siblings_score, 
        c(people_siblings_all_L1, people_siblings_mostly_L1, people_siblings_half_and_half,
          people_siblings_mostly_L2, people_siblings_all_L2), sep = "", na.rm = TRUE) %>% 
  mutate(dual_siblings_score = as.numeric(dual_siblings_score),
         dual_siblings_score = quantised_rescue(dual_siblings_score)) %>% 
  mutate(people_grandparents_all_L1 = if_else(people_grandparents_all_L1=="All Language 1", 1, NA_real_),
         people_grandparents_mostly_L1 = if_else(people_grandparents_mostly_L1=="Mostly Language 1", 2, NA_real_),
         people_grandparents_half_and_half = if_else(people_grandparents_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         people_grandparents_mostly_L2 = if_else(people_grandparents_mostly_L2=="Mostly the other language", 4, NA_real_),
         people_grandparents_all_L2 = if_else(people_grandparents_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_grandparents_score, 
        c(people_grandparents_all_L1, people_grandparents_mostly_L1, people_grandparents_half_and_half,
          people_grandparents_mostly_L2, people_grandparents_all_L2), sep = "", na.rm = TRUE) %>% 
  mutate(dual_grandparents_score = as.numeric(dual_grandparents_score),
         dual_grandparents_score = quantised_rescue(dual_grandparents_score)) %>% 
  mutate(people_other_relatives_all_L1 = if_else(people_other_relatives_all_L1=="All Language 1", 1, NA_real_),
         people_other_relatives_mostly_L1 = if_else(people_other_relatives_mostly_L1=="Mostly Language 1", 2, NA_real_),
         people_other_relatives_half_and_half = if_else(people_other_relatives_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         people_other_relatives_mostly_L2 = if_else(people_other_relatives_mostly_L2=="Mostly the other language", 4, NA_real_),
         people_other_relatives_all_L2 = if_else(people_other_relatives_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_other_relatives_score, 
        c(people_other_relatives_all_L1, people_other_relatives_mostly_L1, people_other_relatives_half_and_half,
          people_other_relatives_mostly_L2, people_other_relatives_all_L2), sep = "", na.rm = TRUE) %>% 
  mutate(dual_other_relatives_score = as.numeric(dual_other_relatives_score),
         dual_other_relatives_score = quantised_rescue(dual_other_relatives_score)) %>% 
  mutate(people_partner_all_L1 = if_else(people_partner_all_L1=="All Language 1", 1, NA_real_),
         people_partner_mostly_L1 = if_else(people_partner_mostly_L1=="Mostly Language 1", 2, NA_real_),
         people_partner_half_and_half = if_else(people_partner_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         people_partner_mostly_L2 = if_else(people_partner_mostly_L2=="Mostly the other language", 4, NA_real_),
         people_partner_all_L2 = if_else(people_partner_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_partner_score, 
        c(people_partner_all_L1, people_partner_mostly_L1, people_partner_half_and_half,
          people_partner_mostly_L2, people_partner_all_L2), sep = "", na.rm = TRUE) %>% 
  mutate(dual_partner_score = as.numeric(dual_partner_score),
         dual_partner_score = quantised_rescue(dual_partner_score)) %>% 
  mutate(people_roommates_all_L1 = if_else(people_roommates_all_L1=="All Language 1", 1, NA_real_),
         people_roommates_mostly_L1 = if_else(people_roommates_mostly_L1=="Mostly Language 1", 2, NA_real_),
         people_roommates_half_and_half = if_else(people_roommates_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         people_roommates_mostly_L2 = if_else(people_roommates_mostly_L2=="Mostly the other language", 4, NA_real_),
         people_roommates_all_L2 = if_else(people_roommates_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_roommates_score, 
        c(people_roommates_all_L1, people_roommates_mostly_L1, people_roommates_half_and_half,
          people_roommates_mostly_L2, people_roommates_all_L2), sep = "", na.rm = TRUE) %>% 
  mutate(dual_roommates_score = as.numeric(dual_roommates_score),
         dual_roommates_score = quantised_rescue(dual_roommates_score)) %>% 
  mutate(people_neighbours_all_L1 = if_else(people_neighbours_all_L1=="All Language 1", 1, NA_real_),
         people_neighbours_mostly_L1 = if_else(people_neighbours_mostly_L1=="Mostly Language 1", 2, NA_real_),
         people_neighbours_half_and_half = if_else(people_neighbours_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         people_neighbours_mostly_L2 = if_else(people_neighbours_mostly_L2=="Mostly the other language", 4, NA_real_),
         people_neighbours_all_L2 = if_else(people_neighbours_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_neighbours_score, 
        c(people_neighbours_all_L1, people_neighbours_mostly_L1, people_neighbours_half_and_half,
          people_neighbours_mostly_L2, people_neighbours_all_L2), sep = "", na.rm = TRUE) %>% 
  mutate(dual_neighbours_score = as.numeric(dual_neighbours_score),
         dual_neighbours_score = quantised_rescue(dual_neighbours_score)) %>% 
  mutate(people_friends_all_L1 = if_else(people_friends_all_L1=="All Language 1", 1, NA_real_),
         people_friends_mostly_L1 = if_else(people_friends_mostly_L1=="Mostly Language 1", 2, NA_real_),
         people_friends_half_and_half = if_else(people_friends_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         people_friends_mostly_L2 = if_else(people_friends_mostly_L2=="Mostly the other language", 4, NA_real_),
         people_friends_all_L2 = if_else(people_friends_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_friends_score, 
        c(people_friends_all_L1, people_friends_mostly_L1, people_friends_half_and_half,
          people_friends_mostly_L2, people_friends_all_L2), sep = "", na.rm = TRUE) %>% 
  mutate(dual_friends_score = as.numeric(dual_friends_score),
         dual_friends_score = quantised_rescue(dual_friends_score))

# community dual use
all_data_prof_combined <- all_data_prof_combined %>% 
  unite(col = dual_infancy_score,
        c(community_infancy_all_L1_quantised, community_infancy_mostly_L1_quantised,
          community_infancy_half_and_half_quantised, community_infancy_mostly_L2_quantised,
          community_infancy_mostly_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_infancy_score = as.numeric(dual_infancy_score), 
         dual_infancy_score = quantised_rescue(dual_infancy_score)) %>% 
  unite(col = dual_preschool_score,
        c(community_preschool_all_L1_quantised, community_preschool_mostly_L1_quantised,
          community_preschool_half_and_half_quantised, community_preschool_mostly_L2_quantised,
          community_preschool_mostly_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_preschool_score = as.numeric(dual_preschool_score), 
         dual_preschool_score = quantised_rescue(dual_preschool_score)) %>% 
  unite(col = dual_primaryschool_score,
        c(community_primaryschool_all_L1_quantised, community_primaryschool_mostly_L1_quantised,
          community_primaryschool_half_and_half_quantised, community_primaryschool_mostly_L2_quantised,
          community_primaryschool_mostly_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_primaryschool_score = as.numeric(dual_primaryschool_score), 
         dual_primaryschool_score = quantised_rescue(dual_primaryschool_score)) %>% 
  unite(col = dual_highschool_score,
        c(community_highschool_all_L1_quantised, community_highschool_mostly_L1_quantised,
          community_highschool_half_and_half_quantised, community_highschool_mostly_L2_quantised,
          community_highschool_mostly_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_highschool_score = as.numeric(dual_highschool_score), 
         dual_highschool_score = quantised_rescue(dual_highschool_score))

#situations dual use score
all_data_prof_combined <- all_data_prof_combined %>% 
  unite(col = dual_home_use,
        c(situations_home_all_L1_quantised, situations_home_mostly_L1_quantised,
          situations_home_half_and_half_quantised, situations_home_mostly_L2_quantised,
          situations_home_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_home_use = as.numeric(dual_home_use),
         dual_home_use = quantised_rescue(dual_home_use)) %>% 
  unite(col = dual_school_use,
        c(situations_school_all_L1_quantised, situations_school_mostly_L1_quantised,
          situations_school_half_and_half_quantised, situations_school_mostly_L2_quantised,
          situations_school_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_school_use = as.numeric(dual_school_use),
         dual_school_use = quantised_rescue(dual_school_use)) %>% 
  unite(col = dual_work_use,
        c(situations_work_all_L1_quantised, situations_work_mostly_L1_quantised,
          situations_work_half_and_half_quantised, situations_work_mostly_L2_quantised,
          situations_work_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_work_use = as.numeric(dual_work_use),
         dual_work_use = quantised_rescue(dual_work_use)) %>% 
  unite(col = dual_socialising_use,
        c(situations_socialising_all_L1_quantised, situations_socialising_mostly_L1_quantised,
          situations_socialising_half_and_half_quantised, situations_socialising_mostly_L2_quantised,
          situations_socialising_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_socialising_use = as.numeric(dual_socialising_use),
         dual_socialising_use = quantised_rescue(dual_socialising_use)) %>% 
  unite(col = dual_religious_activities_use,
        c(situations_religious_activities_all_L1_quantised, situations_religious_activities_mostly_L1_quantised,
          situations_religious_activities_half_and_half_quantised, situations_religious_activities_mostly_L2_quantised,
          situations_religious_activities_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_religious_activities_use = as.numeric(dual_religious_activities_use),
         dual_religious_activities_use = quantised_rescue(dual_religious_activities_use)) %>% 
  unite(col = dual_extracurricular_activities_use,
        c(situations_extracurricular_activities_all_L1_quantised, situations_extracurricular_activities_mostly_L1_quantised,
          situations_extracurricular_activities_half_and_half_quantised, situations_extracurricular_activities_mostly_L2_quantised,
          situations_extracurricular_activities_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_extracurricular_activities_use = as.numeric(dual_extracurricular_activities_use),
         dual_extracurricular_activities_use = quantised_rescue(dual_extracurricular_activities_use)) %>% 
  unite(col = dual_shopping_restaurants_use,
        c(situations_shopping_restaurants_all_L1_quantised, situations_shopping_restaurants_mostly_L1_quantised,
          situations_shopping_restaurants_half_and_half_quantised, situations_shopping_restaurants_mostly_L2_quantised,
          situations_shopping_restaurants_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_shopping_restaurants_use = as.numeric(dual_shopping_restaurants_use),
         dual_shopping_restaurants_use = quantised_rescue(dual_shopping_restaurants_use)) %>% 
  unite(col = dual_health_gov_use,
        c(situations_health_gov_all_L1_quantised, situations_health_gov_mostly_L1_quantised,
          situations_health_gov_half_and_half_quantised, situations_health_gov_mostly_L2_quantised,
          situations_health_gov_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_health_gov_use = as.numeric(dual_health_gov_use),
         dual_health_gov_use = quantised_rescue(dual_health_gov_use))

all_data_prof_combined <- all_data_prof_combined %>% 
  mutate(activity_reading_all_L1_quantised = if_else(activity_reading_all_L1=="All Language 1", 1, NA_real_),
         activity_reading_mostly_L1_quantised = if_else(activity_reading_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_reading_half_and_half_quantised = if_else(activity_reading_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_reading_mostly_L2_quantised = if_else(activity_reading_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_reading_all_L2_quantised = if_else(activity_reading_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_reading_use,
        c(activity_reading_all_L1_quantised, activity_reading_mostly_L1_quantised,
          activity_reading_half_and_half_quantised, activity_reading_mostly_L2_quantised,
          activity_reading_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_reading_use = as.numeric(dual_reading_use),
         dual_reading_use = quantised_rescue(dual_reading_use)) %>% 
  mutate(activity_emailing_all_L1_quantised = if_else(activity_emailing_all_L1=="All Language 1", 1, NA_real_),
         activity_emailing_mostly_L1_quantised = if_else(activity_emailing_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_emailing_half_and_half_quantised = if_else(activity_emailing_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_emailing_mostly_L2_quantised = if_else(activity_emailing_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_emailing_all_L2_quantised = if_else(activity_emailing_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_emailing_use,
        c(activity_emailing_all_L1_quantised, activity_emailing_mostly_L1_quantised,
          activity_emailing_half_and_half_quantised, activity_emailing_mostly_L2_quantised,
          activity_emailing_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_emailing_use = as.numeric(dual_emailing_use),
         dual_emailing_use = quantised_rescue(dual_emailing_use)) %>% 
  mutate(activity_texting_all_L1_quantised = if_else(activity_texting_all_L1=="All Language 1", 1, NA_real_),
         activity_texting_mostly_L1_quantised = if_else(activity_texting_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_texting_half_and_half_quantised = if_else(activity_texting_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_texting_mostly_L2_quantised = if_else(activity_texting_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_texting_all_L2_quantised = if_else(activity_texting_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_texting_use,
        c(activity_texting_all_L1_quantised, activity_texting_mostly_L1_quantised,
          activity_texting_half_and_half_quantised, activity_texting_mostly_L2_quantised,
          activity_texting_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_texting_use = as.numeric(dual_texting_use),
         dual_texting_use = quantised_rescue(dual_texting_use)) %>% 
  mutate(activity_social_media_all_L1_quantised = if_else(activity_social_media_all_L1=="All Language 1", 1, NA_real_),
         activity_social_media_mostly_L1_quantised = if_else(activity_social_media_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_social_media_half_and_half_quantised = if_else(activity_social_media_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_social_media_mostly_L2_quantised = if_else(activity_social_media_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_social_media_all_L2_quantised = if_else(activity_social_media_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_social_media_use,
        c(activity_social_media_all_L1_quantised, activity_social_media_mostly_L1_quantised,
          activity_social_media_half_and_half_quantised, activity_social_media_mostly_L2_quantised,
          activity_social_media_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_social_media_use = as.numeric(dual_social_media_use),
         dual_social_media_use = quantised_rescue(dual_social_media_use)) %>% 
  mutate(activity_writing_lists_notes_all_L1_quantised = if_else(activity_writing_lists_notes_all_L1=="All Language 1", 1, NA_real_),
         activity_writing_lists_notes_mostly_L1_quantised = if_else(activity_writing_lists_notes_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_writing_lists_notes_half_and_half_quantised = if_else(activity_writing_lists_notes_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_writing_lists_notes_mostly_L2_quantised = if_else(activity_writing_lists_notes_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_writing_lists_notes_all_L2_quantised = if_else(activity_writing_lists_notes_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_writing_lists_notes_use,
        c(activity_writing_lists_notes_all_L1_quantised, activity_writing_lists_notes_mostly_L1_quantised,
          activity_writing_lists_notes_half_and_half_quantised, activity_writing_lists_notes_mostly_L2_quantised,
          activity_writing_lists_notes_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_writing_lists_notes_use = as.numeric(dual_writing_lists_notes_use),
         dual_writing_lists_notes_use = quantised_rescue(dual_writing_lists_notes_use)) %>% 
  mutate(activity_watching_tv_listening_radio_all_L1_quantised = if_else(activity_watching_tv_listening_radio_all_L1=="All Language 1", 1, NA_real_),
         activity_watching_tv_listening_radio_mostly_L1_quantised = if_else(activity_watching_tv_listening_radio_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_watching_tv_listening_radio_half_and_half_quantised = if_else(activity_watching_tv_listening_radio_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_watching_tv_listening_radio_mostly_L2_quantised = if_else(activity_watching_tv_listening_radio_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_watching_tv_listening_radio_all_L2_quantised = if_else(activity_watching_tv_listening_radio_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_watching_tv_listening_radio_use,
        c(activity_watching_tv_listening_radio_all_L1_quantised, activity_watching_tv_listening_radio_mostly_L1_quantised,
          activity_watching_tv_listening_radio_half_and_half_quantised, activity_watching_tv_listening_radio_mostly_L2_quantised,
          activity_watching_tv_listening_radio_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_watching_tv_listening_radio_use = as.numeric(dual_watching_tv_listening_radio_use),
         dual_watching_tv_listening_radio_use = quantised_rescue(dual_watching_tv_listening_radio_use)) %>% 
  mutate(activity_watching_movies_all_L1_quantised = if_else(activity_watching_movies_all_L1=="All Language 1", 1, NA_real_),
         activity_watching_movies_mostly_L1_quantised = if_else(activity_watching_movies_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_watching_movies_half_and_half_quantised = if_else(activity_watching_movies_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_watching_movies_mostly_L2_quantised = if_else(activity_watching_movies_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_watching_movies_all_L2_quantised = if_else(activity_watching_movies_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_watching_movies_use,
        c(activity_watching_movies_all_L1_quantised, activity_watching_movies_mostly_L1_quantised,
          activity_watching_movies_half_and_half_quantised, activity_watching_movies_mostly_L2_quantised,
          activity_watching_movies_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_watching_movies_use = as.numeric(dual_watching_movies_use),
         dual_watching_movies_use = quantised_rescue(dual_watching_movies_use)) %>% 
  mutate(activity_browsing_internet_all_L1_quantised = if_else(activity_browsing_internet_all_L1=="All Language 1", 1, NA_real_),
         activity_browsing_internet_mostly_L1_quantised = if_else(activity_browsing_internet_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_browsing_internet_half_and_half_quantised = if_else(activity_browsing_internet_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_browsing_internet_mostly_L2_quantised = if_else(activity_browsing_internet_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_browsing_internet_all_L2_quantised = if_else(activity_browsing_internet_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_browsing_internet_use,
        c(activity_browsing_internet_all_L1_quantised, activity_browsing_internet_mostly_L1_quantised,
          activity_browsing_internet_half_and_half_quantised, activity_browsing_internet_mostly_L2_quantised,
          activity_browsing_internet_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_browsing_internet_use = as.numeric(dual_browsing_internet_use),
         dual_browsing_internet_use = quantised_rescue(dual_browsing_internet_use)) %>% 
  mutate(activity_praying_all_L1_quantised = if_else(activity_praying_all_L1=="All Language 1", 1, NA_real_),
         activity_praying_mostly_L1_quantised = if_else(activity_praying_mostly_L1=="Mostly Language 1", 2, NA_real_),
         activity_praying_half_and_half_quantised = if_else(activity_praying_half_and_half=="Half Language 1 half other language", 3, NA_real_),
         activity_praying_mostly_L2_quantised = if_else(activity_praying_mostly_L2=="Mostly the other language", 4, NA_real_),
         activity_praying_all_L2_quantised = if_else(activity_praying_all_L2=="Only the other language", 5, NA_real_)) %>% 
  unite(col = dual_praying_use,
        c(activity_praying_all_L1_quantised, activity_praying_mostly_L1_quantised,
          activity_praying_half_and_half_quantised, activity_praying_mostly_L2_quantised,
          activity_praying_all_L2_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(dual_praying_use = as.numeric(dual_praying_use),
         dual_praying_use = quantised_rescue(dual_praying_use))

#switching measures, fam, friends, social media
all_data_prof_combined <- all_data_prof_combined %>% 
  mutate(language_switch_with_parents_and_family_never_quantised = if_else(language_switch_with_parents_and_family_never=="Never", 1, NA_real_),
         language_switch_with_parents_and_family_rarely_quantised = if_else(language_switch_with_parents_and_family_rarely=="Rarely", 2, NA_real_),
         language_switch_with_parents_and_family_sometimes_quantised = if_else(language_switch_with_parents_and_family_sometimes=="Sometimes", 3, NA_real_),
         language_switch_with_parents_and_family_frequently_quantised = if_else(language_switch_with_parents_and_family_frequently=="Frequently", 4, NA_real_),
         language_switch_with_parents_and_family_always_quantised = if_else(language_switch_with_parents_and_family_always=="Always", 5, NA_real_)) %>% 
  unite(col = switching_with_parents_and_family,
        c(language_switch_with_parents_and_family_never_quantised, language_switch_with_parents_and_family_rarely_quantised,
          language_switch_with_parents_and_family_sometimes_quantised, language_switch_with_parents_and_family_frequently_quantised,
          language_switch_with_parents_and_family_always_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(switching_with_parents_and_family = as.numeric(switching_with_parents_and_family),
         switching_with_parents_and_family = quantised_rescue(switching_with_parents_and_family)) %>% 
  mutate(language_switch_with_friends_never_quantised = if_else(language_switch_with_friends_never=="Never", 1, NA_real_),
         language_switch_with_friends_rarely_quantised = if_else(language_switch_with_friends_rarely=="Rarely", 2, NA_real_),
         language_switch_with_friends_sometimes_quantised = if_else(language_switch_with_friends_sometimes=="Sometimes", 3, NA_real_),
         language_switch_with_friends_frequently_quantised = if_else(language_switch_with_friends_frequently=="Frequently", 4, NA_real_),
         language_switch_with_friends_always_quantised = if_else(language_switch_with_friends_always=="Always", 5, NA_real_)) %>% 
  unite(col = switching_with_friends,
        c(language_switch_with_friends_never_quantised, language_switch_with_friends_rarely_quantised,
          language_switch_with_friends_sometimes_quantised, language_switch_with_friends_frequently_quantised,
          language_switch_with_friends_always_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(switching_with_friends = as.numeric(switching_with_friends),
         switching_with_friends = quantised_rescue(switching_with_friends)) %>% 
  mutate(language_switch_on_social_media_never_quantised = if_else(language_switch_on_social_media_never=="Never", 1, NA_real_),
         language_switch_on_social_media_rarely_quantised = if_else(language_switch_on_social_media_rarely=="Rarely", 2, NA_real_),
         language_switch_on_social_media_sometimes_quantised = if_else(language_switch_on_social_media_sometimes=="Sometimes", 3, NA_real_),
         language_switch_on_social_media_frequently_quantised = if_else(language_switch_on_social_media_frequently=="Frequently", 4, NA_real_),
         language_switch_on_social_media_always_quantised = if_else(language_switch_on_social_media_always=="Always", 5, NA_real_)) %>% 
  unite(col = switching_on_social_media,
        c(language_switch_on_social_media_never_quantised, language_switch_on_social_media_rarely_quantised,
          language_switch_on_social_media_sometimes_quantised, language_switch_on_social_media_frequently_quantised,
          language_switch_on_social_media_always_quantised), sep = "", na.rm = TRUE) %>% 
  mutate(switching_on_social_media = as.numeric(switching_on_social_media),
         switching_on_social_media = quantised_rescue(switching_on_social_media))

#self ratings of reading and speaking Ls 1 and 2 (and 3) at home/work/social
all_data_prof_combined <- all_data_prof_combined %>% 
  unite(col = self_rate_L1_use,
        c(rate_use_L1_reading_at_home, rate_use_L1_reading_at_work,
          rate_use_L1_reading_in_social_settings, rate_use_L1_speaking_at_home,
          rate_use_L1_speaking_at_work, rate_use_L1_speaking_in_social_settings),
        sep = "", na.rm = TRUE) %>% 
  mutate(self_rate_L1_use = as.numeric(self_rate_L1_use),
         self_rate_L1_use = quantised_rescue(self_rate_L1_use)) %>% 
  unite(col = self_rate_L2_use,
        c(rate_use_L2_reading_at_home, rate_use_L2_reading_at_work,
          rate_use_L2_reading_in_social_settings, rate_use_L2_speaking_at_home,
          rate_use_L2_speaking_at_work, rate_use_L2_speaking_in_social_settings),
        sep = "", na.rm = TRUE) %>% 
  mutate(self_rate_L2_use = as.numeric(self_rate_L2_use),
         self_rate_L2_use = quantised_rescue(self_rate_L2_use)) %>% 
  unite(col = self_rate_L3_use,
        c(rate_use_L3_reading_at_home, rate_use_L3_reading_at_work,
          rate_use_L3_reading_in_social_settings, rate_use_L3_speaking_at_home,
          rate_use_L3_speaking_at_work, rate_use_L3_speaking_in_social_settings),
        sep = "", na.rm = TRUE) %>% 
  mutate(self_rate_L3_use = as.numeric(self_rate_L3_use),
         self_rate_L3_use = quantised_rescue(self_rate_L3_use))

#actual proficiency scores and combined time of use scores (with rowwise ops)
all_data_prof_combined <- all_data_prof_combined %>% 
  mutate(across(c(L1_speaking_proficiency,
                  L1_understanding_proficiency,
                  L1_reading_proficiency,
                  L1_writing_proficiency), as.numeric)) %>% 
  mutate(across(c(L2_speaking_proficiency,
                  L2_understanding_proficiency,
                  L2_reading_proficiency,
                  L2_writing_proficiency), as.numeric)) %>% 
  rowwise() %>% 
  mutate(L1_overall_proficiency = mean(c(L1_speaking_proficiency,
                                         L1_understanding_proficiency,
                                         L1_reading_proficiency,
                                         L1_writing_proficiency), na.rm = TRUE)) %>% 
  mutate(L2_overall_proficiency = mean(c(L2_speaking_proficiency,
                                         L2_understanding_proficiency,
                                         L2_reading_proficiency,
                                         L2_writing_proficiency), na.rm = TRUE)) %>% 
  mutate(L1_overall_time = mean(c(L1_speaking_time,
                                  L1_listening_time,
                                  L1_reading_time,
                                  L1_writing_time), na.rm = TRUE)) %>% 
  mutate(L2_overall_time = mean(c(L2_speaking_time,
                                  L2_listening_time,
                                  L2_reading_time,
                                  L2_writing_time), na.rm = TRUE)) %>%
  mutate(life_schooling_dual_use = mean(c(dual_infancy_score,
                                dual_preschool_score,
                                dual_primaryschool_score,
                                dual_highschool_score), na.rm = TRUE)) %>% 
  mutate(close_social_dual_use = mean(c(dual_use_parents_score,
                                        dual_siblings_score,
                                        dual_grandparents_score,
                                        dual_other_relatives_score,
                                        dual_partner_score,
                                        dual_roommates_score,
                                        dual_neighbours_score,
                                        dual_friends_score), na.rm = TRUE)) %>% 
  mutate(wider_community_dual_use = mean(c(dual_home_use,
                                           dual_school_use,
                                           dual_work_use,
                                           dual_socialising_use,
                                           dual_religious_activities_use,
                                           dual_extracurricular_activities_use,
                                           dual_shopping_restaurants_use,
                                           dual_health_gov_use), na.rm = TRUE)) %>%
  mutate(activity1_dual_use = mean(c(dual_social_media_use,
                                     dual_writing_lists_notes_use,
                                     dual_watching_tv_listening_radio_use,
                                     dual_watching_movies_use,
                                     dual_browsing_internet_use,
                                     dual_praying_use), na.rm = TRUE)) %>% 
  mutate(activity2_dual_use = mean(c(dual_reading_use,
                                     dual_emailing_use,
                                     dual_texting_use), na.rm = TRUE)) %>% 
  mutate(switching_score = mean(c(switching_with_parents_and_family,
                                  switching_with_friends,
                                  switching_on_social_media), na.rm = TRUE)) %>% 
  mutate(dual_1 = if_else(close_social_dual_use > 3 & close_social_dual_use < 3.4, 
                                 true = "dual", false = "single")) %>% 
  mutate(dual_2 = if_else(close_social_dual_use > 2.8 & close_social_dual_use < 3.6, 
                                 true = "dual", false = "single")) %>% 
  ungroup()

all_data_prof_combined <- all_data_prof_combined %>% 
  mutate(self_bilingual = if_else(is.na(L2), 
                                  true = "monolingual", 
                                  false = "bilingual")) %>% 
  mutate(maternal_bilingual = if_else(is.na(maternal_L2), 
                                      true =  "monolingual", 
                                      false = "bilingual")) %>% 
  mutate(paternal_bilingual = if_else(is.na(paternal_L2), 
                                      true =  "monolingual", 
                                      false =  "bilingual")) %>% 
  mutate(bilingual_onset = age_start_double_language_use)

#process more variables from second language questionnaire

all_data_prof_combined <- all_data_prof_combined %>% 
  mutate(fluency_l1 = as.numeric(fluency_l1),
         fluency_l2 = as.numeric(fluency_l2),
         fluency_l3 = as.numeric(l3_fluency),
         bilingual_onset_2 = as.numeric(age_bilingual_quantised)-1) %>% 
  mutate(num_lang = fct_reorder(as_factor(number_of_languages), 
                                as.numeric(number_of_languages_quantised), 
                                .fun = median),
         childhood_eng_percent = as.numeric(percent_english_childhood),
         daily_eng_percent = as.numeric(percent_english_daily_life),
         childhood_other_percent = as.numeric(language_spoken_in_childhood_other),
         daily_other_percent = as.numeric(other_language_daily_life),
         current_language_use = fct_reorder(as_factor(current_language_use),
                                            as.numeric(current_language_use_quantised),
                                            .fun = median),
         years_away_from_mother_tongue = fct_reorder(as_factor(years_away), 
                                  as.numeric(years_away_quantised), 
                                  .fun = median))

#process data from the Participant Questions bit

#change the data for the person that selected "other" for education level

all_data_prof_combined[all_data_prof_combined$participant_private_id == 5036188, c("self_education", "self_education_quantised")] <- all_data_prof_combined[all_data_prof_combined$participant_private_id == 5036173, c("self_education", "self_education_quantised")]


all_data_prof_combined <- all_data_prof_combined %>% 
  mutate(musical_training_years = as.numeric(musical_training),
         music_playing_freq = fct_reorder(as_factor(musical_instrument),
                                       6-as.numeric(musical_instrument_quantised),
                                       .fun = median),
         multitask_freq = fct_reorder(as_factor(multitask),
                                    6-as.numeric(multitask_quantised),
                                    .fun = median),
         sport_freq = fct_reorder(as_factor(sport),
                                      6-as.numeric(6-as.numeric(sport_quantised)),
                                      .fun = median),
         team_sports_div_attention = fct_reorder(as_factor(team_sports),
                                                 as.numeric(team_sports_quantised),
                                                 .fun = median),
         mindfulness_freq = fct_reorder(as_factor(mindfulness),
                                   as.numeric(mindfulness_quantised),
                                   .fun = median),
         travel_experience = fct_reorder(as_factor(travelled),
                                         as.numeric(travelled_quantised),
                                         .fun = median),
         self_education = fct_reorder(as_factor(self_education),
                                      as.numeric(self_education_quantised),
                                      .fun = median),
         ses_relative = fct_reorder(as_factor(ses), 
                           as.numeric(ses_quantised), 
                           .fun = median))

  
#select relevant data
all_data_combined <- all_data_prof_combined %>%  #start with demographics
  select(participant_private_id, local_date, experiment_id, age_in_years, gender,
         occupation, handedness, handedness2, videogame_hours,
         vision_prob_glasses_or_contacts, headinjury, neuroimpair, psychmeds,
         bornUK, live_away_anglosphere,
         maternal_job, maternal_L1, maternal_L2, #mum occupation and languages
         paternal_job, paternal_L1, paternal_L2, #dad occupation and languages
         maternal_bilingual, paternal_bilingual,
         maternal_education, paternal_education,
         L1, L2, L1_AoA, L2_AoA, L3, L1_overall_proficiency, #langdata LSBQ
         L1_overall_time, L2_overall_proficiency, L2_overall_time, 
         life_schooling_dual_use, close_social_dual_use, wider_community_dual_use,
         activity1_dual_use, activity2_dual_use, switching_score,
         dual_1, dual_2, bilingual_onset,
         bilingual_onset_2,fluency_l1, fluency_l2, fluency_l3, num_lang,
         childhood_eng_percent,childhood_other_percent, daily_eng_percent, 
         daily_other_percent, years_away_from_mother_tongue, current_language_use,
         home_dual_language1, home_dual_language2, home_dual_language3,
         home_dual_language4, work_dual_language, work_dual_language2,
         work_dual_language3, work_dual_language4, socialisation_dual_language,
         socialisation_dual_language2, socialisation_dual_language3,
         socialisation_dual_language4,
         musical_training_years, music_playing_freq, multitask_freq,
         sport_freq, team_sports_div_attention, mindfulness_freq, 
         travel_experience, self_education, ses_relative)

#social, life and community
#life use is schooling - 

#polly kept the following variables from LSBQ and broad demographic questionnaire: "Participant.Private.ID","Participant.Public.ID",'Experiment.Version',"Experiment.ID","mum.occ","mum.L1","mum.L2","dad.occ","dad.L1","dad.L2","sex", "occupation","age","videogames","Language1","Language2","bilingual","age.learn.lang1","age.learn.lang2","bilingual.onset", "mum.bilingual","dad.bilingual","social","life","community","activity1","activity2","switch","ProfL1","ProfL2", "useL1","useL2","lang1.use", "lang2.use","dual","dual2") 
# check that they're all there

saveRDS(all_data_combined, file = "all_language_data.rds")
#write_csv(all_data_combined, file="all_lang_data.csv")

rm(list=ls())

all_lang_data <- readRDS("all_language_data.rds")
#all_lang_data <- read_csv("all_lang_data.csv")