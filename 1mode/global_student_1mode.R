library(tidyverse)
library(janitor)
library(widyr)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()
source("../../../R_functions/func.r", encoding = 'utf-8')
source("../../../R_functions/engagement_network_data.r", encoding = 'utf-8')
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))


  


############################################################################
######################### 수강이력 네트워크 ################################
############################################################################

course_history_files <- list.files("../", pattern = "(재학생_학부_수강이력|졸업생_학부_수강이력)(.+)(.txt)", full.names = T)

course_history_files <- course_history_files[15]
course_history_raw <- lapply(course_history_files, 
                             FUN = function(files) {
                               read.delim(files, header=T, 
                                          sep = "|", stringsAsFactors = FALSE)
                             }) %>% 
  bind_rows() %>% 
  as_tibble()

course_history_joined <- course_history_raw %>%
  inner_join(student_info %>%
                     select(식별자, 대학, 학과, student_code), by = "식별자") %>% 
  # filter(이수구분 == "교양") %>%  # 교양으로 통제
  semi_join(student_info, by = "식별자")


course_history_by_student <- course_history_joined %>% 
  # group_by(student_code, 년도, 학기) %>% # dynamic
  group_by(student_code) %>% # no dynamic
  summarise(N_course_by_semester = sum(학점수)) %>% 
  ungroup() %>% 
  group_by(student_code) %>% 
  mutate(N_within = cumsum(N_course_by_semester)) %>% 
  arrange(student_code)

course_history_network_processed <-
  course_history_joined %>% 
  mutate(과목명_년도_학기_분반 = paste(과목명, 년도, 학기, 분반, sep="_"),
                 수강여부 = 1) %>% 
  # group_by(년도, 학기) %>%  # No dynamic
  pairwise_count(student_code, 과목명_년도_학기_분반, wt = 학점수, sort = T) %>%
  rename(Source = item1, Target = item2) %>% 
  ungroup() 

course_history_network_processed2 <- course_history_network_processed
# %>%
#   group_by(Source, Target) %>% 
#   mutate(N_max = sum(n)) %>% 
#   filter(N_max > 3) %>% # 조절값
#   arrange(년도, 학기) %>% 
#   mutate(N_cumsum = cumsum(n)) %>% 
#   ungroup()

  
course_history_network <- course_history_network_processed2 %>%
  left_join(course_history_by_student %>%
              select(student_code, N_within), by = c("Source" = "student_code")) %>%
  # left_join(course_history_by_student %>%
  #             select(student_code, 년도, 학기, N_within), by = c("Source" = "student_code",
  #                                             "년도", "학기")) %>%
  # mutate(year_term = paste(년도, 학기, sep = "_")) %>%
  # group_by(year_term) %>%
  mutate(Label = "course_history",
         Target = as.character(Target),
         Domain = "course_history",
         Weight = n / N_within) %>% 
         # Weight = N_cumsum / max(N_cumsum)) %>% # Zero Weight
  # select(-년도, -학기, -N_max) %>%
  filter(Weight != 0) %>% 
  rename(n_course_history = n)


edges <- edges %>% 
  bind_rows(course_history_network) %>% 
  semi_join(student_info %>% filter(졸업년도 == 2019), by = c("source" = "student_code"))

edges <- edges %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "Num_year_term") %>% 
  mutate(`start date` = Num_year_term,
         `end date` = Num_year_term + 1) %>% 
  mutate(`start date`  = year_term_tl$date[`start date`],
         `end date`  = year_term_tl$date[`end date`]) %>% 
  select(-Num_year_term)

nodes <- student_info %>% 
  semi_join(edges, by = c("student_code" = "source")) %>% 
  select(-식별자)

############################################################################
######################### Gephi Data  ################################
############################################################################


# isolate index
isoIndex <- edges %>% 
  group_by(Source)  %>% 
  summarise(isoIndex = sum(Weight, na.rm=T)) %>% 
  arrange(isoIndex)

nodes <- nodes %>% 
  left_join(isoIndex, by = c("student_code" = "Source")) %>% 
  rename(Id = student_code)

edges <- edges %>% filter(!is.na(`start date`))

# make nodes 
write.csv(nodes, file="nodes_student.csv", fileEncoding = 'utf-8', row.names=F)
write.csv(edges, file="edges_student.csv", fileEncoding = 'utf-8', row.names=F)

edge_list <- c("preschool",
               "college",
               "major",
               "course_history",
               "exchange_student")

#make edges (note, columns must be titled "Source", and "Target")
for(i in edge_list) {
  print(i)
write.csv(edges %>% 
            filter(Domain == i), file=paste0("edges_student_", i, ".csv"), fileEncoding = 'utf-8',
          row.names = F) 
}

i = edge_list[4]
write.csv(edges %>% 
            filter(Domain == i), file=paste0("edges_student_", i, ".csv"), fileEncoding = 'utf-8',
          row.names = F) 
