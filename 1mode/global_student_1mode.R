library(tidyverse)
library(janitor)
library(widyr)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()
source("../func.r")

year_term_tl <- tibble("year_term" = paste(rep(2001:2019, each=2), c("1R", "2R"), sep = "_"),
           "Num_year_term" = 1:38)

year_term_tl <- tibble(year = rep(2001:2020, each = 2),
                       term = rep(c("1R", "2R"), 20)) %>% 
  mutate(date = case_when(term == "1R" ~ "01/03",
                          TRUE ~ "01/09"),
         date = paste0(date,"/",year),
         year_term = paste(year, term, sep = "_"),
         Num_year_term = 1:n())

year_term_tl

############################################################################
######################### 재학생 기본정보 ################################
############################################################################
student_info <- read.delim("../재학생_학부_기본정보.txt", header = T, 
                           sep = "|", stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  mutate(student_code = 1:n()) %>% 
  filter(입학년도 == 2015)


############################################################################
######################### 단과대학 네트워크 ################################
############################################################################

college_network_processed <- student_info %>% 
  group_by(대학) %>% 
  pairwise_count(student_code, 대학코드, sort = T) %>%
  rename(Source = item1, Target = item2) %>% 
  mutate(N_within = n_distinct(Source)) %>% 
  ungroup() 

college_network <-
  college_network_processed %>% 
  mutate(Domain = "college",
         Weight = n / sqrt(N_within)) %>%   # 파견 학생 수에 따른 가중치
  left_join(student_info %>% 
              select(student_code, 입학년도, 입학학기), by = c("Source" = "student_code")) %>% 
  mutate(year_term = paste(입학년도, 입학학기, sep = "_")) %>% 
  select(-입학년도, -입학학기) %>% 
  rename(Label = 대학)




############################################################################
######################### 학과 네트워크 ################################
############################################################################

major_network_processed <- student_info %>% 
  group_by(대학, 학과) %>% 
  pairwise_count(student_code, 학과코드, sort = T) %>%
  rename(Source = item1, Target = item2) %>% 
  mutate(N_within = n_distinct(Source),) %>% 
  ungroup()

major_network <- major_network_processed %>%
  mutate(Domain = "major",
         Weight = n / sqrt(N_within)) %>%    # 파견 학생 수에 따른 가중치
  left_join(student_info %>% 
              select(student_code, 입학년도, 입학학기), by = c("Source" = "student_code")) %>% 
  mutate(year_term = paste(입학년도, 입학학기, sep = "_")) %>% 
  select(-입학년도, -입학학기, -대학) %>% 
  rename(Label = 학과)


  


############################################################################
######################### 수강이력 네트워크 ################################
############################################################################

course_history_files <- list.files("../", pattern = "재학생_학부_수강이력", full.names = T)

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
         Domain = "course_history",
         Weight = n / N_within) %>% 
         # Weight = N_cumsum / max(N_cumsum)) %>% # Zero Weight
  # select(-년도, -학기, -N_max) %>%
  filter(Weight != 0) %>% 
  rename(n_course_history = n)

  

############################################################################
######################### 교환학생 네트워크 ################################
############################################################################

exchge_raw <- read.delim("../전체_교환학생.txt", header=T, sep = "|",
                         stringsAsFactors = FALSE) %>% 
  as_tibble()


exchge_joined <- exchge_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  filter(합격여부 == "Y",
             !파견학기수 %in% c("%", "% ")) %>% # 퍼센트는 뭐지??? 
  mutate(year_term = paste(신청년도, 신청학기, sep = "_")) %>% 
  mutate(N_term = as.numeric(substr(파견학기수, 1, 1))) %>% 
  uncount(N_term) %>%  
  group_by(식별자, year_term) %>% 
  mutate(Num_term = 1:n()) %>% 
  ungroup() %>% 
  left_join(year_term_tl, by = "year_term") %>% 
  mutate(Num_year_term = Num_year_term + Num_term, # 실제 파견 시기는 신청 + 1 학기
         Label = 파견대학,
         school_time = paste(파견대학, Num_year_term, sep="_")) %>% 
  select(-year_term) %>% 
  left_join(year_term_tl, by = "Num_year_term")
  

exchge_network_processed <- exchge_joined %>% 
  group_by(Label, year_term) %>% 
  pairwise_count(item = student_code, feature = school_time) %>% 
  rename(Source = item1, Target = item2) %>% 
  mutate(N_within = n_distinct(Source)) %>% 
  ungroup()

exchge_network <-
exchge_network_processed %>% 
  mutate(Domain = "exchange_student",
         Weight = n / sqrt(N_within))   # 파견 학생 수에 따른 가중치
  
############################################################################
######################### 출신학교 네트워크 ################################
############################################################################

# 졸업 년도에 따른 가중치 고려?

preschool_raw <- read.delim("../재학생_학부_출신학교.txt", header=T, 
                            sep = "|", stringsAsFactor = F) 


preschool_joined <- student_info %>% 
  left_join(preschool_raw, by = "식별자") %>% 
  filter(출신교명 != "",
             !is.na(출신교졸업년도)) %>% 
  mutate(Label = 출신교명,
             preschool_time = paste(출신교명, 출신교졸업년도, sep="_")) 


### This process takes longer time ########
preschool_network_processed <- preschool_joined %>% 
  group_by(Label, 출신교졸업년도) %>% 
  pairwise_count(item = student_code, feature = preschool_time) %>% 
  rename(Source = item1, Target = item2) %>% 
  mutate(N_within = n_distinct(Source)) %>% 
  ungroup()
#############################################


preschool_network <- preschool_network_processed %>% 
  left_join(student_info %>% 
              select(student_code, 입학년도, 입학학기), by = c("Source" = "student_code")) %>% 
  select(-출신교졸업년도) %>% 
  mutate(year_term = paste(입학년도, 입학학기, sep = "_"),
         Domain = "preschool",
         Weight = n / sqrt(N_within)) %>%  # 졸업생 수에 따른 가중치
  select(-입학년도, - 입학학기) %>% 
  arrange(N_within, desc(Weight))
  
############################################################################
######################### Binding Data  ################################
############################################################################

edges <- exchge_network %>% 
  bind_rows(preschool_network) %>% 
  bind_rows(course_history_network) %>% 
  bind_rows(major_network) %>% 
  bind_rows(college_network)

edges <- edges %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  group_by(Source, Domain) %>% 
  mutate(`peak moment` = Num_year_term[which.max(Weight)]) %>% 
  ungroup() %>% 
  mutate(`start date` = Num_year_term,
         `end date` = Num_year_term + 1) %>% 
  mutate(`peak moment` = year_term_tl$date[`peak moment`],
         `start date`  = year_term_tl$date[`start date`],
         `end date`  = year_term_tl$date[`end date`]) %>% 
  select(-Num_year_term)

nodes <- student_info %>% 
  semi_join(edges, by = c("student_code" = "Source")) %>% 
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

# make nodes 
write.csv(nodes, file="nodes_student.csv", fileEncoding = 'utf-8', row.names=F)

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
