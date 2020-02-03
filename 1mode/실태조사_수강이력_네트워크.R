library(tidyverse)
library(janitor)
library(widyr)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()
source("../../../R_functions/func.r", encoding = 'utf-8')

load("../../../R_functions/network_data.rdata")
load("../../../R_functions/전체_학부_수강이력_네트워크.rdata")
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))


d_joined <- readxl::read_xlsx("../../../../../2019 대학생활 실태조사/d_joined.xlsx")
학년 <-  read.delim("../../../전체_학부_학년.txt", header = T,
                  sep = "|", stringsAsFactors = FALSE) %>% 
  filter(년도 == 2019, 학기 == "2R")


############################################################################
######################### 수강이력 네트워크 ################################
############################################################################

course_history_network_processed <-
  course_history_joined %>%
  filter(년도 >=2012) %>% 
  filter(!이수구분 %in% c("전공지도", "학부공통")) %>%
  # filter(!grepl("1학년 세미나", 과목명)) %>%
  # filter(!grepl("전공", 이수구분)) %>%
  # filter(!grepl("교직", 이수구분)) %>%
  # filter(grepl("교양|전공탐색", 이수구분)) %>% 
  mutate(과목명_년도_학기_분반 = paste(과목명, 년도, 학기, 분반, sep="_"),
                     수강여부 = 1) %>% 
  # group_by(년도, 학기) %>%  # No dynamic
  pairwise_count(식별자, 과목명_년도_학기_분반) %>%
  rename(Source = item1, Target = item2) 


  
course_history_network <- course_history_network_processed %>%
  filter(n > 3) %>% 
  mutate(Label = "course_history",
         Target = as.character(Target),
         Domain = "course_history") 


# edges <- edges %>% 
#   bind_rows(course_history_network) %>% 
#   semi_join(student_info %>% filter(졸업년도 == 2019), by = c("source" = "student_code"))
# 
# edges <- edges %>% 
#   left_join(year_term_tl %>% 
#               select(year_term, Num_year_term), by = "Num_year_term") %>% 
#   mutate(`start date` = Num_year_term,
#          `end date` = Num_year_term + 1) %>% 
#   mutate(`start date`  = year_term_tl$date[`start date`],
#          `end date`  = year_term_tl$date[`end date`]) %>% 
#   select(-Num_year_term)

edges <- course_history_network

nodes <- student_info %>% 
  left_join(학년 %>% select(식별자, 학년), by = "식별자") %>% 
  filter(입학년도 >= 2012) %>% 
  left_join(d_joined %>% 
              select_if(!names(.) %in% names(student_info %>% 
                                               select(-식별자))) %>% 
              select(-학년, - 학점), by = "식별자") %>% 
  select(-Id) %>% 
  rename(Id = 식별자)



############################################################################
######################### Gephi Data  ################################
############################################################################



# make nodes 
write.csv(nodes, file="nodes_student.csv", fileEncoding = 'utf-8', row.names=F)
write.csv(edges, file="edges_student.csv", fileEncoding = 'utf-8', row.names=F)

