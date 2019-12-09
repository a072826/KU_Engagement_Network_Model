library(tidyverse)
library(janitor)
library(widyr)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()
source("../../func.r")

year_term_tl <- tibble("year_term" = paste(rep(2001:2019, each=2), c("1R", "2R"), sep = "_"),
           "Num_year_term" = 1:38)

year_term_tl <- tibble(year = rep(2001:2020, each = 2),
                       term = rep(c("1R", "2R"), 20)) %>% 
  mutate(date = case_when(term == "1R" ~ "01/03",
                          TRUE ~ "01/09"),
         date = paste0(date,"/",year),
         year_term = paste(year, term, sep = "_"),
         Num_year_term = 1:n())

학과정보 <- read.csv("../../학과정보.csv")

  
############################################################################
######################### 재학생 기본정보 ################################
############################################################################
student_info <- read.delim("../../재학생_학부_기본정보.txt", header = T, 
                           sep = "|", stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  mutate(student_code = 1:n()) %>% 
  filter(입학년도 == 2015) %>% 
  left_join(학과정보, by = "학과코드") %>% 
  filter(캠퍼스구분 == 1)



############################################################################
######################### 단과대학 네트워크 ################################
############################################################################

college_network <- student_info %>% 
  select(대학, student_code) %>% 
  mutate(Domain = "단과대학") %>% 
  rename(Source = student_code, Target = 대학) %>% 
  mutate(Label = Target)


############################################################################
######################### 학과 네트워크 ################################
############################################################################

major_network <- student_info %>% 
  select(학과, student_code) %>% 
  mutate(Domain = "학과") %>% 
  rename(Source = student_code, Target = 학과) %>% 
  mutate(Label = Target)



############################################################################
######################### 상벌 네트워크 ################################
############################################################################

award_raw <- read_xls("../../재학생_학부_상벌정보.xls")


award_network <- award_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  mutate(Source = student_code,
         Target = case_when(grepl("우수|특대생|명예장학생", 상벌유형)~ "성적우수", # 상벌 유형에대한 공부 필요
                            grepl("성적경고|유급", 상벌유형)~ "성적경고및유급" ),
         Domain = Target,
         Label = 상벌유형) %>% 
  select(Source, Target, Domain, Label) %>% 
  filter(!is.na(Target))


############################################################################
######################### 교환학생 네트워크 ################################
############################################################################

exchge_raw <- read.delim("../../전체_교환학생.txt", header=T, sep = "|",
                         stringsAsFactors = FALSE) %>% 
  as_tibble()


exchge_network <- exchge_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  filter(합격여부 == "Y",
             !파견학기수 %in% c("%", "% ")) %>% # 퍼센트는 뭐지??? 
  select(student_code, 파견대학) %>% 
  mutate(Domain = "해외대학파견",
         Target = "해외대학파견",
         Source = student_code,
         Label = 파견대학) %>% 
  select(Source, Target, Domain, Label)


############################################################################
######################### 출신학교 네트워크 ################################
############################################################################

# 졸업 년도에 따른 가중치 고려?

preschool_raw <- read.delim("../../재학생_학부_출신학교.txt", header=T, 
                            sep = "|", stringsAsFactor = F) 

preschool_network <- preschool_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  filter(출신교명 != "",
             !is.na(출신교졸업년도)) %>% 
  mutate(Source = student_code,
         Domain = "출신교",
         Target = 출신교명) %>% 
  select(Source, Target, Domain) %>% 
  mutate(Label = Target)


############################################################################
######################### 장학금 네트워크 ################################
############################################################################


############################################################################
######################### Binding Data  ################################
############################################################################

edges <- exchge_network %>% 
  bind_rows(preschool_network) %>% 
  # bind_rows(course_history_network) %>% 
  bind_rows(major_network) %>% 
  bind_rows(college_network) %>% 
  bind_rows(award_network)

temp_nodes <- student_info %>% 
  distinct(student_code) %>% 
  rename(Id = student_code) %>% 
  mutate(Domain = "학생",
         Label = "", 
         Source = Id,
         Id = as.character(Id))

nodes <- edges %>% 
  distinct(Target, .keep_all = T) %>% 
  rename(Id = Target) %>% 
  bind_rows(temp_nodes) %>% 
  mutate(Label = case_when(Domain == "해외대학파견" ~ "해외대학파견",
                           TRUE ~ Label))

# nodes <- student_info %>%
#   semi_join(edges, by = c("student_code" = "Source")) %>%
#   select(-식별자) %>%
#   rename(Id = student_code) %>% 
#   bind_rows()

############################################################################
######################### Gephi Data  ################################
############################################################################


# make nodes 
write.csv(nodes, file="nodes_student.csv", fileEncoding = 'utf-8', row.names=F)
write.csv(edges, file="edges_student.csv", fileEncoding = 'utf-8', row.names=F)


