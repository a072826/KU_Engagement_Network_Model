library(tidyverse)
library(janitor)
library(widyr)
library(readxl)
library(Matrix)
library(widyr)
library(rstudioapi)
# current_path = rstudioapi::getActiveDocumentContext()$path
# setwd(dirname(current_path ))
# getwd()
setwd("C:/Users/Kibum Moon/Dropbox/study/DataHub/19_12_02_KU_Engagement_Network_Model/KU_Engagement_Network_Model/2mode")
source("../../../R_functions/func.r", encoding = 'utf-8')


################
.accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}



year_term_tl <- tibble("year_term" = paste(rep(2000:2019, each=2), c("1R", "2R"), sep = "_"),
                       "Num_year_term" = 1:40)

year_term_tl <- tibble(year = rep(2000:2020, each = 2),
                       term = rep(c("1R", "2R"), 21)) %>% 
  mutate(date = case_when(term == "1R" ~ "01/03",
                          TRUE ~ "01/09"),
         date = paste0(date,"/",year),
         year_term = paste(year, term, sep = "_"),
         Num_year_term = 1:n())

학과정보 <- read.csv("../../../학과정보.csv") %>% 
  select(-X)


############################################################################
######################### 학생 기본정보 ################################
############################################################################

student_info_alumni <- read.delim("../../../졸업생_학부_기본정보.txt", header = T,
                                sep = "|", stringsAsFactors = FALSE) %>% 
  filter(졸업년도 >= 2010) %>%
  filter(입학년도 >= 2000)   %>%
  left_join(학과정보, by = "학과코드") %>% 
  mutate(생년월일 = as.numeric(str_extract(string = (생년월일), "[0-9]{4}"))) %>% 
  filter(캠퍼스구분 == 1) 


student_info_std <- read.delim("../../../재학생_학부_기본정보.txt", header = T,
                               sep = "|", stringsAsFactors = FALSE) %>% 
  filter(입학년도 >= 2006) %>%
  left_join(학과정보, by = "학과코드") %>% 
  mutate(생년월일 = as.numeric(str_extract(string = (생년월일), "[0-9]{4}"))) %>% 
  filter(캠퍼스구분 == 1) 

# DB에서 생년월일에 대한 자료형이 다르기 때문에 각각 따로 처리
student_info <- student_info_alumni %>% 
  bind_rows(student_info_std) %>% 
  mutate(나이 = (입학년도 - 생년월일) +1 ) %>% 
  mutate(student_code = 1:n()) %>% 
  as_tibble() %>% 
  .입학유형정리()

rm(student_info_alumni)
rm(student_info_std)

############################################################################
######################### 학년 및 나이 네트워크  ################################
############################################################################
school_year_age_raw <- read.delim("../../../전체_학부_학년.txt", header = T,
                              sep = "|", stringsAsFactors = FALSE) %>% 
  inner_join(student_info %>% 
               select(식별자, student_code, 나이, 입학년도), by = "식별자") %>%
  mutate(학년 = ifelse(!학년%in%c(1:5), NA, 학년)) %>% 
  mutate(나이 = 나이 + (년도 - 입학년도)) %>% 
  mutate(year_term = paste(년도, 학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  arrange(년도, 학기) %>%
  # 2015년2학기(학년 = 0), 2017년 자료 (학년 = -9)라는 문제 있음  
  group_by(식별자) %>%
  fill(학년, .direction = "downup") %>% 
  ungroup() %>% 
  as_tibble()

network_school_year_age <- school_year_age_raw %>%
  pivot_longer(cols = c("학년", "나이"), names_to = "Domain", values_to = "Target") %>% 
  mutate(Target = as.character(Target)) %>% 
  rename(source = student_code) %>% 
  select(source, Target, Num_year_term, Domain) %>% 
  mutate(Label = Target) %>% 
  as_tibble()


rm(school_year_age_raw)

############################################################################
######################### 국적 네트워크 ################################
############################################################################

network_nationality <- student_info %>% 
  select(student_code, 국적, 입학년도) %>% 
  mutate(Domain = "국적") %>% 
  
  rename(source = student_code, 
         Target = 국적,
         Year = 입학년도
  ) %>% 
  mutate(Label = Target)


############################################################################
######################### 입학전형 네트워크 ################################
############################################################################

network_ent <- student_info %>% 
  select(student_code, 입학유형, 입학년도) %>% 
  mutate(Domain = "입학유형") %>% 
  
  rename(source = student_code, 
         Target = 입학유형,
         Year = 입학년도
         ) %>% 
  mutate(Label = Target)



############################################################################
######################### 출신학교 네트워크 ################################
############################################################################

# # 졸업 년도에 따른 가중치 고려?
# 
# preschool_alumni <- read.delim("../../../졸업생_학부_출신학교.txt", header=T, 
#                             sep = "|", stringsAsFactor = F) %>% 
#   mutate(출신교졸업년도 = as.numeric(출신교졸업년도))
# 
# preschool_std <- read.delim("../../../재학생_학부_출신학교.txt", header=T, 
#                             sep = "|", stringsAsFactor = F) %>% 
#   mutate(출신교졸업년도 = as.numeric(출신교졸업년도))
# 
# #  자료 join
# preschool_raw <-  bind_rows(preschool_alumni, preschool_std) %>% 
#   as_tibble() %>% 
#   select(-출신교코드)
# 
# network_preschool <- preschool_raw %>% 
#   inner_join(student_info, by = "식별자") %>% 
#   filter(출신교명 != "",
#              !is.na(출신교졸업년도)) %>% 
#   mutate(source = student_code,
#          Domain = "출신교",
#          Target = 출신교명,
#          Category = "출신교") %>% 
#   mutate(year_term = paste(입학년도, 입학학기, sep="_")) %>% 
#   left_join(year_term_tl %>% 
#               select(year_term, Num_year_term), by = "year_term") %>% 
#   select(source, Target, Num_year_term, Domain) %>% 
#   mutate(Label = Target) %>% 
#   as_tibble()
# 
# rm(preschool_alumni)
# rm(preschool_std)
# rm(preschool_raw)

############################################################################
######################### 학과 네트워크 ################################
############################################################################

network_major <- student_info %>% 
  mutate(Domain = "학과") %>% 
  rename(source = student_code, 
         Target = 학과) %>% 
  mutate(year_term = paste(입학년도, 입학학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  select(source, Target, Num_year_term, Domain) %>% 
  mutate(Label = Target) %>% 
  as_tibble()


############################################################################
######################### 단과대학 네트워크 ################################
############################################################################

network_college <- student_info %>% 
  mutate(Domain = "대학") %>% 
  rename(source = student_code, 
         Target = 대학) %>% 
  mutate(year_term = paste(입학년도, 입학학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  select(source, Target, Num_year_term, Domain) %>% 
  mutate(Label = Target) %>% 
  as_tibble()



############################################################################
######################### 학적변동 네트워크 ################################
############################################################################


record_alumni <- read.delim("../../../졸업생_학부_학적변동내역.txt", header = T,
                           sep = "|", stringsAsFactors = FALSE)
record_std <- read.delim("../../../재학생_학부_학적변동내역.txt", header = T,
                        sep = "|", stringsAsFactors = FALSE)

# 학적변동 자료 join
record_raw <-  bind_rows(record_alumni, record_std) %>% 
  inner_join(student_info, by = "식별자") %>% 
  mutate(year_term = paste(년도, 학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
            select(year_term, Num_year_term), by = "year_term") %>% 
  as_tibble()


network_leaveOfAbsence <- record_raw %>% 
  filter(grepl("휴학|복학", 변동유형)) %>%  # 학적 변동 중 휴학 및 복학만 활용
  select(student_code, Num_year_term, 변동유형) %>% 
  arrange(student_code, Num_year_term) %>% 

  # 휴학한 학기를 추출함 
  group_by(student_code, idx = cumsum(grepl("복학|제적", 변동유형))) %>%   
  mutate(count = 1:n()) %>% 
  group_by(student_code, idx, 변동유형) %>% 
  summarise(Num_year_term = min(Num_year_term)) %>% 
  ungroup() %>% 
  arrange(student_code, Num_year_term) %>% 
  mutate(idx = ifelse(grepl("복학|제적", 변동유형), idx-1, idx))  %>% 
  # 휴학유형 구분 (군복무 vs. 일반)
  mutate(휴학유형 = ifelse(grepl("군", 변동유형), "군복무", "일반")) %>% 
  group_by(student_code, idx, 휴학유형) %>% 
  summarise(start = min(Num_year_term),
         end = max(Num_year_term),
         휴학기간 = end-start) %>%
  uncount(휴학기간) %>% 
  group_by(student_code, idx) %>% 
  mutate(Num_year_term = start + row_number()-1) %>% 
  ungroup() %>% 
  
  # 휴학 Edge 데이터
  mutate(source = student_code,
         Target = paste0(휴학유형, "휴학"),
         Domain = paste0(휴학유형, "휴학"),
         Label = paste0(휴학유형, "휴학")) %>% 
  select(source, Target, Domain, Num_year_term, Label)


# 휴학 제외 학적변동
network_record <- record_raw %>% 
  filter(!grepl("휴학|복학|휴학경과제적", 변동유형)) %>% 
  select(student_code, Num_year_term, 변동유형) %>% 
  arrange(student_code, Num_year_term) %>%
  mutate(source = student_code,
         Target = 변동유형,
         Domain = 변동유형,
         Label = 변동유형) %>% 
  select(source, Target, Domain, Num_year_term, Label)

rm(record_alumni)
rm(record_std)
rm(record_raw)

############################################################################
######################### 장학금 네트워크 ################################
############################################################################

# 어떻게 활용할 지 좀 더 고민해보자

scholarship_alumni <- read.delim("../../../졸업생_학부_장학금수혜.txt", header = T,
                         sep = "|", stringsAsFactors = FALSE)

scholarship_std <- read.delim("../../../재학생_학부_장학금수혜.txt", header = T,
                        sep = "|", stringsAsFactors = FALSE)

scholarship_raw <- bind_rows(scholarship_alumni, scholarship_std) %>% 
  as_tibble() %>% 
  mutate(입학금 = abs(입학금),
         등록금 = abs(등록금),
         기타금액 = abs(기타금액),
         기성회비 = replace_na(기성회비, 0),  
         입학금 = replace_na(입학금, 0),  
         등록금 = replace_na(등록금, 0),  
         기타금액 = replace_na(기타금액, 0),  
         수혜금액 = 기성회비 + 입학금 + 등록금 + 기타금액)  %>% 
  filter(grepl("^성적우수장학금\\(|진리형|진리장학금",장학금명)) 


network_scholarship <- scholarship_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  mutate(year_term = paste(년도, 학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  select(student_code, Num_year_term, 장학금명) %>% 
  arrange(student_code, Num_year_term) %>%
  mutate(source = student_code,
         Target = 장학금명,
         Domain = "장학금수혜",
         Label = 장학금명) %>% 
  select(source, Target, Domain, Num_year_term, Label)

rm(scholarship_alumni)
rm(scholarship_std)
rm(scholarship_raw)

############################################################################
######################### 상벌 네트워크 ################################
############################################################################


award_alumni <- read.delim("../../../졸업생_학부_상벌정보.txt", header = T,
                         sep = "|", stringsAsFactors = FALSE)

award_std <- read.delim("../../../재학생_학부_상벌정보.txt", header = T,
                        sep = "|", stringsAsFactors = FALSE)
 
award_raw <-  bind_rows(award_alumni, award_std) %>% 
  mutate(year_term = paste(년도, 학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  as_tibble()


network_award <- award_raw %>% 
  mutate(상벌유형 = replace(상벌유형, grep("학기우등생|학기최우등생|특대생", 상벌유형), "성적우수표창")) %>% # 성적우수표창 합치기
  inner_join(student_info, by = "식별자") %>% 
  mutate(source = student_code,
         Target = 상벌유형,
         Domain = Target,
         Label = 상벌유형) %>% 
  select(source, Target, Domain, Num_year_term, Label) %>% 
  filter(nchar(Target)!=0)

rm(award_alumni)
rm(award_std)
rm(award_raw)

############################################################################
######################### 학생상담센터 네트워크 ################################
############################################################################

# 학기 데이터 필요
kuscc_raw <- read_xlsx("../../../전체_학부_학생상담센터.xlsx") %>% 
  distinct(year, term, group, 식별자) %>% 
  mutate(year_term = paste(year, term, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  group_by(group) %>% 
  mutate(n_group = n()) %>% 
  filter(n_group > 10) %>% 
  ungroup()
   
network_kuscc <- kuscc_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  mutate(source = student_code,
         Target = group,
         Domain = "학생상담센터",
         Label = group) %>% 
  select(source, Target, Domain, Num_year_term, Label) %>% 
  filter(nchar(Target)!=0)

rm(kuscc_raw)  

############################################################################
######################### 교환학생 네트워크 ################################
############################################################################

# 학적 변동의 교환학생과 다름 점은 구체적인 학교까지 살펴볼 수 있다는 점

# exchange_raw <- read.delim("../../../전체_교환학생.txt", header=T, sep = "|",
#                          stringsAsFactors = FALSE) %>% 
#   as_tibble()
# 
# 
# exchange_network <- exchange_raw %>% 
#   inner_join(student_info, by = "식별자") %>% 
#   filter(합격여부 == "Y",
#              !파견학기수 %in% c("%", "% ")) %>% # 퍼센트는 뭐지??? 
#   select(student_code, 파견대학) %>% 
#   mutate(Domain = "해외대학파견",
#          Target = "해외대학파견",
#          source = student_code,
#          Label = 파견대학) %>% 
#   select(source, Target, Domain, Label)


############################################################################
######################### Binding Data  ################################
############################################################################

# 엣지 합치기
edges <- network_nationality %>% 
  # bind_rows(network_preschool) %>%
  bind_rows(network_school_year_age) %>%
  bind_rows(network_scholarship) %>%
  # bind_rows(network_exchange) %>%
  # bind_rows(network_course_history) %>% 
  bind_rows(network_college) %>% 
  bind_rows(network_major) %>% 
  bind_rows(network_award) %>% 
  bind_rows(network_record) %>% 
  bind_rows(network_leaveOfAbsence) %>% 
  bind_rows(network_kuscc) %>% 
  as_tibble() %>% 
  mutate(Target = gsub("[[:space:]]", "", Target)) %>% 
  mutate(참여여부 = 1) 
  

edges <- edges %>% 
  filter(Num_year_term >= 21) # 2010 1학기 이후
  

nodes_engagement <- edges %>% 
  filter(!Domain %in% c("국적", "출신교", "대학", "학과", "성별", "나이", "학년")) %>% 
  reshape2::dcast(source ~ Domain) %>% 
  janitor::clean_names() %>% 
  as_tibble()


nodes_std <-
  student_info %>% 
  rename(Id = student_code) %>% 
  left_join(nodes_engagement, by = c("Id" = "source")) %>% 
  mutate(Domain = "학생",
         Label = "", 
         source = Id,
         Id = as.character(Id))
  

nodes <- edges %>% 
  distinct(Target, Num_year_term, .keep_all = T) %>% 
  rename(Id = Target) %>% 
  bind_rows(nodes_std) %>% 
  mutate(Label = case_when(Domain == "해외대학파견" ~ "해외대학파견",
                           TRUE ~ Label)) %>% 
  select(-참여여부, -source, -식별자)


# # make nodes
# write.csv(nodes, file="nodes.csv", fileEncoding = 'utf-8', row.names=F)
# 
# # make edges
# write.csv(edges, file="edges.csv", fileEncoding = 'utf-8', row.names=F)







