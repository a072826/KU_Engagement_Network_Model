library(tidyverse)
library(janitor)
library(widyr)
library(readxl)
library(Matrix)
library(widyr)
# library(rstudioapi)
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
  filter(입학년도 >= 2006)   %>%
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
  as_tibble()

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
edges <- network_school_year_age %>% 
  # bind_rows(network_preschool) %>% 
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
  filter(!Domain %in% c("출신교", "대학", "학과", "성별", "나이", "학년")) %>% 
  reshape2::dcast(source ~ Domain) %>% 
  janitor::clean_names() %>% 
  as_tibble()


nodes_std <-
  student_info %>% 
  rename(Id = student_code) %>% 
  inner_join(nodes_engagement, by = c("Id" = "source")) %>% 
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

# # make edges 
# write.csv(edges, file="edges.csv", fileEncoding = 'utf-8', row.names=F)





############################################################################
######################### 다양성 분석  ################################
############################################################################


#####################################
####### 기본정보 추가   #########
#####################################

student_info_for_idx <- nodes_std  
   # 출신교 join: 해외학교 및 편입 제외
  # left_join(network_preschool %>%
  #             select(source, Target) %>%
  #             rename(출신교 = Target), by = "source") %>%
  # mutate(출신교 = case_when((국적=="KOR" & !grepl("편입", 입학유형)) ~ 출신교,
  #                          TRUE ~ NA_character_))
  
student_info_for_idx %>% 
  distinct(대학) %>% 
  nrow()



#####################################
####### 당학기 재적생 분석  ######### 
##################################### # 휴학생을 해당 학년 재적인원에 반영할지 고민 필요


student_info_by_semester <- student_info_for_idx %>% 
  
  # 학교 입학 후 졸업할 때까지의 record 생성 
  mutate(ent_year_term = paste(입학년도, 입학학기, sep="_"),
         grd_year_term = paste(졸업년도, 졸업학기, sep="_"), 
         com_year_term = paste(수료년도, 졸업학기, sep="_")) %>% 
  left_join(year_term_tl[,4:5], by = c("ent_year_term" = "year_term")) %>% 
  left_join(year_term_tl[,4:5], by = c("grd_year_term" = "year_term"))  %>% 
  left_join(year_term_tl[,4:5], by = c("com_year_term" = "year_term"))  %>% 
  rename(Num_year_term_ent = Num_year_term.x,
         Num_year_term_grd = Num_year_term.y,
         Num_year_term_com = Num_year_term) %>% 
  mutate(재학기간 = case_when(!grepl("졸업|조기졸업", 학적상태) ~ as.integer(40 + 1 - Num_year_term_ent), # 현재(2019-2)학기 40
                              grepl("졸업|조기졸업", 학적상태) ~ as.integer(Num_year_term_grd - Num_year_term_ent))) %>% 

    # 재학 기간만큼 한 학기당 1개씩 행 생성
  uncount(재학기간) %>% 
  group_by(source) %>% 
  mutate(Num_year_term = Num_year_term_ent + row_number()-1) %>% 
  anti_join(network_leaveOfAbsence %>% 
              filter(Target == "군복무휴학"), by = c("source" , "Num_year_term")) %>% 
  left_join(network_school_year_age %>% # 학년 데이터 학기별로 합치기 / 학적 기록 시작 학기 이전 기록 제거 
              pivot_wider(id_cols = c("source", "Num_year_term"),
                          names_from = Domain, values_from = Target) %>% # 학년 데이터만 join 나이 데이터는 추후 직접 계산
              group_by(source) %>% 
              mutate(Num_year_term_record_started = min(Num_year_term)) %>% 
              ungroup() %>% 
              select(-나이), by = c("source" , "Num_year_term")) %>% 
  group_by(source) %>%  
  mutate(재적여부 = case_when(((is.na(졸업년도) & is.na(수료년도)) & is.na(학년))~ "제외",
                          TRUE ~ "재적")) %>% # 제적 아님. 재적. 해당 학기에 학교에 존재했는지 여부
  filter(재적여부 == "재적") %>% 
  fill(Num_year_term_record_started, .direction = "up") %>% # 학적 시작 학기

  # 학기별 학적 기록  
  mutate(당시학적 = case_when(Num_year_term <  Num_year_term_record_started ~ "입학전",
                          is.na(Num_year_term_com) ~ "재학중",
                          Num_year_term < Num_year_term_com ~ "재학중",
                          !is.na(학년) ~ "재학중",
                          is.na(학년) ~ "수료")) %>% 
    filter(당시학적 != "입학전") %>%
    left_join(year_term_tl %>% select(year, Num_year_term, year_term), by = "Num_year_term") %>% 
    mutate(생년 = as.numeric(str_extract(string = (생년월일), "[0-9]{4}"))) %>% 
    mutate(나이 = year - 생년 + 1) %>%   # 학생 생년과 해당 학기 년도를 통해 나이 재계산
  fill(학년, .direction = "updown") %>%  # 수료 후 학년은 고정
  select(-재적여부) %>% 
  ungroup()


# student_info_by_semester %>% 
#   mutate(구분 = ifelse(grepl(pattern = "졸업", x = 학적상태), "졸업", "재학"),
#          Num_year_term = as.factor(Num_year_term)) %>% 
#   ggplot(aes(Num_year_term, fill = 구분)) +
#   geom_histogram(binwidth = 1, color = "black") 
# 
#   scale_x_continuous(aes(labels = as.character(Num_year_term), breaks = Num_year_term))
# 
#   scale_x_discrete(aes(breaks = seq(range(Num_year_term))))
#   scale_x_discrete(aes(labels=year_term))



# 분석 활동 설정
list_attributes <- c("성별", "국적", "출신교",
                     "대학", "학과", "입학유형", 
                     "학년", "나이")

list_domains <- setdiff(unique(edges$Domain), list_attributes)

activities_interested <- edges %>% 
  filter(Domain %in% list_domains)

student_info_by_semester_domain_joined <- student_info_by_semester %>%
  left_join(activities_interested %>% 
              select(-Domain, -Label), by = c("source", "Num_year_term"))

N_by_domain_by_semester <- student_info_by_semester_domain_joined %>% 
  filter(!is.na(Domain)) %>% 
  group_by(Num_year_term, Domain) %>% 
  summarise(N_by_domain = n()) %>% 
  ungroup() 

# make empty data frame
index_list_raw <- data.frame()  # 최종 Index 계산 결과 저장 
prob_semester_by_domain <- data.frame() # 

for(i in list_attributes) {
  print(paste0("attribute: ",i))
  expected_prob <-
    student_info_by_semester %>%
    group_by(Num_year_term) %>%
    count(i) %>% 
    rename(cate = i, n_expected = n) %>% 
    filter(!is.na(cate)) %>% 
    mutate(N_total_expected = sum(n_expected),
           q = n_expected / N_total_expected)
    
  domain_joined <-
    activities_interested %>%  
    left_join(student_info_by_semester_domain_joined %>% 
                select_if(names(.) %in% c(list_attributes, "source", "Num_year_term")), 
              by = c("source", "Num_year_term")) %>% 
    rename(cate = i)

  prob_attribute <- data.frame() # 초기화
  # attribute X domain probability
  for (j in list_domains) {
    print(paste0("domain : ",j))
    prob_attribute_temp <-
      domain_joined %>% 
      filter(Domain == j) %>% 
      count(cate, Num_year_term) %>% 
      rename(n_observed = n) %>% 
      right_join(expected_prob, by = c("cate", "Num_year_term")) %>% 
      mutate(Domain = j,
             n_observed = replace_na(n_observed, 0)) %>% 
      group_by(Num_year_term) %>% 
      mutate(n_within_attribute = n_distinct(cate[n_observed > 0]),
             p = n_observed / sum(n_observed),
             I = p * log(p),
             num_attr = 1:n()) %>% 
      ungroup()     %>% 
      mutate(cate = as.character(cate),
             attribute = i)
    
    prob_attribute <- prob_attribute %>% 
      bind_rows(prob_attribute_temp)
  }
  
  
  # 지수 계산
  index_list_raw_temp <- prob_attribute %>%
    group_by(Domain, Num_year_term, attribute, n_within_attribute) %>% 
    summarise(Shannon_Entropy = -sum(I, na.rm=T),
              sump = sum(p),
              sumq = sum(q),
              Gini_Simpson_Index = (1 - sum(p^2, na.rm=T)),
              KLD = sum(I, na.rm=T) - sum(p * log(q), na.rm=T)) 
  
  # 지수 저장    
  index_list_raw <- index_list_raw %>% 
    bind_rows(index_list_raw_temp)
  
  # prob 원자료
  prob_semester_by_domain <- rbind(prob_semester_by_domain, prob_attribute)
}


index_list <- index_list_raw %>% 
  filter(n_within_attribute != 0) %>% # 해당 학기 해당 attribute 종류가 0개인 경우는 제거
  left_join(year_term_tl[,4:5], by = "Num_year_term") %>% 
  as_tibble()


  



