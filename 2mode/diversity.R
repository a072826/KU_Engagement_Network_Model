library(tidyverse)
library(janitor)
library(widyr)
library(readxl)
library(Matrix)
library(widyr)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()

source("../../../R_functions/engagement_network_data.R", encoding = 'utf-8')

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
list_attributes <- c("성별", "국적", 
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
    count_(i) %>% 
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




