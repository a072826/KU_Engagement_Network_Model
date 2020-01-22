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
# source("../../R_functions/func.r", encoding = 'utf-8')
# setwd(dirname(current_path ))
# getwd()
source("../../R_functions/engagement_network_data.R", encoding = 'utf-8')
# setwd(dirname(current_path))
# getwd()

# student_info_for_idx <- student_info_for_idx %>% 
#   filter(grepl("졸업", 학적상태))
# 
# student_info_for_idx <- student_info_for_idx %>% 
#   filter(!grepl("편입", 입학유형)) 


###############################
# 졸업생 학생 성장 지표 ######
###############################


# # 성적 향상률
# gpa_record_alumni <-  read.delim("../../졸업생_학부_학점_누계.txt", header = T,
#                                      sep = "|", stringsAsFactors = FALSE) %>%
#   semi_join(student_info_for_idx, by = "식별자") %>% 
#   as_tibble() 
# 
# 
# gpa_total <- gpa_record_alumni %>%
#   filter(년도 == 0, 학기 == "00")
# 
# 
# student_info_for_idx %>% 
#   count(입학년도)
# 
# student_info_for_idx %>% 
#   group_by(입학년도) %>% 
#   summarise(n = n_distinct(국적))
#   
# 
# student_info_for_idx %>% 
#   filter(입학구분 == "정원외신입학") %>% 
#   group_by(입학년도) %>% 
#   summarise(n = n())
# 
# 
# student_info_for_idx %>% 
#   filter(입학구분 == "정원외신입학") %>% 
#   distinct(입학유형) %>% View()
#  
# 
# student_info_for_idx %>% 
#   anti_join(gpa_total %>% select(식별자), by = "식별자") %>% View()
# 
# 
# gpa_record <- gpa_record_alumni %>%
#   filter(nchar(년도)>=4) %>%
#   filter(!is.na(학점), 학점!=0, 이수학점 >= 12) %>%  # 12학점 이상 수강 학기만 인정
#   filter(!학기 %in% c("1S", "2W")) %>%
# 
#   mutate(year_term = paste(년도, 학기, sep="_")) %>%
#   left_join(year_term_tl %>%
#               select(year_term, Num_year_term), by = "year_term") %>%
# 
#   arrange(Num_year_term) %>%
#   group_by(식별자) %>%
#   mutate(누적학기 = 1:n()) %>%
#   ungroup() %>%
#   arrange(식별자, 누적학기)
# 
# 
# 
# gpa_slope <- std_gpa_record_final %>%
#   group_by(식별자) %>%
#   do(tidy(lm(학점 ~ 누적학기, data=.))) %>%
#   filter(term != "(Intercept)")
# 
# gpa_slope %>%
#   arrange(-estimate) %>% View()
# 


# ###############################
# 다양성 ######
###############################

# student_info_for_idx %>% 
#   count(입학년도, 국적)


# student_minority %>% 
# count(입학년도) %>% 
#   mutate(입학년도 = as.factor(입학년도)) %>% 
#   ggplot(aes(입학년도, n, group = 1)) +
#   geom_col() +
#   geom_line() +
#   geom_point() +
#   labs(y = "입학생") +
#   theme(axis.text.x = element_text(angle = 45)) +
#   ggsave("년도별입학생.png", height = 5, width = 5)
# 
# 
# student_minority %>% 
#   count(입학유형)
# 
# 
# student_minority %>% 
#   group_by(ㄱ) %>% 
#   count(진리형_프로그램장학금_1학기) %>% View()
#   
#   count(입학유형, ) %>% View()

  


