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
source("../../../R_functions/func.r", encoding = 'utf-8')

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
  filter(졸업년도 > 2010) %>% 
  filter(입학년도 >= 2000)   %>% 
  left_join(학과정보, by = "학과코드") %>% 
  mutate(학적상태 = "졸업") %>% 
  filter(캠퍼스구분 == 1) 


student_info_std <- read.delim("../../../재학생_학부_기본정보.txt", header = T,
                               sep = "|", stringsAsFactors = FALSE) %>% 
  filter(입학년도 %in% c(2001:2019)) %>% 
  left_join(학과정보, by = "학과코드") %>% 
  mutate(학적상태 = "재학") %>% 
  filter(캠퍼스구분 == 1)

student_info <- student_info_alumni %>% 
  bind_rows(student_info_std)

student_info <- student_info %>% 
  mutate(student_code = 1:n()) %>% 
  as_tibble()



############################################################################
######################### 입학전형 네트워크 ################################
############################################################################

network_ent <- student_info %>% 
  select(student_code, 입학유형, 입학년도) %>% 
  mutate(Domain = "입학유형") %>% 
  
  rename(Source = student_code, 
         Target = 입학유형,
         Year = 입학년도
         ) %>% 
  mutate(Label = Target)


############################################################################
######################### 출신학교 네트워크 ################################
############################################################################

# 졸업 년도에 따른 가중치 고려?

preschool_raw <- read.delim("../../../재학생_학부_출신학교.txt", header=T, 
                            sep = "|", stringsAsFactor = F) 

network_preschool <- preschool_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  filter(출신교명 != "",
             !is.na(출신교졸업년도)) %>% 
  mutate(Source = student_code,
         Domain = "출신교",
         Target = 출신교명,
         Category = "출신교") %>% 
  mutate(year_term = paste(입학년도, 입학학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  select(Source, Target, Num_year_term, Domain) %>% 
  mutate(Label = Target) %>% 
  as_tibble()


############################################################################
######################### 학과 네트워크 ################################
############################################################################

network_major <- student_info %>% 
  mutate(Domain = "학과") %>% 
  rename(Source = student_code, 
         Target = 학과) %>% 
  mutate(year_term = paste(입학년도, 입학학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  select(Source, Target, Num_year_term, Domain) %>% 
  mutate(Label = Target) %>% 
  as_tibble()


############################################################################
######################### 단과대학 네트워크 ################################
############################################################################

network_college <- student_info %>% 
  mutate(Domain = "대학") %>% 
  rename(Source = student_code, 
         Target = 대학) %>% 
  mutate(year_term = paste(입학년도, 입학학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  select(Source, Target, Num_year_term, Domain) %>% 
  mutate(Label = Target) %>% 
  as_tibble()



############################################################################
######################### 학적변동 네트워크 ################################
############################################################################


record_alumni <- read.delim("../../../졸업생_학부_학적변동내역.txt", header = T,
                           sep = "|", stringsAsFactors = FALSE)
record_std <- read.delim("../../../재학생_학부_학적변동내역.txt", header = T,
                        sep = "|", stringsAsFactors = FALSE)

record_raw <-  bind_rows(record_alumni, record_std) %>% 
  inner_join(student_info, by = "식별자") %>% 
  mutate(year_term = paste(년도, 학기, sep="_")) %>% 
  left_join(year_term_tl %>% 
              select(year_term, Num_year_term), by = "year_term") %>% 
  as_tibble()

network_leaveOfAbsence <-
record_raw %>% 
  filter(grepl("휴학|복학", 변동유형)) %>% 
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
  
  # Edge 데이터
  mutate(Source = student_code,
         Target = paste0(휴학유형, "휴학"),
         Domain = paste0(휴학유형, "휴학"),
         Label = paste0(휴학유형, "휴학")) %>% 
  select(Source, Target, Domain, Num_year_term, Label)

# 휴학 제외 학적 

network_record <- record_raw %>% 
  filter(!grepl("휴학|복학|휴학경과제적", 변동유형)) %>% 
  select(student_code, Num_year_term, 변동유형) %>% 
  arrange(student_code, Num_year_term) %>%
  mutate(Source = student_code,
         Target = 변동유형,
         Domain = 변동유형,
         Label = 변동유형) %>% 
  select(Source, Target, Domain, Num_year_term, Label)

############################################################################
######################### 장학금 네트워크 ################################
############################################################################

# 어떻게 활용할 지 좀 더 고민해보자

# scholarship_alumni <- read.delim("../../../졸업생_학부_장학금수혜.txt", header = T,
#                          sep = "|", stringsAsFactors = FALSE)
# 
# scholarship_std <- read.delim("../../../졸업생_학부_장학금수혜.txt", header = T,
#                         sep = "|", stringsAsFactors = FALSE)
# 
# scholarship_raw <-  bind_rows(scholarship_alumni, scholarship_std)


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
  mutate(Source = student_code,
         Target = 상벌유형,
         Domain = Target,
         Label = 상벌유형) %>% 
  select(Source, Target, Domain, Num_year_term, Label) %>% 
  filter(nchar(Target)!=0)


############################################################################
######################### 학생상담센터 네트워크 ################################
############################################################################

# 학기 데이터 필요
# kuscc_raw <- read_xlsx("../../../재학생_학부_학생상담센터.xlsx")


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
#          Source = student_code,
#          Label = 파견대학) %>% 
#   select(Source, Target, Domain, Label)


############################################################################
######################### Binding Data  ################################
############################################################################

# 엣지 합치기
edges <- network_preschool %>% 
  # bind_rows(network_exchange) %>% 
  # bind_rows(network_course_history) %>% 
  bind_rows(network_college) %>% 
  bind_rows(network_major) %>% 
  bind_rows(network_award) %>% 
  bind_rows(network_record) %>% 
  bind_rows(network_leaveOfAbsence) %>% 
  as_tibble() %>% 
  mutate(Target = gsub("[[:space:]]", "", Target)) %>% 
  mutate(참여여부 = 1)
  

nodes_engagement <- edges %>% 
  filter(!Domain %in% c("단과대학", "학과")) %>% 
  reshape2::dcast(Source ~ Domain, value.var = "참여여부") %>% 
  janitor::clean_names()

nodes_std <-
student_info %>%
  rename(Id = student_code) %>% 
  left_join(nodes_engagement, by = c("Id" = "source")) %>% 
  mutate(Domain = "학생",
         Label = "", 
         Source = Id,
         Id = as.character(Id))
  

nodes <- edges %>% 
  distinct(Target, .keep_all = T) %>% 
  rename(Id = Target) %>% 
  bind_rows(nodes_std) %>% 
  mutate(Label = case_when(Domain == "해외대학파견" ~ "해외대학파견",
                           TRUE ~ Label)) %>% 
  select(-참여여부, -Source)


# make nodes 
# write.csv(nodes, file="nodes.csv", fileEncoding = 'utf-8', row.names=F)

# make edges 
# write.csv(edges, file="edges.csv", fileEncoding = 'utf-8', row.names=F)





############################################################################
######################### 다양성 분석  ################################
############################################################################


#####################################
####### 기본정보 추가   #########
#####################################

student_info_for_idx <- student_info %>% 
  # 출신교 join
  left_join(network_preschool %>% 
              select(Source, Target) %>% 
              rename(출신교 = Target), by = c("student_code" = "Source"))
# %>% 
#   mutate(출신교 = ifelse(국적!= "KOR", NA, 출신교))  
  


#####################################
####### 당학기 재학생 분석  #########
#####################################
student_info_by_semester <- student_info_for_idx %>% 
  mutate(ent_year_term = paste(입학년도, 입학학기, sep="_"),
         grd_year_term = paste(졸업년도, 졸업학기, sep="_")) %>% 
  left_join(year_term_tl[,4:5], by = c("ent_year_term" = "year_term")) %>% 
  left_join(year_term_tl[,4:5], by = c("grd_year_term" = "year_term"))  %>% 
  mutate(재학기간 = case_when(학적상태=="재학" ~ 40 + 1 - Num_year_term.x,
                              학적상태=="졸업" ~ Num_year_term.y +1 - Num_year_term.x)) %>% 
  
  rename(Num_ent_year_term = Num_year_term.x,
           Num_grd_year_term = Num_year_term.y) %>% 
    
  # 년도_학기별 재학생
  uncount(재학기간) %>% 
  group_by(student_code) %>% 
  mutate(Num_year_term = Num_ent_year_term + row_number()-1) %>% 
  
  # 휴학 기간 포함 인원 제거
  anti_join(network_leaveOfAbsence, by = c("student_code" = "Source", "Num_year_term")) 
  
  


# 분석 활동 설정
list_attributes <- c("성별", "학과", "국적", "입학유형", "대학")
list_domains <- c("학기우등생", "학기최우등생", "성적경고",
                  "성적경고해제", "조기졸업", "수료",
                  "교환학생(국외)", "이중전공포기", "일반휴학")


activities_interested <- edges %>% 
  filter(Domain %in% list_domains)

# make empty data frame
index_list <- data.frame()
attributes_raw <- data.frame()
cutoff_n = 0 # 활동당 최소 인원
cutoff_year = 1 # 시작 시점 2011_1R (23번), 

for(i in list_attributes) {
expected_prob <-
  student_info_by_semester %>%
  group_by(Num_year_term) %>% 
  count_(i) %>% 
  rename(cate = i) %>% 
  mutate(total_N = sum(n),
         q = n / total_N) %>% 
  rename(n_expected  = n) %>% 
  filter(Num_year_term >= cutoff_year)

activities_interested_selected <- activities_interested %>%
  select(-참여여부) %>% 
  group_by(Num_year_term, Domain) %>% 
  mutate(N_domain = n()) %>% 
  filter(N_domain >= cutoff_n,
         Num_year_term >= cutoff_year) %>% 
  ungroup() 
  
activities_interested_names <- activities_interested_selected %>% 
  distinct(list_name = paste(Domain, Num_year_term, sep="_")) 

# 개별 속성의 확률 확인용 
activities_interested_processed <- activities_interested_selected %>%  
  group_by(Domain, Num_year_term) %>% 
  left_join(student_info_for_idx, by = c("Source" = "student_code")) %>% 
  count_(i) %>% 
  rename(cate = i) %>% 
  mutate(p = n / sum(n)) %>% 
  inner_join(expected_prob, by = c("cate", "Num_year_term")) %>% 
  mutate(
    I = -p * log(p),
    KLD = -p * log(q) - (-p * log(p)),
    num_attr = 1:n())


# 지수 계산
index_list_temp <- activities_interested_processed %>%
  group_by(Domain, Num_year_term) %>% 
         summarise(Shannon_Entropy = sum(I),
                   Gini_Simpson_Index = (1 - sum(p^2)),
                   KLD = sum(KLD),
                   attribute = i)

# 지수 저장    
index_list <- index_list %>% 
  bind_rows(index_list_temp) %>% 
  arrange(desc(KLD), Domain, attribute)

# 개별 속성 저장
attributes_raw <- attributes_raw %>% 
  bind_rows(activities_interested_processed)

}

index_list <- index_list %>% 
  left_join(year_term_tl[,4:5], by = "Num_year_term")

attributes_raw <- attributes_raw %>% 
  left_join(year_term_tl[,4:5], by = "Num_year_term")


index_list %>%
  ggplot(aes(year_term, Gini_Simpson_Index, 
             fill = attribute, group = attribute)) +
  geom_line() +
  geom_point(size = 2, shape = 21) +
  facet_wrap(~Domain) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Spectral")  +
  labs(x = "Year")
   
index_list %>%
  ggplot(aes(year_term, Shannon_Entropy, 
             fill = attribute, group = attribute)) +
  geom_line() +
  geom_point(size = 2, shape = 21) +
  facet_wrap(~Domain) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Spectral")  +
  labs(x = "Year")


index_list %>%
  ggplot(aes(year_term, KLD, 
             fill = attribute, group = attribute)) +
  geom_line() +
  geom_point(size = 2, shape = 21) +
  facet_wrap(~Domain, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Spectral")  +
  labs(x = "Year")


index_list %>%
  ggplot(aes(Gini_Simpson_Index, Shannon_Entropy, color = attribute,
             fill = attribute, group = attribute)) +
  geom_line() +
  geom_point(aes(group = seq_along(Num_year_term)), 
             size = 2, shape = 21, color = "black") +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Domain)
  

library(gganimate)

animate <-
index_list %>%
  ggplot(aes(Gini_Simpson_Index, Shannon_Entropy, color = attribute,
             fill = attribute, group = attribute)) +
  geom_line() +
  geom_point(aes(group = seq_along(Num_year_term)), # 점들이 순차적으로 등장하게 만들기 위해서 필요
             size = 2, shape = 21, color = "black", alpha = 0.7) +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Domain) +
  transition_reveal(along = Num_year_term) + 
  # view_follow() +
  labs(title = "Year: {ceiling(frame_along/2-1)+2000}")
  
animate(animate, height = 1000, width = 1000, 
        res = 100,
        renderer = gifski_renderer("gganim.gif"))

#################################################################
# plotly로 animation 만드는 법 
#################################################################
# p <- index_list %>%
#   accumulate_by(~ Num_year_term) %>% 
#   ggplot(aes(year_term, KLD, 
#                    fill = attribute)) +
#   geom_line(aes(frame = frame,group = attribute)) +
#   geom_point(alpha = 0.7, shape = 21, color="black", size = 3, 
#              aes(frame=frame)) +
#   labs(x = "", y = "") + 
#   theme_minimal() +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45)) + 
#   scale_fill_brewer(palette = "Spectral") +
#   facet_wrap(~Domain, scales="free_y") 
#   
# ggplotly(p) %>% 
#   layout(
#     yaxis = list(
#       title = "KLD",
#       zeroline = F
#     ),
#     xaxis = list(
#       zeroline = F, 
#       showgrid = F
#     )
#   ) %>% 
#   animation_opts(
#     frame = 100, 
#     transition = 0, 
#     easing = "linear",
#     redraw = FALSE
#   ) %>%
#   animation_slider(
#     hide = T
#   ) %>%
#   animation_button(
#     x = 1, xanchor = "right", y = 0, yanchor = "top"
#   )
