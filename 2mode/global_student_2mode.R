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

year_term_tl <- tibble("year_term" = paste(rep(2001:2019, each=2), c("1R", "2R"), sep = "_"),
                       "Num_year_term" = 1:38)

year_term_tl <- tibble(year = rep(2001:2020, each = 2),
                       term = rep(c("1R", "2R"), 20)) %>% 
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
  filter(캠퍼스구분 == 1)


student_info_std <- read.delim("../../../재학생_학부_기본정보.txt", header = T,
                               sep = "|", stringsAsFactors = FALSE) %>% 
  filter(입학년도 %in% c(2001:2019)) %>% 
  left_join(학과정보, by = "학과코드") %>% 
  filter(캠퍼스구분 == 1)


student_info <- student_info_alumni %>% 
  bind_rows(student_info_std)

student_info <- student_info %>% 
  mutate(student_code = 1:n()) %>% 
  as_tibble()


student_info_alumni %>% nrow()
student_info_std %>% nrow()
student_info %>% nrow()


############################################################################
######################### 입학전형 네트워크 ################################
############################################################################

ent_network <- student_info %>% 
  select(입학유형, student_code) %>% 
  mutate(Domain = "입학유형") %>% 
  rename(Source = student_code, Target = 입학유형) %>% 
  mutate(Label = Target)


############################################################################
######################### 출신학교 네트워크 ################################
############################################################################

# 졸업 년도에 따른 가중치 고려?

preschool_raw <- read.delim("../../../재학생_학부_출신학교.txt", header=T, 
                            sep = "|", stringsAsFactor = F) 

preschool_network <- preschool_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  filter(출신교명 != "",
             !is.na(출신교졸업년도)) %>% 
  mutate(Source = student_code,
         Domain = "출신교",
         Target = 출신교명,
         Category = "출신교") %>% 
  select(Source, Target, Domain) %>% 
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
######################### 단과대학 네트워크 ################################
############################################################################

college_network <- student_info %>% 
  select(대학, student_code) %>% 
  mutate(Domain = "단과대학") %>% 
  rename(Source = student_code, Target = 대학) %>% 
  mutate(Label = Target)


############################################################################
######################### 학적변동 네트워크 ################################
############################################################################

record_alumni <- read.delim("../../../졸업생_학부_학적변동내역.txt", header = T,
                           sep = "|", stringsAsFactors = FALSE)

record_std <- read.delim("../../../졸업생_학부_학적변동내역.txt", header = T,
                        sep = "|", stringsAsFactors = FALSE)

record_raw <-  bind_rows(record_alumni, record_std)

record_network <- record_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  filter(!grepl("복학", 변동유형)) %>% 
  mutate(Source = student_code,
         Target = 변동유형,
         Domain = 변동유형,
         Label = 변동유형) %>% 
  select(Source, Target, Domain, Label) %>% 
  filter(!is.na(Target))




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

award_raw <-  bind_rows(award_alumni, award_std)

award_network <- award_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  mutate(Source = student_code,
         Target = 상벌유형,
         Domain = Target,
         Label = 상벌유형) %>% 
  select(Source, Target, Domain, Label) %>% 
  filter(nchar(Target)!=0)


############################################################################
######################### 학생상담센터 네트워크 ################################
############################################################################

kuscc_raw <- read_xlsx("../../../재학생_학부_학생상담센터.xlsx")

kuscc_network <- kuscc_raw %>% 
  inner_join(student_info, by = "식별자") %>% 
  mutate(Source = student_code,
         Target = 구분,
         Domain = Target,
         Label = 구분) %>% 
  select(Source, Target, Domain, Label) %>% 
  filter(!is.na(Target))

  
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
edges <- preschool_network %>% 
  # bind_rows(exchange_network) %>% 
  # bind_rows(course_history_network) %>% 
  bind_rows(college_network) %>% 
  bind_rows(major_network) %>% 
  bind_rows(award_network) %>% 
  bind_rows(record_network) %>% 
  bind_rows(kuscc_network) %>% 
  as_tibble() %>% 
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
  mutate(Target = gsub("[[:space:]]", "", Target)) %>% 
  distinct(Target, .keep_all = T) %>% 
  rename(Id = Target) %>% 
  bind_rows(nodes_std) %>% 
  mutate(Label = case_when(Domain == "해외대학파견" ~ "해외대학파견",
                           TRUE ~ Label)) %>% 
  select(-참여여부, -Source)


# make nodes 
write.csv(nodes, file="nodes.csv", fileEncoding = 'utf-8', row.names=F)

# make edges 
write.csv(edges, file="edges.csv", fileEncoding = 'utf-8', row.names=F)


############################################################################
######################### 다양성 분석  ################################
############################################################################


# 분석 활동 설정
list_attributes <- c("성별", "학과", "국적", "입학유형")
list_domains <- unique(edges$Domain)[1:10]


activities_interested <- edges %>% 
  filter(Domain %in% list_domains)

# make empty data frame
index_list <- data.frame()
attributes_raw <- data.frame()
cutoff = 20

for(i in list_attributes) {
expected_prob <-
  student_info %>% 
  count_(i) %>% 
  rename(cate = i) %>% 
  mutate(total_N = sum(n),
         q = n / total_N) %>% 
  rename(n_expected  = n)

activities_interested_selected <- activities_interested %>%
  count(Source, Domain) %>%
  mutate(total_N = n()) %>% 
  arrange(Domain, n) %>% 
  group_by(Domain, n) %>% 
  mutate(domain_n = n()) %>% 
  filter(domain_n >= cutoff) %>% 
  ungroup()
  
activities_interested_names <- activities_interested_selected %>% 
  distinct(Domain, n) %>% 
  mutate(list_name = paste(Domain, n, sep="_")) 

# 개별 속성의 확률 확인용 
activities_interested_processed <- activities_interested_selected %>% 
  group_split(Domain, n) %>% 
  setNames(activities_interested_names$list_name) %>% 
  map( ~ left_join(.x, student_info, by = c("Source" = "student_code")) %>% 
         count_(i) %>% 
         rename(cate = i) %>% 
         mutate(p = n / sum(n)) %>% 
         inner_join(expected_prob, by = "cate") %>% 
         mutate(I = -p * log(p),
                KLD = -p*log(q) - (-p*log(p)),
                num_attr = 1:n())) %>% 
  bind_rows(.id = "domain")

# 지수 계산
index_list_temp <- activities_interested_processed %>% 
  group_by(domain) %>% 
         summarise(Shannon_Entropy = sum(I),
                   Gini_Simpson_Index = (1 - sum(p^2)),
                   KLD = sum(KLD),
                   attribute = i)

# 지수 저장    
index_list <- rbind(index_list, index_list_temp) %>% 
  arrange(desc(KLD), domain, attribute)

# 개별 속성 저장
attributes_raw <- rbind(attributes_raw, activities_interested_processed)

}


index_list %>%
  mutate(broad_cate = str_extract(domain, pattern = ".+(?=_)")) %>%  # 정규식 표현 전방탐색 (?=) 
  ggplot(aes(Gini_Simpson_Index, KLD, color = attribute)) +
  geom_point() +
  facet_wrap(~broad_cate) +
  geom_text(aes(label = domain), hjust = 1, vjust=1)
