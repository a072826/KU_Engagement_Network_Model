library(tidyverse)
library(janitor)
library(widyr)
library(readxl)
library(Matrix)
library(widyr)
library(rstudioapi)
library(broom)

current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()
source("../../../R_functions/func.r", encoding = 'utf-8')
source("../../../R_functions/engagement_network_data.r", encoding = 'utf-8')
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()

###############################################
# 12-22-2019 criteria 및 attribute 결정 필요##
###############################################

##############################################
## 분석 개요##################################
##############################################

#########################################
# 분석 대상 #############################
# 학적     : 졸업생
# 입학 유형: 편입학 제외
# 입학     : 2010년 이후
#########################################

std_gpa_record_alumni <-  read.delim("../../../졸업생_학부_학점_누계.txt", header = T,
                                     sep = "|", stringsAsFactors = FALSE) %>% 
  semi_join(student_info, by = "식별자")

std_gpa_record_std <-  read.delim("../../../재학생_학부_학점_누계.txt", header = T,
                                  sep = "|", stringsAsFactors = FALSE) %>% 
  semi_join(student_info, by = "식별자")

std_gpa_record_final <- std_gpa_record_alumni %>% 
  bind_rows(std_gpa_record_std) %>% 
  as_tibble()  %>% 
  filter(년도 == 0, 학기 == "00")





# 개방성 측면에서의 다양성
criteria <- edges %>%
  distinct(Domain)

c(criteria)

# criteria <- c("성적우수표창", "성적경고")

# criteria <- c("성적우수표창", "성적경고",
#               "교환학생_국외", "학점교류_국내",
#               "이중전공신청", "이중전공포기",
#               "융합전공신청", "융합전공포기") # 학생성공과 관련있는 지표



criteria <- c("성적우수표창", 
              "교환학생_국외", "학점교류_국내",
              "이중전공신청", "장학금수혜",
              "융합전공신청") # 학생성공과 관련있는 지표


criteria <- c("학점_증명용") # 학생성공과 관련있는 지표

attributes <- c("성별", "입학유형")



optimum_raw <- student_info_for_idx %>%
  select(-학점) %>%  # 네트워크 데이터에서 넘어온 학점 제거
  
  filter(학적상태 %in% c("졸업", "조기졸업")) %>%
  filter(!grepl("편입", 입학유형)) %>%
  filter(입학년도 >= 2000) %>%
  filter(졸업년도 >= 2000) %>%
  mutate(perspective = paste(입학년도, 대학, sep = "_")) %>%
  mutate(다전공이름 = case_when(다전공종류=="심화전공"~학과,
                                다전공종류=="이중전공"~이중전공,
                                다전공종류=="융합전공"~융합전공)) %>%
  # mutate(국적 = fct_lump(국적, n = 20))  %>%
  # unite_("element", c(attributes[1], attributes[2]), sep = "|")
  mutate_(element = attributes[2]) %>% 
  left_join(std_gpa_record_final %>% 
              select(식별자, 학점_증명용, 학점, 신청학점, 이수학점), by = "식별자")


element_p <- optimum_raw %>%
  select_if(names(.) %in% c("element", criteria, "perspective")) %>%
  group_by(perspective) %>%
  mutate(N_year = sum(n())) %>%
  group_by(perspective, N_year) %>%
  count(element) %>%
  mutate(p = n / N_year)  %>%
  group_by(perspective) %>%
  mutate(Num_row = 1:n(),
         Num_element = n()) %>%
  ungroup()


optimum_temp <- optimum_raw %>%
  group_by(perspective) %>%
  select_if(names(.) %in% c("element", criteria)) %>%
  replace(is.na(.), 0) %>%
  group_by(perspective, element) %>%
  summarise_at(.vars = criteria, .funs = c(mean = "mean"), na.rm=T) %>%
  group_by(perspective) %>%
  mutate(Num_row = 1:n()) %>%
  ungroup()



# w_c <- data.frame(criteria = criteria, importance = c(1, -1))
# 
# optimum_raw %>%
#   replace(is.na(.), 0) %>%
#   group_by(element) %>%
#   # mutate(n_within_element = n()) %>%
#   # filter(n_within_element > 1) %>%
#   # group_by(perspective) %>%
#   # mutate(n_within_perspective = n()) %>%
#   # filter(n_within_perspective > n_within_element*2) %>%
#   group_by(대학) %>%
#   nest() %>%
#   mutate(fit = map(data, ~ lm(성적우수표창 ~ element, data = .x)),
#          tidied = map(fit, tidy)) %>%
#   unnest(tidied) %>%
#   filter(term != "(Intercept)")

attribute_d <- optimum_temp %>%
  mutate_at(vars(contains("mean")), ~scale(.)) %>% # 전체 기록 바탕으로 element별 difference 계산
  group_split(perspective) %>%
  setNames(unique(sort(as.character(optimum_temp$perspective)))) %>%
  map(~ select(.x, -element, -perspective) %>%
        column_to_rownames(var = "Num_row") %>%
        proxy::dist(method = "Euclidean") %>%
        as.matrix() %>%
        reshape2::melt(varnames = c("row", "col"))) %>%
  map_df(~as.data.frame(.x), .id = "perspective") %>%
  left_join(element_p %>% select(element, Num_row, p, perspective), by = c("row" = "Num_row", "perspective")) %>%
  rename(row_element = element,
         p_row = p) %>%
  left_join(element_p %>% select(element, Num_row, p, perspective), by = c("col" = "Num_row", "perspective")) %>%
  rename(col_element = element,
         p_col = p,
         d = value)  %>% 
  as_tibble()

diversity_matrix <- attribute_d %>% 
  group_by(perspective) %>% 
  filter(n() < 2 | row > col) %>%  # variety가 1이면 유지. 2이상이면, lower_diagonal만 남김
  separate(perspective, c("입학년도", "대학"), "_") %>%  
  ungroup()  


# diversity_matrix %>% 
#   distinct(입학년도, 대학, col, p_col) %>% 
#   mutate(entropy = p_col * log(p_col))  %>% 
#   group_by(입학년도, 대학) %>% 
#   summarise(shannon_entropy = -sum(entropy)) %>% View()

diversity_data <-  diversity_matrix %>% 
  group_by(입학년도, 대학) %>%
  summarise(variety = max(row),  # scaled variety
            balance = 2 * sum(p_row * p_col),  # balence-weighted variety
            diversity = sum(d * p_row * p_col))  %>% # balance/disparity-weighted variety
  mutate(balance = case_when(variety ==1 ~ 0,
                             TRUE ~ balance)) %>% 
  arrange(-diversity) %>%
  ungroup()
  # mutate(입학년도 = as.numeric(입학년도))

heuristic_data <- diversity_data %>% 
  filter(입학년도 < 2016) %>% 
  mutate(입학년도 = as.numeric(입학년도)) %>%
  # mutate(variety = as.factor(variety)) %>%
  filter(!대학 %in% c("법과대학", "정보통신대학"))


# p_balance <- .diversity_plot(data = heuristic_data, group_value = "대학", 
#                              x_value = "입학년도", x_label = "입학년도",
#                              y_value = "balance", y_label = "균등성 (balance)",
#                              size_value = "variety", size_label = "다종성 (variety)",
#                              startColor = "#0078bc", endColor = "#ae0e36",
#                              ncol = 3) 
# 
# ggsave(p_balance, filename = "p_balance.png", dpi = 300, width = 5.5, height = 6)
# 
# 
# p_diversity <- .diversity_plot(data = heuristic_data, group_value = "대학", 
#                              x_value = "입학년도", x_label = "입학년도",
#                              y_value = "diversity", y_label = "다양성 (diversity)",
#                              size_value = "variety", size_label = "다종성 (variety)",
#                              startColor = "#0078bc", endColor = "#ae0e36",
#                              ncol = 3)
# 
# ggsave(p_diversity, filename = "p_diversity.png", dpi = 300, width = 5.5, height = 6)

############################################################
################## animation 용 ############################

library(gganimate)
temp_color = colorRampPalette(brewer.pal(11,"Spectral"))(16)

d_balance_by_diversity <- heuristic_data %>%
  rename(x = balance,
         y = diversity,
         group = 대학) %>%
  mutate(입학년도 = as.numeric(입학년도)) %>%
  filter(!group %in% c("법과대학"))
  

balance_by_diversity_order <- d_balance_by_diversity %>% 
  group_by(group) %>% 
  do(tidy(lm(y ~ x, data=.))) %>% 
  mutate(n_term = n()) %>% 
  filter(n_term == 1 | term != "(Intercept)") %>% 
  arrange(-estimate)  %>% 
  ungroup() %>% 
  mutate(order = 1:n())


p_balance_by_diversity <- d_balance_by_diversity %>% 
  left_join(balance_by_diversity_order %>% select(group, order), by = "group") %>% 
  mutate(group = fct_reorder(group, order)) %>%
  ggplot(aes(x, y, color = group,
             fill = group, group = group,
             alpha = 입학년도 - min(입학년도) / max(입학년도))) +
  geom_line() +
  geom_point(aes(group = seq_along(입학년도),
                 size = variety),  # 점들이 순차적으로 등장하게 만들기 위해서 필요
             shape = 21, color = "white") +
  scale_fill_manual(name = "College (대학)", values = temp_color) +
  scale_color_manual(name = "College (대학)", values = temp_color) +
  scale_size_continuous(range = c(1, 4)) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  scale_y_continuous(limits = c(0, NA)) +
  ggdark::dark_theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~group, ncol=5)  +
  guides(alpha = F, fill = F, color = F) +
  # guides(fill = guide_legend(override.aes = list(size=4, linetype = 0))) +
  labs(x = "Balance (균등성, 0=완전불균형, 1=완전균형)",
       y = "Diversity (다양성)",
       size = "Variety (다종성)") +
  geom_smooth(formula = y ~ x, method = "lm", linetype=3, 
              size = 0.5, color = "white", se = F, fullrange = T) 
p_balance_by_diversity
ggsave(p_balance_by_diversity, filename = "p_balance_by_diversity.png", dpi = 300, width = 6, height = 5)


p_animate <- p_balance_by_diversity +
  transition_reveal(along = x) +
  # view_follow() +
  labs(title = "Year: {ceiling(frame_along)+1999}")

animate(p_animate, width = 1920, height = 1080,
        res = 150, end_pause = 50, nframes = 100,
        renderer = gifski_renderer("test.gif"))
######################################################################################################

summary(heuristic_data$variety)




p <- optimum_raw %>% 
  filter(대학 %in% c("미디어학부", "국제학부")) %>% 
  group_by(대학) %>% 
  mutate(입학유형 = fct_lump(입학유형, n = 4, other_level = "기타")) %>% 
  group_by(입학유형) %>% 
  mutate(n = n(),
         mean = mean(학점_증명용, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(입학유형 = fct_reorder(입학유형, -n)) %>% 
  ggplot(aes(학점_증명용, fill = 입학유형)) +
  geom_density(alpha = 0.3) + 
  facet_wrap(~대학, scale="free_y", ncol = 5) 
  geom_vline(aes(xintercept = mean, color = 입학유형), size = 2)

  .myggplotly(p)
  