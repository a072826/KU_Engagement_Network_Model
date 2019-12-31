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
source("engagement_network_data.r", encoding = 'utf-8')

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


# 개방성 측면에서의 다양성
criteria <- edges %>%
  distinct(Domain)

c(criteria)

criteria <- c("성적우수표창", "성적경고")

criteria <- c("성적우수표창", "성적경고",
              "수료","이중전공신청", "이중전공포기",
              "교환학생_국외", "학점교류_국내",
              "융합전공신청", "융합전공포기") # 학생성공과 관련있는 지표

attributes <- c("성별", "입학유형")



optimum_raw <- student_info_for_idx %>%
  filter(학적상태 %in% c("졸업", "조기졸업")) %>%
  filter(!grepl("편입", 입학유형)) %>%
  filter(입학년도 >= 2000) %>%
  filter(졸업년도 >= 2010) %>%
  mutate(perspective = paste(졸업년도, 대학, sep = "_")) %>%
  mutate(다전공이름 = case_when(다전공종류=="심화전공"~학과,
                                다전공종류=="이중전공"~이중전공,
                                다전공종류=="융합전공"~융합전공)) %>%
  # mutate(국적 = fct_lump(국적, n = 20))  %>%
  # unite_("element", c(attributes[1], attributes[2]), sep = "|")
  mutate_(element = attributes[2])


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



w_c <- data.frame(criteria = criteria, importance = c(1, -1))

optimum_raw %>%
  replace(is.na(.), 0) %>%
  group_by(element) %>%
  # mutate(n_within_element = n()) %>%
  # filter(n_within_element > 1) %>%
  # group_by(perspective) %>%
  # mutate(n_within_perspective = n()) %>%
  # filter(n_within_perspective > n_within_element*2) %>%
  group_by(대학) %>%
  nest() %>%
  mutate(fit = map(data, ~ lm(성적우수표창 ~ element, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term != "(Intercept)")


mutate_at(vars(criteria), scale)


attribute_d <- optimum_temp %>%
  mutate_at(vars(contains("mean")), funs(scale)) %>% # 전체 기록 바탕으로 element별 difference 계산
  group_split(perspective) %>%
  setNames(unique(sort(as.character(optimum_temp$perspective)))) %>%
  map(~ select(.x, -element, -perspective) %>%
        column_to_rownames(var = "Num_row") %>%
        proxy::dist(method = "Euclidean") %>%
        as.matrix() %>%
        reshape2::melt(varnames = c("row", "col")) %>%
        filter(row > col)) %>%
  map_df(~as.data.frame(.x), .id = "perspective") %>%
  left_join(element_p %>% select(element, Num_row, p, perspective), by = c("row" = "Num_row", "perspective")) %>%
  rename(row_element = element,
         p_row = p) %>%
  left_join(element_p %>% select(element, Num_row, p, perspective), by = c("col" = "Num_row", "perspective")) %>%
  rename(col_element = element,
         p_col = p,
         d = value)  %>%
  separate(perspective, c("졸업년도", "대학"), "_")




heuristic_d <- attribute_d %>%
  group_by(졸업년도, 대학) %>%
  summarise(variety = n_distinct(p_row),  # scaled variety
            balance = sum(p_row * p_col),  # balence-weighted variety
            diversity = sum(d * p_row * p_col))  %>% # balance/disparity-weighted variety
  arrange(-diversity) %>%
  ungroup()
  # mutate(졸업년도 = as.numeric(졸업년도))


p <- heuristic_d %>%
  mutate(졸업년도 = as.factor(졸업년도)) %>%
  filter(대학 != "법과대학") %>%
  group_by(대학) %>%
  mutate(overall_diversity = mean(diversity)) %>%
  ungroup() %>%
  mutate(대학 = fct_reorder(대학, overall_diversity)) %>%
  ggplot(aes(졸업년도, diversity, color = 대학,
             fill = 대학, group = 대학)) +
  geom_line() +
  geom_point(aes(group = seq_along(졸업년도),
                 size = variety),  # 점들이 순차적으로 등장하게 만들기 위해서 필요
             shape = 21, color = "white") +
  scale_fill_manual(name = "대학", values = rev(.colfunc(17, endcolor = "#0078bc"))) +
  scale_color_manual(name = "대학", values = rev(.colfunc(17, endcolor = "#0078bc"))) +
  ggdark::dark_theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45)) +
  facet_wrap(~대학)  +
  guides(alpha = F, size = F) +
  guides(fill = guide_legend(override.aes = list(size=4, linetype = 0)))

ggsave(p, filename = "heuristic_diversity_by_year.png", dpi = 200, width = 12, height = 10)





library(gganimate)

temp_color = colorRampPalette(brewer.pal(11,"Spectral"))(17)

p <- heuristic_d %>%
  mutate(졸업년도 = as.numeric(졸업년도)) %>% 
  filter(!대학 %in% c("법과대학", "정보보호학부")) %>%
  mutate(대학 = fct_reorder(대학, -diversity)) %>% 
  ggplot(aes(balance*2, diversity, color = 대학,
             fill = 대학, group = 대학,
             alpha = (졸업년도 - min(졸업년도) / max(졸업년도)))) +
  geom_line() +
  geom_point(aes(group = seq_along(졸업년도),
                 size = variety),  # 점들이 순차적으로 등장하게 만들기 위해서 필요
             shape = 21, color = "white") +
  scale_fill_manual(name = "대학", values = temp_color) +
  scale_color_manual(name = "대학", values = temp_color) +
  ggdark::dark_theme_minimal() +
  # theme(legend.position = "top",
  #       axis.text.x = element_text(angle = 90)) +
  facet_wrap(~대학)  +
  guides(alpha = F) +
  guides(fill = guide_legend(override.aes = list(size=4, linetype = 0))) +
  # theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Balance (균등성, 0=완전불균형, 1=완전균형)", 
       y = "Diversity (다양성)",
       size = "Variety (다종성)" )
p
ggsave(p, filename = "heuristic_diversity.png", dpi = 300, width = 10, height = 10)

p_animate <- p +
  transition_reveal(along = 졸업년도) +
  # view_follow() +
  labs(title = "기준: {ceiling(frame_along)}년")

animate(p_animate, width = 1920, height = 1080,
        res = 150, end_pause = 50, nframes = 100,
        renderer = gifski_renderer("test.gif"))


