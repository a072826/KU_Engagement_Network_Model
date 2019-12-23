library(tidyverse)
library(janitor)
library(widyr)
library(readxl)
library(Matrix)
library(widyr)
# library(rstudioapi)
# current_path = rstudioapi::getActiveDocumentContext()$path
# setwd(dirname(current_path ))
getwd()
source("../../../R_functions/func.r", encoding = 'utf-8')
source("engagement_network_data.r", encoding = 'utf-8')

#####################################################################
## 시각화 ##########################################################
#####################################################################

p <-
  index_list %>%
  filter(Num_year_term > 13 ) %>% 
  filter(Domain != "학생상담센터") %>% 
  # filter(attribute %in% c("학과", "대학", "성별", "입학유형")) %>% 
  ggplot(aes(Gini_Simpson_Index, Shannon_Entropy, color = attribute,
             fill = attribute, group = attribute)) +
  geom_line() +
  geom_point(shape = 21, color = "white",
             aes(size = n_within_attribute, alpha = (Num_year_term - min(Num_year_term) / max(Num_year_term)))) +
  facet_wrap(~Domain) +
  ggdark::dark_theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Spectral")  +
  scale_color_brewer(palette = "Spectral")  +
  guides(alpha = F, size = F)
p

ggsave(p, filename = "gs_s.png", dpi = 300, width = 10, height = 10)


library(gganimate)

# animate <- 
index_list %>%
  filter(Domain %in% c("융합전공신청")) %>% 
  filter(!attribute %in% c("출신교")) %>% 
  filter(Num_year_term >= 13 & 
           Num_year_term < 40) %>% 
  filter(!(Domain == "수료" & Num_year_term < 23)) %>% 
  mutate(Domain = gsub("학기우등생|학기최우등생", "학기우등 및 최우등", Domain))  %>% 
  ggplot(aes(Gini_Simpson_Index, KLD, color = attribute,
             fill = attribute, group = attribute,
             alpha = (Num_year_term - min(Num_year_term) / max(Num_year_term)))) +
  geom_line() +
  geom_point(aes(group = seq_along(Num_year_term),
                 size = n_within_attribute),  # 점들이 순차적으로 등장하게 만들기 위해서 필요
             shape = 21, color = "white") +
  scale_fill_brewer(name = "속성", palette = "Spectral") +
  scale_color_brewer(name = "속성", palette = "Spectral") +
  ggdark::dark_theme_minimal() +
  # theme(legend.position = "top",
  #       axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Domain)  +
  guides(alpha = F, size = F) +
  guides(fill = guide_legend(override.aes = list(size=4, linetype = 0)))

ggsave(animate, filename = "static.png", dpi = 300, width = 10, height = 10)

animate <- animate +
  transition_reveal(along = Num_year_term) + 
  # view_follow() +
  labs(title = "기준: {ceiling(frame_along/2-1)+2000}년 {2 - floor(frame_along)%%2}학기")

animate(animate, width = 1920, height = 1080,  
        res = 150, end_pause = 50, nframes = 200,
        renderer = gifski_renderer("test.gif"))


# 엔트로피합 

temp <- index_list %>% 
  group_by(Domain, Num_year_term, year_term) %>% 
  summarise(Shannon_Entropy_sum = sum(Shannon_Entropy)) %>% 
  ungroup() %>% 
  filter(Num_year_term >= 23) %>% 
  .accumulate_by(~ Num_year_term) %>% 
  as_tibble()

prob_semester_by_domain

#################################################################
# plotly로 animation 만드는 법 
#################################################################
# p <-
#   temp %>%
#   ggplot(aes(year_term, Shannon_Entropy_sum, group = Domain, frame = frame,
#              color = Domain, fill = Domain)) +
#   geom_line() +
#   geom_point(alpha = 0.7, shape = 21, color="black", size = 3) +
#   labs(x = "", y = "") + 
#   geom_smooth(method = "loess", lty = 2) +
#   theme_minimal() +
#   theme(legend.position = "top",
#         axis.text.x = element_text(angle = 45)) +
#   # view_follow() +
#   scale_fill_brewer(name = "", palette = "Set3") +
#   scale_color_brewer(name = "", palette = "Set3")
# 
# 
# ggplotly(p) %>%
#   layout(
#     yaxis = list(
#       title = "Shannon_Entropy 합",
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
#   )