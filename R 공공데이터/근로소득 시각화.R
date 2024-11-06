#### 1. 분석준비 ####
library(pacman)
library(remotes)
remotes::install_github("ricardo-bion/ggradar", dependencies = TRUE, force=T)

pacman::p_load(foreign,                                        # sav 파일 불러오기
               webr, ggridges, ggradar, easyalluvial, eulerr,  # 데이터의 시각화
               sjPlot, ggstatsplot,                            # 통계 관련
               caret, rsample, ranger, gbm,                    # 머신러닝 관련
               janitor, inspectdf, magrittr, tidyverse)        # 데이터 전처리

source("funs.R")


#### 2. 데이터 전처리 ####
근로소득원본 <- read.csv("근로소득.csv", fileEncoding = "CP949")

근로소득 <- 근로소득원본 %>% slice(-c(1:2)) %>%
  filter(신고항목별.1.=="급여총계" & 신고항목별.2.=="소계") %>% 
  rename_at(vars(-c(1,2)), 
            ~case_when(grepl("\\.1", .) ~ gsub("\\.1", "_전체금액", .),
                       grepl("\\.2", .) ~ gsub("\\.2", "_상위10인원", .),
                       grepl("\\.3", .) ~ gsub("\\.3", "_상위10금액", .),
                       TRUE ~ paste0(., "_전체인원"))) %>% 
  pivot_longer(everything()) %>% 
  slice(-c(1:2)) %>% 
  separate(name, into=c("년", "구분"), sep="_") %>% 
  pivot_wider(names_from = 구분, values_from = value) %>% 
  mutate(년=as.integer(str_sub(년, 2, -1))) %>% 
  mutate_at(2:5, ~as.numeric(.)) %>% 
  mutate(인당금액=전체금액/전체인원,
         상위10=상위10금액/상위10인원,
         하위90=(전체금액-상위10금액)/(전체인원-상위10인원))


#### 3. 데이터 시각화 ####
근로소득 %>% ggplot(aes(년, 인당금액)) + geom_point() + geom_line()
근로소득 %>% pivot_longer(c(상위10, 하위90), names_to = "구분") %>% 
  ggplot(aes(년, value, color=구분)) + 
  geom_point() + geom_line()  

