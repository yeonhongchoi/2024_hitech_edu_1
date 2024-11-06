#### 1. 분석준비 ####
library(pacman)
library(remotes)
remotes::install_github("ricardo-bion/ggradar", dependencies = TRUE, force=T)

pacman::p_load(foreign,                                        # sav 파일 불러오기
               webr, ggridges, ggradar, easyalluvial, eulerr,  # 데이터의 시각화
               sjPlot, ggstatsplot,                            # 통계 관련
               caret, rsample, ranger, gbm,                    # 머신러닝 관련
               janitor, inspectdf, magrittr, broom, tidyverse) # 데이터 전처리

source("funs.R")


#### 2. 데이터 전처리 ####
근로소득원본 <- read.csv("근로소득.csv", fileEncoding = "CP949")
소비자물가지수원본 <- read.csv("소비자물가지수.csv", fileEncoding = "CP949")

소비자물가지수 <- 소비자물가지수원본 %>% 
  select(-시도별) %>%                                 # 시도별을 제외한 나머지
  pivot_longer(everything()) %>%                      # long type으로 변환
  mutate(name=str_sub(name, start=2, end=-1)) %>%     # 2번째부터 맨 끝까지 선택
  separate(name, into=c("년", "월"), sep="\\.") %>%   # '.'을 기준으로 '년'과 '월'을 분리
  mutate_at(vars(년, 월), ~as.integer(.)) %>%         # '년'과 '월'을 정수로 인식
  rename("소비자물가"=3) %>%                          # 3번째 'value'를 '소비자물가'로 정정
  group_by(년) %>%                                    # 년을 기준으로
  summarise(소비자물가=mean(소비자물가))              # 소비자물가지수의 평균 구하기

df <- 근로소득 %>% left_join(소비자물가지수)

df %>% lm_r2(소비자물가~상위10+하위90)



