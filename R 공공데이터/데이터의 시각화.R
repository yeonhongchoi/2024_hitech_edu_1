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
rdata09 <- read.spss("y9STU.sav", to.data.frame=T) %>% filter(Y9STU=="설문지 참여함")

attributes(rdata09) %>% extract2(4) %>% .[grep("수학", .)]

data <- rdata09 %>% 
  select(STUID, 성별, Y9H_ST2_2, Y9H_ST2_3, Y9H_ST12_2_1:Y9H_ST12_2_2, Y9H_ST8_1, 
         Y9H_ST9_1, Y9H_ST9_3, Y9H_ST11_1:Y9H_ST11_6, grep("ST17_", names(.)), 
         Y9H_ST19_1:Y9H_ST19_5, grep("ST20_", names(.)), grep("ST21_", names(.)), 
         Y9H_ST23_13:Y9H_ST23_16, Y9H_ST24_4, Y9H_ST24_8, Y9H_ST24_12, Y9H_ST24_16, 
         grep("ST26_", names(.)), grep("ST32M_", names(.)), grep("Y9H_ST33M", names(.)),
         grep("ST35M_", names(.)), grep("ST36M_", names(.)), Y9H_ST38_1:Y9H_ST38_6, 
         grep("ST46_", names(.)), Y9H_ST39_3, Y9H_ST39_3T, Y9H_ST53, Y9H_ST59_2, 
         Y9H_ST60_2_2, Y9H_ST42, Y9H_ST63_7_1, Y9H_ST30_3, Y9H_ST31_3)

inspect_na(data)

summary(as.factor(rdata09$Y9H_ST2_2))
summary(as.factor(rdata09$Y9H_ST63_7_1))

data <- data %>% 
  mutate_at(vars(Y9H_ST2_2:Y9H_ST12_2_2),
            ~case_when(.=="전혀 안함"~0, .=="30분 미만"~0.25, .=="30분-1시간 미만"~0.75, 
                       .=="1시간 30분-2시간 미만"~1.75, .=="2시간-3시간 미만"~2.5, 
                       .=="3시간-4시간 미만"~3.5, .=="4시간-5시간 미만"~4.5, 
                       .=="5시간 이상"~5.5)) %>% 
  mutate_at(vars(Y9H_ST9_1:Y9H_ST9_3),
            ~case_when(.=="전혀 없다"~0, .=="1년 1-2번"~1, .=="한 학기 1-2번"~3, 
                       .=="한 달 1-2번"~24, .=="한 주 1-2번"~96, .=="거의 매일"~224)) %>% 
  mutate_at(vars(Y9H_ST11_1:Y9H_ST46_12),
            ~case_when(.=="전혀 그렇지 않다"~1, .=="그렇지 않다"~2, 
                       .=="보통이다"~3, .=="그렇다"~4, .=="매우 그렇다"~5)) %>% 
  mutate(Y9H_ST63_7_1=case_when(Y9H_ST63_7_1=="나의 적성과 흥미" ~ "적성 흥미",
                                grepl("성적", Y9H_ST63_7_1) ~ "성적",
                                Y9H_ST63_7_1=="취업 전망" ~ "취업 전망",
                                grepl("명성", Y9H_ST63_7_1) ~ "대학 명성",
                                is.na(Y9H_ST63_7_1) ~ "NA",
                                TRUE ~ "기타")) %>% 
  mutate(Y9H_ST42=case_when(Y9H_ST42=="전혀 만족하지 않음"~1, Y9H_ST42=="만족하지 않음"~2, 
                            Y9H_ST42=="보통임"~3, Y9H_ST42=="만족함"~4, 
                            Y9H_ST42=="매우 만족함"~5)) %>% 
  mutate_at(vars(Y9H_ST30_3),  
            ~case_when(.=="20% 이하"~10, .=="21-40%"~30, .=="41-60%"~50, 
                       .=="61-80%"~70, .=="81% 이상"~90)) %>% 
  mutate_at(vars(Y9H_ST31_3), 
            ~case_when(.=="0-10분"~5, .=="11-20분"~15, .=="21-30분"~25, 
                       .=="31-40분"~35, .=="41분 이상"~45)) %>% 
  rename(학원숙제시간=3, 자기주도시간=4, 주중게임시간=5, 주말게임시간=6, 
         임원여부=7, 떠든일수=8, 잠잔일수=9) %>% 
  mutate(독서=rowMeans(select(., grep("ST11_", names(.))), na.rm=T), .before=10) %>% 
  mutate(우울=rowMeans(select(., grep("ST17_", names(.))), na.rm=T), .before=11) %>% 
  mutate(자아개념=rowMeans(select(., grep("ST19_", names(.))), na.rm=T), .before=12) %>% 
  mutate(자아존중감=rowMeans(select(., grep("ST20_", names(.))), na.rm=T), .before=13) %>% 
  mutate(자기효능감=rowMeans(select(., grep("ST21_", names(.))), na.rm=T), .before=14) %>% 
  mutate(초인지=rowMeans(select(., grep("ST24_", names(.))), na.rm=T), .before=15) %>% 
  mutate(학업스트레스=rowMeans(select(., grep("ST26_", names(.))), na.rm=T), .before=16) %>% 
  mutate(수학학습태도=rowMeans(select(., grep("ST32M_", names(.))), na.rm=T), .before=17) %>% 
  mutate(수학효능감=rowMeans(select(., grep("ST35M_", names(.))), na.rm=T), .before=18) %>% 
  mutate(수학흥미=rowMeans(select(., grep("ST36M_", names(.))), na.rm=T), .before=19) %>% 
  mutate(부모신뢰=rowMeans(select(., grep("ST38_", names(.))), na.rm=T), .before=20) %>% 
  mutate(학교적응=rowMeans(select(., grep("ST46_", names(.))), na.rm=T), .before=21) %>% 
  mutate(수학교사역량=rowMeans(select(., grep("ST33M", names(.))), na.rm=T), .before=22) %>% 
  mutate(수학사교육=ifelse(Y9H_ST39_3=="불참", 0, Y9H_ST39_3T), .before=23) %>% 
  mutate(수학이해도=Y9H_ST30_3, .before=24) %>% 
  mutate(수학집중시간=Y9H_ST31_3, .before=25) %>% 
  mutate(수학내신=Y9H_ST59_2, .before=26) %>% 
  mutate(모평수학등급=Y9H_ST60_2_2, .before=27) %>% 
  mutate(학교만족도=Y9H_ST42, .before=28) %>% 
  mutate(혁신학교여부=Y9H_ST53, .before=29) %>% 
  mutate(대학선택기준=Y9H_ST63_7_1, .before=30) %>% 
  mutate(수학성적=rowMeans(select(., 수학내신, 모평수학등급), na.rm=T), .before=31) %>% 
  select(1:31)


#### 3. 데이터 시각화(EDA) ####
inspect_types(data) %>% show_plot()
inspect_na(data) %>% show_plot()
inspect_cat(data) %>% show_plot()
inspect_num(data) %>% show_plot()


#### 가. 범주형 단일 ####
PieDonut(data, aes(성별))
PieDonut(data, aes(대학선택기준))
PieDonut(data, aes(대학선택기준), r0=0.6, explode=6)

plot_frq(data, 대학선택기준)
plot_frq(data, 대학선택기준, sort.frq="desc", geom.colors="tomato")


#### 나. 범주형 2개 ####
PieDonut(data, aes(혁신학교여부, 대학선택기준))
PieDonut(data, aes(대학선택기준, 성별), explode=1, explodeDonut=T)

with(data, plot_grpfrq(대학선택기준, 혁신학교여부))
with(data, plot_grpfrq(대학선택기준, 혁신학교여부, geom.colors = "gs"))

with(data, plot_xtab(대학선택기준, 혁신학교여부, margin="row", bar.pos="stack"))
with(data, plot_xtab(대학선택기준, 혁신학교여부, margin="row", bar.pos="stack", 
                     coord.flip = T, geom.colors = c("tomato", "light blue")))


#### 다. 연속형 단일 ####
plot_frq(data, 수학성적, type="histogram", show.mean=T) # 평균과 표준편차
plot_frq(data, 수학성적, type="histogram", geom.colors = "pink", 
         show.mean=TRUE, normal.curve=T)


#### 라. 리커트 척도 ####
data %>% select(독서:학업스트레스) %>% mutate_all(~round(.)) %>% 
  plot_likert()
data %>% select(독서:학업스트레스) %>% mutate_all(~round(.)) %>% 
  plot_likert(sort.frq="pos.asc")


#### 마. 막대그래프 ####
data %>% select(학원숙제시간:주말게임시간) %>% ez_plot()
data %>% select(임원여부, 학원숙제시간:주말게임시간) %>% na.omit() %>% 
  ez_plot(geom="bar", by="임원여부", legend="top", palette="Greens")


#### 바. 다층 밀도 그래프 ####
data %>% select(독서:학업스트레스) %>% ez_plot(geom="ridges", palette="Spectral")
data %>% select(임원여부, 독서:학업스트레스) %>% na.omit() %>% 
  ez_plot(geom="ridges", by="임원여부", palette="Spectral")
# Dark2, Spectral, Set1,2,3, Pastel1,2, Paired


#### 사. 레이더 챠트 ####
data %>% select(독서:학업스트레스) %>% ez_plot(geom="radar", max=5)
data %>% select(성별, 독서:학업스트레스) %>% ez_plot(geom="radar", by="성별", max=5)


#### 아. 생키 다이어그램(Sankey Diagram) ####
data %>% select(성별, 대학선택기준) %>% alluvial_wide()
data %>% select(성별, 대학선택기준, 수학성적) %>% alluvial_wide()


#### 자. 연속형에 따른 연속형 ####
data %>% mutate(수학성적=round(수학성적)) %>% na.omit() %>% 
  group_by(수학성적) %>% summarise(학업스트레스=mean(학업스트레스, na.rm=T)) %>% 
  ggplot(aes(수학성적, 학업스트레스)) + 
  geom_point() + geom_line()

data %>% mutate(수학성적=round(수학성적)) %>% na.omit() %>% 
  ggplot(aes(수학성적, 학업스트레스)) +
  stat_summary(fun.data=mean_cl_normal, geom="pointrange") +
  stat_summary(geom="line")

data %>% mutate(수학성적=round(수학성적)) %>% na.omit() %>% 
  group_by(수학성적) %>% 
  summarise(학업스트레스=mean(학업스트레스, na.rm=T),
            자기효능감=mean(자기효능감, na.rm=T)) %>% 
  pivot_longer(c(자기효능감, 학업스트레스), names_to="구분") %>% 
  ggplot(aes(수학성적, value, color=구분)) +
  geom_point() + geom_line()

data %>% mutate(수학성적=round(수학성적)) %>% na.omit() %>% 
  pivot_longer(c(자기효능감, 학업스트레스), names_to="구분") %>% 
  ggplot(aes(수학성적, value, color=구분)) +
  stat_summary(fun.data=mean_cl_normal, geom="pointrange") +
  stat_summary(geom="line")
