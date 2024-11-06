#### 1. 분석준비 ####
pacman::p_load(foreign,                                        # sav 파일 불러오기
               webr, ggridges, ggradar, easyalluvial, eulerr,  # 데이터의 시각화
               sjPlot, ggstatsplot,                            # 통계 관련
               caret, rsample, ranger, gbm,                    # 머신러닝 관련
               janitor, inspectdf, magrittr, broom, tidyverse) # 데이터 전처리

source("funs.R")


#### 2. T 검정 ####
data %>% ggbetweenstats(성별, 수학성적)


#### 3. ANOVA ####
data %>% ggbetweenstats(대학선택기준, 수학성적)


#### 4. 상관관계 ####
data %>% ggscatterstats(수학이해도, 수학성적)
data %>% ggscatterstats(수학이해도, 수학성적, point.args=list(size=2, alpha=0.01))

data %>% inspect_cor(with_col = "수학성적") %>% show_plot()


#### 4. 회귀분석 ####
#### 가. 변인통제 의미 ####
lm(수학성적~수학학습태도, data) %>% summary()          
data %>% lm_r2(수학성적~수학학습태도)

lm(수학성적~잠잔일수, data) %>% summary()              
data %>% lm_r2(수학성적~잠잔일수)

lm(수학성적~수학학습태도+잠잔일수, data) %>% summary() 
data %>% lm_r2(수학성적~수학학습태도+잠잔일수)


#### 나. 사교육의 효과성 검토 ####
data %>% lm_r2(수학성적~수학이해도)
data %>% lm_r2(수학성적~수학학습태도)
data %>% lm_r2(수학성적~수학사교육)
data %>% lm_r2(수학성적~자기주도시간)
data %>% lm_r2(수학성적~수학이해도+수학학습태도)
data %>% lm_r2(수학성적~수학이해도+수학사교육)
data %>% lm_r2(수학성적~수학이해도+자기주도시간)


#### 다. 오컴의 면도날 ####
lm(수학성적~수학이해도+수학학습태도+수학사교육+자기주도시간, data) %>% summary()  
lm(수학성적~수학이해도+수학학습태도+수학사교육+자기주도시간+
     수학흥미+초인지, data) %>% summary()
lm(수학성적~수학이해도+수학학습태도+수학사교육+자기주도시간+
     수학흥미+초인지+수학집중시간+잠잔일수+주중게임시간, data) %>% summary()


#### 라. 단계적 회귀분석 ####
lm(수학성적~수학이해도+수학학습태도+수학사교육+자기주도시간+
     수학흥미+초인지+수학집중시간+잠잔일수+주중게임시간, data) %>% 
  step()
lm(수학성적~수학이해도+수학학습태도+수학사교육+자기주도시간+
     수학흥미+초인지+수학집중시간+잠잔일수+주중게임시간, data) %>% 
  stats::step() %>% summary()


#### 마. 범주형 변수 포함 ####
data %>% lm_r2(수학성적~수학이해도+성별)
lm(수학성적~수학이해도+성별, data) %>% summary()
lm(수학성적~수학이해도+임원여부, data) %>% summary()




fit_lm <- train(수학성적~수학이해도+수학학습태도+수학집중시간+수학흥미+초인지+수학사교육, data, na.action=na.pass, method="lm")
fit_lm <- lm(수학성적~수학이해도+수학학습태도+수학집중시간+수학흥미+초인지+수학사교육, data)

data %>% inspect_cor(with_col = "잠잔일수") %>% show_plot()

summary(fit_lm)
tidy(fit_lm, conf.int = T) %>% 
  mutate(p.value=round(p.value, 4)) %>% 
  rename(b=2, se=3, t=4, p=5) %>% 
  mutate_at(vars(2:4), ~round(., 4)) %>% 
  filter(term!="(Intercept)") %>% 
  mutate(group=factor(ifelse(p<0.05, "sig", "insig"), levels=c("sig", "insig"))) %>% 
  mutate(star=ifelse(p<0.001,"***", 
                     ifelse(p<0.01, "**",
                            ifelse(p<0.05, "*", "")))) %>%
  ggplot(aes(b, term, xmin=conf.low, xmax=conf.high, color=group)) + 
  geom_vline(xintercept = 0, linetype=2) +
  geom_point() + geom_errorbar(width=0.1) +
  geom_text(aes(label=paste0(sprintf("%.4f", b), star)), vjust=-2, size=4) +
  theme(panel.background = element_blank(),
        legend.position = "none") +
  labs(y=NULL, title=NULL)

plot_model(fit_lm, show.values = T, axis.lim=c(-0.5, 0.5))

data %>% lm_r2(수학성적~수학이해도)

df <- data %>% select_if(is.numeric) %>% scale() %>% data.frame()
fit_lm <- lm(수학성적~수학이해도+수학학습태도+수학집중시간+수학흥미+초인지+수학사교육, df)
df %>% lm_r2(수학성적~수학이해도+수학학습태도+수학집중시간+수학흥미+초인지+수학사교육)


lm(수학성적~1, df)$df.residual                            # y 전체가 4860
lm(잠잔일수~1, df)$df.residual                            # 잠잔일수 5178
lm(수학학습태도~1, df)$df.residual                        # 수학학습태도 5090
lm(수학성적~수학학습태도, df)$df.residual                 # 4793
df.residual(lm(수학성적~수학학습태도, df))                # 4793
df %>% select(수학성적, 수학학습태도) %>% na.omit() %>% 
  lm(수학성적~수학학습태도, .) %>% df.residual()          # 4793

df %>% select(수학학습태도, 수학성적) %>% na.omit() %>% 
  mutate(수학성적평균=mean(수학성적)) %>% 
  mutate(resid=(수학성적-수학성적평균)^2) %>% 
  pull(resid) %>% sum()                                   # 4764.756
sum((lm(수학성적~수학학습태도, df)$residuals)^2)          # 3229.515
deviance(lm(수학성적~수학학습태도, df))                   # 3229.515
(4764.756-3229.515)/4764.756                              # 0.3222077
df %>% select(수학성적, 수학학습태도) %>% na.omit() %>% 
  lm(수학성적~수학학습태도, .) %>% summary() %>% 
  extract2("r.squared")                                   # 0.3222077
df %>% lm_r2(수학성적~수학학습태도)


df %>% select(grep(paste0(c(y, x1), collapse="|"), names(.))) %>% na.omit() %>% 
  aov(formula(paste(y, "~1")), .) %>% tidy() %>% pull(sumsq)
y_total <- df %>% select(grep(paste0(c(y, x1), collapse="|"), names(.))) %>% na.omit() %>% 
  mutate(!!paste0(y, "mean"):=mean(get(y), na.rm=T)) %>% 
  mutate(resid=(get(y)-get(paste0(y, "mean")))^2) %>% 
  pull(resid) %>% sum(na.rm=T)

y_plus_x1 <- df %>% select(grep(paste0(c(y, x1), collapse="|"), names(.))) %>% na.omit() %>% 
  aov(formula(paste(y, "~", x1)), .) %>%            # D+G 
  tidy() %>% filter(term == x1) %>% pull(sumsq)
y_plus_x1/y_total

df %>% select(grep(paste0(c(y, x1), collapse="|"), names(.))) %>% na.omit() %>% 
  lm(formula(paste(y, "~", x1)), .) %>% deviance()

(y_total-3229.515)/y_total
  
y_total-y_plus_x1


(4793-3229.515)/4793                                      # 0.3262018
summary(lm(수학성적~수학학습태도, df))$r.squared          # 0.3222077

df.residual(lm(수학성적~1, df))
sum(resid(lm(수학성적~1, df))^2)

df.residual(lm(수학성적~수학학습태도+잠잔일수, df))       # 4780
df %>% select(수학성적, 수학학습태도, 잠잔일수) %>% 
  na.omit() %>% 
  mutate(수학성적평균=mean(수학성적)) %>% 
  mutate(resid=(수학성적-수학성적평균)^2) %>% 
  pull(resid) %>% sum()                                   # 4759.018
deviance(lm(수학성적~수학학습태도+잠잔일수, df))          # 3223.902
sum(resid(lm(수학성적~수학학습태도+잠잔일수, df))^2)      # 3223.902
summary(lm(수학성적~수학학습태도+잠잔일수, df))$r.squared # 0.3225699
(4759.018-3223.902)/4759.018                              # 0.3225699
(4860-3223.902)/4860                                      # 0.3366457

c("y" = 3223.902, "x1" = x1_alone, "x2" = x2_alone,
  "x1&y" = y_x1_alone, "x2&y" = y_x2_alone, 
  "x1&x2" = x1_x2_alone, 
  "y&x1&x2" = y_x1_x2_alone) %>% euler()

fit_lm <- lm(수학성적~수학학습태도+잠잔일수, df)
summary(fit_lm)
sum((fit_lm$residuals)^2)             # y를 설명되고 남는 것
sum((lm(수학성적~1, df)$residuals)^2)
(sum((lm(수학성적~1, df)$residuals)^2) - sum((fit_lm$residuals)^2))/sum((lm(수학성적~1, df)$residuals)^2)


aov(수학성적~1, df) %>% tidy() %>% filter(term == "Residuals") %>% pull(sumsq)
aov(수학성적~수학학습태도+잠잔일수, df) %>% tidy() %>% filter(term == "Residuals") %>% pull(sumsq)

sum((lm(수학성적~1, df)$residuals)^2)
sum((lm(수학성적~수학학습태도+잠잔일수, df)$residuals)^2)
