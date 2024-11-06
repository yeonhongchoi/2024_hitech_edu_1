pacman::p_load(foreign,                                  # sav 파일 불러오기
               webr, ggridges, ggradar, eulerr,          # 데이터의 시각화
               sjPlot, ggstatsplot,                      # 통계 관련
               caret, rsample, ranger, gbm,              # 머신러닝 관련
               janitor, inspectdf, magrittr, tidyverse)  # 데이터 전처리

set_theme(
  legend.pos = "top",
  base = theme_bw()
)

ez_plot <- function(data, by=NULL, geom="bar", legend=NULL, palette=NULL,
                    max=5, ...){
  set_theme(
    legend.pos = ifelse(!is.null(legend), legend, "top"),
    base = theme_bw()
  )
  if(grepl("ba|co", geom)){
    if(is.null(by)){
      p <- data %>% pivot_longer(everything(), names_to = "구분") %>% 
        group_by(구분) %>% summarise(value=mean(value, na.rm=T)) %>% 
        ggplot(aes(value, reorder(구분, value), fill=구분)) +
        geom_col(show.legend = F) +
        geom_text(aes(label=round(value, 1)), hjust=-0.5) + 
        scale_x_continuous(expand =expansion(mult=c(0, 0.15))) +
        labs(x="value", y=NULL)
    }else{
      p <- data %>% 
        pivot_longer(-all_of(by), names_to = "구분") %>% 
        group_by(get(by), 구분) %>% 
        summarise(value=mean(value, na.rm=T)) %>% 
        rename(!!by:=1) %>% ungroup() %>% 
        ggplot(aes(value, reorder(구분, value), fill=get(by))) +
        geom_col(position="dodge2") +
        geom_text(aes(label=round(value, 1)), 
                  position=position_dodge2(width=0.9), hjust=-0.5) + 
        scale_x_continuous(expand =expansion(mult=c(0, 0.15))) +
        guides(fill = guide_legend(title = by))+
        labs(x="value", y=NULL)
    }
  }else if(grepl("ri", geom)){
    if(is.null(by)){
      p <- data %>% 
        pivot_longer(everything(), names_to = "구분") %>% 
        ggplot(aes(x = value, y = 구분, fill = 구분)) +
        geom_density_ridges(bandwidth=0.5, show.legend = "none") +
        labs(x="value", y=NULL)
    }else{
      p <- data %>% 
        pivot_longer(-all_of(by), names_to = "구분") %>% 
        ggplot(aes(x = value, y = 구분, fill = get(by))) +
        geom_density_ridges(bandwidth=0.5, alpha=0.5) +
        guides(fill = guide_legend(title = by))+
        labs(x="value", y=NULL)
    }
  }else if(grepl("ra", geom)){
    if(is.null(by)){
      p <- data %>% 
        summarise_all(~mean(., na.rm=T)) %>% 
        mutate(group="", .before=1) %>% 
        ggradar(grid.max=max)
    }else{
      p <- data %>% 
        select(all_of(by), everything()) %>% rename(group=1) %>% 
        group_by(group) %>% summarise_all(~mean(., na.rm=T)) %>% 
        ggradar(grid.max=max)
    }
  }
  
  if(!is.null(palette)){
    p + scale_fill_brewer(palette=palette)
  }else{
    p
  }
}


lm_r2 <- function(data, formula){
  # https://www.andrewheiss.com/blog/2021/08/21/r2-euler/
  vars <- formula %>% as.character()
  ind_vars <- vars[3] %>% str_split("\\+", simplify = T) %>% str_trim()
  y <- vars[2]
  x1 <- ind_vars[1]
  x2 <- ind_vars[2]
  
  data <- data %>% select(grep(paste0(c(y, x1, x2), collapse="|"), names(.))) %>% 
    scale() %>% data.frame() %>% na.omit()
  
  y_total <- aov(formula(paste(y, "~1")), data) %>% tidy() %>% pull(sumsq)
  x1_total <- aov(formula(paste(x1, "~1")), data) %>% tidy() %>% pull(sumsq)
  y_plus_x1 <- aov(formula(paste(y, "~", x1)), data) %>%            # D+G 
    tidy() %>% filter(term == x1) %>% pull(sumsq)
  
  if(!is.na(x2)){
    x2_total <- aov(formula(paste(x2, "~1")), data) %>% tidy() %>% pull(sumsq)
    
    y_alone <- aov(formula(paste(y, "~", x1, "+", x2)), data) %>%   # A
      tidy() %>% filter(term == "Residuals") %>% pull(sumsq)
    x1_alone <- aov(formula(paste(x1, "~", y, "+", x2)), data) %>%  # B
      tidy() %>% filter(term == "Residuals") %>% pull(sumsq)
    x2_alone <- aov(formula(paste(x2, "~", y, "+", x1)), data) %>%  # C
      tidy() %>% filter(term == "Residuals") %>% pull(sumsq)
    
    y_plus_x2 <- aov(formula(paste(y, "~", x2)), data) %>%          # E+G 
      tidy() %>% filter(term == x2) %>% pull(sumsq)
    x1_plus_x2 <- aov(formula(paste(x1, "~", x2)), data) %>%        # F+G 
      tidy() %>% filter(term == x2) %>% pull(sumsq)
    
    y_x1_alone <- y_total - y_alone - y_plus_x2    # D=(A+D+E+G)−A−(E+G)
    y_x2_alone <- y_total - y_alone - y_plus_x1    # E=(A+D+E+G)−A−(D+G)
    y_x1_x2_alone <- y_plus_x1 - y_x1_alone        # G=(D+G)−D
    x1_x2_alone <- x1_plus_x2 - y_x1_x2_alone      # F=(F+G)-G
    
    tryCatch({
      all_pieces <- c("y" = y_alone, "x1" = x1_alone, "x2" = x2_alone,
                      "x1&y" = y_x1_alone, "x2&y" = y_x2_alone, 
                      "x1&x2" = ifelse(x1_x2_alone<0, 0, x1_x2_alone),      # F 에러 대처
                      "y&x1&x2" = ifelse(y_x1_x2_alone<0, 0, y_x1_x2_alone) # G 에러 대처
      ) %>% euler(shape = "ellipse")
      r.squared <- lm(formula(paste(y, "~", x1, "+", x2)), data) %>% 
        glance() %>% pull(r.squared)
      
      rownames(all_pieces$ellipses) <- c(y, x1, x2)
      
      all_pieces %>% 
        plot(quantities = c(paste0("A(", round(y_alone/y_total, 3), ")"), 
                            paste0("B"), 
                            paste0("C"),
                            paste0("D(", round(y_x1_alone/y_total, 3), ")"),
                            paste0("E(", round(y_x2_alone/y_total, 3), ")"),
                            paste0("F"),
                            paste0("G(", round(y_x1_x2_alone/y_total, 3), ")")),
             fills = list(
               fill = c("#7FDBFF", "grey80", "grey80",
                        "#FF851B", "#FF851B", "grey80", "#FF851B"),
               alpha = c(1, 0.5, 0.5, 0.5, 0.5, 0.3, 0.3)),
             main = list(
               label = bquote(R^2 ~ "= D+E+G =" ~ 
                                .(round(r.squared, 3))), cex = 1))
    },
    error=function(cond) {
      r2_1 <- lm(formula(paste(y, "~", x1)), data) %>% 
        glance() %>% pull(r.squared)
      r2_2 <- lm(formula(paste(y, "~", x2)), data) %>% 
        glance() %>% pull(r.squared)
      
      if(r2_1>r2_2){
        error_msg <- paste0("계산 과정에서의 문제때문에 상대적으로 설명력이 작은 변수(", x2,")를 제외하였습니다.")
        r2 <- r2_1
      }else{
        error_msg <- paste0("계산 과정에서의 문제때문에 상대적으로 설명력이 작은 변수(", x1,")를 제외하였습니다.")
        x1 <- x2
        r2 <- r2_2
        x1_total <- aov(formula(paste(x1, "~1")), data) %>% tidy() %>% pull(sumsq)
        y_plus_x1 <- aov(formula(paste(y, "~", x1)), data) %>%            # D+G 
          tidy() %>% filter(term == x1) %>% pull(sumsq)
      }
      
      two_pieces <- c("y" = y_total-y_plus_x1 , 
                      "x1" = x1_total-y_plus_x1, 
                      "x1&y" = y_plus_x1) %>% euler()
      rownames(two_pieces$ellipses) <- c(y, x1)
      
      two_pieces %>% 
        plot(quantities = 
               c(paste0("A(", 1-as.numeric(round(r2, 3)), ")"), 
                 "B", paste0("C(", as.numeric(round(r2, 3)), ")")),
             fills = list(
               fill = c("#7FDBFF", "grey80", "#FF851B"),
               alpha = c(1, 0.5, 0.5)),
             main = list(
               label = bquote(atop(R^2 ~ "= C =" ~ 
                                     .(round(r2, 3)),
                                   .(error_msg))), cex = 1))
    })
  }else{
    two_pieces <- c("y" = y_total-y_plus_x1 , 
                    "x1" = x1_total-y_plus_x1, 
                    "x1&y" = y_plus_x1) %>% euler()
    
    r2 <- lm(formula(paste(y, "~", x1)), data) %>% 
      glance() %>% pull(r.squared)
    
    rownames(two_pieces$ellipses) <- c(y, x1)
    
    two_pieces %>% 
      plot(quantities = 
             c(paste0("A(", 1-as.numeric(round(r2, 3)), ")"), 
               "B", paste0("C(", as.numeric(round(r2, 3)), ")")),
           fills = list(
             fill = c("#7FDBFF", "grey80", "#FF851B"),
             alpha = c(1, 0.5, 0.5)),
           main = list(
             label = bquote(R^2 ~ "= C =" ~ 
                              .(round(r2, 3))), cex = 1))
  }
}
