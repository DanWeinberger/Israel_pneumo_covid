attrib_pct_waffle_age <- function(ds){
  attrib.pct.any <- ds$attrib_pct$attrib_pct_any_age
  
  attrib.pct.rsv <- ds$attrib_pct$attrib_pct_RSV_age
  
  attrib.pct.hmpv <- ds$attrib_pct$attrib_pct_HMPV_age
  
  attrib.pct.flu <- ds$attrib_pct$attrib_pct_Flu_age
  
  attrib.pct.adeno <- ds$attrib_pct$attrib_pct_Adeno_age
  
  attrib.pct.Paraflu <- ds$attrib_pct$attrib_pct_Paraflu_age
  
  
  attrib.pct.df <- bind_rows(attrib.pct.any,attrib.pct.rsv,attrib.pct.hmpv,attrib.pct.flu,attrib.pct.adeno,attrib.pct.Paraflu)
  
  attrib.pct.df$attrib_pct_med <- round(attrib.pct.df$attrib_pct_med)
  
  
  combos <- unique(attrib.pct.df[,c('agec')])
  
  attrib.pct.df$virus <- rep(c('any','rsv','hmpv','flu', 'adeno', 'paraflu'), each=nrow(combos))
  
  
  attrib.pct.df.plot <- attrib.pct.df
  
  attrib.pct.df.plot$attrib_pct_med[attrib.pct.df.plot$attrib_pct_med<0] <- 0
  
  attrib.pct.df.plot$attrib_pct_med[attrib.pct.df.plot$virus=='any'] <- 100 - attrib.pct.df.plot$attrib_pct_med[attrib.pct.df.plot$virus=='any']
  
  attrib.pct.df.plot$virus[attrib.pct.df.plot$virus=='any'] <- 'None'
  
  inc <- all1 %>%
    group_by(agec, date) %>% #aggregate across ethnicity by date
    summarize(pop=sum(pop), CAAP=sum(CAAP)) %>%
    ungroup() %>%
    group_by(agec) %>%
    summarize(pop1=mean(pop), CAAP1=sum(CAAP), CAAP_inc=round(CAAP1/pop1*10000))
  
  attrib.pct.df.plot <- attrib.pct.df.plot %>%
    group_by(agec) %>%
    mutate(attrib_pct_scaled = round(100*attrib_pct_med/sum(attrib_pct_med)))
  
  attrib.pct.df.plot <- merge(attrib.pct.df.plot, inc, by='agec')
  
  attrib.pct.df.plot$attrib_inc <- round(attrib.pct.df.plot$CAAP_inc * attrib.pct.df.plot$attrib_pct_scaled/100)
  
  attrib.pct.df.plot$virus <- factor(attrib.pct.df.plot$virus, levels=c('None','rsv','flu','hmpv','adeno','paraflu'))
  
  attrib.pct.df.plot$Age_group <- NA
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==1] <- '<12m'
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==2] <- '12-23m'
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==3] <- '24-59m'
  
  
  nrows.plot <- 20
  p1 <- ggplot(attrib.pct.df.plot, aes(fill = virus, values = attrib_inc)) +
    geom_waffle(color = "white", size = .25, n_rows = nrows.plot, flip = TRUE) +
    facet_wrap(~Age_group, nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x * nrows.plot, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    ggthemes::scale_fill_tableau(name=NULL) +
     scale_fill_manual(name = NULL,
                       values = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                  "#F0E442", "#0072B2" ),
                       labels = c("Other", "RSV", "Influenza","hMPV","Adenovirus","Parainfluenza") )+
    coord_equal() +
    labs(
      x = "Age group",
      y = "Incidence"
    ) +
    theme_minimal(base_family = "Roboto Condensed") +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
    guides(fill = guide_legend(reverse = TRUE))
  
  
  return(p1)
}
