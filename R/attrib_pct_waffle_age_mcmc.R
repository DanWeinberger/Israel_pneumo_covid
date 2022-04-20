attrib_pct_waffle_age_mcmc <- function(ds){
  
  attrib.pct.df <- mod.results[[ds]]$attrib.pct.agec
  
  
  attrib.pct.df.plot <- attrib.pct.df
  
  attrib.pct.df.plot$Attrib_pct[attrib.pct.df.plot$Attrib_pct<0] <- 0
  
  attrib.pct.df.plot$Attrib_pct[attrib.pct.df.plot$Virus=='Virus'] <- 100 - attrib.pct.df.plot$Attrib_pct[attrib.pct.df.plot$Virus=='Virus']
  
  attrib.pct.df.plot$Virus <- as.character(attrib.pct.df.plot$Virus)
  
  attrib.pct.df.plot$Virus[attrib.pct.df.plot$Virus=='Virus'] <- 'None'
  
  inc <- all1 %>%
    group_by(agec, date) %>% #aggregate across ethnicity by date
    summarize(pop=sum(pop), CAAP=sum(CAAP)) %>%
    ungroup() %>%
    group_by(agec) %>%
    summarize(pop1=mean(pop), CAAP1=sum(CAAP), CAAP_inc=round(CAAP1/pop1*10000))
  
  attrib.pct.df.plot <- attrib.pct.df.plot %>%
    group_by(agec) %>%
    mutate(attrib_pct_scaled = round(100*Attrib_pct/sum(Attrib_pct)))
  
  attrib.pct.df.plot <- merge(attrib.pct.df.plot, inc, by='agec')
  
  attrib.pct.df.plot$attrib_inc <- round(attrib.pct.df.plot$CAAP_inc * attrib.pct.df.plot$attrib_pct_scaled/100)
  
  attrib.pct.df.plot$Virus <- factor(attrib.pct.df.plot$Virus, levels=c('None','RSV','Influenza','hMPV','Parainfluenza','Adenovirus'))
  
  
  attrib.pct.df.plot$Age_group <- NA
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==1] <- '<12m'
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==2] <- '12-23m'
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==3] <- '24-59m'
  
  attrib.pct.df.plot <- attrib.pct.df.plot[order(attrib.pct.df.plot$agec, attrib.pct.df.plot$Virus),]
  
  nrows.plot <- 20
  p1 <- ggplot(attrib.pct.df.plot, aes(fill = Virus, values = attrib_inc)) +
    geom_waffle(color = "white", size = .25, n_rows = nrows.plot, flip = TRUE) +
    facet_wrap(~Age_group, nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x * nrows.plot, # make this multiplier the same as n_rows
                       expand = c(0,0)) +
    ggthemes::scale_fill_tableau(name=NULL) +
     scale_fill_manual(name = NULL,
                       values = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                  "#F0E442", "#0072B2" ),
                       labels = c("Other Virus/None", "RSV", "Influenza","hMPV","Parainfluenza","Adenovirus") )+
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
