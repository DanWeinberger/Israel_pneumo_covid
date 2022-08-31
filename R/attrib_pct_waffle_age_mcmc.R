attrib_pct_waffle_age_mcmc <- function(ds, outcomevar,inc.denom=100000, plot.title, off.legend=F,n.waffles=1000,fill.waffle=F){
  
  attrib.pct.df <- mod.results[[ds]]$attrib.pct.agec
  
  
  attrib.pct.df.plot <- attrib.pct.df
  
  attrib.pct.df.plot$Attrib_pct[attrib.pct.df.plot$Attrib_pct<0] <- 0
  
  attrib.pct.df.plot$Attrib_pct[attrib.pct.df.plot$Virus=='Virus'] <- 100 - attrib.pct.df.plot$Attrib_pct[attrib.pct.df.plot$Virus=='Virus']
  
  attrib.pct.df.plot$Virus <- as.character(attrib.pct.df.plot$Virus)
  
  attrib.pct.df.plot$Virus[attrib.pct.df.plot$Virus=='Virus'] <- 'None'
  
  all1$outcome.var <- all1[,outcomevar]
  
  inc <- all1 %>%
    group_by(agec, date) %>% #aggregate across ethnicity by date
    summarize(pop=sum(pop), outcome.var=sum(outcome.var)) %>%
    ungroup() %>%
    group_by(agec) %>%
    summarize(pop1=mean(pop), outcome.var1=sum(outcome.var), n_months=n(), outcome_inc=round(outcome.var1/pop1/n_months*12*inc.denom)) %>%
    ungroup() %>%
    mutate( scaled.inc=outcome_inc/max(outcome_inc)*n.waffles)
  
  max_inc <- inc %>% summarize(max_inc=max(outcome_inc))
  
  attrib.pct.df.plot <- attrib.pct.df.plot %>%
    group_by(agec) %>%
    mutate(attrib_pct_scaled = 100*Attrib_pct/sum(Attrib_pct))
  
  attrib.pct.df.plot <- merge(attrib.pct.df.plot, inc, by='agec')
  
  attrib.pct.df.plot$attrib_inc <- round(attrib.pct.df.plot$scaled.inc * attrib.pct.df.plot$attrib_pct_scaled/100)
  
  attrib.pct.df.plot$Virus <- factor(attrib.pct.df.plot$Virus, levels=c('None','RSV','Influenza','hMPV','Parainfluenza','Adenovirus'))
  
  #10+62+6+16+7
  
  attrib.pct.df.plot$Age_group <- NA
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==1] <- '<1 year'
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==2] <- '1 year'
  attrib.pct.df.plot$Age_group[attrib.pct.df.plot$agec==3] <- '2-4 years'
  
  attrib.pct.df.plot <- attrib.pct.df.plot[order(attrib.pct.df.plot$agec, attrib.pct.df.plot$Virus),]
  
  nrows.plot <- 20 #50 # round(n.waffles/30) # 20 #NOTE THIS IS ACTUALLY n cols
  
  p1 <- ggplot(attrib.pct.df.plot, aes(fill = Virus, values = attrib_inc)) +
    geom_waffle(color='white', size = 0.25, n_rows = nrows.plot, flip = TRUE)+
    facet_wrap(~Age_group, nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
     scale_fill_manual(name = NULL,
                       values = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                  "#F0E442", "#0072B2" ),
                       labels = c("Other Virus/None", "RSV", "Influenza","hMPV","Parainfluenza","Adenovirus") )+
    coord_equal() +
    labs(
      x = "Age group",
      y = paste0("Annual Cases/100,000")
    ) +
     theme_minimal(base_family = "Roboto Condensed") +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
    guides(fill = guide_legend(reverse = TRUE))+
    scale_y_continuous(breaks=c(10,20,30,40,50),expand = c(0,0), 
                       labels= as.character(round(c(10,20,30,40,50)/50*max_inc$max_inc,-1) )) +
    ggtitle(plot.title)
 

if(fill.waffle==T){
  p1 <- ggplot(attrib.pct.df.plot, aes(fill = Virus, values = attrib_inc)) +
    geom_waffle(aes(color=Virus), size = 1, n_rows = nrows.plot, flip = TRUE) +
    scale_color_manual( values = alpha(c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                         "#F0E442", "#0072B2" ),0.0001) )  +
    facet_wrap(~Age_group, nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
    scale_fill_manual(name = NULL,
                      values = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442", "#0072B2" ),
                      labels = c("Other Virus/None", "RSV", "Influenza","hMPV","Parainfluenza","Adenovirus") )+
    coord_equal() +
    labs(
      x = "Age group",
      y = paste0("Annual Cases/100,000")
    ) +
    theme_minimal(base_family = "Roboto Condensed") +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
    guides(fill = guide_legend(reverse = TRUE))+
    scale_y_continuous(breaks=c(10,20,30,40,50),expand = c(0,0), 
                       labels= as.character(round(c(10,20,30,40,50)/50*max_inc$max_inc,-1) )) +
    ggtitle(plot.title)  
}
  
  if(off.legend==T){
    p1 <- p1 + theme(legend.position="none")
  } 

  return(p1)
}
