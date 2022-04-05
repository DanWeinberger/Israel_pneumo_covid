attrib_pct_waffle <- function(ds){
  attrib.pct.any <- ds$attrib_pct$attrib_pct_any
  
  attrib.pct.rsv <- ds$attrib_pct$attrib_pct_RSV
  
  attrib.pct.hmpv <- ds$attrib_pct$attrib_pct_HMPV
  
  attrib.pct.flu <- ds$attrib_pct$attrib_pct_Flu
  
  attrib.pct.adeno <- ds$attrib_pct$attrib_pct_Adeno
  
  attrib.pct.Paraflu <- ds$attrib_pct$attrib_pct_Paraflu
  
  
  attrib.pct.df <- round(bind_rows(attrib.pct.any,attrib.pct.rsv,attrib.pct.hmpv,attrib.pct.flu,attrib.pct.adeno,attrib.pct.Paraflu))
  
  attrib.pct.df$virus <- c('any','rsv','hmpv','flu', 'adeno', 'paraflu')
  
  attrib.pct.df.plot <- attrib.pct.df
  
  attrib.pct.df.plot$attrib_pct_med[attrib.pct.df.plot$attrib_pct_med<0] <- 0
  
  attrib.pct.df.plot$attrib_pct_med[attrib.pct.df.plot$virus=='any'] <- 100 - sum(attrib.pct.df.plot$attrib_pct_med[attrib.pct.df.plot$virus!='any'])
  
  attrib.pct.df.plot$virus[attrib.pct.df.plot$virus=='any'] <- 'None'
  
  
  nrows.plot <- 10
  p1 <- ggplot(attrib.pct.df.plot, aes(fill = virus, values = attrib_pct_med)) +
    geom_waffle(color = "white", size = .25, n_rows = nrows.plot, flip = TRUE) +
    #facet_wrap(~year, nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x * nrows.plot, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    ggthemes::scale_fill_tableau(name=NULL) +
    coord_equal() +
    labs(
      x = "Virus",
      y = "Count"
    ) +
    theme_minimal(base_family = "Roboto Condensed") +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
    guides(fill = guide_legend(reverse = TRUE))
  
  

  return(p1)
}