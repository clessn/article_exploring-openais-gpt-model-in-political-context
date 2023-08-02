coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x"){
    "y"
  } else {
    "x"
  }
  ggproto("CordRadar", CoordPolar, theta = theta, clip = "off", r = r, 
  start = start, direction = sign(direction), is_linear = function(coord) TRUE)
}


spider_graph <- function(data,
                         t=0, r=0, b=0, l=-300){
  data <- ByParty %>% filter(party == "PLC")
  data %>% 
    ggplot(aes(x = theme, y = prop)) +
    geom_point(color = "#525252", size = 40,
               shape = 21, fill = NA,
               aes(y = 0), stroke = 2.5) +
    geom_polygon(aes(group = party),
                 fill = "white", color = "white",
                 size = 1, alpha = 0.2) +
    geom_polygon(aes(group = party, fill = party, color = party),
                 size = 1, alpha = 0.2) +
    geom_line(aes(group = party, color = party),
              size = 1) +
    geom_text(aes(x = theme,
                  label = theme),
              y = 1.5,
              color = "black",
              lineheight = 0.25,
              show.legend = F) +
    clessnverse::theme_clean_light() +
    coord_radar()# +
    #theme(axis.text.y = element_blank(),
    #      axis.ticks.y = element_blank(),
    #      axis.text.x = element_blank(),
    #      #axis.text.x = element_text(size = 75,
    #      #                           lineheight = 0.25,
    #      #                           vjust="inward",hjust="inward"),
    #      axis.line.x = element_blank(),
    #      panel.grid.major.x = element_line(color = "#525252", size = 1.5),
    #      panel.grid.major.y = element_blank(),
    #      #panel.grid.major.y = element_line(color = "#525252", size = 5),
    #      text = element_text(family = "VT323", lineheight = 0.25),
    #      plot.background = element_rect(fill = "#494949"),
    #      panel.background = element_rect(fill = "#494949"),
    #      plot.title = element_text(family = "VT323", face = "bold", hjust = 0.5,
    #                                size = 250, lineheight = 0.35),
    #      plot.title.position = "panel",
    #      plot.margin = margin(t=t,r=r,b=b,l=l))
  return(plot)
}