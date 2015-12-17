
add_credits = function(fontsize = 12, color = "#777777", xpos = 0.99) {
  grid.text("junma5.weebly.com",
            x = xpos,
            y = 0.04,
            just = "right",
            gp = gpar(fontsize = fontsize, col = color))
}

title_with_subtitle = function(title, subtitle = "") {
  ggtitle(bquote(atop(.(title), atop(.(subtitle)))))
}


theme_tws = function(base_size = 12) {
  bg_color = "#f4f4f4"
  bg_rect = element_rect(fill = bg_color, color = bg_color)
  
  theme_bw(base_size) +
    theme(#axis.title.x=element_text(vjust=-0.1),
          #axis.title.y=element_text(vjust= 0.3),
          plot.background = bg_rect,
          panel.background = bg_rect,
          legend.background = bg_rect,
          panel.grid.major = element_line(colour = "grey80", size = 0.25),
          panel.grid.minor = element_line(colour = "grey80", size = 0.25),
          legend.key = element_blank())
}


theme_tws_map = function(...) {
  theme_tws(...) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
          #axis.ticks.length = unit(0, "cm"),
          #plot.margin = unit(c(1, 1, 1, 0.5), "lines")
          )
}

