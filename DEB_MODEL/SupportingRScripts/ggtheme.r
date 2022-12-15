

# ~~ REQUIRED PACKAGES ~~ #

library(ggplot2); 
library(gridExtra)
require(grid)
#install.packages("extrafont");
# library(extrafont)
#font_import("")


# ~~ THEME ~~ #


theme_dream <- function(base_size = 26, base_family = "Time", plot.type = "informal", axis.font = base_family, title.size = 16, legend.size = base_size,
                        bg.col = ifelse(plot.type == "formal", "white", "#F0F0F0"), title.font = base_family , base.col = "black", axis.lines = TRUE,
                        minor.grid = TRUE, vert.grid = TRUE, ticks.length = 0.2, horz.grid = TRUE){
  theme_bw()+
    ggplot2::theme(
      # Plot margins and finally line annotations
      plot.margin = grid::unit(c(1, 1, .5, .7), "cm"),
      
      text = ggplot2::element_text(family = base_family, size = base_size),
      axis.line =  element_line(size = ifelse(axis.lines, 0.7,0), color = "black"),
      axis.ticks.length = grid::unit(ticks.length, "cm"),
      axis.text.x = ggplot2::element_text(size = base_size, colour = base.col, family = axis.font),
      axis.text.y = ggplot2::element_text(size = base_size, colour = base.col, family = axis.font),
      axis.title.y = ggplot2::element_text(size =  base_size, colour = base.col, vjust = 1.5, family = axis.font),
      axis.title.x = ggplot2::element_text(size = base_size,colour = base.col,vjust = -.5, family = axis.font),
      panel.background = ggplot2::element_rect(fill = bg.col),
      plot.background = ggplot2::element_rect(fill = bg.col),
      panel.border = ggplot2::element_rect(colour = bg.col),
      # Format the grid
      panel.grid.major.x = ggplot2::element_line(colour = ifelse(vert.grid, "grey60",bg.col), size = ifelse(vert.grid,0.45, 0)),
      panel.grid.minor.x = ggplot2::element_line(colour = ifelse(vert.grid, ifelse(minor.grid, "grey80",bg.col),bg.col), size = ifelse(vert.grid,0.35, 0)),
      panel.grid.major.y = ggplot2::element_line(colour = ifelse(horz.grid, "grey60",bg.col), size = ifelse(horz.grid,0.45, 0)),
      panel.grid.minor.y = ggplot2::element_line(colour = ifelse(horz.grid, ifelse(minor.grid, "grey80",bg.col),bg.col), size = ifelse(horz.grid,0.35, 0)),
      panel.grid.major = ggplot2::element_line(colour = "grey40", size=0.45),
      plot.title = ggplot2::element_text(face="bold",hjust = ifelse(plot.type == "formal", 0.5, 0) ,vjust = 2, colour = base.col, size = title.size, family = title.font),
      legend.background = ggplot2::element_rect(fill = scales::alpha('white', 0.1)), legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = legend.size),
      #legend.title = element_blank(),
      #strip.background = ggplot2::element_rect(fill = ifelse(plot.type == "formal", bg.col, "grey70")),
      #strip.background = ggplot2::element_blank(), # removes boxes around panel labels
      strip.text.x = ggplot2::element_text(size = base_size + 1),
      axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1)
      #panel.border = element_rect(colour = "black", fill=NA, size=1) # overrides previous panel.border
      
      
    )
} 




