#' theme for lis plots
#'
#'Ref: https://joeystanley.com/blog/custom-themes-in-ggplot2
#'
#' @param dataset is a character specifiying the dataset to request.
#'
#'
#' @examples
#'
#' @import tidyverse
#'
#' @export

theme_lis <- function () {

    theme_bw(base_size=12, base_family="Avenir") +
    theme(legend.position = "top",
          panel.border = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          plot.title = element_text(color = "#36648B", face = "bold", hjust=0),
          panel.background = element_rect(fill = "#EBEBEB", colour = NA),
          plot.background = element_rect(fill = "#EBEBEB"),
          panel.grid.major = element_line(colour = "#8F8F8F", linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#EBEBEB"),
          legend.background = element_rect(fill = "#EBEBEB", size = .4, colour = "#EBEBEB"),
          legend.key=element_blank())
}

fun2 <- function(data) {

  data
    annotation_compass(l, position = "NE") +
    coord_cartesian(clip = 'off') +
    theme_lis()

}

fun2 <- function(x) {
  list(annotation_compass(l, position = "NE"),
       coord_cartesian(clip = 'off'),
       theme_lis())
}


library(tidyverse)
library(lis)
mtcars %>%
  ggplot(aes(mpg,drat, col=factor(cyl))) +
  geom_point() +
  labs(title = "First plot") +
  scale_color_lis(palette = "ccvi") +
  fun2()

#' Positioning logo
#'
#'
#' @param
#'
#' @examples
#'
#' @import RCurl
#' @import png
#' @import tidyverse
#'

annotation_compass <- function(label,sc=.1,
                               position = c('N','NE','E','SE','S','SW','W','NW'),
                               padding = grid::unit(c(0.5,0.5),"line"), ...){
  position <- match.arg(position)
  x <- switch (position,
               N = 0.5,
               NE = 1,
               E = 1,
               SE = 1,
               S = 0.5,
               SW = 0,
               W = 0,
               NW = 0
  )
  y <- switch (position,
               N = 1,
               NE = 1,
               E = 0.5,
               SE = 0,
               S = 0,
               SW = 0,
               W = 0.5,
               NW = 1
  )
  hjust <- switch (position,
                   N = 0.5,
                   NE = 1,
                   E = 1,
                   SE = 1,
                   S = 0.5,
                   SW = 0,
                   W = 0,
                   NW = 0
  )
  vjust <- switch (position,
                   N = 1,
                   NE = 1,
                   E = 0.5,
                   SE = 0,
                   S = 0,
                   SW = 0,
                   W = 0.5,
                   NW = 1
  )
  f1 <- switch (position,
                N = 0,
                NE = -1,
                E = -1,
                SE = -1,
                S = 0,
                SW = 1,
                W = 1,
                NW = 1
  )
  f2 <- switch (position,
                N = -1,
                NE = -1,
                E = 0,
                SE = 1,
                S = 1,
                SW = 1,
                W = 0,
                NW = -1
  )
  # annotation_custom(grid::textGrob(label,
  #                                  x=grid::unit(x,"npc") + f1*padding[1] ,
  #                                  y=grid::unit(y,"npc") + f2*padding[2],
  #                                  hjust=hjust,vjust=vjust, ...))
  #annotation_custom(grid::rasterGrob(png::readPNG(getURLContent("https://raw.githubusercontent.com/healthinnovation/LIS_brownbag/master/_figs/lis_logo.png")),
  annotation_custom(grid::rasterGrob(png::readPNG("./files/logo.png"),
                                     interpolate = TRUE,
                                     width = unit(sc,"npc"),
                                     height = unit(sc,"npc"),
                                     x=grid::unit(x,"npc") + f1*padding[1]*sc,
                                     y=grid::unit(y,"npc") + f2*padding[2]*sc,
                                     hjust=1.3,vjust=-.3, ...))
}
