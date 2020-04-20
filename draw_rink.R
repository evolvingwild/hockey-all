## --------------------------- ##
##   Shot Location Functions   ##
## --------------------------- ##

#################################

fun.draw_rink <- function(lines_scale = 0.5, rink_type = "full") {
  
  ## Necessary objects for plotting
  xseq <- seq(-4, 4, length = 100)
  theta1 <- seq(0, 2 * pi, length = 300)
  theta <- seq(0, 2 * pi, length = 300)
  dd <- (5 + 7 / 12) / 2
  lines_mult <- lines_scale #<- 0.5
  
  
  ## Full NHL Rink
  if (rink_type == "full") { 
    
    rink <- ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) + 
      
      ## Rink border
      geom_path(
        data = data.frame(
          x = c(15, 87 + 13 * sin(seq(0, pi / 2, length = 20)), 87 + 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
          y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5)
          )
        ) + 
      geom_path(
        data = data.frame(
          x = c(15, -87 - 13 * sin(seq(0, pi / 2, length = 20)), -87 - 13 * sin(seq(pi / 2, 0, length = 20)), 15), 
          y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5)
          )
        ) + 
      
      ## Goal Lines
      geom_path(
        data = data.frame(
          x = c(89),
          y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))
          ), 
        color = 'red', 
        size = .5 * lines_mult
        ) + 
      geom_path(
        data = data.frame(
          x = c(-89), 
          y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))
          ), 
        color = 'red', 
        size = .5 * lines_mult
        ) +
      
      ## Nets
      geom_path(
        data = data.frame(
          x = c(90, 92, 92, 90), 
          y = c(-3, -3, 3, 3)
          ), 
        size = .5 * lines_mult
        ) + 
      geom_path(
        data = data.frame(
          x = c(-90, -92, -92, -90), 
          y = c(-3,-3, 3, 3)
          ), 
        size = .5 * lines_mult
        ) +
      
      ## Restricted Area
      geom_segment(
        aes(x = 89, y = -11, xend = 100, yend = -14), 
        color = 'red', 
        size = .5 * lines_mult
        ) + 
      geom_segment(
        aes(x = 89, y = 11, xend = 100, yend = 14), 
        color = 'red', 
        size = .5 * lines_mult
        ) + 
      geom_segment(
        aes(x = -89, y = -11, xend = -100, yend = -14), 
        color = 'red', 
        size = .5 * lines_mult
        ) + 
      geom_segment(
        aes(x = -89, y = 11, xend =-100, yend = 14), 
        color = 'red', 
        size = .5 * lines_mult
        ) +
      
      ## Red Line (Center Ice)
      geom_segment(
        aes(x = 0, y = -42.5, xend = 0, yend = 42.5), 
        color = 'red', 
        size = .75 * lines_mult
        ) +
      
      ## Blue Lines
      geom_segment(
        aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), 
        color = 'blue', 
        size = .75 * lines_mult
        ) + 
      geom_segment(
        aes(x = -25, y = -42.5, xend = -25,  yend = 42.5), 
        color = 'blue', 
        size = .75 * lines_mult
        ) +
      
      ## Crease
      geom_polygon(
        data = data.frame(
          x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
          y = c(-4, xseq, 4), 
          size = .5 * lines_mult
          ), 
        color = 'red', 
        fill = 'deepskyblue2', 
        size = .5 * lines_mult, 
        alpha = 0.75
        ) + 
      geom_polygon(
        data = data.frame(
          x = -1 * c(89, 83 + xseq^2 / 4^2 * 1.5, 89),
          y = c(-4, xseq, 4)
          ), 
        color = 'red', 
        fill = 'deepskyblue2', 
        size = .5 * lines_mult, 
        alpha = .75
        ) +
      ## Center Ice Circle
      geom_path(
        data = data.frame(x = 15 * sin(theta1)), 
        y = 15 * cos(theta1), 
        color = 'deepskyblue2', 
        size = .75 * lines_mult
        ) +
      ## Faceoff Dots
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                     x = 20 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                     x = -20 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                     x = -20 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                     x = 20 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                     x = -69 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), 
                                     x = 69 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                     x = -69 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) + 
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), 
                                     x = 69 + 1 * sin(theta)), 
                   fill = "red", alpha = .6) +
      
      ## Faceoff Circles
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                       yend = 22 + 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                       yend = 22 + 0.75, xend = 69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                       yend = -22 + 0.75, xend = 69 - 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                       yend = -22 + 0.75, xend = 69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                       yend = -22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                       yend = -22 - 0.75, xend = 69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                       yend = 22 - 0.75, xend = 69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                       yend = 22 + 0.75, xend = -69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                       yend = 22 - 0.75, xend = -69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                       yend = 22 + 0.75, xend = -69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                       yend = -22 + 0.75, xend = -69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                       yend = 22 - 0.75, xend = -69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                       yend = -22 + 0.75, xend = -69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                       yend = -22 - 0.75, xend = -69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                       yend = -22 - 0.75, xend = -69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = 69 - dd, 
                       yend = 22 - 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = 69 + dd, 
                       yend = 22 - 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = 69 + dd, 
                       yend = 22+17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = 69 - dd, 
                       yend = 22 + 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = 69 - dd, 
                       yend = -22 + 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = 69 + dd, 
                       yend = -22 + 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = 69 - dd, 
                       yend = -22 - 17, xend = 69 - dd), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = 69 + dd, 
                       yend = -22 - 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = -69 + dd, 
                       yend = -22 + 17, xend = -69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = -69 - dd, 
                       yend = -22 - 17, xend = -69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = -69 + dd, 
                       yend = -22 - 17, xend = -69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = -69 - dd, 
                       yend = -22 + 17, xend = -69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = -69 + dd, 
                       yend = 22 - 17, xend = -69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = -69 - dd, 
                       yend = 22 - 17, xend = -69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = -69 - dd, 
                       yend = 22 + 17, xend = -69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = -69 + dd, 
                       yend = 22 + 17, xend = -69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                       yend = 22 + 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                       yend = 22 + 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                       yend = 22 - 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = -69 + 2, 
                       yend = 22 + 3.75, xend = -69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = -69 - 2, 
                       yend = 22 + 3.75, xend = -69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = -69 + 2, 
                       yend = 22 - 3.75, xend = -69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = -69 - 2, 
                       yend = 22 - 3.75, xend = -69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = -69 + 2, 
                       yend = -22 - 3.75, xend = -69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = -69 - 2, 
                       yend = -22 - 3.75, xend = -69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = -69 + 2, 
                       yend = -22 + 3.75, xend = -69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = -69 - 2, 
                       yend = -22 + 3.75, xend = -69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                       yend = -22 + 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                       yend = -22 - 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                       yend = -22 + 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                       yend = -22 - 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                  x = 69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                  x = -69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                  x = -69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                  x = 69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      theme_void()
    
    }
  
  ## Half NHL Rink
  else if (rink_type == "half") {
    
    rink <- ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) + 
      ## Rink
      geom_path(
        data = data.frame(
          x = c(0, 87 + 13 * sin(seq(0, pi / 2, length = 20)), 87 + 13 * sin(seq(pi / 2, 0, length = 20)), 0), 
          y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5)
          )
        ) + 
      
      ## Goal Lines
      geom_path(
        data = data.frame(
          x = c(89),
          y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))
          ), 
        color = 'red', 
        size = .5 * lines_mult
        ) + 
      geom_path(
        data = data.frame(x = c(90, 92, 92, 90)), y = c(-3, -3, 3, 3), 
        size = .5 * lines_mult
        ) + 
      
      ## Restricted Area
      geom_segment(
        aes(x = 89, y = -11, xend = 100, yend = -14), 
        color = 'red', 
        size = .5 * lines_mult
        ) + 
      geom_segment(
        aes(x = 89, y = 11, xend = 100, yend = 14), 
        color = 'red', 
        size = .5 * lines_mult
        ) + 
      
      ## Red Line (Center Ice)
      geom_segment(
        aes(x = 0, y = -42.5, xend = 0, yend = 42.5), 
        color = 'red', 
        size = .75 * lines_mult
        ) +
      
      ## Blue Lines
      geom_segment(
        aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), 
        color = 'blue', 
        size = .75 * lines_mult
        ) + 
      
      ## Crease
      geom_polygon(
        data = data.frame(
          x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
          y = c(-4, xseq, 4), 
          size = .5 * lines_mult
          ), 
        color = 'red', 
        fill = 'deepskyblue2', 
        size = .5 * lines_mult, 
        alpha = 0.75
        ) + 
      
      ## Center Ice Circle
      geom_path(data = data.frame(x = abs(15 * sin(theta1))), 
                y = 15 * cos(theta1), color = 'deepskyblue2', 
                size = .75 * lines_mult) +
      
      ## Faceoff Dots
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta),
                                     x = 20 + 1 * sin(theta)),
                   fill = "red", alpha = .6) +
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta),
                                     x = 20 + 1 * sin(theta)),
                   fill = "red", alpha = .6) +
      geom_polygon(data = data.frame(y = 22 + 1 * cos(theta),
                                     x = 69 + 1 * sin(theta)),
                   fill = "red", alpha = .6) +
      geom_polygon(data = data.frame(y = -22 + 1 * cos(theta),
                                     x = 69 + 1 * sin(theta)),
                   fill = "red", alpha = .6) +
      
      ## Faceoff Circles
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                       yend = 22 + 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                       yend = 22 + 0.75, xend = 69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                       yend = -22 + 0.75, xend = 69 - 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                       yend = -22 + 0.75, xend = 69 + 6), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                       yend = -22 - 0.75, xend = 69 - 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                       yend = -22 - 0.75, xend = 69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                       yend = 22 - 0.75, xend = 69 + 6), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = 69 - dd, 
                       yend = 22 - 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 15, x = 69 + dd, 
                       yend = 22 - 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = 69 + dd, 
                       yend = 22+17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 15, x = 69 - dd, 
                       yend = 22 + 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = 69 - dd, 
                       yend = -22 + 17, xend = 69 - dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 15, x = 69 + dd, 
                       yend = -22 + 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = 69 - dd, 
                       yend = -22 - 17, xend = 69 - dd), color= 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 15, x = 69 + dd, 
                       yend = -22 - 17, xend = 69 + dd), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 + 2, 
                       yend = 22 + 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 + 0.75, x = 69 - 2, 
                       yend = 22 + 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 + 2, 
                       yend = 22 - 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = 22 - 0.75, x = 69 - 2, 
                       yend = 22 - 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 + 2, 
                       yend = -22 + 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 - 2, 
                       yend = -22 - 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 + 0.75, x = 69 - 2, 
                       yend = -22 + 3.75, xend = 69 - 2), color = 'red', size = .5 * lines_mult) + 
      geom_segment(aes(y = -22 - 0.75, x = 69 + 2, 
                       yend = -22 - 3.75, xend = 69 + 2), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = 22 + 15 * cos(theta), 
                                  x = 69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      geom_path(data = data.frame(y = -22 + 15 * cos(theta), 
                                  x = 69 + 15 * sin(theta)), color = 'red', size = .5 * lines_mult) + 
      
      coord_flip(xlim = c(-1, 100), expand = TRUE) + 
      
      theme_void() + 
      theme(aspect.ratio = 1.165)
    
    }
  
  return(rink)
  
  }

#################################

