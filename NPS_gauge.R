#################################
# NPS Gauge
#################################

# colours used to create the gradient in the gauge
my_colourscheme <- c(
  "#e13b3c",
  "#e1623c",
  "#e18a3c",
  "#e1b43c",
  "#e1e13c",
  "#b5e13c",
  "#8ae13c",
  "#62e13c",
  "#3ce13c"
)

gg.gauge <-
  function(NPS_score,
           breaks = c(0, 12.5, 25, 37.5, 50, 62.5, 75, 87.5, 100),
           needle_thickness = 1,
           needle_start = 0.0,
           needle_end = 1) {
    require(ggplot2)
    pos <- (0.5 * NPS_score) + 50
    white_thick <- 0.7
    my_colours <- my_colourscheme
    get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
      th.start <- pi * (1 - a / 100)
      th.end   <- pi * (1 - b / 100)
      th       <- seq(th.start, th.end, length = 2000)
      x        <- r1 * cos(th)
      xend     <- r2 * cos(th)
      y        <- r1 * sin(th)
      yend     <- r2 * sin(th)
      data.frame(x, y, xend, yend)
    }
    ggplot() +
      # Gauge splits
      geom_segment(
        data = get.poly(breaks[1], breaks[2] - white_thick, 0.75),
        aes(x = x, y = y, xend = xend, yend = yend, color = xend)
      ) +
      geom_segment(
        data = get.poly(breaks[2] + white_thick, breaks[3] - white_thick, 0.75),
        aes(x = x, y = y, xend = xend, yend = yend, color = xend)
      ) +
      geom_segment(
        data = get.poly(breaks[3] + white_thick, breaks[4] - white_thick, 0.75),
        aes(x = x, y = y, xend = xend, yend = yend, color = xend)
      ) +
      geom_segment(
        data = get.poly(breaks[4] + white_thick, breaks[5] - white_thick, 0.75),
        aes(x = x, y = y, xend = xend, yend = yend, color = xend)
      ) +
      geom_segment(
        data = get.poly(breaks[5] + white_thick, breaks[6] - white_thick, 0.75),
        aes(x = x, y = y, xend = xend, yend = yend, color = xend)
      ) +
      geom_segment(
        data = get.poly(breaks[6] + white_thick, breaks[7] - white_thick, 0.75),
        aes(x = x, y = y, xend = xend, yend = yend, color = xend)
      ) +
      geom_segment(
        data = get.poly(breaks[7] + white_thick, breaks[8] - white_thick, 0.75),
        aes(x = x, y = y, xend = xend, yend = yend, color = xend)
      ) +
      geom_segment(
        data = get.poly(breaks[8] + white_thick, breaks[9], 0.75),
        aes(x = x, y = y, xend = xend, yend = yend, color = xend)
      ) +
      # Color scheme for gauge
      scale_color_gradientn(colors = my_colours) +
      # Needle
      geom_segment(
        data = get.poly(
          pos - needle_thickness - 100,
          pos + needle_thickness - 100,
          needle_start,
          needle_end
        ),
        aes(
          x = x + cos(pi * (1 - pos / 100)),
          y  = y + sin(pi * (1 - pos / 100)),
          xend = xend + cos(pi * (1 - pos / 100)),
          yend = yend + sin(pi * (1 - pos / 100))
        )
      ) +
      geom_segment(data = get.poly(-100, 100, 0, 0.04),
                   aes(x = x, y  = y, xend = xend, yend = yend)
      ) +
      # Text for -100, 0, +100
      annotate("text",
               x = -0.88,
               y = -0.15,
               label = "-100",
               vjust = 0,
               size = 12.5,
               family = "Lato"
      ) +
      annotate("text",
               x = 0.87,
               y = -0.15,
               label = "+100",
               vjust = 0,
               size = 12.5,
               family = "Lato"
      ) +
      annotate("text",
               x = 0,
               y = -0.15,
               label = "0",
               vjust = 0,
               size = 12.5,
               family = "Lato"
      ) +
      # Text for NPS score
      annotate("text",
               x  = 0,
               y = -0.33,
               label = ifelse(NPS_score > 0, paste("+", NPS_score, sep = ""), NPS_score),
               vjust = 0,
               size = 15,
               fontface = "bold",
               family = "Lato"
      ) +
      coord_fixed() +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.position = "none"
      )
  }
