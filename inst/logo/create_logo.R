## code to prepare the `logo` goes here

library(hexSticker)
library(tidyverse)
library(showtext)
library(cowplot)
library(geomtextpath)

# Loading Google fonts (http://www.google.com/fonts)
font_add_google("Open Sans")
# Automatically use showtext to render text for future devices
showtext_auto()


# Curved package name -----------------------------------------------------

sticker_svg <- ggplot() +
  geom_hexagon(size = 2, fill = "#364e60", color = "#54a6ba") +
  geom_textcurve(data = data.frame(x = 0.25, xend = 1.75, y = 1.25, yend = 1.25),
                 aes(x, y, xend = xend, yend = yend), hjust = 0.5, ncp = 50,
                 curvature = -0.5, label = "serotrackr", color = "white",
                 size = 14, text_only = TRUE, fontface = "bold", remove_long = TRUE,
                 family = "Open Sans") +
  draw_image("inst/logo/ST_logo.png", x = 0.5, y = 0.325, scale = 1.2) +
  theme_sticker()
ggsave("inst/logo/pkg_logo.svg", plot = sticker_svg, bg = "transparent",
       units = "mm", width = 43.9*2, height = 50.8*2, dpi = 150)

# sticker_png <- ggplot() +
#   geom_hexagon(size = 2, fill = "#364e60", color = "#54a6ba") +
#   geom_textcurve(data = data.frame(x = 0.25, xend = 1.75, y = 1.3, yend = 1.3),
#                  aes(x, y, xend = xend, yend = yend), hjust = 0.5, ncp = 50,
#                  curvature = -0.5, label = "serotrackr", color = "white",
#                  size = 18, text_only = TRUE, fontface = "bold", remove_long = TRUE,
#                  family = "Open Sans") +
#   draw_image("inst/logo/ST_logo.png", x = 0.5, y = 0.35, scale = 1.2) +
#   theme_sticker()
# ggsave("inst/logo/pkg_logo.png", plot = sticker_png, bg = "transparent",
#        units = "mm", width = 43.9*2, height = 50.8*2, dpi = 150)


# Oblique package name ----------------------------------------------------

# sticker_png <- ggplot() +
#   geom_hexagon(size = 1.5, fill = "#364e60", color = "#54a6ba") +
#   draw_image("inst/logo/ST_logo.png", x = 0.5, y = 0.35, scale = 1.25) +
#   theme_sticker() +
#   geom_pkgname("serotrackr", x = 0.65, y = 1.6, size = 15, angle = 30,
#                family = "Open Sans", fontface = "bold")
#
# sticker_svg <- ggplot() +
#   geom_hexagon(size = 1.5, fill = "#364e60", color = "#54a6ba") +
#   draw_image("inst/logo/ST_logo.png", x = 0.5, y = 0.35, scale = 1.25) +
#   theme_sticker() +
#   geom_pkgname("serotrackr", x = 0.65, y = 1.6, size = 4.5, angle = 30,
#                family = "Open Sans", fontface = "bold")
#
# save_sticker("inst/logo/pkg_logo.png", sticker = sticker_png)
# save_sticker("inst/logo/pkg_logo.svg", sticker = sticker_svg)
