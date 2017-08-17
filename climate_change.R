library(tidyverse)
library(ggthemes)
library(cowplot)
library(grid)
library(Cairo)


### Visual setup

bad.col <- "#ff0b3a"
good.col <- "black"

fill.col <- "gray80"
text.col <- "gray48"
title.col <- "gray26"
fontfam <- "sans"
text.fontfam <- "Palatino"

theme_sl <- function() {
  # theme colors
  plot.bg <- "white"
  panel.bg <- plot.bg
  text.col <- text.col
  plot.title.col <- text.col
  axis.title.col <- text.col
  axis.text.col <- text.col
  axis.tick.col <- text.col
  grid.col <- "gray80"

  theme_wsj() +
  theme(
    plot.margin = margin(0.75, 0.75, 0.5, 0.75, "cm"),    
    text = element_text(family=fontfam),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    axis.text = element_text(colour = axis.text.col, size=18),
    plot.title = element_text(colour = plot.title.col,
                              face = "bold", size = 18, vjust = 1),
    axis.title = element_text(colour = axis.title.col,
                              face = "bold", size = 18),
    panel.grid.major.y = element_line(colour = grid.col),
    strip.text = element_text(size=18),
    axis.ticks = element_line(colour = axis.tick.col)
  )
}


# load data
ice.data <- read.table("antarctica_mass_200204_201701.txt",
                       comment.char="#", header=T)

co2.data <- read.table("co2_mm_mlo.txt",
                       comment.char="#", header=T)
co2.data <- filter(co2.data, avg != -99.99)

temp.data <- read.table("647_Global_Temperature_Data_File.txt",
                        comment.char="#", header=T)

sea.data <- read.table("GMSL_TPJAOS_V4_199209_201704.txt",
                       comment.char="#", header=T)


# graphs
floor.bounds <- function(vec) {
  return(c(floor(min(vec)), floor(max(vec))))
}

ice.timeseries <- function() {
  p <- ggplot(ice.data,
              aes(x=year.fraction, y=mass.gt)) +
       geom_ribbon(aes(ymax=(mass.gt + mass.gt.1sig),
                       ymin=(mass.gt - mass.gt.1sig)),
                   alpha=0.4,
                   fill="#3e8da3") +
       geom_line(col="#3e8da3") +
       labs(x="", y="") +
       scale_y_continuous(breaks=c(0, -1000, -2000)) +
       scale_x_continuous(breaks=floor.bounds(ice.data$year.fraction)) +
       geom_label(inherit.aes=F,
                  aes(x=floor(min(year.fraction)), y=-1000,
                      label="gigatonnes"),
                  color=text.col, hjust=0.55,
                  label.size=NA, size=4) +
       theme_sl()
      
  return(p)
}

co2.timeseries <- function() {
  p <- ggplot(co2.data,
              aes(x=year.fraction, y=avg)) +
       geom_point(col="gray40", size=0.8, alpha=0.5) +
       geom_line(col="gray40", alpha=0.5) +
       geom_line(aes(y=trend), col="red") +
       #geom_smooth(span=0.08, col="red", method="loess") +
       labs(x="", y="") +
       scale_y_continuous(breaks=c(320, 360, 400)) +
       scale_x_continuous(breaks=floor.bounds(co2.data$year.fraction)) +
       geom_label(inherit.aes=F,
                  aes(x=1958, y=400,
                      label="ppm"),
                  color=text.col, hjust=1.2,
                  label.size=NA, size=4) +
       theme_sl()

  return(p)
}

temp.timeseries <- function() {
  p <- ggplot(temp.data,
              aes(x=year, y=mean)) +
       geom_line(col="gray40", alpha=0.5) +
       geom_point(col="gray40", size=0.8, alpha=0.5) +
       geom_line(aes(y=five.year.mean),
                 col="red") +
       labs(x="", y="") +
       scale_y_continuous(breaks=c(0, 0.5, 1)) +
       scale_x_continuous(breaks=floor.bounds(temp.data$year)) +
       geom_label(inherit.aes=F,
                  aes(x=floor(min(year)), y=1,
                      label=paste("degree C change")),
                  color=text.col, hjust=0.35, label.size=NA, size=4) +
       theme_sl()

  return(p)
}

sea.timeseries <- function() {
  min.val <- min(sea.data$smooth.gmsl.gia.rmanseman)
  p <- ggplot(sea.data,
              aes(x=year.fraction)) +
       #geom_ribbon(aes(ymin=(gmsl.var - gmsl.var.stddev),
       #                ymax=(gmsl.var + gmsl.var.stddev)),
       #            alpha=0.4) +
       geom_area(aes(y=smooth.gmsl.gia.rmanseman - min.val),
                 fill="#003399", alpha=0.1) +
       geom_line(aes(y=gmsl.gia.var - min.val),
                 col="#003399",
                 alpha=0.4) +
       #geom_line(aes(y=smooth.gmsl.gia - min.val),
       #          col="gray40") +
       geom_line(aes(y=smooth.gmsl.gia.rmanseman - min.val),
                 #fill="#003399",
                 col="#003399",
                 alpha=1) +
       labs(x="", y="") +
       scale_y_continuous(breaks=c(0, 75)) +
       scale_x_continuous(breaks=floor.bounds(sea.data$year.fraction)) +
       geom_label(inherit.aes=F,
                  aes(x=floor(min(year.fraction)), y=75,
                      label=paste("mm")),
                  color=text.col, hjust=1.4, label.size=NA, size=4) +
       theme_sl()

  return(p)
}

sea.year.norm <- function(year) {
  data <- filter(data.sea, floor(year.fraction) == year)
  num.obs <- sum(data$num.observations)
  mean.mean <- mean(data$gmsl.var)
  stddev.mean <- mean(data$gmsl.var.stddev)
  p <- ggplot(data,
              aes(x=gmsl.var)) +
       stat_function(fun=dnorm, n=1000,
                     args=list(mean=mean.mean, sd=stddev.mean)) +
       labs(x="", y="") +
       scale_x_continuous(lim=c(-150, 150)) +
       theme_sl()

  return(p)
}


### Grid layouts
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


grid_text_box <- function(lines, vp=vplayout(2, 12),
                          gp=gpar(fontfamily=text.fontfam,
                                  col=text.col, cex=1.3),
                          x=0.5, y=0.5, line.gap=0.33, hjust=0.5) {
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }

  print_line <- function(line, line.num) {
    grid.text(line,
              vp=vp,
              x=unit(x, "npc"),
              y=unit(newline(y, line.num), "npc"),
              hjust=hjust,
              gp=gp)
  }
  sapply(1:length(lines), function(i) print_line(lines[i], i))
}

grid_vital_sign <- function(p, title, text.lines=c(), ...) {

  ### Grid layout stuff
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(p, vp = vplayout(2:9, 1:11))

  # Title
  grid.text(title,
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))


  ## MAIN TEXT
  if (length(text.lines) > 0) {
    grid_text_box(text.lines, ...)
  }
}

grid_temp <- function() {
  grid_vital_sign(temp.timeseries(),
                  "Temperature Anomalies from Mean 1950-1981 Levels",
                  text.lines=c("The average",
                               "temperature now",
                               "is a whole",
                               "degree warmer",
                               "than the average",
                               "1950 - 1981."),
                  x=-0.55, y=0.76, hjust=0,
                  line.gap=0.26,
                  gp=gpar(fontfamily=text.fontfam, col=text.col,
                          cex=1.2))
}

grid_co2 <- function() {
  grid_vital_sign(co2.timeseries(),
                  "Atmospheric Carbon Dioxide",
                  text.lines=c("There is more",
                               "carbon dioxide",
                               "in the",
                               "atmosphere",
                               "now that at any",
                               "other point in",
                               "human history."),
                  x=-0.55, y=0.66, hjust=0,
                  line.gap=0.26,
                  gp=gpar(fontfamily=text.fontfam, col=text.col,
                          cex=1.2))
}

grid_sea <- function() {
  grid_vital_sign(sea.timeseries(),
                  "Sea Level Rise",
                  text.lines=c("Sea level rise",
                               "is already",
                               "displacing",
                               "coastal",
                               "communities",
                               "around the",
                               "world."),
                  x=-0.55, y=0.48, hjust=0,
                  line.gap=0.26,
                  gp=gpar(fontfamily=text.fontfam, col=text.col,
                          cex=1.2))
}

grid_ice <- function() {
  grid_vital_sign(ice.timeseries(),
                  "Land Ice Shrinkage in Antarctica",
                  text.lines=c("Melting ice",
                               "contributes",
                               "greatly to",
                               "sea level rise."),
                  x=-0.55, y=-4.46, hjust=0,
                  line.gap=0.26,
                  gp=gpar(fontfamily=text.fontfam, col=text.col,
                          cex=1.2))
}

make_temp_svg <- function() {
  svg("temp_timeseries.svg", width=12, height=8)
  grid_temp()
  dev.off()
}

make_co2_svg <- function() {
  svg("co2_timeseries.svg", width=12, height=8)
  grid_co2()
  dev.off()
}

make_sea_svg <- function() {
  svg("sea_timeseries.svg", width=12, height=8)
  grid_sea()
  dev.off()
}

make_ice_svg <- function() {
  svg("ice_timeseries.svg", width=12, height=8)
  grid_ice()
  dev.off()
}

