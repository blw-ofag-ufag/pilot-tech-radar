#' ---
#' title: Script to generate a visualization of the technology radar
#' author: Damian Oswald
#' date: 2023-10-13
#' ---

#' Global parameters
rotation <- 10 # in degrees [0,360]

#' Attach packages to search path.
library(magrittr)

#' Read the data from a CSV file.
data <- read.csv("data.csv", sep = ";")

#' Omit data without Sector or Status information
data <- data[!as.logical(rowSums(data[,2:3]=="")),]

#' Change variable classes
for (i in c("Sector","Status")) data[,i] %<>% tolower %>% as.factor

#' Information relevant for the dynamic plotting
d <- nlevels(data[,"Status"])
g <- nlevels(data[,"Sector"])

#' Give an x and y axis position to every observation
data[,"x"] <- (as.integer(data[,"Sector"])-1)/(g-1)
data[,"y"] <- (as.integer(data[,"Status"])-1)/(d-1)

#' Add a x-axis bias such that no point overlaps
for (sector in levels(data[,"Sector"])) {
  for (status in levels(data[,"Status"])) {
    x <- data[data[,"Sector"]==sector & data[,"Status"]==status,"x"]
    while(any(diff(sort(x))<(1/g/5))) x <- data[data[,"Sector"]==sector & data[,"Status"]==status,"x"] + (lhs::randomLHS(length(x), 1) - 0.5)*1/(g+1)
    if(length(x)==0) next
    else data[data[,"Sector"]==sector & data[,"Status"]==status,"x"] <- x
  }
}

#' Open an empty SVG file.
svg("technology-radar.svg", width = 8, height = 12)

#' Set graphical parameters
par(bg = "gray20", mar = rep(3,4), fg = "gray90")
layout(c(1,1,2))
palette <- colorRampPalette(c("red","#517A7B"))

#' Set the plotting plane
plot(NA, xlim = c(-2,2), ylim = c(-1,2), axes = FALSE, xlab = "", ylab = "", asp = 1)

#' Add the rings and sector segments
rings <- seq(-pi*(g/((g-1)*2)), pi*(g/((g-1)*2)),l=500)
for (i in seq(1,2,0.25)) lines(sin(rings)*i, cos(rings)*i, lwd = 20, col = palette(7)[(i-1)*5+1])
for (i in seq(-g/((g-1)*2),g/((g-1)*2),l=g+1)) lines(x = c(0, 3*sin(i*pi)), y = c(0, 3*cos(i*pi)), col = par()$bg, lwd = 20)

#' Add the numbered white points
with(data, points((sin(x*pi - pi/2))*(y+1), cos(x*pi-pi/2)*(y+1), pch = 16, cex = 5, lwd = 3))
with(data, text((sin(x*pi - pi/2))*(y+1), cos(x*pi-pi/2)*(y+1), 1:nrow(data), col = par()$bg, font = 2))

#' Add text
for (i in 1:nrow(data)) {
  color <- palette(6)[6-as.integer(data[i,"Status"])]
  points(x = -2, y = -1.2 - i*0.1, cex = 2.2, xpd = NA, pch = 16)
  text(x = -2, y = -1.2 - i*0.1, labels = i, xpd = NA, cex = 0.6, font = 2, col = par()$bg)
  text(x = -1.97, y = - 1.215 - i*0.1, labels = bquote(bold(.(data[i,"Name"]))~~italic(.(data[i,"Description"]))), xpd = NA, pos = 4, font = 2, col = color)
}

#' Add a title
text(0, 2.4, "The FOAG Technology Radar (Pilot)", xpd = NA, cex = 3, font = 2)

dev.off()



