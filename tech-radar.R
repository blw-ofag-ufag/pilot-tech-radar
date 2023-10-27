#' ---
#' title: Script to generate a visualization of the technology radar
#' author: Damian Oswald
#' date: 2023-10-13
#' ---

#' Fix random processes
set.seed(1)

#' Attach packages to search path.
library(magrittr)

#' Read the data from a CSV file.
data <- read.csv("data.csv", sep = ";")

#' Omit data without Sector or Status information
data <- data[!as.logical(rowSums(data[,2:3]=="")),]

#' Change variable classes
for (i in c("Sector","Status")) data[,i] %<>% tolower %<>% as.factor

#' Sort data
data <- data[with(data, order(Sector, Status)),]

#' Information relevant for the dynamic plotting
d <- nlevels(data[,"Status"])
g <- nlevels(data[,"Sector"])

#' Give an x and y axis position to every observation
data[,"x"] <- (as.integer(data[,"Sector"])-1)/(g-1)
data[,"y"] <- 1 - (as.integer(data[,"Status"])-1)/(d-1)

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
png("technology-radar.png", width = 12, height = 8, res = 300, unit = "in")

#' Set graphical parameters
par(bg = "white", mar = rep(0,4))
palette <- colorRampPalette(c("red","#517A7B"))

#' Set the plotting plane
plot(NA, xlim = c(-2,2), ylim = c(-1,2), axes = FALSE, xlab = "", ylab = "", asp = 1)

#' Add the rings and sector segments
width <- 30
rings <- seq(-pi*(g/((g-1)*2)), pi*(g/((g-1)*2)),l=500)
for (i in seq(1,2,0.25)) lines(sin(rings)*i, cos(rings)*i, lwd = width, col = palette(7)[(i-1)*5+1])
for (i in seq(-g/((g-1)*2),g/((g-1)*2),l=g+1)) lines(x = c(0, 3*sin(i*pi)), y = c(0, 3*cos(i*pi)), col = par()$bg, lwd = width)

#' Add the numbered white points
with(data, points((sin(x*pi - pi/2))*(y+1), cos(x*pi-pi/2)*(y+1), pch = 21, cex = sqrt(Relevance)*2, lwd = sqrt(Relevance), bg = par()$bg))
with(data, text((sin(x*pi - pi/2))*(y+1), cos(x*pi-pi/2)*(y+1), 1:nrow(data), cex = sqrt(Relevance)*0.6, col = par()$fg, font = 2))

#' Close SVG file
dev.off()

#' Write the legend as text into the README file
sink("README.md")
readLines("HEADER.md") |> cat(sep="\n")
for (i in 1:nrow(data)) cat("\n\n(", i, ") **", data[i,"Name"],":** ", data[i,"Description"], sep = "")
sink()
