#' ---
#' title: Script to generate a visualization of the technology radar
#' author: Damian Oswald
#' date: 2023-10-13
#' ---

#' Attach packages to search path.
library(magrittr)

#' Attach packages to read in svg files, convert to png and place within plot.
library(rsvg)
library(png)
library(grid)


for (language in c("English", "German", "French", "Italian")) {
  
  #' Fix random processes
  set.seed(1)
  
  #' Read the data from a CSV file.
  data <- read.csv(file.path(language, paste0("data-",language,".csv")), sep = ";")
  
  #' Omit data without Sector or Status information
  data <- data[!as.logical(rowSums(data[,2:3]=="")),]
  
  #' Change variable classes
  factor_names <- read.csv(file.path("resources","factor-translations.csv"))
  for (i in c("Sector","Status")) {
    levelnames <- factor_names[factor_names$Variable==i & factor_names$Language==language,"Translation"]
    if (i=="Status") levelnames <- rev(levelnames)
    data[,i] <- factor(tolower(data[,i]), levels = levelnames)
  }
  
  #' Sort data
  data <- data[with(data, order(Sector, Status)),]
  
  #' Give new row names
  row.names(data) <- 1:nrow(data)
  
  #' Information relevant for the dynamic plotting
  d <- nlevels(data[,"Status"])
  g <- nlevels(data[,"Sector"])
  
  #' give a position to every observation in polar coordinates
  data[,"phi"] <- pi * ((as.integer(data[,"Sector"])-1)/(g-1) - 0.5) # should range from -pi to +pi
  data[,"R"] <- 1 - (as.integer(data[,"Status"])-1)/(d-1) + 1 # should act as a scalar and range from 1 to 2
  
  #' Add some noise to phi such that no point overlaps, but look aesthetically pleasing
  for (sector in levels(data[,"Sector"])) {
    for (status in levels(data[,"Status"])) {
      phi <- data[data[,"Sector"]==sector & data[,"Status"]==status,"phi"]
      while(any(diff(sort(phi))<(1/g/2))) phi <- data[data[,"Sector"]==sector & data[,"Status"]==status,"phi"] + pi*(lhs::optimumLHS(length(phi), 1) - 0.5)*1/(g+1)
      if(length(phi)==0) next
      else data[data[,"Sector"]==sector & data[,"Status"]==status,"phi"] <- phi
    }
  }
  
  #' override CSV file to store the polar coordinates
  write.table(data, file = file.path(language,paste0("data-",language,".csv")), sep = ";", row.names = FALSE)
  
  #' Convert logo to png for better handling
  rsvg_png(file.path("resources", "logo.svg"), "logo.png", width = 1000, height = 1000)
  
  #' Read in png logo
  logo <- readPNG("logo.png")
  
  #' Open an empty SVG file.
  png(file.path(language,"technology-radar.png"), width = 12.5, height = 8, res = 300, unit = "in")
  
  #' Set graphical parameters
  par(bg = "white", mar = c(0,0,2,0))
  palette <- colorRampPalette(c("darkred","turquoise"))(d+1)
  
  #' Set the plotting plane
  plot(NA, xlim = c(-2,2), ylim = c(-1.5,2), axes = FALSE, xlab = "", ylab = "", asp = 1)
  
  #' Add the rings and sector segments
  width <- 35
  rings <- seq(-pi*(g/((g-1)*2)), pi*(g/((g-1)*2)),l=500)
  for (i in seq(1,2,l=d)) lines(sin(rings)*i, cos(rings)*i, lwd = width, col = palette[(i-1)*5+1])
  for (i in seq(-g/((g-1)*2),g/((g-1)*2),l=g+1)) lines(x = c(0, 3*sin(i*pi)), y = c(0, 3*cos(i*pi)), col = par()$bg, lwd = width)
  
  #' Add the numbered white points
  with(data, points((sin(phi))*R, cos(phi)*R, pch = 21, cex = sqrt(Relevance)*2.5*0.8, lwd = sqrt(Relevance), bg = par()$bg))
  with(data, text((sin(phi))*R, cos(phi)*R, 1:nrow(data), cex = sqrt(Relevance)*0.75*0.8, col = par()$fg, font = 2))
  
  #' Labels of Sectors
  phi <- seq(-pi/2,pi/2,l=g)
  R <- c(2.5,2.4,2.23,2.4,2.5)
  text(x = R*sin(phi), y = R*cos(phi), toupper(gsub(" ", "\n", levels(data$Sector))), xpd = NA, font = 2, cex = 1.5)
  
  #' Labels of Status
  text(x=0, seq(-0.9,-1.5,l=d), rev(toupper(levels(data$Status))), font = 2, cex = 1.2, xpd = NA, col = palette)
  
  #' Add logo
  grid.raster(logo, x=.5, y=.43, width=.26)
  
  #' Close
  dev.off()
  
  #' Delete logo.png file again
  file.remove("logo.png")
  
  #' Write the legend as text into the README.md file
  sink(file.path(language, "README.md"))
  "![](technology-radar.png)" |>
    cat(sep="\n")
  for (i in 1:nrow(data)) {
    link <- if(data[i,"Example_URL"]!="") paste0(" [", data[i,"Example_Name"]," 🡥](", data[i,"Example_URL"], ")") else ""
    cat("\n\n(", i, ") **", data[i,"Name"],":** ", data[i,"Description"], " ", data[i,"Link"], link, sep = "")
  }
  sink()
  
}

