###############################################################################
# R CODE: Locally adaptive change point detection
###############################################################################
# Moradi, M., Montesino-SanMartin, M., Ugarte, M.D., Militino, A.F.
# Public University of Navarre
# License: Availability of material under 
# [CC-BY-SA](https://creativecommons.org/licenses/by-sa/2.0/).

###############################################################################
# PACKAGES
###############################################################################

# handling
library(raster)
library(sf)

# graphics
library(leaflet)
library(mapview)
library(mapedit)
library(tmap)
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(grid)

###############################################################################
# DATASET
###############################################################################

# directory
root.dir <- "D:/change_point/wadir_as_shiram/results"
out.dir  <- "D:/change_point/wadir_as_shiram/graphs"
  
# files available
resl.fil <- list.files(root.dir, full.names = TRUE)

# file selection
sel.fil <- data.frame(field1 = 7,
                      field2 = 6,
                      field3 = 13)
num.fil <- length(sel.fil)

# field data frames
fields <- list()
for(i in 1:num.fil){
  fil <- resl.fil[unlist(sel.fil[i])]
  fil.nm <- gsub(".RData", "", basename(fil))
  load(fil)
  fields[[i]]      <- list()
  fields[[i]]$id   <- strsplit(fil.nm, "_")[[1]][1]
  fields[[i]]$acp  <- as.numeric(strsplit(fil.nm, "_")[[1]][2]) + 1985
  fields[[i]]$roi  <- sample.roi
  fields[[i]]$resl <- resl
  fields[[i]]$vals <- sample.val
  fields[[i]]$nna  <- which(!is.na(sample.val[[1]][]))
}

###############################################################################
# RESULTS (as images)
###############################################################################
# Retrieve and re-organize results as images or stacks of images

# Define configuration: 2:3, 2:4, 2:5, ..., 2:10
conf <- 3
conf.nms <- c("k=2:3","k=2:4","k=2:5","k=2:6","k=2:7","k=2:8","k=2:9","k=2:10")
sign.lvl <- 0.05

# Autocorrelation
for(i in 1:num.fil){
  x <- as.matrix(fields[[i]]$vals)
  auto.img <- raster(fields[[i]]$vals)
  for(j in 1:nrow(x)){
    if(!any(is.na(x[j,]))){
      auto.img[][j] <- unlist(pacf(x[j,], plot = FALSE)[[1]][1])}
  }
  fields[[i]]$auto <- auto.img
}

# pvalue
for(i in 1:num.fil){
  pval.img <- raster(fields[[i]]$vals)
  pval.img[][fields[[i]]$nna] <- unlist(lapply(fields[[i]]$resl, function(x){
    attr(x, "hist")[conf,"p-value"]
  }))
  names(pval.img) <- conf.nms[conf]
  fields[[i]]$pval <- pval.img
}

# pmask
for(i in 1:num.fil){
  pmsk.img <- raster(fields[[i]]$vals)
  pmsk.img[][fields[[i]]$pval[] > sign.lvl] <- NA
  pmsk.img[][fields[[i]]$pval[] <= sign.lvl] <- 1
  fields[[i]]$pmsk <- pmsk.img
}

# time
for(i in 1:num.fil){
  time.img <- raster(fields[[i]]$vals)
  time.img[][fields[[i]]$nna] <- unlist(lapply(fields[[i]]$resl, function(x){
    attr(x, "hist")[conf,"change-point"]
  }))
  names(time.img) <- conf.nms[conf]
  fields[[i]]$time <- (time.img + 1985) * fields[[i]]$pmsk
}

# time ci
for(i in 1:num.fil){
  tint.img <- raster(fields[[i]]$vals)
  tint.img[][fields[[i]]$nna] <- unlist(lapply(fields[[i]]$resl, function(x){
    ps <- attr(x, "allps")[[conf]]
    st <- which(ps < sign.lvl)
    if(length(st) > 0){
      rng <- abs(diff(range(st)))
    }else{
      rng <- NA
    }
    rng
}))
  names(time.img) <- conf.nms[conf]
  fields[[i]]$tint <- tint.img * fields[[i]]$pmsk
}

# magnitude
for(i in 1:num.fil){
  magn.img <- raster(fields[[i]]$vals)
  magn.img[][fields[[i]]$nna] <- unlist(lapply(fields[[i]]$resl, function(x){
    attr(x, "hist")[conf,"magnitude"]
  }))
  names(magn.img) <- conf.nms[conf]
  fields[[i]]$magn <- magn.img * fields[[i]]$pmsk
}

###############################################################################
# AUTOCORRELATION PLOTS
###############################################################################

# Graphical parameters
bbox_plot <-list(field2 = c(xmin = 38.13170, ymin = 30.28560, xmax = 38.15630, ymax = 30.30645),
                 field14 = c(xmin = 38.57768, ymin = 29.64759, xmax = 38.60205, ymax = 29.66662),
                 field8 = c(xmin =38.06830, ymin = 29.84690, xmax = 38.09200, ymax = 29.87100))
mark_coor <- list(field2 = list(x =c(38.14,38.15), y = c(30.29,30.30)),
                  field14 = list(x = c(38.58,38.59,38.60),y = c(29.65,29.66)),
                  field8 = list(x =c(38.075,38.085), y = c(29.855,29.865)))

# first field
auto1 <- tm_shape(fields[[1]]$auto,
                  bbox = st_bbox(bbox_plot[[1]], crs = 4326)) + 
  tm_raster(title = "Autocorrelation",
            style = "cont",
            breaks = seq(-1,1,0.2),
            palette = rev(brewer.pal(6,"RdBu")),
            legend.reverse = TRUE) + 
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[1]]$x,
          y = mark_coor[[1]]$y,
          alpha = 0,
          labels.size = 3) + 
  tm_scale_bar(position = c("right", "bottom"),
               width = 0.3,
               text.size = 4) + 
  tm_compass(position = c("right", "top"),
             text.size = 3) + 
  tm_layout(asp = 1,
            legend.position = c("left", "top"),
            legend.text.size = 2.5,
            legend.title.size = 4)

# second field
auto2 <- tm_shape(fields[[2]]$auto,
                  bbox = st_bbox(bbox_plot[[2]], crs = 4326)) + 
  tm_raster(legend.show = FALSE,
            style = "cont",
            breaks = seq(-1,1,0.2),
            palette = rev(brewer.pal(6,"RdBu"))) + 
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[2]]$x,
          y = mark_coor[[2]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(asp = 1)


# third field
auto3 <- tm_shape(fields[[3]]$auto,
                  bbox = st_bbox(bbox_plot[[3]], crs = 4326)) + 
  tm_raster(style = "cont",
            breaks = seq(-1,1,0.2),
            palette = rev(brewer.pal(6,"RdBu")),
            legend.show = FALSE) + 
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[3]]$x,
          y = mark_coor[[3]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(asp = 1)


# save the output
out.file <- file.path(out.dir, "autocor_sp.png")
png(out.file, height = 800, width = 2400)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(auto1, vp=viewport(layout.pos.col = 1, layout.pos.row = 1))
print(auto2, vp=viewport(layout.pos.col = 2, layout.pos.row = 1))
print(auto3, vp=viewport(layout.pos.col = 3, layout.pos.row = 1))
dev.off()

###############################################################################
# TMAPS - PVALUES
###############################################################################

# first field
map1 <- tm_shape(fields[[1]]$pval,
                 bbox = st_bbox(bbox_plot[[1]], crs = 4326)) + 
  tm_raster(title = "adjusted",
            breaks = c(seq(0,0.05,0.05),seq(0.2,1,0.2)),
            palette = c(rev(brewer.pal(3, "Blues"))[1], brewer.pal(5, "Reds")),
            legend.reverse = TRUE) +
  tm_compass(position = c("right", "top"),
             text.size = 3) + 
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0.,0.25,0.5),
               text.size = 4) +
  tm_shape(fields[[1]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[1]]$x,
          y = mark_coor[[1]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(legend.position = c("left", "top"),
            legend.text.size = 2,
            legend.title.size = 4,
            legend.format = list(text.separator = "-"),
            title = "p-value",
            title.size = 4,
            title.position = c("left", "top"),
            asp = 1)

# second field
map2 <- tm_shape(fields[[2]]$pval,
                 bbox = st_bbox(bbox_plot[[2]], crs = 4326)) + 
  tm_raster(title = "(adjusted)",
            legend.show = FALSE,
            breaks = c(seq(0,0.05,0.05),seq(0.2,1,0.2)),
            palette = c(rev(brewer.pal(3, "Blues"))[1], brewer.pal(5, "Reds"))) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0.,0.25,0.5),
               text.size = 4) +
  tm_shape(fields[[2]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[2]]$x,
          y = mark_coor[[2]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(asp = 1)

# third field
map3 <- tm_shape(fields[[3]]$pval,
                 bbox = st_bbox(bbox_plot[[3]], crs = 4326)) + 
  tm_raster(title = "(adjusted)",
            legend.show = FALSE,
            breaks = c(seq(0,0.05,0.05),seq(0.2,1,0.2)),
            palette = c(rev(brewer.pal(3, "Blues"))[1], brewer.pal(5, "Reds"))) +
  tm_scale_bar(position = c("right", "bottom"),
               breaks = c(0.,0.25,0.5),
               text.size = 5) +
  tm_shape(fields[[3]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[3]]$x,
          y = mark_coor[[3]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(asp = 1)

# save the output
out.file <- file.path(out.dir, "pval_sp.png")
png(out.file, height = 800, width = 2400)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row = 1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row = 1))
print(map3, vp=viewport(layout.pos.col = 3, layout.pos.row = 1))
dev.off()

###############################################################################
# TMAPS - TIME
###############################################################################

# first field
map1 <- tm_shape(fields[[1]]$time,
                 bbox = st_bbox(bbox_plot[[1]], crs = 4326)) + 
  tm_raster(title = "year",
            legend.show = TRUE,
            style = "cont",
            breaks = seq(1986,2019,5),
            palette = viridis(10),
            legend.reverse = TRUE) +
  tm_shape(fields[[1]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[1]]$x,
          y = mark_coor[[1]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(legend.position = c("left", "top"),
            legend.text.size = 2.5,
            legend.title.size = 4,
            legend.format = list(text.separator = "-"),
            title = "Time",
            title.size = 5,
            title.position = c("left", "top"),
            asp = 1)

# second field
map2 <- tm_shape(fields[[2]]$time,
                 bbox = st_bbox(bbox_plot[[2]], crs = 4326)) + 
  tm_raster(title = "year",
            legend.show = FALSE,
            style = "cont",
            breaks = seq(1986,2019,5),
            palette = viridis(10)) +
  tm_shape(fields[[2]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[2]]$x,
          y = mark_coor[[2]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(asp = 1)

# third field
map3 <- tm_shape(fields[[3]]$time,
                 bbox = st_bbox(bbox_plot[[3]], crs = 4326)) + 
  tm_raster(title = "year",
            legend.show = FALSE,
            style = "cont",
            breaks = seq(1986,2019,5),
            palette = viridis(10)) +
  tm_shape(fields[[3]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[3]]$x,
          y = mark_coor[[3]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(asp = 1)

# save the output
out.file <- file.path(out.dir, "time_sp.png")
png(out.file, height = 800, width = 2400)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row = 1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row = 1))
print(map3, vp=viewport(layout.pos.col = 3, layout.pos.row = 1))
dev.off()

###############################################################################
# TMAPS - NDVI
###############################################################################

# first field
map1 <- tm_shape(fields[[1]]$magn,
                 bbox = st_bbox(bbox_plot[[1]], crs = 4326)) + 
  tm_raster(title = "unitless",
            style = "cont",
            breaks = seq(0,0.7,0.1),
            legend.show = TRUE,
            palette = rev(terrain.colors(9)),
            legend.reverse = TRUE)+
  tm_shape(fields[[1]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[1]]$x,
          y = mark_coor[[1]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(legend.position = c("left", "top"),
            legend.text.size = 2.5,
            legend.title.size = 4,
            legend.format = list(text.separator = "-"),
            title = "Delta NDVI",
            title.size = 5,
            title.position = c("left", "top"),
            asp = 1)

# second field
map2 <- tm_shape(fields[[2]]$magn,
                 bbox = st_bbox(bbox_plot[[2]], crs = 4326)) + 
  tm_raster(style = "cont",
            breaks = seq(0,0.7,0.1),
            legend.show = FALSE,
            palette = rev(terrain.colors(9)))+
  tm_shape(fields[[2]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[2]]$x,
          y = mark_coor[[2]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(asp = 1)

# third field
map3 <- tm_shape(fields[[3]]$magn,
                 bbox = st_bbox(bbox_plot[[3]], crs = 4326)) + 
  tm_raster(style = "cont",
            breaks = seq(0,0.7,0.1),
            legend.show = FALSE,
            palette = rev(terrain.colors(9)))+
  tm_shape(fields[[3]]$roi$finished) + tm_polygons(alpha = 0,border.col="black") +
  tm_grid(labels.show = c(TRUE,TRUE),
          x = mark_coor[[3]]$x,
          y = mark_coor[[3]]$y,
          alpha = 0,
          labels.size = 3) +
  tm_layout(asp = 1)

# save the output
out.file <- file.path(out.dir, "magn_sp.png")
png(out.file, height = 800, width = 2400)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,3)))
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row = 1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row = 1))
print(map3, vp=viewport(layout.pos.col = 3, layout.pos.row = 1))
dev.off()

###############################################################################
# TEMPORAL EVALUATION
###############################################################################

# data frame
df <- data.frame()
ftype <- c("Field 1","Field 2","Field 3")

# for each field
for(i in 1:num.fil){
  # Time-period
  x <- fields[[i]]$resl[[1]]$s + 1985

  # Recover Z
  z.raw <- lapply(fields[[i]]$resl, function(x) attr(x, "allzs")[[conf]])
  z.sta <- do.call(rbind, z.raw)
  msk <- which(fields[[i]]$magn[][fields[[i]]$nna] > 0.6) # significant and crop
  z.sta <- z.sta[msk,]
  z.mns <- apply(z.sta,2,median) 
  
  # p-values
  p.raw <- lapply(fields[[i]]$resl, function(x) attr(x, "allps")[[conf]])
  p.val <- do.call(rbind, p.raw)
  p.val <- p.val[msk,]
  p.mns <- as.numeric(colMeans(p.val)) #apply(p.val,2,median)
  
  # magnitudes
  m.raw <- lapply(fields[[i]]$resl, function(x) attr(x, "allmags")[[conf]])
  m.val <- do.call(rbind, m.raw)
  m.val <- m.val[msk,]
  m.mns <- colMeans(m.val)#apply(m.val,2,median)
  
  # time estimate
  cp <- x[which.max(z.mns)]
  cprng <- range(x[which(p.mns<=sign.lvl)])
  
  # build data frame
  df <- rbind(df,
              data.frame(
                ID = ftype[i],
                x = rep(x,3),
                stat = factor(c(rep("Z",29),
                                rep("p-value",29),
                                rep("Magnitude",29)),
                              levels = c("Z", "p-value","Magnitude")),
                v = c(z.mns, p.mns, m.mns),
                t = cp,
                tst = c(rep(NA, 29), rep(cprng[1],29), rep(NA, 29)),
                tnd = c(rep(NA, 29), rep(cprng[2],29), rep(NA, 29)),
                sgn = c(rep(NA, 29), rep(sign.lvl,29), rep(NA, 29))))
}

# graph z, p-value, and magnitude
graph <- ggplot(df, aes(x = x, y = v)) +
  geom_line(lwd=2) +
  labs(x="Year",y= "")+
  facet_grid(stat~ID, scales = "free_y") +
  geom_vline(aes(xintercept= t),color="red",lwd=2) +
  geom_hline(aes(yintercept= sgn),color="brown",lwd=2) +
  geom_vline(aes(xintercept= tst),color="orange",lwd=2,linetype = "dashed") +
  geom_vline(aes(xintercept= tnd),color="orange",lwd=2,linetype = "dashed") +
  scale_x_continuous(breaks = seq(1985,2020,5)) +
  theme(legend.position="bottom",aspect.ratio=1,axis.text=element_text(size=30),
        axis.text.x = element_text(angle = 315),
        axis.title=element_text(size=30), strip.text=element_text(size=35),
        legend.text=element_text(size=rel(3)),legend.title=element_text(size=20))

# save the output
out.file <- file.path(out.dir,"curves_ts.png")
png(out.file, height = 1600, width = 1600)
graph
dev.off()

###############################################################################
# TEMPORAL EVALUATION
###############################################################################

# helper function
create_zones <- function(polygn){
  pol <- st_coordinates(polygn)
  rect <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addPolygons(lng = pol[,"X"], lat = pol[,"Y"], color = NA) %>%
    editMap()
  st_coordinates(rect$finished)
}

# first field
# locations
crop <- data.frame(X = c(38.14057, 38.15091, 38.14809, 38.13787, 38.14057),
                   Y = c(30.30184, 30.29940, 30.29038, 30.29308, 30.30184))
farm <- data.frame(X = c(38.15085, 38.14812, 38.14747, 38.15026, 38.15085),
                   Y = c(30.29937, 30.30000, 30.29800, 30.29731, 30.29937))

# general overview
map_general <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(lng = crop[,"X"], lat = crop[,"Y"], fill = FALSE) %>%
  addPolygons(lng = farm[,"X"], lat = farm[,"Y"], color = "red", fill = FALSE) %>%
  addScaleBar("topright") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery, zoomLevelOffset = -8)

# specific detail
map_detail <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(minZoom = 16, maxZoom = 17)) %>%
  addPolygons(lng = farm[,"X"], lat = farm[,"Y"], color = "red", fill = FALSE) %>%
  addScaleBar("topright") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery, zoomLevelOffset = -5)

# combine
map_complete <- latticeView(map_all, map_detail, no.initial.sync = TRUE)

# save the output
out.file <- file.path(out.dir, "field1_general.png")
mapshot(map_general,
        file = out.file,
        remove_controls = c("zoomControl"))
out.file <- file.path(out.dir, "field1_detail1.png")
mapshot(map_detail,
        file = out.file,
        remove_controls = c("zoomControl"))


# second field
# locations
crop <- data.frame(X = c(38.58462, 38.57769, 38.59482, 38.60224, 38.58462),
                   Y = c(29.64760, 29.65438, 29.66661, 29.65930, 29.64760))
farm1 <- data.frame(X = c(38.59889, 38.59889, 38.60260, 38.60260, 38.59889),
                   Y = c(29.65787, 29.66079, 29.66079, 29.65787, 29.65787))
farm2 <- data.frame(X = c(38.58537, 38.58923, 38.58626, 38.58216, 38.58537),
                    Y = c(29.65423, 29.65720, 29.66022, 29.65743, 29.65423))

# general overview
map_general <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(lng = crop[,"X"], lat = crop[,"Y"], fill = FALSE) %>%
  addPolygons(lng = farm1[,"X"], lat = farm1[,"Y"], color = "red", fill = FALSE) %>%
  addPolygons(lng = farm2[,"X"], lat = farm2[,"Y"], color = "red", fill = FALSE) %>%
  addScaleBar("topright") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery, zoomLevelOffset = -7)

# specific detail 1
map_detail1 <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(minZoom = 16, maxZoom = 17)) %>%
  addPolygons(lng = farm1[,"X"], lat = farm1[,"Y"], color = "red", fill = FALSE) %>%
  addScaleBar("topright") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery, zoomLevelOffset = -5)

# specific detail 2
map_detail2 <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(minZoom = 16, maxZoom = 17)) %>%
  addPolygons(lng = farm2[,"X"], lat = farm2[,"Y"], color = "red", fill = FALSE) %>%
  addScaleBar("topright") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery, zoomLevelOffset = -5)

# save the output
out.file <- file.path(out.dir, "field2_general.png")
mapshot(map_general,
        file = out.file,
        remove_controls = c("zoomControl"))
out.file <- file.path(out.dir, "field2_detail1.png")
mapshot(map_detail1,
        file = out.file,
        remove_controls = c("zoomControl"))
out.file <- file.path(out.dir, "field2_detail2.png")
mapshot(map_detail2,
        file = out.file,
        remove_controls = c("zoomControl"))

# third field
# locations
crop <- data.frame(X = c(38.07441, 38.08772, 38.08771, 38.07440, 38.07441),
                   Y = c(29.86838, 29.86839, 29.85165, 29.85164, 29.86838))
farm <- data.frame(X = c(38.07537, 38.07843, 38.07565, 38.07245, 38.07537),
                    Y = c(29.86819, 29.86731, 29.86322, 29.86402, 29.86819))
# general overview
map_general <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(lng = crop[,"X"], lat = crop[,"Y"], fill = FALSE) %>%
  addPolygons(lng = farm[,"X"], lat = farm[,"Y"], color = "red", fill = FALSE) %>%
  addScaleBar("topright") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery, zoomLevelOffset = -8)

# specific detail 
map_detail <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(minZoom = 16, maxZoom = 17)) %>%
  addPolygons(lng = farm[,"X"], lat = farm[,"Y"], color = "red", fill = FALSE) %>%
  setView(lng = 38.07494, lat = 29.86474 , zoom = 17) %>%
  addScaleBar("topright") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery, zoomLevelOffset = -5)

# save the output
out.file <- file.path(out.dir, "field3_general.png")
mapshot(map_general,
        file = out.file,
        remove_controls = c("zoomControl"))
out.file <- file.path(out.dir, "field3_detail1.png")
mapshot(map_detail,
        file = out.file,
        remove_controls = c("zoomControl"))
