#### Draw GPS and meshes plots for poster presentation

#### GPS data
library(ggplot2)

## Create the base of word map
world_map <- map_data("world2")
p <- ggplot() + coord_fixed() + xlab("") + ylab("") 
#Add map to base plot
base_world <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                     colour="grey", fill="khaki1")

base_world



## Load the GPS data
GPS_obs <- read.table("experimentBHM/GPS_combined_20170530_final.txt", header = T)
GPS_obs$lon <- ifelse(GPS_obs$lon < 0, GPS_obs$lon + 360, GPS_obs$lon)
std_size <- sqrt(sqrt(GPS_obs$std))

map_data <- base_world +
  geom_point(data=GPS_obs, aes(x=lon, y=lat, col = trend), pch=20, size = std_size*10, alpha=0.7) + 
  scale_color_gradient2(low = "dark green", mid = "darkolivegreen1", high = "orangered3", midpoint = 5, name = "trend \n mm/yr",
                        guide = guide_colorbar(barwidth = 2, barheight = 10, label.position = "right")) 

beauty <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = 'white'), 
        axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(),axis.ticks=element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.position = "left")
map_data2 <- map_data + beauty
                                                    
png("GPS_map.png", width = 1200, height = 600, pointsize = 20)
print(map_data2)
dev.off()



#### Plot GIA Mesh
## Continents and ocean islands
library(rgdal)
library(GEOmap)
library(maptools)
COIlines <- readOGR(dsn = "glbm/TestMVST/data/Shapefiles/GSHHS", layer = "GSHHS_c_L1")
## Antarctica grounding line
AnGlines <- readOGR(dsn= "glbm/TestMVST/data/Shapefiles/GSHHS", layer = "GSHHS_c_L6")
## Conbine the two SpatialPolygonDataFrame objs
row.names(COIlines) <- paste("C", row.names(COIlines), sep="_")
row.names(AnGlines) <- paste("A", row.names(AnGlines), sep="_")
BLand <- spRbind(COIlines, AnGlines)
## Convert the 2D polygon to 3d
Polys2D <- BLand@polygons
polys3D <- lapply(Polys2D, function(x) do.call(cbind, Lll2xyz(lat = x@Polygons[[1]]@coords[,2], lon = x@Polygons[[1]]@coords[,1])))

## Read in GIA and the small regular mesh
load("experimentBHM/GIA_sp_info.RData")
load("experimentBHM/mesh_reg.RData")
mesh_GIA <- mesh_regs[[1]]

MlocLL <- Lxyz2ll(list(x=mesh_GIA$loc[,1], y = mesh_GIA$loc[,2], z = mesh_GIA$loc[,3]))
MlocLL$lon <- ifelse(MlocLL$lon < 0, MlocLL$lon + 360, MlocLL$lon)
MlocLL$lon <- ifelse(MlocLL$lon > 359.5, MlocLL$lon - 360, MlocLL$lon)
M_sp <- SpatialPoints(data.frame(lon = MlocLL$lon, lat = MlocLL$lat), proj4string = CRS("+proj=longlat")) #This convert GIA_ice6g a SpatialPointDataFrame
Midx <- over(M_sp, Plist)
GIA_mu <- GIA_ice6g_sp$trend[Midx]


## Plot the mesh and GIA and coastlines
library(INLA)
library(rgl)
vals <- GIA_mu

t_lim <- c(-30, 20)
t_Clens <- round((t_lim[2] - t_lim[1])*100) + 1
t_Cpal <- topo.colors(t_Clens, alpha=0)
t_Cols<- t_Cpal[round((vals - t_lim[1])*100) + 1]
plot(mesh_GIA, rgl = TRUE, col= t_Cols, edge.color = "dark blue", add = TRUE)
for (i in 1:length(polys3D)){
  lines3d(x=polys3D[[i]][,1], y = polys3D[[i]][,2], z = polys3D[[i]][,3], add=T, lwd = 2)
}

## load another mesh
load("glbm/TestMVST/MeshGlobe.RData")
mesh_GIA <- MeshB

MlocLL <- Lxyz2ll(list(x=mesh_GIA$loc[,1], y = mesh_GIA$loc[,2], z = mesh_GIA$loc[,3]))
MlocLL$lon <- ifelse(MlocLL$lon < 0, MlocLL$lon + 360, MlocLL$lon)
MlocLL$lon <- ifelse(MlocLL$lon > 359.5, MlocLL$lon - 360, MlocLL$lon)
M_sp <- SpatialPoints(data.frame(lon = MlocLL$lon, lat = MlocLL$lat), proj4string = CRS("+proj=longlat")) #This convert GIA_ice6g a SpatialPointDataFrame
Midx <- over(M_sp, Plist)
GIA_mu <- GIA_ice6g_sp$trend[Midx]


## Plot the mesh and GIA and coastlines

vals <- GIA_mu

t_lim <- c(-30, 20)
t_Clens <- round((t_lim[2] - t_lim[1])*100) + 1
t_Cpal <- topo.colors(t_Clens, alpha=0)
t_Cols<- t_Cpal[round((vals - t_lim[1])*100) + 1]
plot(mesh_GIA, rgl = TRUE, col= t_Cols, edge.color = "dark blue", add = TRUE)
for (i in 1:length(polys3D)){
  lines3d(x=polys3D[[i]][,1], y = polys3D[[i]][,2], z = polys3D[[i]][,3], add=T, lwd = 3)
}

##### Plot prelim results of GIA
## load data results
load("experimentBHM//1blMesh_inla.RData")
## GIA
GIA_ice6g2 <- read.table(file = "experimentBHM/inla_GIApred.txt", header = TRUE)
GPS_obs$spost <- res_inla$summary.linear.predictor$sd[481:960]

map_GIA <- ggplot(data=GIA_ice6g2) + geom_raster(aes(x = x_center, y = y_center, fill = GIA_mpost)) + 
  coord_fixed() + xlab("Longitude") + ylab("Latitude") + 
  scale_x_continuous(limits=c(0,360),  expand = c(0, 0)) + scale_y_continuous(limits=c(-90,90),  expand = c(0, 0)) + 
  scale_fill_gradient2(low = "deepskyblue", mid = "white", high = "orange red", midpoint = 0, name = "mm/yr", limit = c(-15, 15),
                        guide = guide_colorbar(barwidth = 2, barheight = 10, label.position = "right", title.position = "bottom")) 

map_GIA2 <- map_GIA + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                                   colour="yellowgreen", fill = NA, alpha = 0.3)


map_GIA3 <- map_GIA2 + geom_point(data=GPS_obs, aes(x=lon, y=lat), pch=19, size = GPS_obs$spost*3,
                                    col = "black", fill = "black", alpha=0.5) 

GIA_std <- GIA_ice6g2[,c("x_center", "y_center", "GIA_spost")]
GIA_std <- subset(GIA_std, x_center %in% seq(5, 360, 15))
GIA_std <- subset(GIA_std, y_center %in% seq(-82.5, 82.5, 12))

map_GIA4 <- map_GIA3 + geom_point(data=GIA_std, aes(x=x_center, y=y_center), pch=19, size = GIA_std$GIA_spost*3,
                                  col = "grey", fill = "grey", alpha=0.5)
beauty <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = 'white'), 
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20), 
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        axis.line = element_line(size = 1),
        plot.title = element_text(hjust = 0.5, size = 30),
        panel.border = element_blank())
       

map_GIAf <- map_GIA4 + beauty + ggtitle("predicted GIA mean field (vertical bedrock movement)") 

png("GIAdif_map.png", width = 1200, height = 600, pointsize = 20)
print(map_GIAf)
dev.off()


