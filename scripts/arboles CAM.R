

# SPATIAL AVAILABILITY OF WOODPIGEON TREE RESOURCES


library(terra)
library(sf)

source('scripts/import data.R')


# points
v_palomas <- palomas_uncount %>% vect(geom=c('x','y'))
crs(v_palomas) <- 'epsg:4326'

# parks
parques <- st_read('data/Parques historicos.kml')[1] %>% vect()

# Madrid tree inventory
mad_trees <- vect('data/parquesyjardines_cam/capas_shp/arbol.shp') %>%
  terra::project('epsg:4326') %>% crop(ext(v_palomas))

plot(parques)
points(mad_trees, col='red')
points(v_palomas, col='blue')


# plot genera used by wood pigeons
mad_trees$GENERO <-  gsub("([A-Za-z]+).*", "\\1", mad_trees$ESPECIE)
mad_trees$GENERO[mad_trees$GENERO=='Sophora'] <- 'Styphnolobium'  

# only Cornus is not present in Madrid's database
table(mad_trees$GENERO[which(mad_trees$GENERO %in% palomas$genus)])

# subset 
mad_trees <- mad_trees[mad_trees$GENERO %in% unique(palomas$genus),]

length(unique(mad_trees$GENERO))
genera1 <- names(table(palomas_uncount$genus)[order(table(palomas_uncount$genus), decreasing=T)])[1:12]
par(mfrow=c(3,4), mar=c(c(0,0,0,0)))
for (g in genera1) {
  plot(mad_trees[mad_trees$GENERO==g,], pch=4, col='green3', main=g)
  # points(v_palomas, pch=4, col='red')
  lines(parques)
}


# diversity x park: size, n_trees, species and genera
div_parks <- matrix(ncol=7, nrow=length(parques$Name)) %>% as.data.frame()
colnames(div_parks) <- c('name','n_woodpigeons','ab_woodpigeons',
                         'area','n_trees','n_species','n_genera')
div_parks$name <- parques$Name
for (r in  1:nrow(parques)) {
  
  # plot(parques[r,])
  # points(mad_trees, col='red')
  # points(v_palomas, col='blue')
  
  trees1 <- intersect(parques[r,], mad_trees)
  pigeons <- intersect(parques[r,], v_palomas)
  
  div_parks$n_woodpigeons[r] <- nrow(unique(pigeons))
  div_parks$ab_woodpigeons[r] <- nrow(pigeons)
  div_parks$area[r] <- expanse(parques[r,])
  div_parks$n_trees[r] <- nrow(trees1)
  div_parks$n_species[r] <- length(unique(trees1$ESPECIE))
  div_parks$n_genera[r] <- length(unique(trees1$GENERO))
}


# 
pairs(div_parks[,2:7],lower.panel=NULL)
