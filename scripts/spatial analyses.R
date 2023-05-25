
library(terra)
library(mapSpain)
library(sf)
library(ggpubr)

source('scripts/import data.R')


# study area
mad_pro <- esp_get_prov(year='2021', epsg="4326") %>% subset(ine.prov.name=='Madrid') %>%
  vect() %>% terra::project('EPSG:32630')
mad_mun <- esp_get_munic(year='2019', epsg="4326") %>% subset(name=='Madrid') %>%
  vect() %>% terra::project('EPSG:32630')
parques <- st_read('data/Parques historicos.kml')[1] %>% vect() %>% terra::project('EPSG:32630')

# siose
file <- 'C:/Users/user/Desktop/SAR2017_28_Madrid_GPKG/28_MADRID.gpkg'
ogrListLayers(file)
siose <- st_read(dsn=file, layer='SAR_28_T_COMBINADA') %>%
  subset(MUNICIPIO_NOMBRE=='MADRID' &
           COBERTURA_DESC %in% c("Zona verde artificial y arbolado urbano", "Cursos de agua",
                                 "Autopistas y autovías", "Vías urbanas")) %>%
  vect() %>% terra::project('EPSG:32630') %>% crop(ext(parques))

siose$COBERTURA_DESC[siose$COBERTURA_DESC=='Autopistas y autovías'] <- 'Roads'
siose$COBERTURA_DESC[siose$COBERTURA_DESC=='Cursos de agua'] <- 'Rivers'
siose$COBERTURA_DESC[siose$COBERTURA_DESC=='Zona verde artificial y arbolado urbano'] <- 'Parks'


# points
v_palomas <- palomas_uncount %>% vect(geom=c('x','y'))
crs(v_palomas) <- 'epsg:4326'
v_palomas <- v_palomas %>% terra::project('EPSG:32630') # let's work in UTM to obtain distances in meters


# plot: study area
par(mfrow=c(1,2), mar = c(3,3,3,3))
plot(mad_pro); lines(mad_mun); lines(ext(parques), col='black', lty='dotted')
plot(siose, 'COBERTURA_DESC', col=c('green3','lightblue','grey'), legend=NULL)
points(v_palomas, pch=4, col='red')


# distance to park
v_palomas2 <- v_palomas[which(!(geom(v_palomas)[,'x']>441500 & geom(v_palomas)[,'y']>4476000)),] # remove Cuarto Deposito
palomas2 <- as.data.frame(v_palomas2) # redo dataframe
# cast polygons to lines, calculate dist and invert in park observations
palomas2$park_dist1 <- v_palomas2 %>% distance(as.lines(parques), unit='m') %>% apply(1, FUN=min)
palomas2$park_dist1[palomas2$parkcity=='park'] <- palomas2$park_dist1[palomas2$parkcity=='park'] * (-1)
boxplot(palomas2[,'park_dist1'])


# plot: park usage per season
par(mfrow=c(2,2), mar=c(4,4,4,4))

plot(siose, 'COBERTURA_DESC', col=c('green3','grey80','grey80'), legend=NULL, main='autumn')
lines(siose, col='grey95')
points(v_palomas2[v_palomas2$season=='autumn' & v_palomas2$parkcity=='park',],, pch=4, col='red')
points(v_palomas2[v_palomas2$season=='autumn' & v_palomas2$parkcity=='city',], pch=4, col='blue')

plot(siose, 'COBERTURA_DESC', col=c('green3','grey80','grey80'), legend=NULL, main='winter')
lines(siose, col='grey95')
points(v_palomas2[v_palomas2$season=='winter' & v_palomas2$parkcity=='park',], pch=4, col='red')
points(v_palomas2[v_palomas2$season=='winter' & v_palomas2$parkcity=='city',], pch=4, col='blue')

plot(siose, 'COBERTURA_DESC', col=c('green3','grey80','grey80'), legend=NULL, main='spring')
lines(siose, col='grey95')
points(v_palomas2[v_palomas2$season=='spring' & v_palomas2$parkcity=='park',], pch=4, col='red')
points(v_palomas2[v_palomas2$season=='spring' & v_palomas2$parkcity=='city',], pch=4, col='blue')

plot(siose, 'COBERTURA_DESC', col=c('green3','grey80','grey80'), legend=NULL, main='summer')
lines(siose, col='grey95')
points(v_palomas2[v_palomas2$season=='summer' & v_palomas2$parkcity=='park',], pch=4, col='red')
points(v_palomas2[v_palomas2$season=='summer' & v_palomas2$parkcity=='city',], pch=4, col='blue')


# distance to park across seasons
my_comparisons <- list(c("winter","summer"))
ggplot(aes(x=season, y=park_dist1), data=palomas2) +
  ylab('Distance to nearest park (m)') + xlab('') +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_jitter(color='gray50', shape=1, position=position_jitter(0.01)) +
  geom_boxplot(alpha=0) +
  stat_compare_means(method='anova') +
  theme_bw() +
  theme(axis.text.x=element_text(size=12))
