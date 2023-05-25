

# IMPORT WOODPIGEON OBSERVATIONS AND TREE DATA


library(readxl)
library(tidyverse)


# import data
palomas <- read_excel("data/palomas_190523.xlsx") %>% as.data.frame()
 str(palomas)
summary(palomas)

palomas$season <- as.factor(palomas$season) # order seasons
palomas$season <- factor(palomas$season, levels=c('autumn','winter','spring','summer'))

palomas$food[palomas$food=='NA'] <- NA # format NAs
palomas$n[is.na(palomas$n)] <- 1 # NAs to 1

palomas$parkcity <- as.factor(palomas$parkcity)
palomas$groundtree <- as.factor(palomas$groundtree)
palomas$food <- as.factor(palomas$food)


# fix errors
palomas$site <- tolower(palomas$site)

palomas$x[palomas$x > -3.5] <- palomas %>% subset(site=='Retiro') %>% # fix
  select(x) %>% colMeans()

palomas$plant[palomas$plant=='Ground'] <- 'ground' # fix

palomas$plant[palomas$plant=='Gledtsia triacanthos'] <- 'Gleditsia triacanthos' # fix
palomas$plant[palomas$plant=='Fraxinus sp'] <- 'Fraxinus sp.'
palomas$plant[palomas$plant=='Olea europaea var.domestica'] <- 'Olea europaea'


# create new variables
palomas$date <- as.Date(palomas$date, format="%Y-%m-%d") # extract month
palomas$month <- format(as.Date(palomas$date, format="%Y-%m-%d"),"%m") # extract month
palomas$month <- as.factor(palomas$month)
levels(palomas$month) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
palomas$month <- factor(palomas$month, c('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep'))
palomas$month[palomas$date < '2021-10-15'] <- 'Oct' # last days of sep '21 to october


# add tree data and merge
trees <- read_excel("data/palomas_190523.xlsx", sheet='trees')
palomas <- merge(palomas, trees, by.x='plant', by.y='species', all.x=T)


# remove uncertain observations
out <- c( which(palomas$genus=='Ulmus' & palomas$food=='root and/or leaves'),
          which(palomas$genus=='Styphnolobium' & palomas$food=='root and/or leaves'),
          which(palomas$genus=='Celtis' & palomas$food=='root and/or leaves') )
palomas <- palomas[-out,]


# create a long format df for barplots
palomas_uncount <- palomas %>% uncount(n)


# 
rm(trees)
