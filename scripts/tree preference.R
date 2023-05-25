

# ANALYSIS OF TREE PREFERENCE BY WOODPIGEONS IN MADRID FROM SEPT. 21 TO SEPT. 22


library(ggpubr)
source('scripts/import data.R')


# flock size per tree
ggplot(aes(y=n, x=genus), data=subset(palomas, plant!='ground')) +
  geom_boxplot() +
  theme_bw() +
  ylab('Flock size') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=9, angle=90, hjust=1, vjust=0.25),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9))


# remove observations from ground
palomas_tree <- palomas_uncount %>% subset(plant!='ground')


# order species and genera by frequency of use and group less frequent taxa
palomas_tree$plant2 <- palomas_tree$plant # simplify plant
v_plant <- names(which(table(palomas_tree$plant2)[order(table(palomas_tree$plant2), decreasing=TRUE)] > 15))
palomas_tree$plant2[!(palomas_tree$plant2%in%v_plant)] <- 'other' #27
palomas_tree$plant2 <- as.factor(palomas_tree$plant2)
palomas_tree$plant2 <- factor(palomas_tree$plant2, levels=c(v_plant, 'other'))

v_plant <- names(which(table(palomas_tree$genus)[order(table(palomas_tree$genus), decreasing=TRUE)] > 5))
palomas_tree$genus[!(palomas_tree$genus%in%v_plant)] <- 'other'
palomas_tree$genus <- as.factor(palomas_tree$genus)
palomas_tree$genus <- factor(palomas_tree$genus, levels=c(v_plant, 'other'))

v_plant <- names(which(table(palomas_tree$family)[order(table(palomas_tree$family), decreasing=TRUE)] > 5))
palomas_tree$family[!(palomas_tree$family%in%v_plant)] <- 'other'
palomas_tree$family <- as.factor(palomas_tree$family)
palomas_tree$family <- factor(palomas_tree$family, levels=c(v_plant, 'other'))

g1 <- ggplot(aes(x=plant2), data=palomas_tree) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_grey() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=9, angle=-55, hjust=0, vjust=1.5),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9))

g2 <- ggplot(aes(x=genus), data=palomas_tree) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_grey() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=9, angle=-55, hjust=0, vjust=1.5),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9))

g3 <- ggplot(aes(x=family), data=palomas_tree) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_grey() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=9, angle=-55, hjust=0, vjust=1.5),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9))

# ggarrange(g1, g2, g3, ncol=1, heights=c(1.2,1))


#  tree preference by season (H3)
ggplot(palomas_tree, aes(x=month, y=n, fill=plant2)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_viridis_d() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.title=element_blank(), legend.position='right',
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12)) +
  guides(fill=guide_legend(ncol=1))

ggplot(palomas_tree, aes(x=month, y=n, fill=genus)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_viridis_d() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.title=element_blank(), legend.position='right',
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12)) +
  guides(fill=guide_legend(ncol=1))


# points plot: species and genua
# Sys.setlocale("LC_ALL", "English") # set language to English for plots
table(palomas_tree$plant2)/nrow(palomas_tree)*100 # 5

temp <- palomas_tree %>% subset(plant2 %in% names(table(palomas_tree$plant2)/nrow(palomas_tree))[1:5])
temp$plant2 <- droplevels(temp$plant2)
temp <- temp %>% unique() %>%
  group_by(date, plant2) %>% summarise(n=n())
temp$n[which(temp$plant2=='Celtis australis' & temp$date=='2022-09-20')] <- 1 # correct outliers

g1 <- ggplot(aes(x=date, y=n, color=plant2), data=temp) +
  # geom_point(shape=2) +
  geom_smooth(method='loess', span=0.75, se=F) +
  theme_bw() +
  ylab('Observations') + xlab('') +
  geom_vline(xintercept=temp$date[c(24,93,158)], linetype=4) +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=9, angle=-45, hjust=0.5),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9))

temp <- palomas_tree %>% subset(plant2 %in% names(table(palomas_tree$plant2)/nrow(palomas_tree))[1:5])
temp$plant2 <- droplevels(temp$plant2)
temp <- temp %>% group_by(date, plant2) %>% summarise(n=n())
temp$n[which(temp$plant2=='Celtis australis' & temp$date=='2022-09-20')] <- 1 # correct outliers
temp$n[which(temp$plant2=='Platanus x hybrida' & temp$date=='2021-11-08')] <- 4 # correct outliers

g2 <- ggplot(aes(x=date, y=n, color=plant2), data=temp) +
  # geom_point(shape=2) +
  geom_smooth(method='loess', span=0.75, se=F) +
  theme_bw() +
  ylab('Abundances') + xlab('') +
  geom_vline(xintercept=temp$date[c(24,93,158)], linetype=4) +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=9, angle=-45, hjust=0.5),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9))

ggarrange(g1, g2, ncol=2)


table(palomas_tree$genus)/nrow(palomas_tree)*100 # 5

temp <- palomas_tree %>% subset(genus %in% names(table(palomas_tree$genus)/nrow(palomas_tree))[1:6])
temp$genus <- droplevels(temp$genus)
temp <- temp %>% unique() %>%
  group_by(date, genus) %>% summarise(n=n())
temp$n[which(temp$genus=='Celtis' & temp$date=='2022-09-20')] <- 1 # correct outliers

g1 <- ggplot(aes(x=date, y=n, color=genus), data=temp) +
  # geom_point(shape=2) +
  geom_smooth(method='loess', span=0.75, se=F) +
  theme_bw() +
  ylab('Observations') + xlab('') +
  geom_vline(xintercept=temp$date[c(31,113,182)], linetype=4) +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=9, angle=-45, hjust=0.6),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9))

temp <- palomas_tree %>% subset(genus %in% names(table(palomas_tree$genus)/nrow(palomas_tree))[1:6])
temp$genus <- droplevels(temp$genus)
temp <- temp %>% group_by(date, genus) %>% summarise(n=n())
temp$n[which(temp$genus=='Celtis' & temp$date=='2022-09-20')] <- 1 # correct outliers
temp$n[which(temp$genus=='Platanus' & temp$date=='2021-11-08')] <- 4 # correct outliers

g2 <- ggplot(aes(x=date, y=n, color=genus), data=temp) +
  # geom_point(shape=2) +
  geom_smooth(method='loess', span=0.75, se=F) +
  theme_bw() +
  ylab('Abundances') + xlab('') +
  geom_vline(xintercept=temp$date[c(31,113,182)], linetype=4) +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=9, angle=-45, hjust=0.6),
        axis.text.y=element_text(size=9),
        axis.title.y=element_text(size=9))


ggarrange(g1, g2, ncol=2)


# tree usage x season
table(palomas_uncount$genus, palomas_uncount$food)
g1 <- ggplot(subset(palomas_uncount,!is.na(food)), aes(x=season, y=n, fill=food)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_viridis_d() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.title=element_blank(), legend.position='top', legend.direction='horizontal',
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))

g2 <- ggplot(subset(palomas_uncount, !is.na(origin)), aes(x=season, y=n, fill=origin)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_viridis_d() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.title=element_blank(), legend.position='top', legend.direction='horizontal',
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))

g3 <- ggplot(subset(palomas_uncount,!is.na(foliage)), aes(x=season, y=n, fill=foliage)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_viridis_d() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.title=element_blank(), legend.position='top', legend.direction='horizontal',
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))

ggarrange(g1, g2, ncol=2)


table(palomas_uncount$plant, palomas_uncount$parkcity)
