
library(viridis)
library(ggpubr)

source('scripts/import data.R')


# Impact of abundances on analyses: flock size per season
ggplot(aes(x=month, y=n), data=palomas) +
  geom_boxplot() +
  theme_bw() +
  ylab('Flock size') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))

ggplot(aes(x=parkcity, y=n), data=palomas) + # groundtree
  geom_boxplot() +
  theme_bw() +
  ylab('Flock size') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))


# Foraging per season (H1)
g1 <- ggplot(aes(x=month, fill=groundtree), data=palomas) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_grey() +
  theme_bw() +
  ylab('prop. observations (presence)') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))

g2 <- ggplot(aes(x=month, fill=groundtree), data=palomas_uncount) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_grey() +
  theme_bw() +
  ylab('prop. observations (abundance)') + xlab('') +
  theme(legend.position='none', legend.title=element_blank(),
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))

ggarrange(g1, g2, ncol=1, heights=c(1.1,1))


# Spatial differences (H4)
g1 <- ggplot(aes(x=month, fill=parkcity), data=palomas) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_grey() +
  theme_bw() +
  ylab('prop. observations (presenece)') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))

g2 <- ggplot(aes(x=month, fill=parkcity), data=palomas_uncount) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_grey() +
  theme_bw() +
  ylab('prop. observations (abundance)') + xlab('') +
  theme(legend.position='none', legend.title=element_blank(),
        axis.text.x=element_text(size=12), axis.title.y=element_text(size=12))

ggarrange(g1, g2, ncol=1, heights=c(1.1,1))


# Ground*City (H1 & H4)
table(palomas$groundtree, palomas$parkcity, palomas$season)
ggplot(aes(x=parkcity,  fill=groundtree), data=palomas_uncount) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_fill_grey() +
  theme_bw() +
  ylab('prop. observations') + xlab('') +
  theme(legend.position='top', legend.title=element_blank(),
        axis.text.x=element_text(size=10), axis.title.y=element_text(size=10)) +
  facet_grid(~season)
