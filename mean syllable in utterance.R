library('ggplot2')
library('ggpubr')

setwd('/Users/alejandracamelocruz/Desktop/Tesis/my_data')
files <- list.files(pattern = ".csv$", recursive = TRUE)

arapaho <- read.csv(files[1])
arapaho$syllable_duration <- arapaho$syllable_duration * 1000

bainouk <- read.csv(files[2])
bainouk$syllable_duration <- bainouk$syllable_duration * 1000
beja <- read.csv(files[3])
beja$syllable_duration <- beja$syllable_duration * 1000
bora <- read.csv(files[4])
bora$syllable_duration <- bora$syllable_duration * 1000
evenki <- read.csv(files[5])
evenki$syllable_duration <- evenki$syllable_duration * 1000
mojeno <- read.csv(files[6])
mojeno$syllable_duration <- mojeno$syllable_duration * 1000
ruuli <- read.csv(files[7])
ruuli$syllable_duration <- ruuli$syllable_duration * 1000
urum <- read.csv(files[8])
urum$syllable_duration <- urum$syllable_duration * 1000
warlpiri <- read.csv(files[9])
warlpiri$syllable_duration <- warlpiri$syllable_duration * 1000
yurakare <- read.csv(files[10])
yurakare$syllable_duration <- yurakare$syllable_duration * 1000


# creating duration means

arapaho_mean <- setNames(aggregate(arapaho$syllable_duration, list(arapaho$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
bainouk_mean <- setNames(aggregate(bainouk$syllable_duration, list(bainouk$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
beja_mean <- setNames(aggregate(beja$syllable_duration, list(beja$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
bora_mean <- setNames(aggregate(bora$syllable_duration, list(bora$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
evenki_mean <- setNames(aggregate(evenki$syllable_duration, list(evenki$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
mojeno_mean <- setNames(aggregate(mojeno$syllable_duration, list(mojeno$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
ruuli_mean <- setNames(aggregate(ruuli$syllable_duration, list(ruuli$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
urum_mean <- setNames(aggregate(urum$syllable_duration, list(urum$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
warlpiri_mean <- setNames(aggregate(warlpiri$syllable_duration, list(warlpiri$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))
yurakare_mean <- setNames(aggregate(yurakare$syllable_duration, list(yurakare$number_in_utterance), FUN=mean), c('number_in_utterance', 'mean'))

#creating line plots

arapaho_line <- ggplot(arapaho_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_line <- ggplot(bainouk_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_line <- ggplot(beja_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_line <- ggplot(bora_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_line <- ggplot(evenki_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_line <- ggplot(mojeno_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_line <- ggplot(ruuli_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_line <- ggplot(urum_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_line <- ggplot(warlpiri_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_line <- ggplot(yurakare_mean,aes(x=number_in_utterance,y=mean))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250, 300), limits = c(100,300))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

comparison1 <- ggarrange(arapaho_line, bainouk_line, beja_line, bora_line + rremove("x.text"),
                        ncol = 2, nrow = 2)
annotate_figure(comparison1, top = text_grob("mean syllable duration vs # of syllable in utterance", 
                                            face = "bold", size = 14))

comparison2 <- ggarrange(evenki_line, mojeno_line, ruuli_line, urum_line + rremove("x.text"),
                        ncol = 2, nrow = 2)
annotate_figure(comparison2, top = text_grob("mean syllable duration vs # of syllable in utterance", 
                                            face = "bold", size = 14))

comparison3 <- ggarrange(warlpiri_line, yurakare_line + rremove("x.text"),
                         ncol = 2, nrow = 1)
annotate_figure(comparison3, top = text_grob("mean syllable duration vs # of syllable in utterance", 
                                             face = "bold", size = 14))


