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

# Subsetting only initial syllables (Utterance and word initial)

arapaho_initial <- subset(arapaho, position == 'utterance initial' | position == 'word initial')
arapaho_initial_mean <- setNames(aggregate(arapaho_initial$syllable_duration, list(arapaho_initial$number), FUN=mean), c('number', 'mean'))

bainouk_initial <- subset(bainouk, position == 'utterance initial' | position == 'word initial')
bainouk_initial_mean <- setNames(aggregate(bainouk_initial$syllable_duration, list(bainouk_initial$number), FUN=mean), c('number', 'mean'))

beja_initial <- subset(beja, position == 'utterance initial' | position == 'word initial')
beja_initial_mean <- setNames(aggregate(beja_initial$syllable_duration, list(beja_initial$number), FUN=mean), c('number', 'mean'))

bora_initial <- subset(bora, position == 'utterance initial' | position == 'word initial')
bora_initial_mean <- setNames(aggregate(bora_initial$syllable_duration, list(bora_initial$number), FUN=mean), c('number', 'mean'))

evenki_initial <- subset(evenki, position == 'utterance initial' | position == 'word initial')
evenki_initial_mean <- setNames(aggregate(evenki_initial$syllable_duration, list(evenki_initial$number), FUN=mean), c('number', 'mean'))

mojeno_initial <- subset(mojeno, position == 'utterance initial' | position == 'word initial')
mojeno_initial_mean <- setNames(aggregate(mojeno_initial$syllable_duration, list(mojeno_initial$number), FUN=mean), c('number', 'mean'))

ruuli_initial <- subset(ruuli, position == 'utterance initial' | position == 'word initial')
ruuli_initial_mean <- setNames(aggregate(ruuli_initial$syllable_duration, list(ruuli_initial$number), FUN=mean), c('number', 'mean'))

urum_initial <- subset(urum, position == 'utterance initial' | position == 'word initial')
urum_initial_mean <- setNames(aggregate(urum_initial$syllable_duration, list(urum_initial$number), FUN=mean), c('number', 'mean'))

warlpiri_initial <- subset(warlpiri, position == 'utterance initial' | position == 'word initial')
warlpiri_initial_mean <- setNames(aggregate(warlpiri_initial$syllable_duration, list(warlpiri_initial$number), FUN=mean), c('number', 'mean'))

yurakare_initial <- subset(yurakare, position == 'utterance initial' | position == 'word initial')
yurakare_initial_mean <- setNames(aggregate(yurakare_initial$syllable_duration, list(yurakare_initial$number), FUN=mean), c('number', 'mean'))

#creating line plots

arapaho_initial_line <- ggplot(arapaho_initial_mean,aes(x=number,y=mean))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_initial_line <- ggplot(bainouk_initial_mean,aes(x=number,y=mean))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12,14), limits = c(0,14))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_initial_line <- ggplot(beja_initial_mean,aes(x=number,y=mean))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12,14), limits = c(0,14))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_initial_line <- ggplot(bora_initial_mean,aes(x=number,y=mean))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_initial_line <- ggplot(evenki_initial_mean,aes(x=number,y=mean))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_initial_line <- ggplot(mojeno_initial_mean,aes(x=number,y=mean))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10), limits = c(0,10))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_initial_line <- ggplot(ruuli_initial_mean,aes(x=number,y=mean))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_initial_line <- ggplot(urum_initial_mean,aes(x=number,y=mean))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,10))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_initial_line <- ggplot(warlpiri_initial_mean,aes(x=number,y=mean))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_initial_line <- ggplot(yurakare_initial_mean,aes(x=number,y=mean))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

comparison <- ggarrange(arapaho_initial_line, bainouk_initial_line, beja_initial_line, bora_initial_line, evenki_initial_line, mojeno_initial_line, ruuli_initial_line, urum_initial_line, warlpiri_initial_line, yurakare_initial_line + rremove("x.text"),
                        ncol = 5, nrow = 2)
annotate_figure(comparison, top = text_grob("mean first syllable duration in 10 non-european languages", 
                                            face = "bold", size = 14))
