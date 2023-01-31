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

# Subsetting only final syllables (Utterance and word final)

arapaho_final <- subset(arapaho, position == 'utterance final' | position == 'word final')
arapaho_final_mean <- setNames(aggregate(arapaho_final$syllable_duration, list(arapaho_final$number), FUN=mean), c('number', 'mean'))

bainouk_final <- subset(bainouk, position == 'utterance final' | position == 'word final')
bainouk_final_mean <- setNames(aggregate(bainouk_final$syllable_duration, list(bainouk_final$number), FUN=mean), c('number', 'mean'))

beja_final <- subset(beja, position == 'utterance final' | position == 'word final')
beja_final_mean <- setNames(aggregate(beja_final$syllable_duration, list(beja_final$number), FUN=mean), c('number', 'mean'))

bora_final <- subset(bora, position == 'utterance final' | position == 'word final')
bora_final_mean <- setNames(aggregate(bora_final$syllable_duration, list(bora_final$number), FUN=mean), c('number', 'mean'))

evenki_final <- subset(evenki, position == 'utterance final' | position == 'word final')
evenki_final_mean <- setNames(aggregate(evenki_final$syllable_duration, list(evenki_final$number), FUN=mean), c('number', 'mean'))

mojeno_final <- subset(mojeno, position == 'utterance final' | position == 'word final')
mojeno_final_mean <- setNames(aggregate(mojeno_final$syllable_duration, list(mojeno_final$number), FUN=mean), c('number', 'mean'))

ruuli_final <- subset(ruuli, position == 'utterance final' | position == 'word final')
ruuli_final_mean <- setNames(aggregate(ruuli_final$syllable_duration, list(ruuli_final$number), FUN=mean), c('number', 'mean'))

urum_final <- subset(urum, position == 'utterance final' | position == 'word final')
urum_final_mean <- setNames(aggregate(urum_final$syllable_duration, list(urum_final$number), FUN=mean), c('number', 'mean'))

warlpiri_final <- subset(warlpiri, position == 'utterance final' | position == 'word final')
warlpiri_final_mean <- setNames(aggregate(warlpiri_final$syllable_duration, list(warlpiri_final$number), FUN=mean), c('number', 'mean'))

yurakare_final <- subset(yurakare, position == 'utterance final' | position == 'word final')
yurakare_final_mean <- setNames(aggregate(yurakare_final$syllable_duration, list(yurakare_final$number), FUN=mean), c('number', 'mean'))

#creating line plots

arapaho_final_line <- ggplot(arapaho_final_mean,aes(x=number,y=mean))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_final_line <- ggplot(bainouk_final_mean,aes(x=number,y=mean))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12,14), limits = c(0,14))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_final_line <- ggplot(beja_final_mean,aes(x=number,y=mean))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12,14), limits = c(0,14))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_final_line <- ggplot(bora_final_mean,aes(x=number,y=mean))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_final_line <- ggplot(evenki_final_mean,aes(x=number,y=mean))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_final_line <- ggplot(mojeno_final_mean,aes(x=number,y=mean))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10), limits = c(0,10))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_final_line <- ggplot(ruuli_final_mean,aes(x=number,y=mean))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_final_line <- ggplot(urum_final_mean,aes(x=number,y=mean))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,10))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_final_line <- ggplot(warlpiri_final_mean,aes(x=number,y=mean))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_final_line <- ggplot(yurakare_final_mean,aes(x=number,y=mean))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

comparison <- ggarrange(arapaho_final_line, bainouk_final_line, beja_final_line, bora_final_line, evenki_final_line, mojeno_final_line, ruuli_final_line, urum_final_line, warlpiri_final_line, yurakare_final_line + rremove("x.text"),
                        ncol = 5, nrow = 2)

annotate_figure(comparison, top = text_grob("mean final syllable duration in 10 non-european languages", 
                                            face = "bold", size = 14))
