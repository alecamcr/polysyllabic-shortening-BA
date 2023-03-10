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

arapaho_medial <- subset(arapaho, position == 'medial')
arapaho_medial_mean <- setNames(aggregate(arapaho_medial$syllable_duration, list(arapaho_medial$number), FUN=mean), c('number', 'mean'))

bainouk_medial <- subset(bainouk, position == 'medial')
bainouk_medial_mean <- setNames(aggregate(bainouk_medial$syllable_duration, list(bainouk_medial$number), FUN=mean), c('number', 'mean'))

beja_medial <- subset(beja, position == 'medial')
beja_medial_mean <- setNames(aggregate(beja_medial$syllable_duration, list(beja_medial$number), FUN=mean), c('number', 'mean'))

bora_medial <- subset(bora, position == 'medial')
bora_medial_mean <- setNames(aggregate(bora_medial$syllable_duration, list(bora_medial$number), FUN=mean), c('number', 'mean'))

evenki_medial <- subset(evenki, position == 'medial')
evenki_medial_mean <- setNames(aggregate(evenki_medial$syllable_duration, list(evenki_medial$number), FUN=mean), c('number', 'mean'))

mojeno_medial <- subset(mojeno, position == 'medial')
mojeno_medial_mean <- setNames(aggregate(mojeno_medial$syllable_duration, list(mojeno_medial$number), FUN=mean), c('number', 'mean'))

ruuli_medial <- subset(ruuli, position == 'medial')
ruuli_medial_mean <- setNames(aggregate(ruuli_medial$syllable_duration, list(ruuli_medial$number), FUN=mean), c('number', 'mean'))

urum_medial <- subset(urum, position == 'medial')
urum_medial_mean <- setNames(aggregate(urum_medial$syllable_duration, list(urum_medial$number), FUN=mean), c('number', 'mean'))

warlpiri_medial <- subset(warlpiri, position == 'medial')
warlpiri_medial_mean <- setNames(aggregate(warlpiri_medial$syllable_duration, list(warlpiri_medial$number), FUN=mean), c('number', 'mean'))

yurakare_medial <- subset(yurakare, position == 'medial')
yurakare_medial_mean <- setNames(aggregate(yurakare_medial$syllable_duration, list(yurakare_medial$number), FUN=mean), c('number', 'mean'))

#creating line plots

arapaho_medial_line <- ggplot(arapaho_medial_mean,aes(x=number,y=mean))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_medial_line <- ggplot(bainouk_medial_mean,aes(x=number,y=mean))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12,14), limits = c(0,14))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_medial_line <- ggplot(beja_medial_mean,aes(x=number,y=mean))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12,14), limits = c(0,14))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_medial_line <- ggplot(bora_medial_mean,aes(x=number,y=mean))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_medial_line <- ggplot(evenki_medial_mean,aes(x=number,y=mean))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_medial_line <- ggplot(mojeno_medial_mean,aes(x=number,y=mean))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10), limits = c(0,10))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'moje??o trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_medial_line <- ggplot(ruuli_medial_mean,aes(x=number,y=mean))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_medial_line <- ggplot(urum_medial_mean,aes(x=number,y=mean))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,10))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_medial_line <- ggplot(warlpiri_medial_mean,aes(x=number,y=mean))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_medial_line <- ggplot(yurakare_medial_mean,aes(x=number,y=mean))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable', breaks=c(0,2,4,6,8,10,12), limits = c(0,12))+
  scale_y_continuous('duration (msec)', breaks=c(0, 50, 100, 150, 200, 250, 300), limits = c(0,300))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

comparison <- ggarrange(arapaho_medial_line, bainouk_medial_line, beja_medial_line, bora_medial_line, evenki_medial_line, mojeno_medial_line, ruuli_medial_line, urum_medial_line, warlpiri_medial_line, yurakare_medial_line + rremove("x.text"),
                        ncol = 5, nrow = 2)
annotate_figure(comparison, top = text_grob("mean medial syllable duration in 10 non-european languages", 
                                            face = "bold", size = 14))
