library('ggplot2')
library('ggpubr')
library('plyr')
library('quantreg')

#put here the path of the folder containing the python customized files
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


#subsetting medial syllables only

arapaho_medial <- subset(arapaho,  position == 'medial')
bainouk_medial <- subset(bainouk,  position == 'medial')
beja_medial <- subset(beja,  position == 'medial')
bora_medial <- subset(bora,  position == 'medial')
evenki_medial <- subset(evenki,  position == 'medial')
mojeno_medial <- subset(mojeno,  position == 'medial')
ruuli_medial <- subset(ruuli,  position == 'medial')
urum_medial <- subset(urum,  position == 'medial')
warlpiri_medial <- subset(warlpiri,  position == 'medial')
yurakare_medial <- subset(yurakare,  position == 'medial')


#count how many data points per group
arapaho_medial_count <- count(arapaho_medial, "number")
bainouk_medial_count <- count(bainouk_medial, "number")
beja_medial_count <- count(beja_medial, "number")
bora_medial_count <- count(bora_medial, "number")
evenki_medial_count <- count(evenki_medial, "number")
mojeno_medial_count <- count(mojeno_medial, "number")
ruuli_medial_count <- count(ruuli_medial, "number")
urum_medial_count <- count(urum_medial, "number")
warlpiri_medial_count <- count(warlpiri_medial, "number")
yurakare_medial_count <- count(yurakare_medial, "number")

#cutting data pints
arapaho_medial <- arapaho_medial[ arapaho_medial$number <= 9, ]
# arapaho_medial <- arapaho_medial[arapaho_medial$number > 2, ]
bainouk_medial <- bainouk_medial[bainouk_medial$number <= 6, ]
beja_medial <- beja_medial[beja_medial$number <= 5, ]
bora_medial <- bora_medial[bora_medial$number <= 8, ]
evenki_medial <- evenki_medial[evenki_medial$number <= 6, ]
mojeno_medial <- mojeno_medial[mojeno_medial$number <= 8, ]
ruuli_medial <- ruuli_medial[ruuli_medial$number <= 7, ]
urum_medial <- urum_medial[urum_medial$number <= 7, ]
warlpiri_medial <- warlpiri_medial[warlpiri_medial$number <= 7, ]
yurakare_medial <- yurakare_medial[yurakare_medial$number <= 6, ]

# Subsetting only medial syllables (Utterance and medial)


arapaho_medial_median <- setNames(aggregate(arapaho_medial$syllable_duration, list(arapaho_medial$number), FUN=median), c('number', 'median'))
bainouk_medial_median <- setNames(aggregate(bainouk_medial$syllable_duration, list(bainouk_medial$number), FUN=median), c('number', 'median'))
beja_medial_median <- setNames(aggregate(beja_medial$syllable_duration, list(beja_medial$number), FUN=median), c('number', 'median'))
bora_medial_median <- setNames(aggregate(bora_medial$syllable_duration, list(bora_medial$number), FUN=median), c('number', 'median'))
evenki_medial_median <- setNames(aggregate(evenki_medial$syllable_duration, list(evenki_medial$number), FUN=median), c('number', 'median'))
mojeno_medial_median <- setNames(aggregate(mojeno_medial$syllable_duration, list(mojeno_medial$number), FUN=median), c('number', 'median'))
ruuli_medial_median <- setNames(aggregate(ruuli_medial$syllable_duration, list(ruuli_medial$number), FUN=median), c('number', 'median'))
urum_medial_median <- setNames(aggregate(urum_medial$syllable_duration, list(urum_medial$number), FUN=median), c('number', 'median'))
warlpiri_medial_median <- setNames(aggregate(warlpiri_medial$syllable_duration, list(warlpiri_medial$number), FUN=median), c('number', 'median'))
yurakare_medial_median <- setNames(aggregate(yurakare_medial$syllable_duration, list(yurakare_medial$number), FUN=median), c('number', 'median'))

#creating line plots

arapaho_medial_line <- ggplot(arapaho_medial_median,aes(x=number,y=median))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_medial_line <- ggplot(bainouk_medial_median,aes(x=number,y=median))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_medial_line <- ggplot(beja_medial_median,aes(x=number,y=median))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_medial_line <- ggplot(bora_medial_median,aes(x=number,y=median))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_medial_line <- ggplot(evenki_medial_median,aes(x=number,y=median))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_medial_line <- ggplot(mojeno_medial_median,aes(x=number,y=median))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_medial_line <- ggplot(ruuli_medial_median,aes(x=number,y=median))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_medial_line <- ggplot(urum_medial_median,aes(x=number,y=median))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_medial_line <- ggplot(warlpiri_medial_median,aes(x=number,y=median))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_medial_line <- ggplot(yurakare_medial_median,aes(x=number,y=median))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))

#customizing line plots

medialsyllable <- ggarrange(arapaho_medial_line, bainouk_medial_line, beja_medial_line, bora_medial_line, evenki_medial_line, mojeno_medial_line, ruuli_medial_line, urum_medial_line, warlpiri_medial_line, yurakare_medial_line + rremove("x.text"),
                        ncol = 5, nrow = 2)
annotate_figure(medialsyllable, top = text_grob("medial syllable in word", 
                                            face = "bold", size = 14))


#statistics

rqfit_arapaho_medial <- rq(syllable_duration ~ number, data = arapaho_medial)
summary(rqfit_arapaho_medial)
rqfit_bainouk_medial <- rq(syllable_duration ~ number, data = bainouk_medial)
summary(rqfit_bainouk_medial)
rqfit_beja_medial <- rq(syllable_duration ~ number, data = beja_medial)
summary(rqfit_beja_medial)
rqfit_bora_medial <- rq(syllable_duration ~ number, data = bora_medial)
summary(rqfit_bora_medial)
rqfit_evenki_medial <- rq(syllable_duration ~ number, data = evenki_medial)
summary(rqfit_evenki_medial)
rqfit_mojeno_medial <- rq(syllable_duration ~ number, data = mojeno_medial)
summary(rqfit_mojeno_medial)
rqfit_ruuli_medial <- rq(syllable_duration ~ number, data = ruuli_medial)
summary(rqfit_ruuli_medial)
rqfit_urum_medial <- rq(syllable_duration ~ number, data = urum_medial)
summary(rqfit_urum_medial)
rqfit_warlpiri_medial <- rq(syllable_duration ~ number, data = warlpiri_medial)
summary(rqfit_warlpiri_medial)
rqfit_yurakare_medial <- rq(syllable_duration ~ number, data = yurakare_medial)
summary(rqfit_yurakare_medial)


# cor.test(arapaho_medial$syllable_duration, arapaho_medial$number, method = "spearman")
# cor.test(bainouk_medial$syllable_duration, bainouk_medial$number, method = "spearman")
# cor.test(beja_medial$syllable_duration, beja_medial$number, method = "spearman")
# cor.test(bora_medial$syllable_duration, bora_medial$number, method = "spearman")
# cor.test(evenki_medial$syllable_duration, evenki_medial$number, method = "spearman")
# cor.test(mojeno_medial$syllable_duration, mojeno_medial$number, method = "spearman")
# cor.test(ruuli_medial$syllable_duration, ruuli_medial$number, method = "spearman")
# cor.test(urum_medial$syllable_duration, urum_medial$number, method = "spearman")
# cor.test(warlpiri_medial$syllable_duration, warlpiri_medial$number, method = "spearman")
# cor.test(yurakare_medial$syllable_duration, yurakare_medial$number, method = "spearman")
