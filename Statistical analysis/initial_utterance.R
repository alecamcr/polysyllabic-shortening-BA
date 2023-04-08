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

# Subsetting only initial syllables (Utterance initial)
arapaho_initial <- subset(arapaho, position == 'utterance initial')
bainouk_initial <- subset(bainouk, position == 'utterance initial')
beja_initial <- subset(beja, position == 'utterance initial')
bora_initial <- subset(bora, position == 'utterance initial')
evenki_initial <- subset(evenki, position == 'utterance initial')
mojeno_initial <- subset(mojeno, position == 'utterance initial')
ruuli_initial <- subset(ruuli, position == 'utterance initial')
urum_initial <- subset(urum, position == 'utterance initial')
warlpiri_initial <- subset(warlpiri, position == 'utterance initial')
yurakare_initial <- subset(yurakare, position == 'utterance initial')

#cutting data points
arapaho_initial_count <- count(arapaho_initial, "number_in_utterance")
bainouk_initial_count <- count(bainouk_initial, "number_in_utterance")
beja_initial_count <- count(beja_initial, "number_in_utterance")
bora_initial_count <- count(beja_initial, "number_in_utterance")
evenki_initial_count <- count(evenki_initial, "number_in_utterance")
mojeno_initial_count <- count(mojeno_initial, "number_in_utterance")
ruuli_initial_count <- count(ruuli_initial, "number_in_utterance")
urum_initial_count <- count(urum_initial, "number_in_utterance")
warlpiri_initial_count <- count(warlpiri_initial, "number_in_utterance")
yurakare_initial_count <- count(yurakare_initial, "number_in_utterance")

#cutting data pints for lack
arapaho_initial <- arapaho_initial[arapaho_initial$number_in_utterance >= 4 & arapaho_initial$number_in_utterance <= 20, ]
bainouk_initial <- bainouk_initial[bainouk_initial$number_in_utterance <= 21 & bainouk_initial$number_in_utterance >= 4, ]
beja_initial <- beja_initial[beja_initial$number_in_utterance <= 21 & beja_initial$number_in_utterance >= 4, ]
bora_initial <- bora_initial[bora_initial$number_in_utterance <= 21 &  bora_initial$number_in_utterance >= 5, ]
evenki_initial <- evenki_initial[evenki_initial$number_in_utterance <= 22 & evenki_initial$number_in_utterance >= 4, ]
mojeno_initial <- mojeno_initial[mojeno_initial$number_in_utterance <= 22  & mojeno_initial$number_in_utterance >= 5, ]
ruuli_initial <- ruuli_initial[ruuli_initial$number_in_utterance <= 20  & ruuli_initial$number_in_utterance >= 5, ]
urum_initial <- urum_initial[urum_initial$number_in_utterance <= 19 & urum_initial$number_in_utterance >= 4, ]
warlpiri_initial <- warlpiri_initial[warlpiri_initial$number_in_utterance <= 24 & warlpiri_initial$number_in_utterance >= 4, ]
yurakare_initial <- yurakare_initial[yurakare_initial$number_in_utterance <= 26 & yurakare_initial$number_in_utterance >= 3, ]

#creating median
arapaho_initial_median <- setNames(aggregate(arapaho_initial$syllable_duration, list(arapaho_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
bainouk_initial_median <- setNames(aggregate(bainouk_initial$syllable_duration, list(bainouk_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
beja_initial_median <- setNames(aggregate(beja_initial$syllable_duration, list(beja_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
bora_initial_median <- setNames(aggregate(bora_initial$syllable_duration, list(bora_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
evenki_initial_median <- setNames(aggregate(evenki_initial$syllable_duration, list(evenki_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
mojeno_initial_median <- setNames(aggregate(mojeno_initial$syllable_duration, list(mojeno_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
ruuli_initial_median <- setNames(aggregate(ruuli_initial$syllable_duration, list(ruuli_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
urum_initial_median <- setNames(aggregate(urum_initial$syllable_duration, list(urum_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
warlpiri_initial_median <- setNames(aggregate(warlpiri_initial$syllable_duration, list(warlpiri_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
yurakare_initial_median <- setNames(aggregate(yurakare_initial$syllable_duration, list(yurakare_initial$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))

#creating line plots

arapaho_initial_line <- ggplot(arapaho_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_initial_line <- ggplot(bainouk_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_initial_line <- ggplot(beja_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_initial_line <- ggplot(bora_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_initial_line <- ggplot(evenki_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_initial_line <- ggplot(mojeno_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_initial_line <- ggplot(ruuli_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_initial_line <- ggplot(urum_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_initial_line <- ggplot(warlpiri_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_initial_line <- ggplot(yurakare_initial_median,aes(x=number_in_utterance,y=median))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

comparison <- ggarrange(arapaho_initial_line, bainouk_initial_line, beja_initial_line, bora_initial_line, evenki_initial_line, mojeno_initial_line, ruuli_initial_line, urum_initial_line, warlpiri_initial_line, yurakare_initial_line + rremove("x.text"),
                        ncol = 5, nrow = 2)
annotate_figure(comparison, top = text_grob("First syllable duration in utterance", 
                                            face = "bold", size = 14))

#statistics
rqfit_arapaho_initial <- rq(syllable_duration ~ number_in_utterance, data = arapaho_initial)
summary(rqfit_arapaho_initial)
rqfit_bainouk_initial <- rq(syllable_duration ~ number_in_utterance, data = bainouk_initial)
summary(rqfit_bainouk_initial)
rqfit_beja_initial <- rq(syllable_duration ~ number_in_utterance, data = beja_initial)
summary(rqfit_beja_initial)
rqfit_bora_initial <- rq(syllable_duration ~ number_in_utterance, data = bora_initial)
summary(rqfit_bora_initial)
rqfit_evenki_initial <- rq(syllable_duration ~ number_in_utterance, data = evenki_initial)
summary(rqfit_evenki_initial)
rqfit_mojeno_initial <- rq(syllable_duration ~ number_in_utterance, data = mojeno_initial)
summary(rqfit_mojeno_initial)
rqfit_ruuli_initial <- rq(syllable_duration ~ number_in_utterance, data = ruuli_initial)
summary(rqfit_ruuli_initial)
rqfit_urum_initial <- rq(syllable_duration ~ number_in_utterance, data = urum_initial)
summary(rqfit_urum_initial)
rqfit_warlpiri_initial <- rq(syllable_duration ~ number_in_utterance, data = warlpiri_initial)
summary(rqfit_warlpiri_initial)
rqfit_yurakare_initial <- rq(syllable_duration ~ number_in_utterance, data = yurakare_initial)
summary(rqfit_yurakare_initial)


# cor.test(arapaho_initial$syllable_duration, arapaho_initial$number_in_utterance, method = "spearman")
# cor.test(bainouk_initial$syllable_duration, bainouk_initial$number_in_utterance, method = "spearman")
# cor.test(beja_initial$syllable_duration, beja_initial$number_in_utterance, method = "spearman")
# cor.test(bora_initial$syllable_duration, bora_initial$number_in_utterance, method = "spearman")
# cor.test(evenki_initial$syllable_duration, evenki_initial$number_in_utterance, method = "spearman")
# cor.test(mojeno_initial$syllable_duration, mojeno_initial$number_in_utterance, method = "spearman")
# cor.test(ruuli_initial$syllable_duration, ruuli_initial$number_in_utterance, method = "spearman")
# cor.test(urum_initial$syllable_duration, urum_initial$number_in_utterance, method = "spearman")
# cor.test(warlpiri_initial$syllable_duration, warlpiri_initial$number_in_utterance, method = "spearman")
# cor.test(yurakare_initial$syllable_duration, yurakare_initial$number_in_utterance, method = "spearman")

