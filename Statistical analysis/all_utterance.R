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

#count how many data points per group
arapaho_count <- count(arapaho, "number_in_utterance")
bainouk_count <- count(bainouk, "number_in_utterance")
beja_count <- count(beja, "number_in_utterance")
bora_count <- count(beja, "number_in_utterance")
evenki_count <- count(evenki, "number_in_utterance")
mojeno_count <- count(mojeno, "number_in_utterance")
ruuli_count <- count(ruuli, "number_in_utterance")
urum_count <- count(urum, "number_in_utterance")
warlpiri_count <- count(warlpiri, "number_in_utterance")
yurakare_count <- count(yurakare, "number_in_utterance")

#cutting data pints for lack
arapaho <- arapaho[arapaho$number_in_utterance >= 4 & arapaho$number_in_utterance <= 35, ]
bainouk <- bainouk[bainouk$number_in_utterance <= 33 & bainouk$number_in_utterance >= 4, ]
beja <- beja[beja$number_in_utterance <= 58 & beja$number_in_utterance >= 5, ]
bora <- bora[bora$number_in_utterance <= 58 &  bora$number_in_utterance >= 5, ]
evenki <- evenki[evenki$number_in_utterance <= 34 & evenki$number_in_utterance >= 4, ]
mojeno <- mojeno[mojeno$number_in_utterance <= 58  & mojeno$number_in_utterance >= 6, ]
ruuli <- ruuli[ruuli$number_in_utterance <= 44  & ruuli$number_in_utterance >= 6, ]
urum <- urum[urum$number_in_utterance <= 40 & ruuli$number_in_utterance >= 4, ]
warlpiri <- warlpiri[warlpiri$number_in_utterance <= 35 & warlpiri$number_in_utterance >= 4, ]
yurakare <- yurakare[yurakare$number_in_utterance <= 42 & warlpiri$number_in_utterance >= 3, ]


# creating duration medians

arapaho_median <- setNames(aggregate(arapaho$syllable_duration, list(arapaho$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
bainouk_median <- setNames(aggregate(bainouk$syllable_duration, list(bainouk$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
beja_median <- setNames(aggregate(beja$syllable_duration, list(beja$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
bora_median <- setNames(aggregate(bora$syllable_duration, list(bora$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
evenki_median <- setNames(aggregate(evenki$syllable_duration, list(evenki$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
mojeno_median <- setNames(aggregate(mojeno$syllable_duration, list(mojeno$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
ruuli_median <- setNames(aggregate(ruuli$syllable_duration, list(ruuli$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
urum_median <- setNames(aggregate(urum$syllable_duration, list(urum$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
warlpiri_median <- setNames(aggregate(warlpiri$syllable_duration, list(warlpiri$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
yurakare_median <- setNames(aggregate(yurakare$syllable_duration, list(yurakare$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))

#creating line plots

arapaho_line <- ggplot(arapaho_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_line <- ggplot(bainouk_median,aes(x=number_in_utterance,y=median))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_line <- ggplot(beja_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_line <- ggplot(bora_median,aes(x=number_in_utterance,y=median))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_line <- ggplot(evenki_median,aes(x=number_in_utterance,y=median))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_line <- ggplot(mojeno_median,aes(x=number_in_utterance,y=median))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_line <- ggplot(ruuli_median,aes(x=number_in_utterance,y=median))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_line <- ggplot(urum_median,aes(x=number_in_utterance,y=median))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_line <- ggplot(warlpiri_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_line <- ggplot(yurakare_median,aes(x=number_in_utterance,y=median))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable utterance')+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

comparison <- ggarrange(arapaho_line, bainouk_line, beja_line, bora_line, evenki_line, mojeno_line, ruuli_line, urum_line, warlpiri_line, yurakare_line + rremove("x.text"),
                        ncol = 5, nrow = 2)
annotate_figure(comparison, top = text_grob("median syllable duration in utterance", 
                                            face = "bold", size = 14))

#statistics
rqfit_arapaho <- rq(syllable_duration ~ number_in_utterance, data = arapaho)
summary(rqfit_arapaho)
rqfit_bainouk <- rq(syllable_duration ~ number_in_utterance, data = bainouk)
summary(rqfit_bainouk)
rqfit_beja <- rq(syllable_duration ~ number_in_utterance, data = beja)
summary(rqfit_beja)
rqfit_bora <- rq(syllable_duration ~ number_in_utterance, data = bora)
summary(rqfit_bora)
rqfit_evenki <- rq(syllable_duration ~ number_in_utterance, data = evenki)
summary(rqfit_evenki)
rqfit_mojeno <- rq(syllable_duration ~ number_in_utterance, data = mojeno)
summary(rqfit_mojeno)
rqfit_ruuli <- rq(syllable_duration ~ number_in_utterance, data = ruuli)
summary(rqfit_ruuli)
rqfit_urum <- rq(syllable_duration ~ number_in_utterance, data = urum)
summary(rqfit_urum)
rqfit_warlpiri <- rq(syllable_duration ~ number_in_utterance, data = warlpiri)
summary(rqfit_warlpiri)
rqfit_yurakare <- rq(syllable_duration ~ number_in_utterance, data = yurakare)
summary(rqfit_yurakare)


# cor.test(arapaho$syllable_duration, arapaho$number_in_utterance, method = "spearman")
# cor.test(bainouk$syllable_duration, bainouk$number_in_utterance, method = "spearman")
# cor.test(beja$syllable_duration, beja$number_in_utterance, method = "spearman")
# cor.test(bora$syllable_duration, bora$number_in_utterance, method = "spearman")
# cor.test(evenki$syllable_duration, evenki$number_in_utterance, method = "spearman")
# cor.test(mojeno$syllable_duration, mojeno$number_in_utterance, method = "spearman")
# cor.test(ruuli$syllable_duration, ruuli$number_in_utterance, method = "spearman")
# cor.test(urum$syllable_duration, urum$number_in_utterance, method = "spearman")
# cor.test(warlpiri$syllable_duration, warlpiri$number_in_utterance, method = "spearman")
# cor.test(yurakare$syllable_duration, yurakare$number_in_utterance, method = "spearman")
