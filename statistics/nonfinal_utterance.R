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

# Subsetting only nonfinal syllables (Utterance nonfinal)
arapaho_nonfinal <- subset(arapaho, position != 'utterance final' & position != 'word final')
bainouk_nonfinal <- subset(bainouk, position != 'utterance final' & position != 'word final')
beja_nonfinal <- subset(beja, position != 'utterance final' & position != 'word final')
bora_nonfinal <- subset(bora, position != 'utterance final' & position != 'word final')
evenki_nonfinal <- subset(evenki, position != 'utterance final' & position != 'word final')
mojeno_nonfinal <- subset(mojeno, position != 'utterance final' & position != 'word final')
ruuli_nonfinal <- subset(ruuli, position != 'utterance final' & position != 'word final')
urum_nonfinal <- subset(urum, position != 'utterance final' & position != 'word final')
warlpiri_nonfinal <- subset(warlpiri, position != 'utterance final' & position != 'word final')
yurakare_nonfinal <- subset(yurakare, position != 'utterance final' & position != 'word final')

#cutting data points
arapaho_nonfinal_count <- count(arapaho_nonfinal, "number_in_utterance")
bainouk_nonfinal_count <- count(bainouk_nonfinal, "number_in_utterance")
beja_nonfinal_count <- count(beja_nonfinal, "number_in_utterance")
bora_nonfinal_count <- count(beja_nonfinal, "number_in_utterance")
evenki_nonfinal_count <- count(evenki_nonfinal, "number_in_utterance")
mojeno_nonfinal_count <- count(mojeno_nonfinal, "number_in_utterance")
ruuli_nonfinal_count <- count(ruuli_nonfinal, "number_in_utterance")
urum_nonfinal_count <- count(urum_nonfinal, "number_in_utterance")
warlpiri_nonfinal_count <- count(warlpiri_nonfinal, "number_in_utterance")
yurakare_nonfinal_count <- count(yurakare_nonfinal, "number_in_utterance")

#cutting data pints for lack
arapaho_nonfinal <- arapaho_nonfinal[arapaho_nonfinal$number_in_utterance >= 4 & arapaho_nonfinal$number_in_utterance <= 35, ]
bainouk_nonfinal <- bainouk_nonfinal[bainouk_nonfinal$number_in_utterance <= 30 & bainouk_nonfinal$number_in_utterance >= 4, ]
beja_nonfinal <- beja_nonfinal[beja_nonfinal$number_in_utterance <= 50 & beja_nonfinal$number_in_utterance >= 5, ]
bora_nonfinal <- bora_nonfinal[bora_nonfinal$number_in_utterance <= 46 &  bora_nonfinal$number_in_utterance >= 5, ]
evenki_nonfinal <- evenki_nonfinal[evenki_nonfinal$number_in_utterance <= 32 & evenki_nonfinal$number_in_utterance >= 5, ]
mojeno_nonfinal <- mojeno_nonfinal[mojeno_nonfinal$number_in_utterance <= 37  & mojeno_nonfinal$number_in_utterance >= 7, ]
ruuli_nonfinal <- ruuli_nonfinal[ruuli_nonfinal$number_in_utterance <= 32  & ruuli_nonfinal$number_in_utterance >= 6, ]
urum_nonfinal <- urum_nonfinal[urum_nonfinal$number_in_utterance <= 32 & urum_nonfinal$number_in_utterance >= 5, ]
warlpiri_nonfinal <- warlpiri_nonfinal[warlpiri_nonfinal$number_in_utterance <= 32 & warlpiri_nonfinal$number_in_utterance >= 4, ]
yurakare_nonfinal <- yurakare_nonfinal[yurakare_nonfinal$number_in_utterance <= 32 & yurakare_nonfinal$number_in_utterance >= 4, ]

#creating median
arapaho_nonfinal_median <- setNames(aggregate(arapaho_nonfinal$syllable_duration, list(arapaho_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
bainouk_nonfinal_median <- setNames(aggregate(bainouk_nonfinal$syllable_duration, list(bainouk_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
beja_nonfinal_median <- setNames(aggregate(beja_nonfinal$syllable_duration, list(beja_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
bora_nonfinal_median <- setNames(aggregate(bora_nonfinal$syllable_duration, list(bora_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
evenki_nonfinal_median <- setNames(aggregate(evenki_nonfinal$syllable_duration, list(evenki_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
mojeno_nonfinal_median <- setNames(aggregate(mojeno_nonfinal$syllable_duration, list(mojeno_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
ruuli_nonfinal_median <- setNames(aggregate(ruuli_nonfinal$syllable_duration, list(ruuli_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
urum_nonfinal_median <- setNames(aggregate(urum_nonfinal$syllable_duration, list(urum_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
warlpiri_nonfinal_median <- setNames(aggregate(warlpiri_nonfinal$syllable_duration, list(warlpiri_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
yurakare_nonfinal_median <- setNames(aggregate(yurakare_nonfinal$syllable_duration, list(yurakare_nonfinal$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))

#creating line plots

arapaho_nonfinal_line <- ggplot(arapaho_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_nonfinal_line <- ggplot(bainouk_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_nonfinal_line <- ggplot(beja_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_nonfinal_line <- ggplot(bora_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_nonfinal_line <- ggplot(evenki_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_nonfinal_line <- ggplot(mojeno_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_nonfinal_line <- ggplot(ruuli_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_nonfinal_line <- ggplot(urum_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_nonfinal_line <- ggplot(warlpiri_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_nonfinal_line <- ggplot(yurakare_nonfinal_median,aes(x=number_in_utterance,y=median))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(50,100, 150, 200, 250), limits = c(50,250))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

comparison <- ggarrange(arapaho_nonfinal_line, bainouk_nonfinal_line, beja_nonfinal_line, bora_nonfinal_line, evenki_nonfinal_line, mojeno_nonfinal_line, ruuli_nonfinal_line, urum_nonfinal_line, warlpiri_nonfinal_line, yurakare_nonfinal_line + rremove("x.text"),
                        ncol = 5, nrow = 2)
annotate_figure(comparison, top = text_grob("nonfinal syllable duration in utterance", 
                                            face = "bold", size = 14))

#statistics
rqfit_arapaho_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = arapaho_nonfinal)
summary(rqfit_arapaho_nonfinal)
rqfit_bainouk_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = bainouk_nonfinal)
summary(rqfit_bainouk_nonfinal)
rqfit_beja_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = beja_nonfinal)
summary(rqfit_beja_nonfinal)
rqfit_bora_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = bora_nonfinal)
summary(rqfit_bora_nonfinal)
rqfit_evenki_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = evenki_nonfinal)
summary(rqfit_evenki_nonfinal)
rqfit_mojeno_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = mojeno_nonfinal)
summary(rqfit_mojeno_nonfinal)
rqfit_ruuli_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = ruuli_nonfinal)
summary(rqfit_ruuli_nonfinal)
rqfit_urum_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = urum_nonfinal)
summary(rqfit_urum_nonfinal)
rqfit_warlpiri_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = warlpiri_nonfinal)
summary(rqfit_warlpiri_nonfinal)
rqfit_yurakare_nonfinal <- rq(syllable_duration ~ number_in_utterance, data = yurakare_nonfinal)
summary(rqfit_yurakare_nonfinal)


# cor.test(arapaho_nonfinal$syllable_duration, arapaho_nonfinal$number_in_utterance, method = "spearman")
# cor.test(bainouk_nonfinal$syllable_duration, bainouk_nonfinal$number_in_utterance, method = "spearman")
# cor.test(beja_nonfinal$syllable_duration, beja_nonfinal$number_in_utterance, method = "spearman")
# cor.test(bora_nonfinal$syllable_duration, bora_nonfinal$number_in_utterance, method = "spearman")
# cor.test(evenki_nonfinal$syllable_duration, evenki_nonfinal$number_in_utterance, method = "spearman")
# cor.test(mojeno_nonfinal$syllable_duration, mojeno_nonfinal$number_in_utterance, method = "spearman")
# cor.test(ruuli_nonfinal$syllable_duration, ruuli_nonfinal$number_in_utterance, method = "spearman")
# cor.test(urum_nonfinal$syllable_duration, urum_nonfinal$number_in_utterance, method = "spearman")
# cor.test(warlpiri_nonfinal$syllable_duration, warlpiri_nonfinal$number_in_utterance, method = "spearman")
# cor.test(yurakare_nonfinal$syllable_duration, yurakare_nonfinal$number_in_utterance, method = "spearman")

