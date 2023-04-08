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

# Subsetting only final syllables (Utterance and word final)

arapaho_final <- subset(arapaho, position == 'utterance final')
bainouk_final <- subset(bainouk, position == 'utterance final')
beja_final <- subset(beja, position == 'utterance final')
bora_final <- subset(bora, position == 'utterance final')
evenki_final <- subset(evenki, position == 'utterance final')
mojeno_final <- subset(mojeno, position == 'utterance final')
ruuli_final <- subset(ruuli, position == 'utterance final')
urum_final <- subset(urum, position == 'utterance final')
warlpiri_final <- subset(warlpiri, position == 'utterance final')
yurakare_final <- subset(yurakare, position == 'utterance final')

#cutting data points
arapaho_final_count <- count(arapaho_final, "number_in_utterance")
bainouk_final_count <- count(bainouk_final, "number_in_utterance")
beja_final_count <- count(beja_final, "number_in_utterance")
bora_final_count <- count(beja_final, "number_in_utterance")
evenki_final_count <- count(evenki_final, "number_in_utterance")
mojeno_final_count <- count(mojeno_final, "number_in_utterance")
ruuli_final_count <- count(ruuli_final, "number_in_utterance")
urum_final_count <- count(urum_final, "number_in_utterance")
warlpiri_final_count <- count(warlpiri_final, "number_in_utterance")
yurakare_final_count <- count(yurakare_final, "number_in_utterance")

#cutting data pints for lack
arapaho_final <- arapaho_final[arapaho_final$number_in_utterance >= 4 & arapaho_final$number_in_utterance <= 23, ]
bainouk_final <- bainouk_final[bainouk_final$number_in_utterance <= 27 & bainouk_final$number_in_utterance >= 4, ]
beja_final <- beja_final[beja_final$number_in_utterance <= 32 & beja_final$number_in_utterance >= 4, ]
bora_final <- bora_final[bora_final$number_in_utterance <= 32 &  bora_final$number_in_utterance >= 4, ]
evenki_final <- evenki_final[evenki_final$number_in_utterance <= 26 & evenki_final$number_in_utterance >= 4, ]
mojeno_final <- mojeno_final[mojeno_final$number_in_utterance <= 22  & mojeno_final$number_in_utterance >= 5, ]
ruuli_final <- ruuli_final[ruuli_final$number_in_utterance <= 26  & ruuli_final$number_in_utterance >= 5, ]
urum_final <- urum_final[urum_final$number_in_utterance <= 27 & urum_final$number_in_utterance >= 4, ]
warlpiri_final <- warlpiri_final[warlpiri_final$number_in_utterance <= 24 & warlpiri_final$number_in_utterance >= 4, ]
yurakare_final <- yurakare_final[yurakare_final$number_in_utterance <= 29 & yurakare_final$number_in_utterance >= 3, ]


#median
arapaho_final_median <- setNames(aggregate(arapaho_final$syllable_duration, list(arapaho_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
bainouk_final_median <- setNames(aggregate(bainouk_final$syllable_duration, list(bainouk_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
beja_final_median <- setNames(aggregate(beja_final$syllable_duration, list(beja_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
bora_final_median <- setNames(aggregate(bora_final$syllable_duration, list(bora_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
evenki_final_median <- setNames(aggregate(evenki_final$syllable_duration, list(evenki_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
mojeno_final_median <- setNames(aggregate(mojeno_final$syllable_duration, list(mojeno_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
ruuli_final_median <- setNames(aggregate(ruuli_final$syllable_duration, list(ruuli_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
urum_final_median <- setNames(aggregate(urum_final$syllable_duration, list(urum_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
warlpiri_final_median <- setNames(aggregate(warlpiri_final$syllable_duration, list(warlpiri_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))
yurakare_final_median <- setNames(aggregate(yurakare_final$syllable_duration, list(yurakare_final$number_in_utterance), FUN=median), c('number_in_utterance', 'median'))

#creating line plots

arapaho_final_line <- ggplot(arapaho_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_final_line <- ggplot(bainouk_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_final_line <- ggplot(beja_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_final_line <- ggplot(bora_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_final_line <- ggplot(evenki_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_final_line <- ggplot(mojeno_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_final_line <- ggplot(ruuli_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_final_line <- ggplot(urum_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_final_line <- ggplot(warlpiri_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_final_line <- ggplot(yurakare_final_median,aes(x=number_in_utterance,y=median))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable')+
  scale_y_continuous('duration (msec)', breaks=c(150, 200, 250,300,350,400), limits = c(150,400))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

comparison <- ggarrange(arapaho_final_line, bainouk_final_line, beja_final_line, bora_final_line, evenki_final_line, mojeno_final_line, ruuli_final_line, urum_final_line, warlpiri_final_line, yurakare_final_line + rremove("x.text"),
                        ncol = 5, nrow = 2)
annotate_figure(comparison, top = text_grob("Final syllable duration in utterance", 
                                            face = "bold", size = 14))

#statistics
rqfit_arapaho_final <- rq(syllable_duration ~ number_in_utterance, data = arapaho_final)
summary(rqfit_arapaho_final)
rqfit_bainouk_final <- rq(syllable_duration ~ number_in_utterance, data = bainouk_final)
summary(rqfit_bainouk_final)
rqfit_beja_final <- rq(syllable_duration ~ number_in_utterance, data = beja_final)
summary(rqfit_beja_final)
rqfit_bora_final <- rq(syllable_duration ~ number_in_utterance, data = bora_final)
summary(rqfit_bora_final)
rqfit_evenki_final <- rq(syllable_duration ~ number_in_utterance, data = evenki_final)
summary(rqfit_evenki_final)
rqfit_mojeno_final <- rq(syllable_duration ~ number_in_utterance, data = mojeno_final)
summary(rqfit_mojeno_final)
rqfit_ruuli_final <- rq(syllable_duration ~ number_in_utterance, data = ruuli_final)
summary(rqfit_ruuli_final)
rqfit_urum_final <- rq(syllable_duration ~ number_in_utterance, data = urum_final)
summary(rqfit_urum_final)
rqfit_warlpiri_final <- rq(syllable_duration ~ number_in_utterance, data = warlpiri_final)
summary(rqfit_warlpiri_final)
rqfit_yurakare_final <- rq(syllable_duration ~ number_in_utterance, data = yurakare_final)
summary(rqfit_yurakare_final)


# cor.test(arapaho_final$syllable_duration, arapaho_final$number_in_utterance, method = "spearman")
# cor.test(bainouk_final$syllable_duration, bainouk_final$number_in_utterance, method = "spearman")
# cor.test(beja_final$syllable_duration, beja_final$number_in_utterance, method = "spearman")
# cor.test(bora_final$syllable_duration, bora_final$number_in_utterance, method = "spearman")
# cor.test(evenki_final$syllable_duration, evenki_final$number_in_utterance, method = "spearman")
# cor.test(mojeno_final$syllable_duration, mojeno_final$number_in_utterance, method = "spearman")
# cor.test(ruuli_final$syllable_duration, ruuli_final$number_in_utterance, method = "spearman")
# cor.test(urum_final$syllable_duration, urum_final$number_in_utterance, method = "spearman")
# cor.test(warlpiri_final$syllable_duration, warlpiri_final$number_in_utterance, method = "spearman")
# cor.test(yurakare_final$syllable_duration, yurakare_final$number_in_utterance, method = "spearman")