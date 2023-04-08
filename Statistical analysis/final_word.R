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

arapaho_final <- subset(arapaho,  position == 'word final')
bainouk_final <- subset(bainouk,  position == 'word final')
beja_final <- subset(beja,  position == 'word final')
bora_final <- subset(bora,  position == 'word final')
evenki_final <- subset(evenki,  position == 'word final')
mojeno_final <- subset(mojeno,  position == 'word final')
ruuli_final <- subset(ruuli,  position == 'word final')
urum_final <- subset(urum,  position == 'word final')
warlpiri_final <- subset(warlpiri,  position == 'word final')
yurakare_final <- subset(yurakare,  position == 'word final')

#count how many data points per group
arapaho_final_count <- count(arapaho_final, "number")
bainouk_final_count <- count(bainouk_final, "number")
beja_final_count <- count(beja_final, "number")
bora_final_count <- count(bora_final, "number")
evenki_final_count <- count(evenki_final, "number")
mojeno_final_count <- count(mojeno_final, "number")
ruuli_final_count <- count(ruuli_final, "number")
urum_final_count <- count(urum_final, "number")
warlpiri_final_count <- count(warlpiri_final, "number")
yurakare_final_count <- count(yurakare_final, "number")

#cutting data pints
arapaho_final <- arapaho_final[arapaho_final$number <= 8, ]
# arapaho_final <- arapaho_final[arapaho_final$number > 2, ]
bainouk_final <- bainouk_final[bainouk_final$number <= 5, ]
beja_final <- beja_final[beja_final$number <= 5, ]
bora_final <- bora_final[bora_final$number <= 7, ]
evenki_final <- evenki_final[evenki_final$number <= 6, ]
mojeno_final <- mojeno_final[mojeno_final$number <= 7, ]
ruuli_final <- ruuli_final[ruuli_final$number <= 6, ]
urum_final <- urum_final[urum_final$number <= 6, ]
warlpiri_final <- warlpiri_final[warlpiri_final$number <= 6, ]
yurakare_final <- yurakare_final[yurakare_final$number <= 6, ]

arapaho_final_median <- setNames(aggregate(arapaho_final$syllable_duration, list(arapaho_final$number), FUN=median), c('number', 'median'))
bainouk_final_median <- setNames(aggregate(bainouk_final$syllable_duration, list(bainouk_final$number), FUN=median), c('number', 'median'))
beja_final_median <- setNames(aggregate(beja_final$syllable_duration, list(beja_final$number), FUN=median), c('number', 'median'))
bora_final_median <- setNames(aggregate(bora_final$syllable_duration, list(bora_final$number), FUN=median), c('number', 'median'))
evenki_final_median <- setNames(aggregate(evenki_final$syllable_duration, list(evenki_final$number), FUN=median), c('number', 'median'))
mojeno_final_median <- setNames(aggregate(mojeno_final$syllable_duration, list(mojeno_final$number), FUN=median), c('number', 'median'))
ruuli_final_median <- setNames(aggregate(ruuli_final$syllable_duration, list(ruuli_final$number), FUN=median), c('number', 'median'))
urum_final_median <- setNames(aggregate(urum_final$syllable_duration, list(urum_final$number), FUN=median), c('number', 'median'))
warlpiri_final_median <- setNames(aggregate(warlpiri_final$syllable_duration, list(warlpiri_final$number), FUN=median), c('number', 'median'))
yurakare_final_median <- setNames(aggregate(yurakare_final$syllable_duration, list(yurakare_final$number), FUN=median), c('number', 'median'))

#creating line plots

arapaho_final_line <- ggplot(arapaho_final_median,aes(x=number,y=median))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8), limits = c(2,8))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_final_line <- ggplot(bainouk_final_median,aes(x=number,y=median))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5), limits = c(2,5))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_final_line <- ggplot(beja_final_median,aes(x=number,y=median))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5), limits = c(2,5))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_final_line <- ggplot(bora_final_median,aes(x=number,y=median))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7), limits = c(2,7))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_final_line <- ggplot(evenki_final_median,aes(x=number,y=median))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6), limits = c(2,6))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_final_line <- ggplot(mojeno_final_median,aes(x=number,y=median))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7), limits = c(2,7))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_final_line <- ggplot(ruuli_final_median,aes(x=number,y=median))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6), limits = c(2,6))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_final_line <- ggplot(urum_final_median,aes(x=number,y=median))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6), limits = c(2,6))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_final_line <- ggplot(warlpiri_final_median,aes(x=number,y=median))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable', breaks=c(1,2,3,4,5,6), limits = c(0,6))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_final_line <- ggplot(yurakare_final_median,aes(x=number,y=median))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable', breaks=c(1,2,3,4,5,6), limits = c(0,6))+
  scale_y_continuous('duration (msec)', breaks=c(50, 100, 150, 200, 250), limits = c(50,250))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

lastsyllable <- ggarrange(arapaho_final_line, bainouk_final_line, beja_final_line, bora_final_line, evenki_final_line, mojeno_final_line, ruuli_final_line, urum_final_line, warlpiri_final_line, yurakare_final_line + rremove("x.text"),
                        ncol = 5, nrow = 2)

annotate_figure(lastsyllable, top = text_grob("final syllable duration word", 
                                            face = "bold", size = 14))

#statistics

rqfit_arapaho_final <- rq(syllable_duration ~ number, data = arapaho_final)
summary(rqfit_arapaho_final)
rqfit_bainouk_final <- rq(syllable_duration ~ number, data = bainouk_final)
summary(rqfit_bainouk_final)
rqfit_beja_final <- rq(syllable_duration ~ number, data = beja_final)
summary(rqfit_beja_final)
rqfit_bora_final <- rq(syllable_duration ~ number, data = bora_final)
summary(rqfit_bora_final)
rqfit_evenki_final <- rq(syllable_duration ~ number, data = evenki_final)
summary(rqfit_evenki_final)
rqfit_mojeno_final <- rq(syllable_duration ~ number, data = mojeno_final)
summary(rqfit_mojeno_final)
rqfit_ruuli_final <- rq(syllable_duration ~ number, data = ruuli_final)
summary(rqfit_ruuli_final)
rqfit_urum_final <- rq(syllable_duration ~ number, data = urum_final)
summary(rqfit_urum_final)
rqfit_warlpiri_final <- rq(syllable_duration ~ number, data = warlpiri_final)
summary(rqfit_warlpiri_final)
rqfit_yurakare_final <- rq(syllable_duration ~ number, data = yurakare_final)
summary(rqfit_yurakare_final)


# cor.test(arapaho_final$syllable_duration, arapaho_final$number, method = "spearman")
# cor.test(bainouk_final$syllable_duration, bainouk_final$number, method = "spearman")
# cor.test(beja_final$syllable_duration, beja_final$number, method = "spearman")
# cor.test(bora_final$syllable_duration, bora_final$number, method = "spearman")
# cor.test(evenki_final$syllable_duration, evenki_final$number, method = "spearman")
# cor.test(mojeno_final$syllable_duration, mojeno_final$number, method = "spearman")
# cor.test(ruuli_final$syllable_duration, ruuli_final$number, method = "spearman")
# cor.test(urum_final$syllable_duration, urum_final$number, method = "spearman")
# cor.test(warlpiri_final$syllable_duration, warlpiri_final$number, method = "spearman")
# cor.test(yurakare_final$syllable_duration, yurakare_final$number, method = "spearman")