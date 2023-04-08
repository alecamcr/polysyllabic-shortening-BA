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
arapaho_count <- count(arapaho, "number")
bainouk_count <- count(bainouk, "number")
beja_count <- count(beja, "number")
bora_count <- count(beja, "number")
evenki_count <- count(evenki, "number")
mojeno_count <- count(mojeno, "number")
ruuli_count <- count(ruuli, "number")
urum_count <- count(urum, "number")
warlpiri_count <- count(warlpiri, "number")
yurakare_count <- count(yurakare, "number")

#cutting data pints for lack

# arapaho <- arapaho[arapaho$number > 2, ]
bainouk <- bainouk[bainouk$number <= 8, ]
beja <- beja[beja$number <= 9, ]
bora <- bora[bora$number <= 10, ]
evenki <- evenki[evenki$number <= 8, ]
mojeno <- mojeno[mojeno$number <= 10, ]
ruuli <- ruuli[ruuli$number <= 8, ]
urum <- urum[urum$number <= 7, ]
warlpiri <- warlpiri[warlpiri$number <= 9, ]
yurakare <- yurakare[yurakare$number <= 8, ]


# creating duration medians

arapaho_median <- setNames(aggregate(arapaho$syllable_duration, list(arapaho$number), FUN=median), c('number', 'median'))
bainouk_median <- setNames(aggregate(bainouk$syllable_duration, list(bainouk$number), FUN=median), c('number', 'median'))
beja_median <- setNames(aggregate(beja$syllable_duration, list(beja$number), FUN=median), c('number', 'median'))
bora_median <- setNames(aggregate(bora$syllable_duration, list(bora$number), FUN=median), c('number', 'median'))
evenki_median <- setNames(aggregate(evenki$syllable_duration, list(evenki$number), FUN=median), c('number', 'median'))
mojeno_median <- setNames(aggregate(mojeno$syllable_duration, list(mojeno$number), FUN=median), c('number', 'median'))
ruuli_median <- setNames(aggregate(ruuli$syllable_duration, list(ruuli$number), FUN=median), c('number', 'median'))
urum_median <- setNames(aggregate(urum$syllable_duration, list(urum$number), FUN=median), c('number', 'median'))
warlpiri_median <- setNames(aggregate(warlpiri$syllable_duration, list(warlpiri$number), FUN=median), c('number', 'median'))
yurakare_median <- setNames(aggregate(yurakare$syllable_duration, list(yurakare$number), FUN=median), c('number', 'median'))

#scatter plot

#creating line plots

arapaho_line <- ggplot(arapaho_median,aes(x=number,y=median))+geom_point(color="darkred")+geom_line(color="darkred")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8,9,10,11,12), limits = c(2,12))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'arapaho')+
  theme(plot.title = element_text(hjust = 0.5))


bainouk_line <- ggplot(bainouk_median,aes(x=number,y=median))+geom_point(color="brown1")+geom_line(color="brown1")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8), limits = c(2,8))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'bainouk')+
  theme(plot.title = element_text(hjust = 0.5))


beja_line <- ggplot(beja_median,aes(x=number,y=median))+geom_point(color="darkorange1")+geom_line(color="darkorange1")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8,9), limits = c(2,9))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'beja')+
  theme(plot.title = element_text(hjust = 0.5))


bora_line <- ggplot(bora_median,aes(x=number,y=median))+geom_point(color="chartreuse2")+geom_line(color="chartreuse2")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8,9,10), limits = c(2,10))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'bora')+
  theme(plot.title = element_text(hjust = 0.5))


evenki_line <- ggplot(evenki_median,aes(x=number,y=median))+geom_point(color="azure3")+geom_line(color="azure3")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8), limits = c(2,8))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'evenki')+
  theme(plot.title = element_text(hjust = 0.5))


mojeno_line <- ggplot(mojeno_median,aes(x=number,y=median))+geom_point(color="deeppink")+geom_line(color="deeppink")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8,9,10), limits = c(2,10))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'mojeño trinitario')+
  theme(plot.title = element_text(hjust = 0.5, size = 8))


ruuli_line <- ggplot(ruuli_median,aes(x=number,y=median))+geom_point(color="darksalmon")+geom_line(color="darksalmon")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8), limits = c(2,8))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'ruuli')+
  theme(plot.title = element_text(hjust = 0.5))


urum_line <- ggplot(urum_median,aes(x=number,y=median))+geom_point(color="deepskyblue")+geom_line(color="deepskyblue")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7), limits = c(2,7))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'urum')+
  theme(plot.title = element_text(hjust = 0.5))


warlpiri_line <- ggplot(warlpiri_median,aes(x=number,y=median))+geom_point(color="darkmagenta")+geom_line(color="darkmagenta")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8,9), limits = c(2,9))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'warlpiri')+
  theme(plot.title = element_text(hjust = 0.5))


yurakare_line <- ggplot(yurakare_median,aes(x=number,y=median))+geom_point(color="aquamarine2")+geom_line(color="aquamarine2")+
  scale_x_continuous('# of syllable', breaks=c(2,3,4,5,6,7,8), limits = c(2,8))+
  scale_y_continuous('duration (msec)', breaks=c(100, 150, 200, 250), limits = c(100,250))+labs(title = 'yurakare')+
  theme(plot.title = element_text(hjust = 0.5))


#customizing line plots

allsyllable <- ggarrange(arapaho_line, bainouk_line, beja_line, bora_line, evenki_line, mojeno_line, ruuli_line, urum_line, warlpiri_line, yurakare_line + rremove("x.text"),
          ncol = 5, nrow = 2)
annotate_figure(allsyllable, top = text_grob("median syllable duration in word", 
                                      face = "bold", size = 14))



#statistics
## summary

# tapply(arapaho$syllable_duration, arapaho$number, summary)
# tapply(bainouk$syllable_duration, bainouk$number, summary)
# tapply(beja$syllable_duration, beja$number, summary)
# tapply(bora$syllable_duration, bora$number, summary)
# tapply(evenki$syllable_duration, evenki$number, summary)
# tapply(mojeno$syllable_duration, mojeno$number, summary)
# tapply(ruuli$syllable_duration, ruuli$number, summary)
# tapply(urum$syllable_duration, urum$number, summary)
# tapply(warlpiri$syllable_duration, warlpiri$number, summary)
# tapply(yurakare$syllable_duration, yurakare$number, summary)

rqfit_arapaho <- rq(syllable_duration ~ number, data = arapaho)
summary(rqfit_arapaho)
rqfit_bainouk <- rq(syllable_duration ~ number, data = bainouk)
summary(rqfit_bainouk)
rqfit_beja <- rq(syllable_duration ~ number, data = beja)
summary(rqfit_beja)
rqfit_bora <- rq(syllable_duration ~ number, data = bora)
summary(rqfit_bora)
rqfit_evenki <- rq(syllable_duration ~ number, data = evenki)
summary(rqfit_evenki)
rqfit_mojeno <- rq(syllable_duration ~ number, data = mojeno)
summary(rqfit_mojeno)
rqfit_ruuli <- rq(syllable_duration ~ number, data = ruuli)
summary(rqfit_ruuli)
rqfit_urum <- rq(syllable_duration ~ number, data = urum)
summary(rqfit_urum)
rqfit_warlpiri <- rq(syllable_duration ~ number, data = warlpiri)
summary(rqfit_warlpiri)
rqfit_yurakare <- rq(syllable_duration ~ number, data = yurakare)
summary(rqfit_yurakare)


# cor.test(arapaho$syllable_duration, arapaho$number, method = "spearman")
# cor.test(bainouk$syllable_duration, bainouk$number, method = "spearman")
# cor.test(beja$syllable_duration, beja$number, method = "spearman")
# cor.test(bora$syllable_duration, bora$number, method = "spearman")
# cor.test(evenki$syllable_duration, evenki$number, method = "spearman")
# cor.test(mojeno$syllable_duration, mojeno$number, method = "spearman")
# cor.test(ruuli$syllable_duration, ruuli$number, method = "spearman")
# cor.test(urum$syllable_duration, urum$number, method = "spearman")
# cor.test(warlpiri$syllable_duration, warlpiri$number, method = "spearman")
# cor.test(yurakare$syllable_duration, yurakare$number, method = "spearman")


# #all data and normal regression
# ggplot(data = arapaho, aes(x = arapaho$number, y = arapaho$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = bainouk, aes(x = bainouk$number, y = bainouk$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = beja, aes(x = beja$number, y = beja$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = bora, aes(x = bora$number, y = bora$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = evenki, aes(x = evenki$number, y = evenki$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = mojeno, aes(x = mojeno$number, y = mojeno$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = ruuli, aes(x = ruuli$number, y = ruuli$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = urum, aes(x = urum$number, y = urum$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = warlpiri, aes(x = warlpiri$number, y = warlpiri$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# ggplot(data = yurakare, aes(x = yurakare$number, y = yurakare$syllable_duration)) +
#   geom_point() +
#   geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
#   theme_bw()
# 
# 
# 
# 
# #-------------------------tests--------------------------------------------
# 
# 
# 
# #Bar plot for skewness visualization
# dur <- bainouk[bainouk$number == 4,]
# hist(dur$syllable_duration, col='steelblue', main='skewed')
# 
# #Comparison lineal and quantile regression
# plot(syllable_duration ~ number, data = arapaho, pch = 16, main = "arapaho")
# abline(lm(syllable_duration ~ number, data = arapaho), col = "red", lty = 2)
# abline(rq(syllable_duration ~ number, data = arapaho), col = "blue", lty = 2)
# legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)
# 
# plot(syllable_duration ~ number, data = yurakare, pch = 16, main = "yurakare")
# abline(lm(syllable_duration ~ number, data = yurakare), col = "red", lty = 2)
# abline(rq(syllable_duration ~ number, data = yurakare), col = "blue", lty = 2)
# legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)
