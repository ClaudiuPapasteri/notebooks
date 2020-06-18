
library(psych)
library(readxl)
library(PerformanceAnalytics)
library(car)
library(xlsx)

setwd("E:/Cinetic idei noi/EXPERIMENTE OGL Frontiers (O.2 & O.0.3 & O.0.2)/O.2 REZULTATE/O.2 Date PrelucrareSPSS cu NEO si STAI")
savePath <- "C:/Users/Mihai/Desktop/Cinetic idei noi/O.2 REZULTATE/O.2 Date PrelucrareSPSS cu NEO si STAI/export TermPlots data/"

data <- read_excel("O.2 Date PrelucrareSPSS cu NEO si STAI.xlsx", 1) 

col10 <- names(data)[28:64]

lm.test <- vector("list", length(col10))

data$VasSPozD <- data$VasS_postPoz - data$VasS_prePoz 
data$VasSNegD <- data$VasS_postNeg - data$VasS_preNeg
data$VasBPozD <- data$VasB_postPoz - data$VasB_prePoz
data$VasBNegD <- data$VasB_postNeg - data$VasB_preNeg 
data$IosPozD <-  data$IOS_postPoz - data$IOS_prePoz
data$IosNegD <- data$IOS_postNeg - data$IOS_preNeg
data$Sam1PozD <- data$Sam1_postPoz - data$Sam1_prePoz   

# library(clinsig)
# clinsig(data$VasS_postPoz, data$VasS_prePoz)  # Reliable Change Index

# Noi nu avem putere statistica, dar ar fi trebuit utilizat macar Residualized gain (change) score  (noi utilizam doar gain scores sau Pre-post change scores)
# Adica era util sa putem facem macar follow up score=constant+axbaseline score+bxgroup     (aici aveam bxNevro ca ex)

# absolute change C = B-F and percentage change P = (B-F)/B   (How to Analyze Change from Baseline: Absolute or Percentage Change?)
# a se vedea Examining change using regression analysis: Three approaches compared pt metoda de aici


for(i in seq_along(col10)){
  lm.test[[i]] <- summary(lm(reformulate(col10[i], "VasSPozD"), data = data))
} 
lm.test
# summary(lm(VasSPozD ~ Neo_O5, data = data))  # O5 Idei prezice crestere a stresului
# summary(lm(VasSPozD ~ VasS_prePoz + Neo_O5, data = data)) # nu mai e semnif 

for(i in seq_along(col10)){
  lm.test[[i]] <- summary(lm(reformulate(col10[i], "VasSNegD"), data = data))
} 
lm.test
# summary(lm(VasSNegD ~ Neo_Nev, data = data))  # Nevro prezice crestere a stresului
# summary(lm(VasSNegD ~ VasS_preNeg + Neo_Nev, data = data))  # RAMANE SEMNIF # http://www.clayford.net/statistics/tag/termplot/
rez1 <- lm(VasSNegD ~ VasS_preNeg + Neo_Nev, data = data);  par(mfrow=c(1,2)); termplot(rez1,partial.resid = TRUE, se = TRUE)
pterms1 <- predict(rez1, type="terms"); partial.residuals1 <- as.data.frame( apply(pterms1,2,function(x)x+resid(rez1)) )
DF.rez1 <- data.frame(Neo_Nev=rez1$model[,"Neo_Nev"], Partial_Rez_Neo_Nev=partial.residuals1$Neo_Nev) # date luate din model pt ca NA
plot(DF.rez1[,1], DF.rez1[,2]); abline(lm(DF.rez1[,2] ~ DF.rez1[,1]), col="red")
write.xlsx(DF.rez1, paste(savePath,"DF.rez1.xlsx"))
# summary(lm(VasSNegD ~ Neo_Con, data = data))  # Cons prezice descrestere a stresului
# summary(lm(VasSNegD ~ VasS_preNeg + Neo_Con, data = data))  # RAMANE SEMNIF
rez2 <- lm(VasSNegD ~ VasS_preNeg + Neo_Con, data = data);  par(mfrow=c(1,2)); termplot(rez2,partial.resid = TRUE, se = TRUE)
pterms2 <- predict(rez2, type="terms"); partial.res2 <- as.data.frame( apply(pterms2,2,function(x)x+resid(rez2)) )
DF.rez2 <- data.frame(Neo_Con=rez2$model[,"Neo_Con"], Partial_Rez_Neo_Con=partial.res2$Neo_Con) # date luate din model pt ca NA
plot(DF.rez2[,1], DF.rez2[,2]); abline(lm(DF.rez2[,2] ~ DF.rez2[,1]), col="red")
write.xlsx(DF.rez2, paste(savePath,"DF.rez2.xlsx"))
# summary(lm(VasSNegD ~ Neo_C3, data = data))  # C3 Simtul datoriei prezice descrestere a stresului
# summary(lm(VasSNegD ~ VasS_preNeg + Neo_C3, data = data))  # nu mai e semnif 
# summary(lm(VasSNegD ~ Neo_C1, data = data))  # C1 Competenta prezice descrestere a stresului
# summary(lm(VasSNegD ~ VasS_preNeg + Neo_C1, data = data))  # RAMANE SEMNIF
rez3 <- lm(VasSNegD ~ VasS_preNeg + Neo_C1, data = data);  par(mfrow=c(1,2)); termplot(rez3,partial.resid = TRUE, se = TRUE)
pterms3 <- predict(rez3, type="terms"); partial.res3 <- as.data.frame( apply(pterms3,2,function(x)x+resid(rez3)) )
DF.rez3 <- data.frame(Neo_C1=rez3$model[,"Neo_C1"], Partial_Rez_Neo_C1=partial.res3$Neo_C1) # date luate din model pt ca NA
plot(DF.rez3[,1], DF.rez3[,2]); abline(lm(DF.rez3[,2] ~ DF.rez3[,1]), col="red")
write.xlsx(DF.rez3, paste(savePath,"DF.rez3.xlsx"))
# summary(lm(VasSNegD ~ Neo_C2, data = data))  # C2 Ordine prezice descrestere a stresului
# summary(lm(VasSNegD ~ VasS_preNeg + Neo_C2, data = data))  # RAMANE SEMNIF
rez4 <- lm(VasSNegD ~ VasS_preNeg + Neo_C2, data = data);  par(mfrow=c(1,2)); termplot(rez4,partial.resid = TRUE, se = TRUE)
pterms4 <- predict(rez4, type="terms"); partial.res4 <- as.data.frame( apply(pterms4,2,function(x)x+resid(rez4)) )
DF.rez4 <- data.frame(Neo_C2=rez4$model[,"Neo_C2"], Partial_Rez_Neo_C2=partial.res4$Neo_C2) # date luate din model pt ca NA
plot(DF.rez4[,1], DF.rez4[,2]); abline(lm(DF.rez4[,2] ~ DF.rez4[,1]), col="red")
write.xlsx(DF.rez4, paste(savePath,"DF.rez4.xlsx"))

for(i in seq_along(col10)){
  lm.test[[i]] <- summary(lm(reformulate(col10[i], "VasBPozD"), data = data))
} 
lm.test
# summary(lm(VasBPozD ~ Neo_Nev, data = data))  # Nevro prezice crestere a St Bine
# summary(lm(VasBPozD ~ VasB_prePoz + Neo_Nev, data = data))  # nu mai e semnif
# summary(lm(VasBPozD ~ Neo_C1, data = data))  # C1 Competenta prezice descrestere a St Bine
# summary(lm(VasBPozD ~ VasB_prePoz + Neo_C1, data = data))  # nu mai e semnif
# summary(lm(VasBPozD ~ Neo_N2, data = data))  # N2 Furie-Ostilitate prezice crestere a St Bine
# summary(lm(VasBPozD ~ VasB_prePoz + Neo_N2, data = data))  # nu mai e semnif
# summary(lm(VasBPozD ~ Neo_O5, data = data))  # O5 Idei prezice descrestere a St Bine
# summary(lm(VasBPozD ~ VasB_prePoz + Neo_O5, data = data))  # nu mai e semnif
# summary(lm(VasBPozD ~ Neo_N6, data = data))  # N6 Vulnerabilitate prezice crestere a St Bine
# summary(lm(VasBPozD ~ VasB_prePoz + Neo_N6, data = data))  # nu mai e semnif

for(i in seq_along(col10)){
  lm.test[[i]] <- summary(lm(reformulate(col10[i], "VasBNegD"), data = data))
} 
lm.test
# summary(lm(VasBNegD ~ Neo_N2, data = data))  # N2 Furie-Ostilitate prezice descrestere a St Bine   (p=0.0508)
# summary(lm(VasBNegD ~ VasB_preNeg + Neo_N2, data = data))  # DEVINE MAI SEMNIF
rez5 <- lm(VasBNegD ~ VasB_preNeg + Neo_N2, data = data);  par(mfrow=c(1,2)); termplot(rez5,partial.resid = TRUE, se = TRUE)
pterms5 <- predict(rez5, type="terms"); partial.res5 <- as.data.frame( apply(pterms5,2,function(x)x+resid(rez5)) )
DF.rez5 <- data.frame(Neo_N2=rez5$model[,"Neo_N2"], Partial_Rez_Neo_N2=partial.res5$Neo_N2) # date luate din model pt ca NA
plot(DF.rez5[,1], DF.rez5[,2]); abline(lm(DF.rez5[,2] ~ DF.rez5[,1]), col="red")
write.xlsx(DF.rez5, paste(savePath,"DF.rez5.xlsx"))
# summary(lm(VasBNegD ~ Neo_E2, data = data))  # E2 Spirit gregar prezice descrestere a St Bine
# summary(lm(VasBNegD ~ VasB_preNeg + Neo_E2, data = data))  # RAMANE SEMNIF
rez6 <- lm(VasBNegD ~ VasB_preNeg + Neo_E2, data = data);  par(mfrow=c(1,2)); termplot(rez6,partial.resid = TRUE, se = TRUE)
pterms6 <- predict(rez6, type="terms"); partial.res6 <- as.data.frame( apply(pterms6,2,function(x)x+resid(rez6)) )
DF.rez6 <- data.frame(Neo_E2=rez6$model[,"Neo_E2"], Partial_Rez_Neo_E2=partial.res6$Neo_E2) # date luate din model pt ca NA
plot(DF.rez6[,1], DF.rez6[,2]); abline(lm(DF.rez6[,2] ~ DF.rez6[,1]), col="red")
write.xlsx(DF.rez6, paste(savePath,"DF.rez6.xlsx"))

for(i in seq_along(col10)){
  lm.test[[i]] <- summary(lm(reformulate(col10[i], "IosPozD"), data = data))
} 
lm.test
# summary(lm(IosPozD ~ Neo_E1, data = data))  # E1 Caldura prezice descrestere pe IOS 
# summary(lm(IosPozD ~ IOS_prePoz + Neo_E1, data = data))  # nu mai e semnif
# summary(lm(IosPozD ~ Neo_N1, data = data))  # N1 Anxietate prezice crestere pe IOS 
# summary(lm(IosPozD ~ IOS_prePoz + Neo_N1, data = data))  # nu mai e semnif
# summary(lm(IosPozD ~ Neo_A3, data = data))  # A3 Altruism prezice descrestere pe IOS 
# summary(lm(IosPozD ~ IOS_prePoz + Neo_A3, data = data))  # nu mai e semnif 

for(i in seq_along(col10)){
  lm.test[[i]] <- summary(lm(reformulate(col10[i], "IosNegD"), data = data))
} 
lm.test
# summary(lm(IosNegD ~ Neo_A5, data = data))  # A5 Modestie prezice crestere pe IOS 
# summary(lm(IosNegD ~ IOS_preNeg + Neo_A5, data = data))  # RAMANE SEMNIF
rez7 <- lm(IosNegD ~ IOS_preNeg + Neo_A5, data = data);  par(mfrow=c(1,2)); termplot(rez7,partial.resid = TRUE, se = TRUE)
pterms7 <- predict(rez7, type="terms"); partial.res7 <- as.data.frame( apply(pterms7,2,function(x)x+resid(rez7)) )
DF.rez7 <- data.frame(Neo_A5=rez7$model[,"Neo_A5"], Partial_Rez_Neo_A5=partial.res7$Neo_A5) # date luate din model pt ca NA
plot(DF.rez7[,1], DF.rez7[,2]); abline(lm(DF.rez7[,2] ~ DF.rez7[,1]), col="red")
write.xlsx(DF.rez7, paste(savePath,"DF.rez7.xlsx"))

# summary(lm(IosNegD ~ Neo_O6, data = data))  # O6 Valori prezice descrestere pe IOS 
# summary(lm(IosNegD ~ IOS_preNeg + Neo_O6, data = data))  # nu mai e semnif 

for(i in seq_along(col10)){
  lm.test[[i]] <- summary(lm(reformulate(col10[i], "Sam1PozD"), data = data))
} 
lm.test
# summary(lm(Sam1PozD ~ StaiSbrut, data = data))  # StaiSbrut prezice descrestere pe SAM1  (Sam1 scor mic valenta poz)
# summary(lm(Sam1PozD ~ Sam1_prePoz + StaiSbrut, data = data))  # nu mai e semnif


##################################################################################################################

data <- read_excel("O.2 Date pt DG BUN cu NEO si STAIY.xlsx", 1) 

col10 <- names(data)[23:59]

lm.test <- vector("list", length(col10))

data$DGNegD <- data$DG_postNegTot - data$DG_preNegTot 

for(i in seq_along(col10)){
  lm.test[[i]] <- summary(lm(reformulate(col10[i], "DGNegD"), data = data))
} 
lm.test
# summary(lm(DGNegD ~ Neo_A5, data = data))  # A5 Modestie prezice descrestere (adica crestere a altruismului)
# summary(lm(DGNegD ~ DG_preNegTot + Neo_A5, data = data))  # nu mai e semnificativ

data$medieDGpre <- rowMeans(data[, c("DG_preNegTot", "DG_prePozTot")])

library(ggplot2)
library(dplyr)
library(corrr)
library(corrplot)
library(Hmisc)


CorDG <- data[, c(col10, "medieDGpre")] %>% 
  correlate(use = "pairwise.complete.obs", method = "pearson") %>% 
  focus(medieDGpre)
CorDG

CorDG %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(medieDGpre)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = medieDGpre)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Corelatie cu medieDGpre") +
  xlab("Variable")

COR <- rcorr(as.matrix(data[, c(col10, "medieDGpre")]))
M <- COR$r
P_MAT <- COR$P
corrplot(M, type = "upper", p.mat = P_MAT, sig.level = 0.01, , insig = "blank")

##################################################################################################################

data <- read_excel("O.2 Date pt Trust BUN cu NEO si STAIY.xlsx", 1) 

col10 <- names(data)[23:59]

data$medieTrustpreMinus <- rowMeans(data[, c("TrustMinPozPre", "TrustMinNegPre")])
data$medieTrustpre0 <- rowMeans(data[, c("Trust0PozPre", "Trust0NegPre")])
data$medieTrustprePlus <- rowMeans(data[, c("TrustPlusPozPre", "TrustPlusNegPre")])
data$medieTrustpreTot <- rowMeans(data[, c("TrustTotPozPre", "TrustTotNegPre")])

COR <- rcorr(as.matrix(data[, c(col10, "medieTrustpreMinus", "medieTrustpre0", "medieTrustprePlus", "medieTrustpreTot")]))
M <- COR$r
P_MAT <- COR$P
corrplot(M, type = "upper", p.mat = P_MAT, sig.level = 0.01, , insig = "blank")


