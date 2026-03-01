setwd("~/Desktop/[LabMZ]_Ye")
library(tidyverse)
##importare i dati iniziali 
ds <- read.csv("ds_salaries.csv")

##serve per controllare se esiste NA
is.na(ds)
sum(is.na(ds))##=0, quindi non esiste i valori mancanti 

glimpse(ds)


table(ds$experience_level)
info1 <- ds %>% select(experience_level,
                       employment_type,salary_in_usd)

## boxplot
##boxplot(salary_in_usd ~ experience_level, data = info1, xlab = "livello di esperienza",ylab = "salari", main = "boxplot")
##boxplot(salary_in_usd ~ employment_type, data = info1, xlab = "tipo di contratto",ylab = "salari", main = "boxplot")
library("scales")
ggplot(info1,aes(x=experience_level,y=salary_in_usd,fill=experience_level))+geom_boxplot() + 
  scale_y_continuous(labels = comma) +labs(title="Intervallo di stipendio in base al livello di esperienza",x="Employment level",y="Salary",fill="Employment level")

library("scales")
ggplot(info1,aes(x=employment_type,y=salary_in_usd,fill=employment_type))+geom_boxplot() + 
  scale_y_continuous(labels = comma)+ labs(title="Intervallo di stipendio in base al tipo di impiego",x="Employment type",y="Salary",fill = "Employment type")

##istogramma
hist(info1$salary_in_usd,xlab="salario",ylab="frequenza", col = c("#D2B48C","#FFA54F","#FFC90E","#8B5A2B","#FFCC99","#FFFF00","#CCFF00"), main ="Istogramma")

## faccio dei sottogruppi
en <- info1 %>%
  filter( experience_level=="EN")

ex <- info1 %>%
  filter( experience_level=="EX")

mi <- info1 %>%
  filter( experience_level=="MI")

se <- info1 %>%
  filter( experience_level=="SE")


##creare una tabella
as.data.frame(table(en$employment_type))## per calcolare il numero di en
EN <- matrix(
  c(sum(en[which(en$employment_type=="CT"),3])/2,
    sum(en[which(en$employment_type=="FL"),3])/2,
    sum(en[which(en$employment_type=="FT"),3])/302,
    sum(en[which(en$employment_type=="PT"),3])/14)
  ,nrow = 1,ncol=4)
colnames(EN) <-c("CT","FL","FT","PT")
rownames(EN) <- c("EN")


as.data.frame(table(ex$employment_type))## per calcolare il numero di ex
EX <- matrix(
  c(sum(ex[which(ex$employment_type=="CT"),3])/1,
    sum(ex[which(ex$employment_type=="FL"),3]),
    sum(ex[which(ex$employment_type=="FT"),3])/113,
    sum(ex[which(ex$employment_type=="PT"),3]))
  ,nrow = 1,ncol=4)
colnames(EX) <-c("CT","FL","FT","PT")
rownames(EX) <- c("EX")


as.data.frame(table(mi$employment_type))## per calcolare il numero di mi 
MI <- matrix(
  c(sum(mi[which(mi$employment_type=="CT"),3])/5,
    sum(mi[which(mi$employment_type=="FL"),3])/5,
    sum(mi[which(mi$employment_type=="FT"),3])/792,
    sum(mi[which(mi$employment_type=="PT"),3])/3)
  ,nrow = 1,ncol=4)
colnames(MI) <-c("CT","FL","FT","PT")
rownames(MI) <- c("MI")

as.data.frame(table(se$employment_type))## per calcolare il numero di se
SE <- matrix(
  c(sum(se[which(se$employment_type=="CT"),3])/2,
    sum(se[which(se$employment_type=="FL"),3])/3,
    sum(se[which(se$employment_type=="FT"),3])/2511,
    sum(se[which(se$employment_type=="PT"),3]))
  ,nrow = 1,ncol=4)
colnames(SE) <-c("CT","FL","FT","PT")
rownames(SE) <- c("SE")

##combinare tutte queste matrici e unisce ed al fine, crea la tabella2.2(salario in media)
tabella <-rbind(EN,MI,SE,EX)
tabella3.1 <-round(tabella,digits = 0) 
write.csv(tabella3.1,"tabella3.1.csv")

##faccio il grafo a barra

barplot(tabella3.1,
        col=c("#D2B48C","#FFA54F","#FFC90E","#8B5A2B"),
        main = "Grafico a barre",
        beside=T,legend=rownames(tabella3.1),
        args.legend = list(x = "topright",inset = c( -0.02,0),cex=0.7),
        horiz =T)



##  crea una tabella che contiene anche i totali
totale <- apply(tabella3.1,2,sum)
tab <- rbind(tabella3.1,totale)
Totale <- apply(tab,1,sum)
tabella3.2 <- cbind(tab,Totale)
write.csv(tabella3.2,"tabella3.2.csv")

## divido per la colonna marginale 
tabella3_3 <- t(apply(tabella3.2, 1, function(x) x / x[5]))
tabella3.3 <-round(tabella3_3,digits = 2) 
write.csv(tabella3.3,"tabella3.3.csv")
## divido per la riga marginale 
tabella3_4 <- t(apply(tabella3.2, 1, function(x) x / tabella3.2[5,]))
tabella3.4 <-round(tabella3_4,digits = 2) 
write.csv(tabella3.4,"tabella3.4.csv")
## divido per il totale
tabella3_5 <- t(apply(tabella3.2, 1, function(x) x / tabella3.2[5,5]))
tabella3.5 <-round(tabella3_5,digits = 2)
write.csv(tabella3.5,"tabella3.5.csv")

## analisi secondo il livello di esperienza 
percentuale <- tabella3.5[,5]
salario <- tabella3.2[,5]
tabella3.6 <- cbind(salario,percentuale)
write.csv(tabella3.6,"tabella3.6.csv") 
##grafico a barre e grafico a torta
t <- tabella3.6[-5,-1]
barplot(t,main = "Grafico a barre",col=c("#D2B48C","#FFA54F","#FFC90E","#8B5A2B"))
etichette <- paste(names(t),t)
etichette <- paste(etichette,"%",sep = "")
pie(t,etichette,main = "Grafico a torta")


## analisi secondo tipo
percentuali <- tabella3.5[5,]
salari <- tabella3.2[5,]
tabella3.7 <- cbind(salari,percentuali)
write.csv(tabella3.7,"tabella3.7.csv")
## grafico a barre e grafico a torta
t1 <- tabella3.7[-5,-1]
barplot(t1,main = "Grafico a barre",col=c("#D2B48C","#FFA54F","#FFC90E","#8B5A2B"))
etichette <- paste(names(t1),t1)
etichette <- paste(etichette,"%",sep = "")
pie(t1,etichette,main = "Grafico a torta")


##
table(info1$experience_level,
      info1$employment_type)
#La maggior parte delle posizioni Data Science, indipendentemente dal livello di esperienza, 
#è offerta con contratto Full-Time, indicando una forte domanda di figure professionali stabili nel settore.


