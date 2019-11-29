#downloading the data
if (!file.exists("data")) {dir.create("data")
    URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(url = URL,destfile = "data/data.zip")}

#reading the data
hdt <- read.csv(file = "data/data.zip",header = TRUE,sep = ",",stringsAsFactors = FALSE)
#to get a sense of the data we use the str function
str(hdt)
#Synopsis
#Across the United States, which types of events 
#(as indicated in the EVTYPE\color{red}{\verb|EVTYPE|}EVTYPE variable) 
#are most harmful with respect to population health?
#Across the United States, which types of events have the greatest economic consequences?
#let's reduce first the aout of the data
names(hdt)
UsflDT <- subset(hdt,select = c("STATE","EVTYPE","LENGTH","WIDTH","F","MAG","FATALITIES","INJURIES",
                      grep(pattern = "ropdmg|ROPDMG",x =names(hdt) ,value = TRUE)))
#We drop the injuries with zeros since we don't need them
UsflDT$EVTYPE <-tolower(UsflDT$EVTYPE) 
#lets load the a text file where the events types are stored as one character and after do the necessary trimings
ref1 <- scan("cat.txt",what = "character",sep =".")#the sep ="." is added to load it a one char
ref2 <- strsplit(split = " +[Z|C|M]+ ",x = ref1)#since the seperation is gonna be the Z M C
ref3 <- as.character(ref2[[1]])
ref4 <- sort(tolower(gsub(pattern = "^ *|[(<= *)]|( *)",replacement = " ",x = ref3)))
eventref <- gsub("^ ","",ref4)#deleting space from the beginings

Dtevent <- sort(unique(Usldt2$EVTYPE))
#31
eventreference1 <- gsub("/|-"," ",eventref)#to delete non alphanumerical stuf
eventreference1
Dtevent1 <- gsub("/|-"," ",Dtevent)
#let's first select the matchings
index <- UsflDT$EVTYPE%in%unique(eventreference1)
nw_dt <- UsflDT[index,]

#Across the United States, which types of events (as indicated in the EVTYPE\color{red}{\verb|EVTYPE|}
#EVTYPE variable) are most harmful with respect to population health?
Usldt2 <- subset(nw_dt,FATALITIES>0 & INJURIES>0)
Ordt1 <- aggregate(INJURIES~EVTYPE,data = Usldt2, sum)
Ordt2 <- aggregate(FATALITIES~EVTYPE,data = nw_dt, sum)
Ordt1[order(Ordt1$INJURIES,decreasing = TRUE),]
fn_df1 <- data.frame(Ordt1[order(Ordt1$INJURIES,decreasing = TRUE),][,1:2], row.names = 1:nrow(Ordt1))
fn_df2 <- data.frame(Ordt2[order(Ordt2$FATALITIES,decreasing = TRUE),][,1:2], row.names = 1:nrow(Ordt2))

#As we can see tornados are the most notorious events on the population health
#let's make a plot
oldpar <- par()
par(mfrow=c(1,2),oma=c(0,0,3,0))
barplot(fn_df1$INJURIES,names.arg = fn_df1$EVTYPE,las=2,
        col = "darkblue",main = "Total Injuries across the US",
        ylim = c(0,50000),crt=90,cex.names = .9,font.main=4)
barplot(fn_df2$FATALITIES,names.arg = fn_df2$EVTYPE,las=2,
        col = "darkblue",main = "Total Fatalities across the US",
        ylim = c(0,5000),font.main=4)

mtext("Most harmful events across the US", 
      outer = TRUE,font = 4,cex=1.3)

par(oldpar)
#Across the United States, which types of events have the greatest economic consequences?
Usldt3 <- subset(nw_dt,PROPDMG>0 | CROPDMG>0)
colnames(Usldt3) <- tolower(names(Usldt3))

Usldt3$propdmgexp <- tolower(Usldt3$propdmgexp)
Usldt3$cropdmgexp <- tolower(Usldt3$cropdmgexp)
head(Usldt3)

unique(Usldt3$cropdmgexp)
Usldt3$propdmgexp <- factor(Usldt3$propdmgexp,levels = c("b","m","k","h","0","?","-",""))
mean(is.na(Usldt3$cropdmgexp))
sum(is.na(Usldt3$propdmgexp))#not that much, so we can delete them completely


#let's repor everything to the billion unit

index1 <- which(!is.na(Usldt3$propdmgexp) &Usldt3$propdmgexp=="m")
index2 <- which(!is.na(Usldt3$propdmgexp) &Usldt3$propdmgexp=="k")
index3 <- which(!is.na(Usldt3$propdmgexp) &Usldt3$propdmgexp=="h")
index4 <- which(!is.na(Usldt3$propdmgexp) &Usldt3$propdmgexp%in%c("","-","?"))
index5 <- which(!is.na(Usldt3$propdmgexp) &Usldt3$propdmgexp%in%0:8)


Usldt3[index1,"propdmg"] <- Usldt3[index1,"propdmg"]/10^3
Usldt3[index2,"propdmg"] <- Usldt3[index2,"propdmg"]/10^6
Usldt3[index3,"propdmg"] <- Usldt3[index3,"propdmg"]/10^7
Usldt3[index4,"propdmg"] <- Usldt3[index4,"propdmg"]/10^9
Usldt3[index5,"propdmg"] <- Usldt3[index5,"propdmg"]/10^8

#let's finally order the data
Usfldt3 <- Usldt3[,c("evtype","propdmg","propdmgexp")]
resh_dt1 <- aggregate(propdmg~evtype,Usfldt3,sum)
or_resh_dt1 <- resh_dt1[order(resh_dt1$propdmg,decreasing = TRUE),]
or_resh_dt2 <- transform(or_resh_dt1, propdmg=signif(propdmg,digits = 4))
finl_dt <- data.frame(or_resh_dt2,row.names = 1:nrow(or_resh_dt2))


#let's now see the crop damage
#let's repor everything to the billion unit


index5 <- which(!is.na(Usldt3$cropdmgexp) &Usldt3$cropdmgexp=="m")
index6 <- which(!is.na(Usldt3$cropdmgexp) &Usldt3$cropdmgexp=="k")
index7 <- which(!is.na(Usldt3$cropdmgexp) &Usldt3$cropdmgexp=="h")
index8 <- which(!is.na(Usldt3$cropdmgexp) &Usldt3$cropdmgexp%in%c("","-","?"))
index9 <- which(!is.na(Usldt3$cropdmgexp) &Usldt3$cropdmgexp%in%0:8)



Usldt3[index5,"cropdmg"] <- Usldt3[index5,"cropdmg"]/10^4
Usldt3[index6,"cropdmg"] <- Usldt3[index6,"cropdmg"]/10^6
Usldt3[index7,"cropdmg"] <- Usldt3[index7,"cropdmg"]/10^7
Usldt3[index8,"cropdmg"] <- Usldt3[index8,"cropdmg"]/10^9
Usldt3[index9,"cropdmg"] <- Usldt3[index9,"cropdmg"]/10^8


#let's finally order the data
Usfldt4 <- Usldt3[,c("evtype","cropdmg","cropdmgexp")]
resh_dt2 <- aggregate(cropdmg~evtype,Usfldt4,sum)
or_resh_dt3 <- resh_dt2[order(resh_dt2$cropdmg,decreasing = TRUE),]
or_resh_dt4 <- transform(or_resh_dt3, cropdmg=signif(cropdmg,digits = 4))
finl_dt2 <- data.frame(or_resh_dt4,row.names = 1:nrow(or_resh_dt4))


#make the plot
oldpar <- par()
par(mfrow=c(1,2),oma=c(0,0,3,0))
barplot(finl_dt$propdmg, names.arg = finl_dt$evtype,main = "Total amount of property damage",
        las=2,col="gold",font.main=3)
barplot(finl_dt2$cropdmg, names.arg = finl_dt2$evtype,las=2,main="Total amount of crop damage",
        col="gold", font.main=3)

mtext("Damage in terms of billion dollars due to natural disaster", 
      outer = TRUE,font = 2,cex=1.3)

par(oldpar)














































#We select the non matchings
nonMatchStrChar <- Dtevent1[is.na(match(Dtevent1,eventreference1))]

#what we can do, is to split the chained caracter to single ones, and look in the original events
#because in the end we need to match as much as possible
spl_nonMatchStChar <- strsplit(nonMatchStrChar," ")

new_nonmatchstrchr <- unlist(lapply(spl_nonMatchStChar, function(x){my_char <- as.character(x)}))
index2 <- pmatch(new_nonmatchstrchr,eventreference1)
new_nonmatchstrchr
new_nonmatchstrchr[!is.na(index2)]
new_nonmatchstrchr[!is.na(index2)] <- eventreference1[na.omit(index2)]
new_nonmatchstrchr[!is.na(index2)]
tail(UsflDT)
unique(UsflDT$EVTYPE)
dt_event <- c("avalanche","flood flash flood","thunderstorm winds","heat")
ref_event <- c("avalanche","flood","heat","winds")
Dtevent1[unlist(sapply(eventreference1, function(x) which(grepl(x, Dtevent1))))]
ref
gsub("Z",replacement = "",x = ref)
