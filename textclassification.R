library(koRpus)
tc <- read.table("/home/personnels/berhe/TextClassification/outputfile.txt", header=FALSE, fill=TRUE, na.strings = "NA", colClasses = NA)
treetag("/home/personnels/berhe/TextClassification/outputfile.txt")
set.kRp.env(TT.cmd="/home/personnels/berhe/treetagger/cmd/tree-tagger-english", lang="en")
get.kRp.env(lang=TRUE, errorIfUnset = TRUE)
agged.results <- treetag(tc, treetagger="KRp.env", lang="KRp.env",
                         TT.options=list(no.unknown=TRUE))
agged.results
readability(agged.results)
lex.div(agged.results)
tagged.results <- treetag("/home/personnels/berhe/TextClassification/outputfile.txt", treetagger="manual", format="obj",
                          TT.tknz=FALSE , lang="en",
                          TT.options=list(path="/home/personnels/berhe/treetagger",no.unknown=TRUE, preset="en"))
tagged.results
head(tc)
length(tc)
levels(tc)
dim(tc)
tc[14,"V1"]
write.table(tc, file = "aman.csv", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = "double",
            fileEncoding = "")
d<-read.table("aman.csv")
d[5, "V3"]
c<-0
k<-0
for(k in 1:nrow(d)){
  if(d[k,"V3"]!=""){
  #print(d[k,"V2"],max.levels=0)
    if(d[k,"V2"]!="SENT"){
   write.table(d[k, "V3"], file = "positiveSample2", append = FALSE, quote = TRUE, sep = " ",
                eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                col.names = FALSE, qmethod = "double",
                fileEncoding = "")
      print(d[k,"V3"],max.levels=0)
  print("positive") 
  c<-c+1
    }
    else
    {}
  }
}
outp<-read.table("positiveSample2")
maximumSentenceLength<-25
for (i in 1:nrow(d) ){ 
  if (d[i, "V2"]=="class='symptom'>") {
    nextWord<-i+1
    prevWord<-i-1
    print(d[i, "V3"]) 
      for(j in maximumSentenceLength){
          if(d[nextWord, "V2"]!="SENT"){
            print(d[nextWord, "V3"])
            nextWord<-nextWord+1
            
          print("stop")
            break
        }
        else {
          print(d[nextWord, "V3"])
          write.table(d[nextWord, "V3"], file = "positiveSample.csv", append = FALSE, quote = TRUE, sep = " ",
                      eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                      col.names = FALSE, qmethod = "double",
                      fileEncoding = "")
          nextWord<-nextWord+1
          print("positive") 
          c<-c+1
        }
    }
  }
  write.table(d[i, "V3"], file = "NegetiveSample.csv", append = TRUE, quote = TRUE, sep = " ",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = "double",
              fileEncoding = "")
  # do more things with the data frame...
} 
output<-read.table("positiveSample.csv",fill= TRUE)
output2<-read.table("NegetiveSample.csv",fill= TRUE)
print("number of minimum positive sentences",quote=FALSE) 
c
tcvac <- read.table("/home/personnels/berhe/TextClassification/pompeData", header=FALSE,na.strings = "NA", colClasses = NA)
head(tcvac)
length(tcvac)
levels(tcvac)
dim(tcvac)
library(rpart)
library(arules)
#m<-create_matrix(tcvac["Text"])
length(tcvac)
dim(tcvac)
tc <- as(tc,"transactions")
tcvac <- as(tcvac,"transactions")
it<-itemFrequency(tc)
itvac<-itemFrequency(tcvac)
it
itvac
color<-c('grey','white','black')
color2<-c('grey','white','black')
X11()
par(mfrow=c(1,2))
itemFrequencyPlot(tcvac,support=.02,cex.names=1.5,popCol = 'red',col=color)
itemFrequencyPlot(tc,support=.02,cex.names=1.5,popCol = 'red',col=color2)
#color<-c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 
par(no.readonly = FALSE)
hist(itvac,col = color)
barplot(itvac, col = color)
barplot(it, col = color)
ratio1<-itvac
ratio2<-it
ratio
for (i in itvac){
  ratio[i]<-(ratio1[i])/(ratio2[i])
}
plot(ratio)
#classification usung rpart
n <- nrow(tcvac) 
train <- sort(sample(1:n, floor(n/2)))
train
tcvac.train <- tcvac[train,] 
tcvac.test <- tcvac[-train,]
tcvac.rp <-rpart(class ~ . ,   
                data = tcvac[1:3],  # Don't take the 5th attribute (the class) for learing the model 
                subset = train, 
                method = "class",
                parms = list(split = "information"), 
                maxsurrogate = 0, 
                cp = 0, 
                minsplit = 2,
                minbucket = 2,
                )
summary(tcvac.rp)

