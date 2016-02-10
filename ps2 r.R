## this is the R file for PS2!
getwd()
setwd("/Users/iramalis/Desktop/4625/wd")

getwd()
setwd("/Users/iramalis/Desktop/4625/wd/")

### Leemis's m
### 
### inputs: veq.freq, vector of proportional frequencies for each digit, 1:9
### outptus: single value, Leemis's m statistic, for the frequencies provided

find.m=function(vec.freq){
  final<-rep(NA,1)
  i<-(1:length(vec.freq))  
  final<-vec.freq-log10(1+1/i)
  return (max(final))
}

### Cho-Gains' d
###
### inputs: vec.freq, vector of proportional frequencies for each digit, 1:9
### outputs: single value, Cho-Gain's d statistic, for the frequencies provided

find.d=function(veq.freq){
  final<-NA
  i<-(1:length(vec.freq))
  final<-vec.freq-log10(1+1/i)
  return(sqrt(sum(final^2)))
}


### getFreq 
### 
### inputs: vec.results, a vector of vote totals
### outputs: vec.freq, a vector of the proportional frequencies with which each 
### digit appears in the first significant digit of the values in the vote totals vector


getFreq=function(vec.results){
  vec.results<-as.character(vec.results)
  results.first<-substr(vec.results) # extracting first digit of each element of vec.results
  table.first<-table(results.first)
  vec.totals<-as.numeric(as.vector(table.first)) # vector of total occurrences of each digit
  vec.freq<-vec.totals/sum(vec.totals) # vector of proportional frequencies of each digit
}



##better name
f=function(vec,want.m=TRUE,want.d=TRUE){

  m.values<-c(0.851,0.967,1.212)
  d.values<-c(1.212,1.330,1.569)
  m<-m(vec)
  d<-d(vec)
  m.sig<-sum(m>=m.values)
  d.sig<-sum(d>=d.values)
  print(m.sig); print(d.sig)
  sigLevels<-c("0.10","0.05","0.01")
  
  if (want.m==TRUE & want.d==TRUE){
    
    return(print(paste("m = ",m(vec),"and is significant at the",sigLevels[m.sig],"level;",
                       "d = ",d(vec),"and is significant at the",sigLevels[d.sig],"level;")))
    
  }
  else if(want.m==TRUE){
    return(m(vec))
  }
  else if(want.d==TRUE){
    return(d(vec))
  }
}

v<-c(.15,.1,.18,.05,.2,.07,.05,.11,.09)
sum(v)
w<-c(0,0,.5,0,0,.3,0,0,.2)
f(v)
w


r
rawVec<-as.character(rawVec)
vecSplit<-strsplit(rawVec, split="")
vecSplit<-
