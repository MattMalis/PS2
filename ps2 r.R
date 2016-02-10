## this is the R file for PS2!
getwd()
setwd("/Users/iramalis/Desktop/4625/wd")

getwd()
setwd("/Users/iramalis/Desktop/4625/wd/")

### function: find.m
### 
### inputs: veq.freq, vector of proportional frequencies for each digit, 1:9
### outptus: single value, Leemis's m statistic, for the frequencies provided

find.m=function(vec.freq){
  final<-rep(NA,1)
  i<-(1:length(vec.freq))  
  final<-vec.freq-log10(1+1/i)
  return (max(final))
}

### function: find.d
###
### inputs: vec.freq, vector of proportional frequencies for each digit, 1:9
### outputs: single value, Cho-Gain's d statistic, for the frequencies provided

find.d=function(veq.freq){
  final<-NA
  i<-(1:length(vec.freq))
  final<-vec.freq-log10(1+1/i)
  return(sqrt(sum(final^2)))
}


### function: get.distrib
### 
### inputs: vec.results, a vector of vote totals
### outputs: vec.distrib, a vector of the distribution of occurrences of each digit (1:9)
###      as the first significant digit of the values in the vote totals vector


get.distrib=function(vec.results){
  vec.results<-as.character(vec.results)
  results.first<-substr(vec.results) # extracting first digit of each element of vec.results
  table.first<-table(results.first)
  vec.distrib<-as.numeric(as.vector(table.first)) # vector of total occurrences of each digit
}

### function: get.freq
### 
### inputs: vec.distrib, a vector of the distribution of occurrences of each digit (1:9)
###       as the first significant digit of the values of vote totals
### outputs: vec.freq, a vector of the proportional frequencies with which each digit
###       appears in the first significant digit of the values of vote totals

get.freq=function(vec.distrib){
  vec.freq<-vec.distrib/sum(vec.distrib)
}

### QUESTION 1: CALCULATING VIOLATIONS
###
### function: benford.results
###
### inputs: vec.results, a vector of election results; want.m, a boolean of whether to 
###     calculate the m-statistic (default TRUE); want.d, a boolean of whether to calculate 
###     the d-statistic (default TRUE)
### outputs: list.benford, a list containing the calculated statistic(s) requested (m, d, or both), 
###     and a vector of digit distributions 

benford.results=function(vec.results,want.m=TRUE,want.d=TRUE){
  vec.distrib<-get.distrib(vec.results) # find distribution of digits
  vec.freq<-get.freq(vec.distrib) # find proportional frequency of digits
  list.benford<-list(null) 
  if (want.m){
    m<-find.m(vec.freq) # calculate m-statistic, if requested
    list.benford[[1]]<-m # add m to list, as first element
  }
  if (want.d){
    d<-find.d(vec.freq) # calculate d-statistic, if requested
    list.benford[[2]]<-d # add d to list, as second element
  }
  list.benford[[3]]<-vec.distrib # add vector of digit distributions to list, as second element
}

### function: benford.critical
###
###
###
###

benford.critical=

  m.values<-c(0.851,0.967,1.212)
d.values<-c(1.212,1.330,1.569)
m<-m(vec)
d<-d(vec)
m.sig<-sum(m>=m.values)
d.sig<-sum(d>=d.values)
sigLevels<-c("0.10","0.05","0.01")

### QUESTION 2: CRITICAL VALUES
### 
### function: print.benford
###
###
###
###



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
