## this is the R file for PS2!
getwd()
setwd("/Users/iramalis/Desktop/4625/wd")

getwd()
setwd("/Users/iramalis/Desktop/4625/wd/")

### Leemis's m
### inputs: v, vector of proportional frequencies for each digit, 1:9

m=function(v){
  final<-rep(NA,1)
  i<-(1:length(v))  
  final<-v-log10(1+1/i)
  return (max(final))
}

### Cho-Gains' d
### inputs: v, vector of proportional frequencies for each digit, 1:9

d=function(v){
  final<-NA
  i<-(1:length(v))
  final<-v-log10(1+1/i)
  return(sqrt(sum(final^2)))
}



### f: function taking in vector of election returns,
f=function(vec,want.m=TRUE,want.d=TRUE){
  m.values<-c(0.851,0.967,1.212)
  d.values<-c(1.212,1.330,1.569)
  
  m<-m(vec)
  d<-d(vec)
  
  m.sig<-sum(m>m.values)
  d.sig<-sum(d>d.values)
  
  signif<-c("0.10","0.05","0.01")
  
  if (want.m==TRUE & want.d==TRUE){
    
    return(print(paste("m = ",m(vec),"and is significant at the",signif[m.sig],"level;",
                       "d = ",d(vec),"and is significant at the",signif[d.sig],"level;")))
    
  }
  else if(want.m==TRUE){
    return(m(vec))
  }
  else if(want.d==TRUE){
    return(d(vec))
  }
}


