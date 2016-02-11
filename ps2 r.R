## this is the R file for PS2!
getwd()
setwd("/Users/iramalis/Desktop/4625/wd")

### EXAMPLE vec.random - VECTOR OF 100 RANDOM VALUES FROM 1 TO 1000

vec.random<-runif(100,1,1000)
not.random<-rep(seq(3,5),300)

### function: get.distrib
### 
### inputs: vec.results, a vector of vote totals
### outputs: vec.distrib, a vector of the distribution of occurrences of each digit (1:9)
###      as the first significant digit of the values in the vote totals vector


get.distrib=function(vec.results){
  vec.results<-as.character(vec.results)
  results.first<-substr(vec.results,1,1) # extracting first digit of each element of vec.results
  table.first<-table(results.first)
  nums<-as.character(c(1:9))
  # creating vector of total occurrences of each digit, ensuring that values are assigned to correct position
  #   in the vector (in case some digits do not appear as the first significant digit in the original vec.results)
  vec.distrib<-rep(0,9)
  for (i in nums){
    digit<-as.numeric(i)
    if(length(table.first[names(table.first)==i])!=0){
      vec.distrib[digit]<-table.first[names(table.first)==i]
    }
  }
  vec.distrib<-as.vector(vec.distrib) # getting rid of names from table
  return(vec.distrib)
}

### TEST get.distrib on vec.random

test.distrib<-get.distrib(vec.random)
fraud.distrib<-get.distrib(not.random)

### function: get.freq
### 
### inputs: vec.distrib, a vector of the distribution of occurrences of each digit (1:9)
###       as the first significant digit of the values of vote totals
### outputs: vec.freq, a vector of the proportional frequencies with which each digit
###       appears in the first significant digit of the values of vote totals

get.freq=function(vec.distrib){
  vec.freq<-vec.distrib/sum(vec.distrib)
}

### TEST get.freq on test.distrib

test.freq<-get.freq(test.distrib)
fraud.freq<-get.freq(fraud.distrib)

### function: find.m
### 
### inputs: veq.freq, vector of proportional frequencies for each digit, 1:9
### outptus: single value, Leemis's m statistic, for the frequencies provided

find.m=function(vec.freq){
  i<-(1:length(vec.freq))  
  values<-vec.freq-log10(1+1/i)
  return (max(values))
}

### TEST find.m on test.freq

test.m<-find.m(test.freq)
fraud.m<-find.m(fraud.freq)

### function: find.d
###
### inputs: vec.freq, vector of proportional frequencies for each digit, 1:9
### outputs: single value, Cho-Gain's d statistic, for the frequencies provided

find.d=function(vec.freq){
  i<-(1:length(vec.freq))
  values<-vec.freq-log10(1+1/i)
  return(sqrt(sum(values^2)))
}

### TEST find.d on test.freq

test.d<-find.d(test.freq)
fraud.d<-find.d(fraud.freq)

### NOTE: Even with a "fraudulent" vector (repeated 3,4,5) as the original input, neither d nor m reaches its 
###     critical value at any level of significance.

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
  list.benford<-list() 
  if (want.m){
    m<-find.m(vec.freq) # calculate m-statistic, if requested
    list.benford$"m value"<-m # add m to list, as first element
  }
  if (want.d){
    d<-find.d(vec.freq) # calculate d-statistic, if requested
    list.benford$"d value"<-d # add d to list, as second element
  }
  list.benford$"digit distribution"<-vec.distrib # add vector of digit distributions to list, as third element
  return(list.benford)
}

### TEST benford.results on vec.random

test.results1<-benford.results(vec.random)
test.results2<-benford.results(vec.random,want.m=FALSE)
test.results3<-benford.results(not.random)

### QUESTION 2: CRITICAL VALUES
### 
### function: print.benfords
###
### inputs: vec.results, a vector of election results
### outputs: table.benfords, a table containing the name and value of each statistic, 
###     the relevant number of asterisks, and a legend explaining the asterisks

print.benfords=function(vec.results){
  vec.distrib<-get.distrib(vec.results) # find distribution of digits
  vec.freq<-get.freq(vec.distrib) # find proportional frequency of digits

  m<-find.m(vec.freq) # calculate m-statistic
  d<-find.d(vec.freq) # calculate d-statistic
  
  m.critical<-c(0.851,0.967,1.212) # vector of critical values for m
  d.critical<-c(1.212,1.330,1.569) # vector of critical values for d
  
  # comparing m and d statistics to their critical values; assigning to m.sig and d.sig values of 
  #   (0, 1, 2, or 3) for (not significant, signifant at α=.10, at α=.05, or α=.10), respectively
  m.sig<-sum(m>=m.critical)
  d.sig<-sum(d>=d.critical)
  
  # m.star and d.star: m and d values pasted to their appropriate number of stars
  stars<-c("*","**","***")
  m.star<-paste(m, stars[m.sig], sep="")
  d.star<-paste(d, stars[d.sig], sep="")
  
  # array of statistics, with names, values, and asterisks
  array.benfords<-array(dim=c(2,2))
  array.benfords[1,]<-c("Leemis's m","Cho-Gains' d")
  array.benfords[2,]<-c(m.star, d.star)
  
  # list combining array above with explanation of stars
  explanation<-cbind(c("* = significant at α=.10", "** = significant at α=.05", "*** = significant at α=.01"))
  list.critical<-list()
  list.critical[[1]]<-array.benfords
  list.critical[[2]]<-explanation
  
  return(list.critical)
}

### TEST

print.benfords(vec.random)
print.benfords(not.random)


### function: benfords.csv
###
### inputs: vec.results (same as above); directory, destination to write the csv file to (default to my working directory)
### outputs: writes a csv of the output of the print.benfords function


benfords.csv=function(vec.results, directory=("/Users/iramalis/Desktop/4625/wd")){
  list.critical<-print.benfords(vec.results) # generating list that is the output from above function
  clean.array<-array(dim=c(6,2)) # empty array
  clean.array[1:2,1:2]<-list.critical[[1]] # adding statistic names and values
  clean.array[4:6,1]<-list.critical[[2]] # adding explanation of asterisks
  #replacing NAs with "", leaving space for legibility
  clean.array[4:6,2]<-""
  clean.array[3,1:2]<-""
  setwd(directory)
  write.csv(clean.array,file="benfords.csv")
}

### TEST
benfords.csv(vec.random, directory=("/Users/iramalis/Desktop"))
benfords.csv(not.random)
