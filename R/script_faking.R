#pacotes que serão utilizados
library(psych)
library(lavaan)
library(dplyr)


#simulating the data
rm(data1,data1.1,modelQuad)
modelQuad <- "f1 =~  1*it_1_1 + 1*it_1_2   + -1*it_1_3   +   -1*it_1_4 +
                    .8*it_e1_1 + -.7*it_e1_2
              f2 =~ .9*it_2_1 + .50*it_2_2 + -.45*it_2_3 + -.85*it_2_4 +
                    .8*it_e2_1 + -.7*it_e2_2
              f3 =~ .8*it_3_1 + .6*it_3_2  + -.05*it_3_3 +  -.9*it_3_4 +
                    .8*it_e3_1 + -.7*it_e3_2
              
              DS =~ 1*it_1_1  +  -1*it_1_2 +   1*it_1_3 +   -1*it_1_4 +
                   .8*it_2_1  + -.7*it_2_2 +  .5*it_2_3 +  -.8*it_2_4 +
                   .9*it_3_1 +  -.03*it_3_2 + .6*it_3_3 + -.75*it_3_4 +
                    1*it_e1_1 + -1*it_e1_2 + 
                   .8*it_e2_1 + -.9*it_e2_2 +
                   .8*it_e3_1 + -.9*it_e3_2 
                    
              DS ~~ .3*DS
              f1 ~~ .8*f1
              f2 ~~ .8*f2
              f3 ~~ .8*f3
              DS ~~ 0*f1
              DS ~~ 0*f2
              DS ~~ 0*f3
              f1 ~~ .3*f2
              f1 ~~ .2*f3
              f2 ~~ .35*f3
"

### item short map
item<-c("it_1_1", "it_1_2", "it_1_3", "it_1_4", "it_e1_1","it_e1_2",
        "it_2_1", "it_2_2", "it_2_3", "it_2_4", "it_e2_1","it_e2_2",
        "it_3_1", "it_3_2", "it_3_3", "it_3_4", "it_e3_1","it_e3_2" )
content<- c(1,1,-1,-1,1,-1,   1,1,-1,-1,1,-1,  1,1,-1,-1,1,-1)
sd  <- c(1,-1,1,-1,1,-1,   1,-1,1,-1,1,-1,  1,-1,1,-1,1,-1)
map<-cbind.data.frame(item,content, sd)



set.seed(123)
data1 <- simulateData(modelQuad, sample.nobs = 1000, 
                      ordered = item)

# convert to likert (random thresholds)
likert_levels <- c("1", "2", "3", "4", "5")
data1.1 <- as.data.frame(lapply(data1, function(x) as.numeric(cut(x, breaks=5, labels=likert_levels))))


# EFA without controling SD
EFA_3f<-fa(data1.1, nfactors=3,cor="poly",rotate = "oblimin")
print(EFA_3f)

# EFA with an extra factor
EFA_4f<-fa(data1.1, nfactors=4,cor="poly",rotate = "oblimin")
print(EFA_4f)

# ESEM - general confirmatory factor + 3 exploratory factors
ESEM_1DS_3f <- '
 SD =~  it_1_1+ it_1_2+ it_1_3+ it_1_4+ it_e1_1+it_e1_2+
        it_2_1+ it_2_2+ it_2_3+ it_2_4+ it_e2_1+it_e2_2+
        it_3_1+ it_3_2+ it_3_3+ it_3_4+ it_e3_1+it_e3_2 
    efa("efa1")*f1 + 
    efa("efa1")*f2 +
    efa("efa1")*f3  =~ it_1_1+ it_1_2+ it_1_3+ it_1_4+ it_e1_1+it_e1_2+
        it_2_1+ it_2_2+ it_2_3+ it_2_4+ it_e2_1+it_e2_2+
        it_3_1+ it_3_2+ it_3_3+ it_3_4+ it_e3_1+it_e3_2 
   SD ~~ 0*f1      
   SD ~~ 0*f2 
   SD ~~ 0*f3
'

ESEM_1DS_3f_fit <- sem(model = ESEM_1DS_3f, data = data1, rotation = "geomin")
summary(ESEM_1DS_3f_fit)


#CFA - bifactor
CFA_1DS_3f <- '
  SD =~  it_1_1+ it_1_2+ it_1_3+ it_1_4+ it_e1_1+it_e1_2+
         it_2_1+ it_2_2+ it_2_3+ it_2_4+ it_e2_1+it_e2_2+
         it_3_1+ it_3_2+ it_3_3+ it_3_4+ it_e3_1+it_e3_2 
   f1 =~ it_1_1+ it_1_2+ it_1_3+ it_1_4+ it_e1_1+it_e1_2
   f2 =~ it_2_1+ it_2_2+ it_2_3+ it_2_4+ it_e2_1+it_e2_2
   f3 =~ it_3_1+ it_3_2+ it_3_3+ it_3_4+ it_e3_1+it_e3_2
   
   SD ~~ 0*f1      
   SD ~~ 0*f2 
   SD ~~ 0*f3
'
CFA_1DS_3f_fit <- sem(model = CFA_1DS_3f, data = data1)
summary(CFA_1DS_3f_fit)


#CFA - two steps - first
CFA_2stp_1 <- '
  SD =~  it_1_1+ it_1_2+ it_1_3+ it_1_4+
         it_2_1+ it_2_2+ it_2_3+ it_2_4+ 
         it_3_1+ it_3_2+ it_3_3+ it_3_4 
   f1 =~ it_1_1+ it_1_2+ it_1_3+ it_1_4
   f2 =~ it_2_1+ it_2_2+ it_2_3+ it_2_4
   f3 =~ it_3_1+ it_3_2+ it_3_3+ it_3_4
   
   SD ~~ 0*f1      
   SD ~~ 0*f2 
   SD ~~ 0*f3
'
CFA_2stp_1_fit <- sem(model = CFA_2stp_1, data = data1)
summary(CFA_2stp_1_fit)


#CFA - two steps - second
CFA_2stp_2 <- '
  SD =~  1*it_1_1+ -1.35*it_1_2+ 1.30*it_1_3+ 
        -.90*it_1_4+   it_e1_1+it_e1_2+
         it_2_1+ it_2_2+ it_2_3+ it_2_4+ it_e2_1+it_e2_2+
         it_3_1+ it_3_2+ it_3_3+ it_3_4+ it_e3_1+it_e3_2
   f1 =~ it_1_1+ it_1_2+ it_1_3+ it_1_4+ it_e1_1+it_e1_2
   f2 =~ it_2_1+ it_2_2+ it_2_3+ it_2_4+ it_e2_1+it_e2_2
   f3 =~ it_3_1+ it_3_2+ it_3_3+ it_3_4+ it_e3_1+it_e3_2
   
   SD ~~ 0*f1      
   SD ~~ 0*f2 
   SD ~~ 0*f3
   SD ~~ .2*SD
'
CFA_2stp_2_fit <- sem(model = CFA_2stp_2, data = data1)
summary(CFA_2stp_2_fit)



# EFA target rotation
target_matrix <- matrix(c(
  1,  1,0,0,   # Item 1
  -1, 1,0,0,   # Item 2
  1,  -1,0,0,   # Item 3
  -1, -1,0,0,   # Item 4
   1,  1,0,0,   # Item 5
  -1, -1,0,0,   # Item 6
    1, 0,1,0,   # Item 7
  -1, 0,1,0,   # Item 8
  1,  0,-1,0,   # Item 9
  -1, 0,-1,0,   # Item 10
  1,  0,1,0,   # Item 11
  -1, 0,-1,0,   # Item 12
    1, 0,0,1,   # Item 13
  -1, 0,0,1,   # Item 14
  1, 0,0,-1,   # Item 15
  -1, 0,0,-1,   # Item 16
  1, 0,0,1,   # Item 17
  -1, 0,0,-1   # Item 18
), nrow = 18, byrow = T) 

target_matrixL <- list(target_matrix)

#oblique
efa_target_Ob <- fa(data1, nfactors = 4, rotate = "TargetQ", Target = target_matrixL)
print(efa_target_O)

#orthogonal
efa_target_Ot <- fa(data1, nfactors = 4, rotate = "TargetT", Target = target_matrix)
print(efa_target_Ot)




### overclaiming

#simulating the data
modelOverc <- " knw =~  1*it1 + -1*it2 + 1*it3 + -1*it4  +
                        1*it5 + -1*it6 + 1*it7 + -1*it8
                bias =~ 1*it1 + 1*it2 + 1*it3 + 1*it4 +
                        1*it5 + 1*it6 + 1*it7 + 1*it8
                knw ~~ .5*knw
                bias ~~ .3*bias
                bias ~~ 0*knw
                it1 | 0*t1
                it2 | -.5*t1 
                it3 | 0*t1 
                it4 | -.5*t1 
                it5 | 0*t1 
                it6 | -.5*t1 
                it7 | 0*t1 
                it8 | -.5*t1 
"

set.seed(123)
dataOv <- simulateData(modelOverc, sample.nobs = 1000, 
                      ordered = T)

dataOv<-dataOv-1

# detection signals
# foils
fake_items <- c('it2', 'it4', 'it6', 'it8')

# real
real_items <- setdiff(names(dataOv), fake_items)

#hits
dataOv$Hits <- (rowSums(dataOv[, real_items] == 1) + rowSums(dataOv[, fake_items] == 0) ) / 8
#dataOv$HitsAlter <- (rowSums(dataOv[, real_items] == 1) ) / 4

#false alarm
dataOv$False_Alarms <- (rowSums(dataOv[, fake_items] == 1))/ length(fake_items)

dataOv$HitsZ   <- scale(dataOv$Hits)
dataOv$False_AlarmsZ <- scale(dataOv$False_Alarms)

# accuracy d
dataOv$Accuracy <- dataOv$HitsZ - dataOv$False_AlarmsZ

# bias c
dataOv$Bias <- - ((dataOv$HitsZ + dataOv$False_AlarmsZ) / 2)


#mirt
modelMirt <- mirt::mirt(dataOV[,1:8], model=2)

