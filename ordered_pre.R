# Packages
library(mlogit)
library(plyr)
library(dplyr)
library(AER)
library(MASS)

Traindata <- read.csv("D:/CVEN/9407/TrainDataset.csv", header = TRUE, sep = ',')
head(Traindata)
Traindata$satisfaction <- "dissatisfaction"

for (i in 1:nrow(Traindata)){
  if (Traindata$losatlc[i] > 7) {
    Traindata$satisfaction[i] = "complete"
  } else {
    if (Traindata$losatlc[i] > 4) {
      Traindata$satisfaction[i] = "medium"
    }
  }
}
Traindata <- na.omit(Traindata)

Traindata_1 <- subset(Traindata, Traindata$hifdip > 0)
Traindata_2 <- subset(Traindata, Traindata$hifdip == 0)
Traindata_1$hifdip <- log(Traindata_1$hifdip)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$tifdip >0)
Traindata_2 <- subset(Traindata, Traindata$tifdip == 0)
Traindata_1$tifdip <- log(Traindata_1$tifdip)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hsvalui > 0)
Traindata_2 <- subset(Traindata, Traindata$hsvalui == 0)
Traindata_1$hsvalui <- log(Traindata_1$hsvalui)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hxyncri >0)
Traindata_2 <- subset(Traindata, Traindata$hxyncri == 0)
Traindata_1$hxyncri <- log(Traindata_1$hxyncri)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hxyhlpi >0)
Traindata_2 <- subset(Traindata, Traindata$hxyhlpi == 0)
Traindata_1$hxyhlpi <- log(Traindata_1$hxyhlpi)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hxyedci >0)
Traindata_2 <- subset(Traindata, Traindata$hxyedci == 0)
Traindata_1$hxyedci <- log(Traindata_1$hxyedci)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hsrnti >0)
Traindata_2 <- subset(Traindata, Traindata$hsrnti == 0)
Traindata_1$hsrnti <- log(Traindata_1$hsrnti)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hsmgi >0)
Traindata_2 <- subset(Traindata, Traindata$hsmgi == 0)
Traindata_1$hsmgi <- log(Traindata_1$hsmgi)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hxypbti >0)
Traindata_2 <- subset(Traindata, Traindata$hxypbti == 0)
Traindata_1$hxypbti <- log(Traindata_1$hxypbti)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hxymvfi >0)
Traindata_2 <- subset(Traindata, Traindata$hxymvfi == 0)
Traindata_1$hxymvfi <- log(Traindata_1$hxymvfi)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)

Traindata_1 <- subset(Traindata, Traindata$hxymvri >0)
Traindata_2 <- subset(Traindata, Traindata$hxymvri == 0)
Traindata_1$hxymvri <- log(Traindata_1$hxymvri)
Traindata <-  rbind.data.frame(Traindata_1, Traindata_2)
write.csv(Traindata, "D:/CVEN/9407/report3/TrainDataset_ordered.csv")

Traindata <- read.csv("D:/CVEN/9407/report3/TrainDataset_ordered.csv", header = TRUE, sep = ',')
OrderedLogit_origin <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + CoupleW + Le_fnw + hhad10_4 + hhec10_5, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit_origin)
coefficients_origin <- coef(summary(OrderedLogit_origin))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit_origin)
BIC(OrderedLogit_origin)
print(coefficients_origin)

# confidence intervals
ci <- confint(OrderedLogit_origin)
print(ci)
# Odd Ratio
exp(coef(OrderedLogit_origin))
exp(cbind(OR = coef(OrderedLogit_origin), ci))

#delete hhad10_4
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + CoupleW + Le_fnw + hhec10_5, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

#delete CoupleW
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + Le_fnw + hhec10_5, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)

BIC(OrderedLogit)
print(coefficients)

###loop_1
variable_list <- read.csv("D:\\CVEN\\9407\\variable_names.csv", sep = ',', header = TRUE)
variable_list <- na.omit(variable_list)
STEP_1 <- matrix(c(0), ncol = 2)
colnames(STEP_1) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + Le_fnw + hhec10_5 +", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_1 <- rbind.data.frame(STEP_1, list(i, AIC_value))
}
  
STEP_1 <- STEP_1[-1, ]
STEP_1 <- as.data.frame(STEP_1)
variable_1 <- which.min(STEP_1$AIC_value)
STEP_1_variable <- subset(STEP_1[variable_1,])
print(STEP_1_variable)

#Add hsmgi
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + Le_fnw + hhec10_5 + hsmgi, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

variable_list <- subset(variable_list, variables != "hsmgi")

###loop_2
STEP_2 <- matrix(c(0), ncol = 2)
colnames(STEP_2) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + Le_fnw + hhec10_5 + hsmgi +", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_2 <- rbind.data.frame(STEP_2, list(i, AIC_value))
}

STEP_2 <- STEP_2[-1, ]
STEP_2 <- as.data.frame(STEP_2)
variable_2 <- which.min(STEP_2$AIC_value)
STEP_2_variable <- subset(STEP_2[variable_2,])
print(STEP_2_variable)

#Add hh10_14
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + Le_fnw + hhec10_5 + hsmgi + hh10_14, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

variable_list <- subset(variable_list, variables != "hh10_14")

###loop_3
STEP_3 <- matrix(c(0), ncol = 2)
colnames(STEP_3) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + Le_fnw + hhec10_5 + hsmgi + hh10_14 + ", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_3 <- rbind.data.frame(STEP_3, list(i, AIC_value))
}

STEP_3 <- STEP_3[-1, ]
STEP_3 <- as.data.frame(STEP_3)
variable_3 <- which.min(STEP_3$AIC_value)
STEP_3_variable <- subset(STEP_3[variable_3,])
print(STEP_3_variable)

#Add hhold
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + Le_fnw + hhec10_5 + hhold + hh10_14 + hsmgi, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

#Delete Le_fnw
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hhold + hh10_14 + hsmgi, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

variable_list <- subset(variable_list, variables != "hhold")

###loop_4
STEP_4 <- matrix(c(0), ncol = 2)
colnames(STEP_4) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hhold + hh10_14 + hsmgi +", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_4 <- rbind.data.frame(STEP_4, list(i, AIC_value))
}

STEP_4 <- STEP_4[-1, ]
STEP_4 <- as.data.frame(STEP_4)
variable_4 <- which.min(STEP_4$AIC_value)
STEP_4_variable <- subset(STEP_4[variable_4,])
print(STEP_4_variable)

variable_list <- subset(variable_list, variables != "hhec10_3")

#Add Le_sep
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hhold + hh10_14 + hsmgi + Le_sep, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

variable_list <- subset(variable_list, variables != "Le_sep")

###loop_5
STEP_5 <- matrix(c(0), ncol = 2)
colnames(STEP_5) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hhold + hh10_14 + hsmgi + Le_sep +", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_5 <- rbind.data.frame(STEP_5, list(i, AIC_value))
}

STEP_5 <- STEP_5[-1, ]
STEP_5 <- as.data.frame(STEP_5)
variable_5 <- which.min(STEP_5$AIC_value)
STEP_5_variable <- subset(STEP_5[variable_5,])
print(STEP_5_variable)

#Add tifdip
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hhold + hh10_14 + hsmgi + Le_sep + tifdip, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

variable_list <- subset(variable_list, variables != "tifdip")

###loop_6
STEP_6 <- matrix(c(0), ncol = 2)
colnames(STEP_6) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hhold + hh10_14 + hsmgi + Le_sep + tifdip +", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_6 <- rbind.data.frame(STEP_6, list(i, AIC_value))
}

STEP_6 <- STEP_6[-1, ]
STEP_6 <- as.data.frame(STEP_6)
variable_6 <- which.min(STEP_6$AIC_value)
STEP_6_variable <- subset(STEP_6[variable_6,])
print(STEP_6_variable)

#Add hgage
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hhold + hh10_14 + hsmgi + Le_sep + tifdip + hgage, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

#delete hhold
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hh10_14 + hsmgi + Le_sep + tifdip + hgage, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

variable_list <- subset(variable_list, variables != "hgage")

###loop_7
STEP_7 <- matrix(c(0), ncol = 2)
colnames(STEP_7) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hh10_14 + hsmgi + Le_sep + tifdip + hgage +", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_7 <- rbind.data.frame(STEP_7, list(i, AIC_value))
}

STEP_7 <- STEP_7[-1, ]
STEP_7 <- as.data.frame(STEP_7)
variable_7 <- which.min(STEP_7$AIC_value)
STEP_7_variable <- subset(STEP_7[variable_7,])
print(STEP_7_variable)

#Add Mltpljob
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hh10_14 + hsmgi + Le_sep + tifdip + hgage + Mltpljob, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

variable_list <- subset(variable_list, variables != "Mltpljob")

###loop_8
STEP_8 <- matrix(c(0), ncol = 2)
colnames(STEP_8) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hh10_14 + hsmgi + Le_sep + tifdip + hgage + Mltpljob +", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_8 <- rbind.data.frame(STEP_8, list(i, AIC_value))
}

STEP_8 <- STEP_8[-1, ]
STEP_8 <- as.data.frame(STEP_8)
variable_8 <- which.min(STEP_8$AIC_value)
STEP_8_variable <- subset(STEP_8[variable_8,])
print(STEP_8_variable)

#Add hhda10_4
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hh10_14 + hsmgi + Le_sep + tifdip + hgage + Mltpljob +
                       hhda10_4, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

variable_list <- subset(variable_list, variables != "hhda10_4")

###loop_9 ############################################################################################################
STEP_9 <- matrix(c(0), ncol = 2)
colnames(STEP_9) <- c("variable", "AIC_value")

for (i in variable_list[ ,1]) {
  data_list <- paste("as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hh10_14 + hsmgi + Le_sep + tifdip + hgage + Mltpljob +
                       hhda10_4 +", i)
  ordered <- polr(data_list, data = Traindata , Hess = TRUE)
  AIC_value <- AIC(ordered)
  STEP_9 <- rbind.data.frame(STEP_9, list(i, AIC_value))
}

STEP_9 <- STEP_9[-1, ]
STEP_9 <- as.data.frame(STEP_9)
variable_9 <- which.min(STEP_9$AIC_value)
STEP_9_variable <- subset(STEP_9[variable_9,])
print(STEP_9_variable)

#Add hxyedci
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hh10_14 + hsmgi + Le_sep + tifdip + hgage + Mltpljob +
                       hhda10_4 + hsrnti, 
                     data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)

#final model
OrderedLogit <- polr(as.factor(satisfaction) ~  lscom + lsvol + jbmsall + Renter + gh1 + hhec10_5 + hh10_14 + hsmgi + 
                       Le_sep + tifdip + hgage, data = Traindata, Hess=TRUE)
summary(OrderedLogit)
coefficients <- coef(summary(OrderedLogit))
p <- pnorm(abs(coefficients[,"t value"]), lower.tail = FALSE) * 2
coefficients <- cbind(coefficients, "p value" = p)
AIC(OrderedLogit)
BIC(OrderedLogit)
print(coefficients)
ci <- confint(OrderedLogit)
confint.default(OrderedLogit)
exp(coef(OrderedLogit))


#Simulation
testdata <- read.csv("D:/CVEN/9407/TestDataset.csv", header = TRUE, sep = ',')
head(testdata)
testdata$satisfaction <- "dissatisfaction"

for (i in 1:nrow(testdata)){
  if (testdata$losatlc[i] > 7) {
    testdata$satisfaction[i] = "complete"
  } else {
    if (testdata$losatlc[i] > 4) {
      testdata$satisfaction[i] = "medium"
    }
  }
}
testdata <- na.omit(testdata)

testdata_1 <- subset(testdata, testdata$hifdip > 0)
testdata_2 <- subset(testdata, testdata$hifdip == 0)
testdata_1$hifdip <- log(testdata_1$hifdip)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$tifdip >0)
testdata_2 <- subset(testdata, testdata$tifdip == 0)
testdata_1$tifdip <- log(testdata_1$tifdip)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hsvalui > 0)
testdata_2 <- subset(testdata, testdata$hsvalui == 0)
testdata_1$hsvalui <- log(testdata_1$hsvalui)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hxyncri >0)
testdata_2 <- subset(testdata, testdata$hxyncri == 0)
testdata_1$hxyncri <- log(testdata_1$hxyncri)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hxyhlpi >0)
testdata_2 <- subset(testdata, testdata$hxyhlpi == 0)
testdata_1$hxyhlpi <- log(testdata_1$hxyhlpi)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hxyedci >0)
testdata_2 <- subset(testdata, testdata$hxyedci == 0)
testdata_1$hxyedci <- log(testdata_1$hxyedci)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hsrnti >0)
testdata_2 <- subset(testdata, testdata$hsrnti == 0)
testdata_1$hsrnti <- log(testdata_1$hsrnti)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hsmgi >0)
testdata_2 <- subset(testdata, testdata$hsmgi == 0)
testdata_1$hsmgi <- log(testdata_1$hsmgi)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hxypbti >0)
testdata_2 <- subset(testdata, testdata$hxypbti == 0)
testdata_1$hxypbti <- log(testdata_1$hxypbti)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hxymvfi >0)
testdata_2 <- subset(testdata, testdata$hxymvfi == 0)
testdata_1$hxymvfi <- log(testdata_1$hxymvfi)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata_1 <- subset(testdata, testdata$hxymvri >0)
testdata_2 <- subset(testdata, testdata$hxymvri == 0)
testdata_1$hxymvri <- log(testdata_1$hxymvri)
testdata <-  rbind.data.frame(testdata_1, testdata_2)

testdata <- testdata %>% arrange(waveid)
write.csv(testdata, "D:/CVEN/9407/report3/testdata_ordered.csv")

fitted <- predict(OrderedLogit, newdata = testdata, type = 'p')
head(fitted)

### 

write.csv(fitted, "D:/CVEN/9407/report3/fitted_ordered.csv")

result <- read.csv("D:/CVEN/9407/report3/fitted_ordered.csv", header = TRUE, sep = ',')
head(result)
predict <- cbind.data.frame(testdata, result)
head(predict)
summary(predict$prediction)
table(predict$satisfaction)
sum(predict$satisfaction == predict$prediction)/nrow(predict)

