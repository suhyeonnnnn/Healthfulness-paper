#
getwd()
setwd("C:/Users/sh921/Desktop/SSCI data")

entry <- read.csv("marketentry_backup.csv")
View(entry)

fis <- read.csv("fissales.csv")

library(readxl)
frame2 <- read_xlsx("frame2.xlsx")
str(fis)
str(entry)

#colum cleansing
fis <- fis[,-c(11:18)]

#change variable name
names(entry)[6] <- c("year")


merge1 <- merge(x=fis, y=entry, by=c("company","brand","year","quarter"), all.x = T, all.y = T)


merge2 <-  merge(x=frame2, y=merge1, by=c("company","brand","year","quarter","q.trend","y.trend"), all.x = T, all.y = T)
View(merge2)

#분기별 중복치 평균치 처리 
#mean by [compnay, brnad, q.trend]
merge2$mean_healthfulness <- with(merge2, ave(healthfulness, labeling, FUN=mean))
merge2$mean_size <- with(merge2, ave(size, labeling, FUN=mean))
merge2$mean_kcal <- with(merge2, ave(kcal, labeling, FUN=mean))

# 변수명으로 제거
merge3 = merge2[!duplicated(merge2[,c('company','brand','labeling')]),] 
View(merge3)

write.csv(merge2, "healthfulness.csv") #Mean health, size, kcal NA행 삭제, FIS SALES NA 행 삭제

###################################################################################################

health <- read.csv("healthfulness.csv")

dart <- read.csv("dart.csv")

fop <- read.csv("fop.csv")

control <- read.csv("control.csv")

product <- read.csv("ProductType.csv")


str(control)
str(product)
names(control)[1] <- c("year(real)")


#dart merge
mergeA <- merge(x=health, y=dart, by=c("company","year"), all.x = T)
sum(is.na(mergeA))
mergeA <- na.omit(mergeA)

#FOP
mergeB <- merge(x=mergeA, y=fop, by=c("company","brand"), all.x = T)
sum(is.na(mergeB)) 

#Control 
mergeC <- merge(x=mergeB, y=control, by=c("year")("category", "subcatetory"), all.x = T)
sum(is.na(mergeC))

#Product Type
names(product)[2] <- c("subcategory")
str(product)
mergeD <- merge(x=mergeC, y=product, by=c("category", "subcategory"), all.x = T)
sum(is.na(mergeD))

df <- mergeD
str(df)
###################################################################################################

library(dplyr)

#varilable 생성

#numeric 변환
df$sga <- as.numeric((df$sga))
#roa
df <-  df %>% mutate(roa = ni/ta)
#ta log
df <-  df %>% mutate(ln_ta = log(ta),
                     ln_fis = log(fissales),
                     ln_ln_fis.t.1 = log(fissales.t.1))

#fis log : (fissales=0인 자료 존재 -> +1 후 로그)
df <-  df %>% mutate(ln_fis = log(fissales+1),
                     ln_fis.t.1 = log(fissales.t.1+1))
#mktg_int
df <-  df %>% mutate(mktg_int = sga/ta)
#leverage
df <-  df %>% mutate(leverage = liab/ta)

#tableuse
##fop
df <- df %>% mutate(product_name2=(product_name+1)*tableuse/100)
df <- df %>% mutate(origin2=(origin+1)*tableuse/100)
df <- df %>% mutate(product_orientation2=(product_orientation+1)*tableuse/100)
df <- df %>% mutate(process_orientation2=(process_orientation+1)*tableuse/100)
df <- df %>% mutate(positive_gain2=(positive_gain+1)*tableuse/100)
df <- df %>% mutate(negative_loss2=(negative_loss+1)*tableuse/100)
df <- df %>% mutate(general_claim2=(general_claim+1)*tableuse/100)
df <- df %>% mutate(licensing_agreement2=(licensing_agreement+1)*tableuse/100)

#product_name
#origin
#product_orientation
#process_orientation
#positive_gain
#negative_loss
#general_claim
#licensing_agreement

##healthfulness
range(df$healthfulness)
df <- df %>% mutate(healthfulness2=(healthfulness-2)*tableuse/100)

##kcal
range(df$kcal)
df <- df %>% mutate(kcal2=(kcal+1)*tableuse/100)

##size
range(df$size)
df <- df %>% mutate(size2=size*tableuse/100)

str(df)

write.csv(df, "master210816.csv") 


###################################################################################################
#healthfulness 가중치로 시계열
library(MASS)
df <- read.csv("master210816.csv") 
de <- read_excel("default.xlsx")
View(de)

merge11 <- merge(x=df, y=de, by=c("company","brand", "year", "quarter"), all.x = T)
View(merge11)
write.csv(merge11, "master210816.csv") 

###################################################################################################
#다중공선성 check
library(car)
vif(model)

#model fitting
df <- read.csv("master210816.csv") 

#lm
model <- lm(ln_fis~ln_fis.t.1+default+product_type+
                  product_name2+
                  origin2+
                  product_orientation2+
                  process_orientation2+
                  positive_gain2+
                  negative_loss2+
                  general_claim2+
                  licensing_agreement2+
                  +mktg_int+ln_ta+roa+leverage+obe+kcal2,
                data=df)
summary(model)

#다중공선성 check
install.packages("car")
library(car)
vif(model)

###INTERACTION RESULT

## health fop interaction 
#posi, negative 유의미
model1 <- lm(ln_fis~ln_fis.t.1
             +default*product_name2
             +default*origin2
             +default*product_orientation2
             +default*process_orientation2
             +default*positive_gain2
             +default*negative_loss2
             +default*general_claim2
             +default*licensing_agreement2
             +default+product_type+
               product_name2+
               origin2+
               product_orientation2+
               process_orientation2+
               positive_gain2+
               negative_loss2+
               general_claim2+
               licensing_agreement2+
               +mktg_int+ln_ta+roa+leverage+obe+kcal2,
             data=df)
summary(model1)

##health & control interaction
#health ln_ta significant
model2 <- lm(ln_fis~ln_fis.t.1+default*ln_ta+default+product_type+
                     product_name2+
                     origin2+
                     product_orientation2+
                     process_orientation2+
                     positive_gain2+
                     negative_loss2+
                     general_claim2+
                     licensing_agreement2+
                     +mktg_int+ln_ta+roa+leverage+obe+kcal2,
                   data=df)
summary(model2)

#health & Kcal significant
model3 <- lm(ln_fis~ln_fis.t.1+default*kcal2+default+
               product_name2+
               origin2+
               product_orientation2+
               process_orientation2+
               positive_gain2+
               negative_loss2+
               general_claim2+
               licensing_agreement2+
               +mktg_int+ln_ta+roa+leverage+obe+kcal2,
             data=df)
summary(model3)

#health &  누적제품수 significant
model4 <- lm(ln_fis~ln_fis.t.1+default*cum_count+default+cum_count+
               product_name2+
               origin2+
               product_orientation2+
               process_orientation2+
               positive_gain2+
               negative_loss2+
               general_claim2+
               licensing_agreement2+
               +mktg_int+ln_ta+roa+leverage+obe+kcal2,
             data=df)
summary(model4)

#health &  제품 타입 significant
model5 <- lm(ln_fis~ln_fis.t.1+default*product_type+default+product_type+
               product_name2+
               origin2+
               product_orientation2+
               process_orientation2+
               positive_gain2+
               negative_loss2+
               general_claim2+
               licensing_agreement2+
               +mktg_int+ln_ta+roa+leverage+obe+kcal2,
             data=df)
summary(model5)

#fop &  제품 타입 interaction
model5.5 <- lm(ln_fis~ln_fis.t.1
               +product_name2*product_type
               +origin2*product_type
               +product_orientation2*product_type
               +process_orientation2*product_type
               +positive_gain2*product_type
               +negative_loss2*product_type
               +general_claim2*product_type
               +licensing_agreement2*product_type
               +default+product_type+
               product_name2+
               origin2+
               product_orientation2+
               process_orientation2+
               positive_gain2+
               negative_loss2+
               general_claim2+
               licensing_agreement2+
               +mktg_int+ln_ta+roa+leverage+obe+kcal2,
             data=df)
summary(model5.5)

###개별로 돌려봄
#fop(제품명) &  제품 타입 interaction
model6 <- lm(ln_fis~ln_fis.t.1+product_name2*product_type+default+product_type+
               product_name2+
               origin2+
               product_orientation2+
               process_orientation2+
               positive_gain2+
               negative_loss2+
               general_claim2+
               licensing_agreement2+
               +mktg_int+ln_ta+roa+leverage+obe+kcal2,
             data=df)
summary(model6)

#fop(부정영양소 down 소구) &  제품 타입 interaction
model7 <- lm(ln_fis~ln_fis.t.1+ negative_loss2*product_type+default+product_type+
               product_name2+
               origin2+
               product_orientation2+
               process_orientation2+
               positive_gain2+
               negative_loss2+
               general_claim2+
               licensing_agreement2+
               +mktg_int+ln_ta+roa+leverage+obe+kcal2,
             data=df)
summary(model7)

#fop(라이센스) &  제품 타입 interaction
model8 <- lm(ln_fis~ln_fis.t.1+ licensing_agreement2*product_type+default+product_type+
               product_name2+
               origin2+
               product_orientation2+
               process_orientation2+
               positive_gain2+
               negative_loss2+
               general_claim2+
               licensing_agreement2+
               +mktg_int+ln_ta+roa+leverage+obe+kcal2,
             data=df)
summary(model8)
