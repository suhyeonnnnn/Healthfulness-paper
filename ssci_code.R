
##ssci paper analysis code by suhyeon
## 2022 AMA winter

#cleansing


#library
library(dplyr)
library(readxl)

#data load
entry <- read.csv("C:/Users/sh921/Desktop/marketentry_backup.csv")
View(entry)

fis <- read.csv("C:/Users/sh921/Desktop/fissales.csv")
View(fis)

frame <- read_excel("C:/Users/sh921/Desktop/frame.xlsx")
View(frame)

frame2 <- read_excel("C:/Users/sh921/Desktop/frame2.xlsx")
View(frame)


#colum cleansing
fis <- fis[,-c(11:18)]

#merge by [company, brand, year, quater]
merge1 <- merge(x=frame, y=entry, by=c("company","brand","year","quarter"), all.x = T)
View(merge1)

merge2 <-  merge(x=merge1, y=fis, by=c("company","brand","year","quarter"), all.x = T)
View(merge2)

merge3 <-  merge(x=frame2, y=merge2, by=c("company","brand","year","quarter"), all.x = T)
View(merge3)


#mean by [compnay, brnad, q.trend]
merge3$mean_healthfulness <- with(merge3, ave(healthfulness, labeling, FUN=mean))
merge3$mean_size <- with(merge3, ave(size, labeling, FUN=mean))
merge3$mean_kcal <- with(merge3, ave(kcal, labeling, FUN=mean))

View(merge3)


###################################################################################################################################
#data load
health <- read.csv("C:/Users/sh921/Desktop/healthfulness.csv")
View(health)

dart <- read.csv("C:/Users/sh921/Desktop/dart.csv")
str(dart)

fop <- read.csv("C:/Users/sh921/Desktop/fop.csv")
View(fop)

control <- read.csv("C:/Users/sh921/Desktop/control.csv")
View(control)

#cleansing
names(control)[1] <- c("year")


#dart merge
mergeA <- merge(x=health, y=dart, by=c("company","year"), all.x = T)

##na 
sum(is.na(mergeA))
mergeA <- na.omit(mergeA)

#FOP
mergeB <- merge(x=mergeA, y=fop, by=c("company","brand"), all.x = T)
sum(is.na(mergeB)) 
mergeB <- na.omit(mergeB)#추후 fop 데이터 업데이트 할 예정
sum(is.na(mergeB)) 

#Control 
mergeC <- merge(x=mergeB, y=control, by=c("year"), all.x = T)
sum(is.na(mergeC))
mergeC <- na.omit(mergeC)
df <- mergeC
str(df)
#####카테고리 dummy 추가해야함

###############################################################################################################################################################
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
  df <- df %>% mutate(size2=(size+1)*tableuse/100)
  
  str(df)
 
write.csv(df, "C:/Users/sh921/Desktop/master.csv") 
#############################################################################################################################################3

  library(MASS)
  library(plm)
  
#ols

model <- lm(ln_fis~ln_fis.t.1+healthfulness+
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

#plm
model2 <- plm(ln_fis~ln_fis.t.1+healthfulness+
              product_name2+
              origin2+
              product_orientation2+
              process_orientation2+
              positive_gain2+
              negative_loss2+
              general_claim2+
              licensing_agreement2+
              +mktg_int+ln_ta+roa+leverage+obe+kcal2,
            data=df, model='random',index="brand")
summary(model2)

model3 <- plm(ln_fis~ln_fis.t.1+healthfulness+
                product_name2+
                origin2+
                product_orientation2+
                process_orientation2+
                positive_gain2+
                negative_loss2+
                general_claim2+
                licensing_agreement2+
                +mktg_int+ln_ta+roa+leverage+obe+kcal2,
              data=df, model='within',index="brand")
summary(model3)
