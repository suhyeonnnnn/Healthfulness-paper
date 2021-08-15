
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
View(frame2)


#colum cleansing
fis <- fis[,-c(11:18)]

merge11 <- merge(x=fis, y=entry, by=c("company","brand","year","quarter"), all.x = T, all.y = T)
View(merge11)

merge3 <-  merge(x=frame2, y=merge11, by=c("company","brand","year","quarter"), all.x = T, all.y = T)
View(merge3)


#mean by [compnay, brnad, q.trend]
merge3$mean_healthfulness <- with(merge3, ave(healthfulness, labeling, FUN=mean))
merge3$mean_size <- with(merge3, ave(size, labeling, FUN=mean))
merge3$mean_kcal <- with(merge3, ave(kcal, labeling, FUN=mean))

View(merge3)

merge4 = merge3[!duplicated(merge3[,c('company','brand','labeling')]),] # 변수명으로 제거
View(merge4)


write.csv(merge4, "C:/Users/sh921/Desktop/healthfulness.csv") 

###################################################################################################################################
#data load
health <- read.csv("C:/Users/sh921/Desktop/healthfulness.csv")
View(health)

dart <- read.csv("C:/Users/sh921/Desktop/dart.csv")
str(dart)

fop <- read.csv("C:/Users/sh921/Desktop/fop_210807.csv")
View(fop)

control <- read.csv("C:/Users/sh921/Desktop/control.csv")
View(control)

#cleansing
names(control)[1] <- c("year(real)")


#dart merge
mergeA <- merge(x=health, y=dart, by=c("company","year"), all.x = T)

##na 
sum(is.na(mergeA))
View(mergeA) 
mergeA <- na.omit(mergeA)

library(naniar)
naniar::miss_var_summary(mergeB)

#FOP
mergeB <- merge(x=mergeA, y=fop, by=c("company","brand"), all.x = T)
sum(is.na(mergeB)) 


#Control 
mergeC <- merge(x=mergeB, y=control, by=c("year"), all.x = T)

sum(is.na(mergeC))


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
  df <- df %>% mutate(size2=size*tableuse/100)
  
  str(df)

####대표제품 healthfulness ver add ####################################################################################################
nutri_old <- read_xlsx("C:/Users/sh921/Desktop/nutriscore.xlsx")
str(nutri_old)

old <-  merge(x=df, y=nutri_old, by=c("brand","company"), all.x =T)
str(df)
sum(is.na(old))
naniar::miss_var_summary(df)
old<- na.omit(df)

range(old$healthfulness_brand)
old <- old %>% mutate(healthfulness3=(healthfulness_brand-1)*tableuse/100)

str(old)
model_old <- lm(ln_fis~ln_fis.t.1+healthfulness3+
              product_name2+
              origin2+
              product_orientation2+
              process_orientation2+
              positive_gain2+
              negative_loss2+
              general_claim2+
              licensing_agreement2+
              +mktg_int+ln_ta+roa+leverage+obe+kcal2,
            data=old)
summary(model_old)




########################################################################################################################################
library(ggplot2)
str(old)
ggplot(data=old,aes(x=heatlhfulenss_mean,y=ln_fis))+geom_point()
ggplot(data=old,aes(x=healthfulness_brand,y=ln_fis))+geom_point()

histo <- hist(old$heatlhfulenss_mean)
histo <- hist(old$healthfulness_brand)


########################################################################################################################################

write.csv(old, "C:/Users/sh921/Desktop/master2.csv") 

######################################################################################################################################
df <- read.csv("C:/Users/sh921/Desktop/master.csv")

master <- read_xlsx("C:/Users/sh921/Dropbox/2021 KCI/analysis/final_kmac.xlsx")

model1 <- lm(ln_fis~ln_fis.t.1+healthfulness4+nutrispec4+foodgroup4+sumindi4
             +mktg_int+dairy+snack+beverage+ln_ta+roa+leverage+obe,
             data=master)
summary(model1)


########################################################################################

str(df)

range(df$heatlhfulenss_mean)
df <- df %>% mutate(healthfulness3=(heatlhfulenss_mean-1)*tableuse/100)
range(df$healthfulness)
df <- df %>% mutate(healthfulness4=(healthfulness-2)^2)
df <- df %>% mutate(healthfulness5=(heatlhfulenss_mean-1)^2)
str(df)
library(dplyr)
library(MASS)
library(plm)
  
#ols

View(df)
str(df)
model <- lm(ln_fis~ln_fis.t.1+healthfulness5+
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

model <- lm(ln_fis~ln_fis.t.1+healthfulness2+
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


#install ttr
install.packages('TTR', dependencies=TRUE, repos='http://cran.rstudio.com/')
###################################################################
#mhealthuflnessverage
library(dplyr)
library(TTR)

#그룹별 이동평균 구하기, groupby Healthfulness 이용, n=3으로 설정
df %>% group_by(brand) %>% filter(n() >= 2) %>% mutate(healthfulness_runaverage = runMean(healthfulness, 2))
View(df)
write.csv(df,"C:/Users/sh921/Desktop/cum.csv")

sum(is.na(df))


df$health_cum = cumsum(c(0, df$healthfulness[-nrow(df)]))/df$q.trend

df %>% group_by(brand) %>% mutate(health_cum = cumsum(c(0, df$healthfulness[-nrow(df)]))/df$q.trend)

summary(df$health_cum)
model <- lm(ln_fis~ln_fis.t.1+health_cum+
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


####################
thelog <- read.csv("C:/Users/sh921/Desktop/TheLogGame.csv")
code <- read.csv("C:/Users/sh921/Desktop/GameNameCodebook.csv")
str(thelog)
str(code)
Thelog <-  merge(x=thelog, y=code, by=c("gameName"), all.x =T)
View(Thelog)
sum(is.na(Thelog))

write.csv(Thelog, "C:/Users/sh921/Desktop/TheLogGame.csv")
