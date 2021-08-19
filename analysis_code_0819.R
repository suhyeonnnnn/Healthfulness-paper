#install.packages("corrgram")
library(readxl)
library(dplyr)
library(ggplot2)
library(dplyr)
library(MASS)
library(corrplot)
library(corrgram)
library(Hmisc)
library(PerformanceAnalytics)
library(plm)
library(car)
library(psych)

setwd("C:/Users/sh921/Desktop/SSCI data")
df <- read.csv("master_0819.csv")
str(df)
#health * table use
range(df$health)
df <- df %>% mutate(health2=(health-2)*tableuse/100)

#correlation



str(df)


cormatrix <- df %>% dplyr::select(sales, sales.t.1,health2, unregulated, foodgroup,nutrient_specific,licensed_mark,product_count
,npd_int.t.1,q.trend,mktg_int,ln_ta,roa,leverage,calories,BMI,cpi_food)

str(df)
corr <- df[,-c(1:2)]
str(corr)
corr <- cor(df)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(corrgram(cormatrix),method="shade", shade.col=NA, tl.col="black", 
         tl.srt=45,col=col(200), addCoef.col="black", addcolorlabel="no")





#install.packages("PerformanceAnalytics")
chart.Correlation(cormatrix, histogram=TRUE, pch=19)




#######################################################################

#lm kmac (change health) #
model1 <- lm(sales~sales.t.1+health
            +general_claim
            +foodgroup
            +nutrient_specific
            +licensed_mark
            +product_type
            +calories
            +mktg_int+ln_ta+roa+leverage+BMI,
            data=df)
summary(model1)

#change posi nega
model2 <- lm(sales~sales.t.1+health2
             +general_claim
             +foodgroup
             +positive_gain
             +negative_loss
             +licensed_mark
             +product_type
             +calories
             +mktg_int+ln_ta+roa+leverage+BMI,
             data=df)
summary(model2)

#jbr 
model3 <- lm(sales~sales.t.1+health2
            +unregulated
            +origin_country
            +ingredients
            +process
            +positive_gain
            +negative_loss
            +licensed_mark
            +product_type
            +calories
            +mktg_int+ln_ta+roa+leverage+BMI+cpi_food,
            data=df)
summary(model3)

#interaction health  and fop (kmac)
model4.5 <- lm(sales~sales.t.1
               +health2*general_claim
               +health2*foodgroup
               +health2*nutrient_specific
               +health2*licensed_mark
               +health2
             +general_claim
             +foodgroup
             +nutrient_specific
             +licensed_mark
             +product_type
             +calories
             +mktg_int+ln_ta+roa+leverage+BMI,
             data=df)
summary(model4.5)

#interaction health  and fop(jbr)
model4 <- lm(sales~sales.t.1
             +health2*mktg_int
             +health2*origin_country
             +health2*ingredients
             +health2*process
             +health2*positive_gain
             +health2*negative_loss
             +health2*licensed_mark
             +health2
             +unregulated
             +origin_country
             +ingredients
             +process
             +positive_gain
             +negative_loss
             +product_type
             +calories
             +mktg_int+ln_ta+roa+leverage+BMI,
             data=df)
summary(model4)
str(df)


#interaction health  and fop(0819)
model3 <- plm(sales~sales.t.1
              +health2
              +unregulated
              +foodgroup
              +nutrient_specific
              +licensed_mark
              +product_count
              +mktg_int+ln_ta+roa+leverage+calories+BMI+y.trend+cpi_food,
              data=df,model="within", index=c("company","brand","q.trend"))
summary(model3)


duplicated(df)

model4 <- lm(sales~
              +health2
              +unregulated
              +foodgroup
              +nutrient_specific
              +licensed_mark
              +product_count
              +mktg_int+ln_ta+roa+leverage+calories+BMI+y.trend+cpi_food,
              data=df)
summary(model4)

df <- unique(df, by = c("brand", "q.trend"))


#final model
model3 <- plm(sales~
              +health2
              +unregulated
              +foodgroup
              +nutrient_specific
              +licensed_mark
              +product_count
              +mktg_int+ln_ta+roa+leverage+calories+BMI+y.trend+cpi_food,
              data=df,model="random", index=c("brand","q.trend"))
summary(model3)

model3 <- plm(sales~sales.t.1
              +health2
              +unregulated
              +foodgroup
              +nutrient_specific
              +licensed_mark
              +product_count
              +mktg_int+ln_ta+roa+leverage+calories+BMI+y.trend+cpi_food,
              data=df,model="random", index=c("brand","q.trend"))
summary(model3)

model4 <- plm(sales~sales.t.1
             +health2
             +unregulated
             +foodgroup
             +nutrient_specific
             +licensed_mark
             +product_count
             +mktg_int+ln_ta+roa+leverage+calories+BMI+y.trend+cpi_food,
             data=df,model="within", index=c("brand","q.trend"))
summary(model4)
vif(model4)



phtest(model4, model3)
table(index(df), useNA= "ifany")

str(df)
table(df$company)
#health * product type(kmac)
model5.5 <- lm(sales~sales.t.1
               +health2*product_type
               +health2
               +general_claim
               +foodgroup
               +nutrient_specific
               +licensed_mark
               +product_type
               +calories
               +mktg_int+ln_ta+roa+leverage+BMI,
               data=df)
summary(model5.5)

#health * product type(jbr)
model5 <- lm(sales~sales.t.1
             +health*product_type
             +health
             +unregulated
             +origin_country
             +ingredients
             +process
             +positive_gain
             +negative_loss
             +licensed_mark
             +product_type
             +calories
             +mktg_int+ln_ta+roa+leverage+BMI,
             data=df)
summary(model5)

#fop * product type(kmac)
model6.5 <- lm(sales~sales.t.1
               +health*general_claim
               +health*foodgroup
               +health*nutrient_specific
               +health*licensed_mark
               +general_claim
               +foodgroup
               +nutrient_specific
               +licensed_mark
               +product_type
               +calories
               +mktg_int+ln_ta+roa+leverage+BMI,
               data=df)
summary(model6.5)

#fop * product type(jbr)
model6 <- lm(sales~sales.t.1
             +product_type*unregulated
             +product_type*origin_country
             +product_type*ingredients
             +product_type*process
             +product_type*positive_gain
             +product_type*negative_loss
             +product_type*licensed_mark
             +health
             +unregulated
             +origin_country
             +ingredients
             +process
             +positive_gain
             +negative_loss
             +product_type
             +calories
             +mktg_int+ln_ta+roa+leverage+BMI,
             data=df)
summary(model6)

#다중공선성 check

vif(model)

###################################################################
#describe
describe(df)

str(df)
df <- transform(df,
                      year12=ifelse(year=="2012",1,0),
                      year13=ifelse(year=="2013",1,0),
                      year14=ifelse(year=="2014",1,0),
                      year15=ifelse(year=="2015",1,0),
                      year16=ifelse(year=="2016",1,0),
                      year17=ifelse(year=="2017",1,0),
                      year18=ifelse(year=="2018",1,0),
                      year19=ifelse(year=="2019",1,0)
)
df <- transform(df,
                      quarter1=ifelse(quarter=="1",1,0),
                      quarter2=ifelse(quarter=="2",1,0),
                      quarter3=ifelse(quarter=="3",1,0),
                      quarter4=ifelse(quarter=="4",1,0)
)

str(df)
##################################################################
#model 0819 5pm meeting
#final model
model3 <- plm(sales~
                +health2
              +unregulated
              +foodgroup
              +nutrient_specific
              +licensed_mark
              +product_count
              +mktg_int+ln_ta+roa+leverage+calories+BMI+y.trend+cpi_food,
              data=df,model="random", index=c("brand","q.trend"))
summary(model3)

model3 <- plm(sales~sales.t.1
              +health2
              +unregulated
              +foodgroup
              +nutrient_specific
              +licensed_mark
              +product_count
              +mktg_int+ln_ta+roa+leverage+calories+BMI+y.trend+cpi_food,
              data=df,model="random", index=c("brand","q.trend"))
summary(model3)

model4 <- plm(sales~sales.t.1
              +health2
              +unregulated
              +foodgroup
              +nutrient_specific
              +licensed_mark
              +product_count
              +mktg_int+ln_ta+roa+leverage+calories+BMI+y.trend+cpi_food,
              data=df,model="within", index=c("brand","q.trend"))
summary(model4)

##################################################################
#model 0819 7pm meeting
model10 <- plm(sales~sales.t.1
              +health2
              +unregulated
              +foodgroup
              +nutrient_specific
              +licensed_mark
              +product_count
              +npd_int.t.1
              +q.trend
              +mktg_int+ln_ta+roa+leverage+calories+BMI+cpi_food,
              data=df,model="random", index=c("brand","q.trend"))
summary(model10)
str(df)
###################################################################
#standard errors
etable(est_c0, est_c1,se = )