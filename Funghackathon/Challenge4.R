library(tidyr)
library(RSQLite)
library('data.table')
library(readxl)
#devtools::install_github("hong-revo/glmnetUtils")
#library(glmnetUtils)
library(reshape2)
library("glmnet", lib.loc = "~/R/win-library/3.4")
library(plyr)
library(dplyr)
library(caret)
library(pROC)
Cotton <-
  as.data.table(
    read_excel(
      "C:/Users/Larry/Downloads/Challenge - Prediction/Challenge - Prediction/Economic Data - Cotton Prices by Index.xlsx"
    )
  )
Minwage <-
  as.data.table(
    read_excel(
      "C:/Users/Larry/Downloads/Challenge - Prediction/Challenge - Prediction/Economic Data - Minimum Wages by Country.xlsx"
    )
  )

CCompositionE <-
  as.data.table(
    read_excel(
      "C:/Users/Larry/Downloads/Challenge - Prediction/Challenge - Prediction/Cost Composition - Brand E excel.xlsx"
    )
  )

CCompositionR <-
  fread(
    "C:/Users/Larry/Downloads/Challenge - Prediction/Challenge - Prediction/Cost Composition - Brand R.csv"
  )
CCompositionS <-
  fread(
    "C:/Users/Larry/Downloads/Challenge - Prediction/Challenge - Prediction/Cost Composition - Brand S.csv"
  )

Cotton <-
  fread(
    "C:/Users/Larry/Downloads/Challenge - Prediction/Economic Data - Cotton Prices by Index.csv"
  )
Minwage <-
  fread(
    "C:/Users/Larry/Downloads/Challenge - Prediction/Economic Data - Minimum Wages by Country.csv"
  )

#Bind R and E
#rbind(CCompositionE,CCompositionR,CCompositionS)

names(CCompositionE)
names(CCompositionR)

names(CCompositionR)[1] <- names(CCompositionE)[3]
names(CCompositionR)[2] <- names(CCompositionE)[4]
names(CCompositionR)[3] <- names(CCompositionE)[11]
names(CCompositionR)[4] <- names(CCompositionE)[8]
names(CCompositionR)[5] <- names(CCompositionE)[9]
names(CCompositionR)[6] <- names(CCompositionE)[12]
names(CCompositionR)[7] <- names(CCompositionE)[10]
names(CCompositionR)[8] <- names(CCompositionE)[13]#other
names(CCompositionR)[9] <- names(CCompositionE)[13]#other
names(CCompositionR)[10] <- names(CCompositionE)[7]
names(CCompositionR)[11] <- names(CCompositionE)[6]
names(CCompositionR)[12] <- names(CCompositionE)[17]#need clean
#names(CCompositionR)[13]<- names(CCompositionE)[]**
names(CCompositionR)[14] <- names(CCompositionE)[18]



TableRE <- rbind(CCompositionE, CCompositionR, fill = TRUE)


####################################Start Data Cleaning
cotton.filp <- dcast(Cotton, Date ~ "Cotton Type", fun = mean)
names(cotton.filp)[2] <- 'price'
plot(cotton.filp, type = "l")

countryls <-
  list("CHINA", "INDONESIA", "INDIA", "BANGLADESH", "VIETNAM")
minwagedt <- data.table(NULL)
table.CW <- data.table(NULL)
cotton.filp <- dcast(Cotton, Date ~ "Cotton Type", fun = mean)
names(cotton.filp)[2] <- 'price'
plot(cotton.filp, type = "l")
for (i in 1:length(countryls)) {
  Minwage.temp <- as.data.table(Minwage[Country == countryls[i], ])
  Minwage.filp <-
    as.data.table(dcast(Minwage.temp, Date ~ "Country", fun = mean))
  names(Minwage.filp)[2] <- 'Min_wage'
  
  Minwage.filp [, . := countryls[i]]
  
  table.try <- merge(cotton.filp, Minwage.filp, all = TRUE)
  names(table.try)[length(table.try)] = "Country"
  table.CW <- rbind(table.CW, table.try)
}
table.CW[, . := month(table.CW$Date)]
names(table.CW)[length(table.CW)] <- "month"
table.CW[, . := year(table.CW$Date)]
names(table.CW)[length(table.CW)] <- "Year"
# sapply(CCompositionE[, (length(table.CW))], as.Date)
CCompositionE[, . := month(as.Date(CCompositionE$Date, "%d/%m/%Y"))]
names(CCompositionE)[length(CCompositionE)] <- "month"
CCompositionE[, . := year(as.Date(CCompositionE$Date, "%d/%m/%Y"))]
names(CCompositionE)[length(CCompositionE)] <- "Year"
CCompositionE$Country_Cd <-
  revalue(
    CCompositionE$Country_Cd,
    c(
      "ID" = "INDONESIA",
      "IN" = "INDIA",
      "BD" = "BANGLADESH",
      "VN" = "VIETNAM",
      "CN" = "CHINA"
    )
  )

for (i in names(CCompositionE)) {
  if (class(CCompositionE[, i]) %in% c("factor", "character")) {
    CCompositionE[, i] <- trimws(CCompositionE[, i])
  }
}
CCompositionE <-
  data.frame(lapply(CCompositionE, trimws), stringsAsFactors = FALSE)####



connlocaldb <- dbConnect(SQLite(), dbname = "~/table_CW.sqlite")
dbWriteTable(
  connlocaldb,
  "table_CW",
  table.CW,
  overwrite = TRUE,
  append = FALSE,
  field.types = NULL,
  temporary = FALSE
)
dbWriteTable(
  connlocaldb,
  "CCompositionE",
  CCompositionE,
  overwrite = TRUE,
  append = FALSE,
  field.types = NULL,
  temporary = FALSE
)
table.rcw <- data.table(
  dbGetQuery(
    connlocaldb,
    "Select
    *
    from CCompositionE  left join
    table_CW
    on CCompositionE.month = table_CW.month
    AND CCompositionE.Year = table_CW.Year
    AND CCompositionE.Country_Cd=table_CW.Year
    "
  )
  )





#################### End of data Cleaning

table.rcw <- data.table(
  dbGetQuery(
    connlocaldb,
    "Select
    Min_wage
    ,price
    ,Style_Estimated_Cost
    ,eaFabricCost
    ,eaCMCost
    ,TypeProduct_Desc
    ,Retail_Class_Desc
    ,Gender_Desc
    ,Country_Cd
    from CCompositionE  left join
    table_CW
    on CCompositionE.month = table_CW.month
    AND CCompositionE.Year = table_CW.Year
    AND CCompositionE.Country_Cd=table_CW.Country
    "
  )
  )


table.rcw[, i := seq_len(nrow(table.rcw))]

table.rcw1 <-
  dcast(
    table.rcw,
    i ~ TypeProduct_Desc,
    fun.aggregate = function(x)
      1L,
    fill = 0L,
    value.var = "TypeProduct_Desc"
  )
for (i in 2:length(names(table.rcw1))) {
  names(table.rcw1)[i] <-
    paste("TypeProduct_Desc", names(table.rcw1)[i], sep = "_")
}
table.rcw2 <-
  dcast(
    table.rcw,
    i ~ Retail_Class_Desc,
    fun.aggregate = function(x)
      1L,
    fill = 0L,
    value.var = "Retail_Class_Desc"
  )
for (i in 2:length(names(table.rcw2))) {
  names(table.rcw2)[i] <-
    paste("Retail_Class_Desc", names(table.rcw2)[i], sep = "_")
}
table.rcw3 <-
  dcast(
    table.rcw,
    i ~ Gender_Desc,
    fun.aggregate = function(x)
      1L,
    fill = 0L,
    value.var = "Gender_Desc"
  )
for (i in 2:length(names(table.rcw3))) {
  names(table.rcw3)[i] <-
    paste("Gender_Desc", names(table.rcw3)[i], sep = "_")
}
table.rcw4 <-
  dcast(
    table.rcw,
    i ~ Country_Cd,
    fun.aggregate = function(x)
      1L,
    fill = 0L,
    value.var = "Country_Cd"
  )
table.rcw.5 <- table.rcw[, 1:5]
table.rcwm <-
  cbind(table.rcw.5, table.rcw1, table.rcw2, table.rcw3, table.rcw4)
# End of Transformation
#table.rcw.5<-table.rcw[,1:(length(table.rcw)-1)]###
#table.rcwm<-cbind(table.rcw.5,table.rcw4)###
table.rcwm$Style_Estimated_Cost <-
  as.numeric(table.rcwm$Style_Estimated_Cost)
table.rcwm$eaFabricCost <- as.numeric(table.rcwm$eaFabricCost)
table.rcwm$eaCMCost <- as.numeric(table.rcwm$eaCMCost)
names(table.rcwm) <- sub(" ", ".", names(table.rcwm))


# Create a list of col
listcol <- str("")
for (i in 1:length(names(table.rcwm))) {
  temp <- names(table.rcwm)[i]
  listcol <- paste(listcol, temp, sep = "+")
}
rm(temp)
listcol
train_rows <-
  sample(1:(dim(table.rcwm)[1]), .5 * dim(table.rcwm)[1])
table.rcwm.sample <- table.rcwm[train_rows, ]
# End

#sample(table.rcwm,10000)

#logistic model

for (i in countryls) {
  formula.temp <-
    as.formula(paste(
      countryls[i],
      "~",
      trimws(
        "Min_wage+price+eaFOB+LandingCosts+i+Accessories+ACCESSORIES MISC+ACCESSORY+BAGS+BELTS+COLD WEATHER+COSTUMES+HAIR+HATS+JEWELRY+Knit+KNIT+Shoes+SHOES+Sleep+SLEEP+Socks+SOCKS+SUNGLASSES+Sweater+SWEATER+TIES+TIGHT+TOYS+UNDERWEAR+Woven+WOVEN+NA+i+Accessories+Active Tops+Bags+Basic Tops+Bibs+Blanket+Bottoms+Boy Sleepwear+Cold Weather+Costume+Denim Pants+Denim Shorts+Dresses+Girl Sleepwear+Graphic Tops+Hair+Hats+Jewelry+Jumpers+Knit Pants+Knit Shorts+Knit Tops+Mens Apparel+Misc+Onepiece+Onepieces+Other+Other Apparel+Other Non-Apparel+Outerwear+Overall/Shortall+Pants+Rompers+Sets+Shoes+Shorts+Skirts+Skorts+Sleep+Sleepwear+Socks+Sunglasses+Sweaters+Swim+Tights+Tops+Toys+Underwear+Uni Sleepwear+Womens Apparel+Woven Pants+Woven Shorts+Woven Tops+NA+i+Boy+Girl+Men+None+Uni+Women+NA+i+BANGLADESH+CHINA+GT+INDIA+INDONESIA+KE+KH+KR+LK+LS+SV+TH+TW+US+VIETNAM+NA"
      )
      ,
      sep = ""
    ))
}
#logistic model
#BANGLADESH
model.bangladesh <-
  glm(
    BANGLADESH ~ Min_wage + price + Style_Estimated_Cost + eaFabricCost + eaCMCost +
      TypeProduct_Desc_Accessories + TypeProduct_Desc_ACCESSORIES.MISC +
      TypeProduct_Desc_ACCESSORY + TypeProduct_Desc_BAGS + TypeProduct_Desc_BELTS +
      TypeProduct_Desc_COLD.WEATHER + TypeProduct_Desc_COSTUMES + TypeProduct_Desc_HAIR +
      TypeProduct_Desc_HATS + TypeProduct_Desc_JEWELRY + TypeProduct_Desc_Knit +
      TypeProduct_Desc_KNIT + TypeProduct_Desc_Shoes + TypeProduct_Desc_SHOES +
      TypeProduct_Desc_Sleep + TypeProduct_Desc_SLEEP + TypeProduct_Desc_Socks +
      TypeProduct_Desc_SOCKS + TypeProduct_Desc_SUNGLASSES + TypeProduct_Desc_Sweater +
      TypeProduct_Desc_SWEATER + TypeProduct_Desc_TIES + TypeProduct_Desc_TIGHT +
      TypeProduct_Desc_TOYS + TypeProduct_Desc_UNDERWEAR + TypeProduct_Desc_Woven +
      TypeProduct_Desc_WOVEN +
      Retail_Class_Desc_Accessories + Retail_Class_Desc_Active.Tops +
      Retail_Class_Desc_Bags + Retail_Class_Desc_Basic.Tops + Retail_Class_Desc_Bibs +
      Retail_Class_Desc_Blanket + Retail_Class_Desc_Bottoms + Retail_Class_Desc_Boy.Sleepwear +
      Retail_Class_Desc_Cold.Weather + Retail_Class_Desc_Costume +
      Retail_Class_Desc_Denim.Pants + Retail_Class_Desc_Denim.Shorts +
      Retail_Class_Desc_Dresses + Retail_Class_Desc_Girl.Sleepwear + Retail_Class_Desc_Graphic.Tops +
      Retail_Class_Desc_Hair + Retail_Class_Desc_Hats + Retail_Class_Desc_Jewelry +
      Retail_Class_Desc_Jumpers + Retail_Class_Desc_Knit.Pants + Retail_Class_Desc_Knit.Shorts +
      Retail_Class_Desc_Knit.Tops + Retail_Class_Desc_Mens.Apparel + Retail_Class_Desc_Misc +
      Retail_Class_Desc_Onepiece +
      Retail_Class_Desc_Onepieces + Retail_Class_Desc_Other + Retail_Class_Desc_Other.Apparel +
      Retail_Class_Desc_Outerwear + Retail_Class_Desc_Pants + Retail_Class_Desc_Rompers +
      Retail_Class_Desc_Sets + Retail_Class_Desc_Shoes + Retail_Class_Desc_Shorts +
      Retail_Class_Desc_Skirts +
      Retail_Class_Desc_Skorts + Retail_Class_Desc_Sleep + Retail_Class_Desc_Sleepwear +
      Retail_Class_Desc_Socks + Retail_Class_Desc_Sunglasses + Retail_Class_Desc_Sweaters +
      Retail_Class_Desc_Swim + Retail_Class_Desc_Tights + Retail_Class_Desc_Tops +
      Retail_Class_Desc_Toys + Retail_Class_Desc_Underwear + Retail_Class_Desc_Uni.Sleepwear +
      Retail_Class_Desc_Womens.Apparel + Retail_Class_Desc_Woven.Pants + Retail_Class_Desc_Woven.Shorts +
      Retail_Class_Desc_Woven.Tops +
      Gender_Desc_Boy + Gender_Desc_Girl + Gender_Desc_Men + Gender_Desc_None +
      Gender_Desc_Uni + Gender_Desc_Women,
    family = binomial(link = "logit"),
    data = table.rcwm.sample
  )
summary(model.bangladesh) # display results
confint(model.bangladesh) # 95% CI for the coefficients
model.bangladesh.fit <-
  coef(model.bangladesh) # exponentiated coefficients
exp(confint(model.bangladesh)) # 95% CI for exponentiated coefficients
predict(model.bangladesh, type = "response") # predicted values
residuals(model.bangladesh, type = "deviance") # residuals

#tranpose the coef table
model.bangladesh.fit.df <- as.data.frame(model.bangladesh.fit)
model.bangladesh.fit.dft <-
  as.data.frame(t(model.bangladesh.fit.df[, 1]))
rownames(model.bangladesh.fit.df)
for (i in 1:length(model.bangladesh.fit.dft)) {
  names(model.bangladesh.fit.dft)[i] <- rownames(model.bangladesh.fit.df)[i]
  
}




#CHINA
model.china <-
  glm(
    CHINA ~ Min_wage + price + Style_Estimated_Cost + eaFabricCost + eaCMCost +
      TypeProduct_Desc_Accessories + TypeProduct_Desc_ACCESSORIES.MISC +
      TypeProduct_Desc_ACCESSORY + TypeProduct_Desc_BAGS + TypeProduct_Desc_BELTS +
      TypeProduct_Desc_COLD.WEATHER + TypeProduct_Desc_COSTUMES + TypeProduct_Desc_HAIR +
      TypeProduct_Desc_HATS + TypeProduct_Desc_JEWELRY + TypeProduct_Desc_Knit +
      TypeProduct_Desc_KNIT + TypeProduct_Desc_Shoes + TypeProduct_Desc_SHOES +
      TypeProduct_Desc_Sleep + TypeProduct_Desc_SLEEP + TypeProduct_Desc_Socks +
      TypeProduct_Desc_SOCKS + TypeProduct_Desc_SUNGLASSES + TypeProduct_Desc_Sweater +
      TypeProduct_Desc_SWEATER + TypeProduct_Desc_TIES + TypeProduct_Desc_TIGHT +
      TypeProduct_Desc_TOYS + TypeProduct_Desc_UNDERWEAR + TypeProduct_Desc_Woven +
      TypeProduct_Desc_WOVEN +
      Retail_Class_Desc_Accessories + Retail_Class_Desc_Active.Tops +
      Retail_Class_Desc_Bags + Retail_Class_Desc_Basic.Tops + Retail_Class_Desc_Bibs +
      Retail_Class_Desc_Blanket + Retail_Class_Desc_Bottoms + Retail_Class_Desc_Boy.Sleepwear +
      Retail_Class_Desc_Cold.Weather + Retail_Class_Desc_Costume +
      Retail_Class_Desc_Denim.Pants + Retail_Class_Desc_Denim.Shorts +
      Retail_Class_Desc_Dresses + Retail_Class_Desc_Girl.Sleepwear + Retail_Class_Desc_Graphic.Tops +
      Retail_Class_Desc_Hair + Retail_Class_Desc_Hats +
      Retail_Class_Desc_Jewelry + Retail_Class_Desc_Jumpers + Retail_Class_Desc_Knit.Pants +
      Retail_Class_Desc_Knit.Shorts + Retail_Class_Desc_Knit.Tops + Retail_Class_Desc_Mens.Apparel +
      Retail_Class_Desc_Misc + Retail_Class_Desc_Onepiece +
      Retail_Class_Desc_Onepieces + Retail_Class_Desc_Other +
      Retail_Class_Desc_Other.Apparel +
      Retail_Class_Desc_Outerwear + Retail_Class_Desc_Pants +
      Retail_Class_Desc_Rompers + Retail_Class_Desc_Sets + Retail_Class_Desc_Shoes +
      Retail_Class_Desc_Shorts + Retail_Class_Desc_Skirts +
      Retail_Class_Desc_Skorts + Retail_Class_Desc_Sleep +
      Retail_Class_Desc_Sleepwear + Retail_Class_Desc_Socks + Retail_Class_Desc_Sunglasses +
      Retail_Class_Desc_Sweaters +
      Retail_Class_Desc_Swim + Retail_Class_Desc_Tights +
      Retail_Class_Desc_Tops + Retail_Class_Desc_Toys + Retail_Class_Desc_Underwear +
      Retail_Class_Desc_Uni.Sleepwear + Retail_Class_Desc_Womens.Apparel + Retail_Class_Desc_Woven.Pants +
      Retail_Class_Desc_Woven.Shorts + Retail_Class_Desc_Woven.Tops +
      Gender_Desc_Boy + Gender_Desc_Girl + Gender_Desc_Men +
      Gender_Desc_None + Gender_Desc_Uni + Gender_Desc_Women
    ,
    family = binomial(link = "logit"),
    data = table.rcwm.sample
  )
summary(model.china) # display results
confint(model.china) # 95% CI for the coefficients
model.china.fit <- coef(model.china) # exponentiated coefficients
exp(confint(model.china)) # 95% CI for exponentiated coefficients
predict(model.china, type = "response") # predicted values
residuals(model.china, type = "deviance")

#tranpose the coef table
model.china.fit.df <- as.data.frame(model.china.fit)
model.china.fit.dft <-
  as.data.frame(t(model.china.fit.df[, 1]))
rownames(model.china.fit.df)
for (i in 1:length(model.china.fit.dft)) {
  names(model.china.fit.dft)[i] <- rownames(model.china.fit.df)[i]
  
}



#INDIA
model.india <-
  glm(
    INDIA ~ Min_wage + price + Style_Estimated_Cost + eaFabricCost + eaCMCost +
      TypeProduct_Desc_Accessories + TypeProduct_Desc_ACCESSORIES.MISC +
      TypeProduct_Desc_ACCESSORY + TypeProduct_Desc_BAGS + TypeProduct_Desc_BELTS +
      TypeProduct_Desc_COLD.WEATHER + TypeProduct_Desc_COSTUMES + TypeProduct_Desc_HAIR +
      TypeProduct_Desc_HATS + TypeProduct_Desc_JEWELRY + TypeProduct_Desc_Knit +
      TypeProduct_Desc_KNIT + TypeProduct_Desc_Shoes + TypeProduct_Desc_SHOES +
      TypeProduct_Desc_Sleep + TypeProduct_Desc_SLEEP + TypeProduct_Desc_Socks +
      TypeProduct_Desc_SOCKS + TypeProduct_Desc_SUNGLASSES + TypeProduct_Desc_Sweater +
      TypeProduct_Desc_SWEATER + TypeProduct_Desc_TIES + TypeProduct_Desc_TIGHT +
      TypeProduct_Desc_TOYS + TypeProduct_Desc_UNDERWEAR + TypeProduct_Desc_Woven +
      TypeProduct_Desc_WOVEN +
      Retail_Class_Desc_Accessories + Retail_Class_Desc_Active.Tops +
      Retail_Class_Desc_Bags + Retail_Class_Desc_Basic.Tops + Retail_Class_Desc_Bibs +
      Retail_Class_Desc_Blanket + Retail_Class_Desc_Bottoms + Retail_Class_Desc_Boy.Sleepwear +
      Retail_Class_Desc_Cold.Weather + Retail_Class_Desc_Costume +
      Retail_Class_Desc_Denim.Pants + Retail_Class_Desc_Denim.Shorts +
      Retail_Class_Desc_Dresses + Retail_Class_Desc_Girl.Sleepwear + Retail_Class_Desc_Graphic.Tops +
      Retail_Class_Desc_Hair + Retail_Class_Desc_Hats +
      Retail_Class_Desc_Jewelry + Retail_Class_Desc_Jumpers + Retail_Class_Desc_Knit.Pants +
      Retail_Class_Desc_Knit.Shorts + Retail_Class_Desc_Knit.Tops + Retail_Class_Desc_Mens.Apparel +
      Retail_Class_Desc_Misc + Retail_Class_Desc_Onepiece +
      Retail_Class_Desc_Onepieces + Retail_Class_Desc_Other +
      Retail_Class_Desc_Other.Apparel +
      Retail_Class_Desc_Outerwear + Retail_Class_Desc_Pants +
      Retail_Class_Desc_Rompers + Retail_Class_Desc_Sets + Retail_Class_Desc_Shoes +
      Retail_Class_Desc_Shorts + Retail_Class_Desc_Skirts +
      Retail_Class_Desc_Skorts + Retail_Class_Desc_Sleep +
      Retail_Class_Desc_Sleepwear + Retail_Class_Desc_Socks + Retail_Class_Desc_Sunglasses +
      Retail_Class_Desc_Sweaters +
      Retail_Class_Desc_Swim + Retail_Class_Desc_Tights +
      Retail_Class_Desc_Tops + Retail_Class_Desc_Toys + Retail_Class_Desc_Underwear +
      Retail_Class_Desc_Uni.Sleepwear + Retail_Class_Desc_Womens.Apparel + Retail_Class_Desc_Woven.Pants +
      Retail_Class_Desc_Woven.Shorts + Retail_Class_Desc_Woven.Tops +
      Gender_Desc_Boy + Gender_Desc_Girl + Gender_Desc_Men +
      Gender_Desc_None + Gender_Desc_Uni + Gender_Desc_Women
    ,
    family = binomial(link = "logit"),
    data = table.rcwm.sample
  )
summary(model.india) # display results
confint(model.india) # 95% CI for the coefficients
model.india.fit <- coef(model.india) # exponentiated coefficients
exp(confint(model.india)) # 95% CI for exponentiated coefficients
predict(model.india, type = "response") # predicted values
residuals(model.india, type = "deviance")

#tranpose the coef table
model.india.fit.df <- as.data.frame(model.india.fit)
model.india.fit.dft <-
  as.data.frame(t(model.india.fit.df[, 1]))
rownames(model.india.fit.df)
for (i in 1:length(model.india.fit.dft)) {
  names(model.india.fit.dft)[i] <- rownames(model.india.fit.df)[i]
  
}

#INDONESIA
model.indonesia <- glm(
  INDONESIA ~ Min_wage + price + Style_Estimated_Cost + eaFabricCost + eaCMCost +
    TypeProduct_Desc_Accessories + TypeProduct_Desc_ACCESSORIES.MISC +
    TypeProduct_Desc_ACCESSORY + TypeProduct_Desc_BAGS + TypeProduct_Desc_BELTS +
    TypeProduct_Desc_COLD.WEATHER + TypeProduct_Desc_COSTUMES + TypeProduct_Desc_HAIR +
    TypeProduct_Desc_HATS + TypeProduct_Desc_JEWELRY + TypeProduct_Desc_Knit +
    TypeProduct_Desc_KNIT + TypeProduct_Desc_Shoes + TypeProduct_Desc_SHOES +
    TypeProduct_Desc_Sleep + TypeProduct_Desc_SLEEP + TypeProduct_Desc_Socks +
    TypeProduct_Desc_SOCKS + TypeProduct_Desc_SUNGLASSES + TypeProduct_Desc_Sweater +
    TypeProduct_Desc_SWEATER + TypeProduct_Desc_TIES + TypeProduct_Desc_TIGHT +
    TypeProduct_Desc_TOYS + TypeProduct_Desc_UNDERWEAR + TypeProduct_Desc_Woven +
    TypeProduct_Desc_WOVEN +
    Retail_Class_Desc_Accessories + Retail_Class_Desc_Active.Tops +
    Retail_Class_Desc_Bags + Retail_Class_Desc_Basic.Tops + Retail_Class_Desc_Bibs +
    Retail_Class_Desc_Blanket + Retail_Class_Desc_Bottoms + Retail_Class_Desc_Boy.Sleepwear +
    Retail_Class_Desc_Cold.Weather + Retail_Class_Desc_Costume +
    Retail_Class_Desc_Denim.Pants + Retail_Class_Desc_Denim.Shorts +
    Retail_Class_Desc_Dresses + Retail_Class_Desc_Girl.Sleepwear + Retail_Class_Desc_Graphic.Tops +
    Retail_Class_Desc_Hair + Retail_Class_Desc_Hats +
    Retail_Class_Desc_Jewelry + Retail_Class_Desc_Jumpers + Retail_Class_Desc_Knit.Pants +
    Retail_Class_Desc_Knit.Shorts + Retail_Class_Desc_Knit.Tops + Retail_Class_Desc_Mens.Apparel +
    Retail_Class_Desc_Misc + Retail_Class_Desc_Onepiece +
    Retail_Class_Desc_Onepieces + Retail_Class_Desc_Other +
    Retail_Class_Desc_Other.Apparel +
    Retail_Class_Desc_Outerwear + Retail_Class_Desc_Pants +
    Retail_Class_Desc_Rompers + Retail_Class_Desc_Sets + Retail_Class_Desc_Shoes +
    Retail_Class_Desc_Shorts + Retail_Class_Desc_Skirts +
    Retail_Class_Desc_Skorts + Retail_Class_Desc_Sleep +
    Retail_Class_Desc_Sleepwear + Retail_Class_Desc_Socks + Retail_Class_Desc_Sunglasses +
    Retail_Class_Desc_Sweaters +
    Retail_Class_Desc_Swim + Retail_Class_Desc_Tights +
    Retail_Class_Desc_Tops + Retail_Class_Desc_Toys + Retail_Class_Desc_Underwear +
    Retail_Class_Desc_Uni.Sleepwear + Retail_Class_Desc_Womens.Apparel + Retail_Class_Desc_Woven.Pants +
    Retail_Class_Desc_Woven.Shorts + Retail_Class_Desc_Woven.Tops +
    Gender_Desc_Boy + Gender_Desc_Girl + Gender_Desc_Men +
    Gender_Desc_None + Gender_Desc_Uni + Gender_Desc_Women
  ,
  family = binomial(link = "logit"),
  data = table.rcwm.sample
)
summary(model.indonesia) # display results
confint(model.indonesia) # 95% CI for the coefficients
model.indonesia.fit <-
  coef(model.indonesia) # exponentiated coefficients
exp(confint(model.indonesia)) # 95% CI for exponentiated coefficients
predict(model.indonesia, type = "response") # predicted values
residuals(model.indonesia, type = "deviance")
#tranpose the coef table
model.indonesia.fit.df <- as.data.frame(model.indonesia.fit)
model.indonesia.fit.dft <-
  as.data.frame(t(model.indonesia.fit.df[, 1]))
rownames(model.indonesia.fit.df)
for (i in 1:length(model.indonesia.fit.dft)) {
  names(model.indonesia.fit.dft)[i] <- rownames(model.indonesia.fit.df)[i]
  
}


#VIETNAM
model.vietnam <-
  glm(
    VIETNAM ~ Min_wage + price + Style_Estimated_Cost + eaFabricCost + eaCMCost +
      TypeProduct_Desc_Accessories + TypeProduct_Desc_ACCESSORIES.MISC +
      TypeProduct_Desc_ACCESSORY + TypeProduct_Desc_BAGS + TypeProduct_Desc_BELTS +
      TypeProduct_Desc_COLD.WEATHER + TypeProduct_Desc_COSTUMES + TypeProduct_Desc_HAIR +
      TypeProduct_Desc_HATS + TypeProduct_Desc_JEWELRY + TypeProduct_Desc_Knit +
      TypeProduct_Desc_KNIT + TypeProduct_Desc_Shoes + TypeProduct_Desc_SHOES +
      TypeProduct_Desc_Sleep + TypeProduct_Desc_SLEEP + TypeProduct_Desc_Socks +
      TypeProduct_Desc_SOCKS + TypeProduct_Desc_SUNGLASSES + TypeProduct_Desc_Sweater +
      TypeProduct_Desc_SWEATER + TypeProduct_Desc_TIES + TypeProduct_Desc_TIGHT +
      TypeProduct_Desc_TOYS + TypeProduct_Desc_UNDERWEAR + TypeProduct_Desc_Woven +
      TypeProduct_Desc_WOVEN +
      Retail_Class_Desc_Accessories + Retail_Class_Desc_Active.Tops +
      Retail_Class_Desc_Bags + Retail_Class_Desc_Basic.Tops + Retail_Class_Desc_Bibs +
      Retail_Class_Desc_Blanket + Retail_Class_Desc_Bottoms + Retail_Class_Desc_Boy.Sleepwear +
      Retail_Class_Desc_Cold.Weather + Retail_Class_Desc_Costume +
      Retail_Class_Desc_Denim.Pants + Retail_Class_Desc_Denim.Shorts +
      Retail_Class_Desc_Dresses + Retail_Class_Desc_Girl.Sleepwear + Retail_Class_Desc_Graphic.Tops +
      Retail_Class_Desc_Hair + Retail_Class_Desc_Hats +
      Retail_Class_Desc_Jewelry + Retail_Class_Desc_Jumpers + Retail_Class_Desc_Knit.Pants +
      Retail_Class_Desc_Knit.Shorts + Retail_Class_Desc_Knit.Tops + Retail_Class_Desc_Mens.Apparel +
      Retail_Class_Desc_Misc + Retail_Class_Desc_Onepiece +
      Retail_Class_Desc_Onepieces + Retail_Class_Desc_Other +
      Retail_Class_Desc_Other.Apparel +
      Retail_Class_Desc_Outerwear + Retail_Class_Desc_Pants +
      Retail_Class_Desc_Rompers + Retail_Class_Desc_Sets + Retail_Class_Desc_Shoes +
      Retail_Class_Desc_Shorts + Retail_Class_Desc_Skirts +
      Retail_Class_Desc_Skorts + Retail_Class_Desc_Sleep +
      Retail_Class_Desc_Sleepwear + Retail_Class_Desc_Socks + Retail_Class_Desc_Sunglasses +
      Retail_Class_Desc_Sweaters +
      Retail_Class_Desc_Swim + Retail_Class_Desc_Tights +
      Retail_Class_Desc_Tops + Retail_Class_Desc_Toys + Retail_Class_Desc_Underwear +
      Retail_Class_Desc_Uni.Sleepwear + Retail_Class_Desc_Womens.Apparel + Retail_Class_Desc_Woven.Pants +
      Retail_Class_Desc_Woven.Shorts + Retail_Class_Desc_Woven.Tops +
      Gender_Desc_Boy + Gender_Desc_Girl + Gender_Desc_Men +
      Gender_Desc_None + Gender_Desc_Uni + Gender_Desc_Women
    ,
    family = binomial(link = "logit"),
    data = table.rcwm.sample
  )
summary(model.vietnam) # display results
confint(model.vietnam) # 95% CI for the coefficients
model.vietnam.fit <-
  coef(model.vietnam) # exponentiated coefficients
exp(confint(model.vietnam)) # 95% CI for exponentiated coefficients
predict(model.vietnam, type = "response") # predicted values
residuals(model.vietnam, type = "deviance")


#tranpose the coef table
model.vietnam.fit.df <- as.data.frame(model.vietnam.fit)
model.vietnam.fit.dft <-
  as.data.frame(t(model.vietnam.fit.df[, 1]))
rownames(model.vietnam.fit.df)
for (i in 1:length(model.vietnam.fit.dft)) {
  names(model.vietnam.fit.dft)[i] <- rownames(model.vietnam.fit.df)[i]
  
}




########End of logistic Model

model.vietnam.fit
pred.table <- as.data.frame(NULL)
names$pred.table
`$`(df , paste0("V1"))
