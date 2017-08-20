
# in Shiny App
result.table<-data.frame("Country"=NULL,"probability"=NULL)
inputshiny <- c("Min_wage", "Gender_Desc_Girl")
#Vietnam
value <-
  model.vietnam.fit.dft[which(names(model.vietnam.fit.dft) == "(Intercept)")][[1]]
Min_wage <-
  model.vietnam.fit.dft[which(names(model.vietnam.fit.dft) == "Min_wage")][[1]] *
  1000
price <-
  model.vietnam.fit.dft[which(names(model.vietnam.fit.dft) == "price")][[1]]
value <- value + Min_wage + price
for (i in inputshiny) {
  value <-
    value + model.vietnam.fit.dft[which(names(model.vietnam.fit.dft) == inputshiny[1])][[1]]
  
}
prob<-(exp(-1 * value) / (1 + exp(-1 * value)))
result.table.temp<-data.frame("Country"="VIETNAM","probability"=prob)
result.table<-rbind(result.table,result.table.temp)

# BANGLADESH
value <-
  model.bangladesh.fit.dft[which(names(model.bangladesh.fit.dft) == "(Intercept)")][[1]]
Min_wage <-
  model.bangladesh.fit.dft[which(names(model.bangladesh.fit.dft) == "Min_wage")][[1]] *
  1000
price <-
  model.bangladesh.fit.dft[which(names(model.bangladesh.fit.dft) == "price")][[1]]
value <- value + Min_wage + price
for (i in inputshiny) {
  value <-
    value + model.bangladesh.fit.dft[which(names(model.bangladesh.fit.dft) == inputshiny[1])][[1]]
  
}
prob<-(exp(-1 * value) / (1 + exp(-1 * value)))
result.table.temp<-data.frame("Country"="BANGLADESH","probability"=prob)
result.table<-rbind(result.table,result.table.temp)

#China
value <-
  model.china.fit.dft[which(names(model.china.fit.dft) == "(Intercept)")][[1]]
Min_wage <-
  model.china.fit.dft[which(names(model.china.fit.dft) == "Min_wage")][[1]] *
  1000
price <-
  model.china.fit.dft[which(names(model.china.fit.dft) == "price")][[1]]
value <- value + Min_wage + price
for (i in inputshiny) {
  value <-
    value + model.china.fit.dft[which(names(model.china.fit.dft) == inputshiny[1])][[1]]
  
}
prob<-(exp(-1 * value) / (1 + exp(-1 * value)))
result.table.temp<-data.frame("Country"="CHINA","probability"=prob)
result.table<-rbind(result.table,result.table.temp)

# INDIA
value <-
  model.india.fit.dft[which(names(model.india.fit.dft) == "(Intercept)")][[1]]
Min_wage <-
  model.india.fit.dft[which(names(model.india.fit.dft) == "Min_wage")][[1]] *
  1000
price <-
  model.india.fit.dft[which(names(model.india.fit.dft) == "price")][[1]]
value <- value + Min_wage + price
for (i in inputshiny) {
  value <-
    value + model.india.fit.dft[which(names(model.india.fit.dft) == inputshiny[1])][[1]]
  
}
prob<-(exp(-1 * value) / (1 + exp(-1 * value)))
result.table.temp<-data.frame("Country"="INDIA","probability"=prob)
result.table<-rbind(result.table,result.table.temp)

# INDONESIA
value <-
  model.indonesia.fit.dft[which(names(model.indonesia.fit.dft) == "(Intercept)")][[1]]
Min_wage <-
  model.indonesia.fit.dft[which(names(model.indonesia.fit.dft) == "Min_wage")][[1]] *
  1000
price <-
  model.indonesia.fit.dft[which(names(model.indonesia.fit.dft) == "price")][[1]]
value <- value + Min_wage + price
for (i in inputshiny) {
  value <-
    value + model.indonesia.fit.dft[which(names(model.indonesia.fit.dft) == inputshiny[1])][[1]]
  
}
prob<-(exp(-1 * value) / (1 + exp(-1 * value)))
result.table.temp<-data.frame("Country"="INDONESIA","probability"=prob)
result.table<-rbind(result.table,result.table.temp)


