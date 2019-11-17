# Read the NAV Data
nav <- read.csv("~/Desktop/ExternalProject/SPHIX-2.csv")

# Clean the NAV Data
library(tidyverse)
nav = nav %>% select(Date,Adj.Close) %>% 
  mutate(price = as.numeric(Adj.Close)) %>% 
  mutate(Date = as.Date(Date,"%Y-%m-%d"))
plot(x = nav$Date,y = nav$Adj.Close,type ="l")

# Read the Index Data
library(readxl)
sheet = excel_sheets("~/Desktop/ExternalProject/Indices.xlsx")
c = setNames(sheet,paste0("Index",1:length(sheet)))

re = function(x){
  a = read_excel("Indices.xlsx",sheet=x) %>% data.frame
  names = colnames(a)
  names[1] = "Date"
  colnames(a) = names
  a = a %>% mutate(Date = as.Date(Date,"%Y-%m-%d"))
  a[names[2]] = as.numeric(a[,2])
  return(a)
}

list2env(lapply(c,function(x){re(x)}),envir = .GlobalEnv)

# Merge all the index together
index = Index1
for(i in 2:17){
  index = inner_join(index, get(paste0("Index",i)), by = "Date")
}

index.t = index %>% dplyr::select(-Date) %>% 
  apply(MARGIN =2, function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))*100})
index.t = data.frame(index.t)
index.t = index.t %>% mutate(Date = index$Date)
index.p = index.t %>% pivot_longer(cols = 1:17, names_to = "Name", values_to = "Index")

ggplot(index.p) +geom_line(aes(x= Date, y = Index, color = Name))

da = index %>% select(-Date) %>% drop_na()
library(corrplot)
cor.in = cor(da)
corrplot(cor.in,method = "circle",type = "upper",diag = F)



