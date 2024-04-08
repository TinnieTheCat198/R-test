# Mo so lieu
setwd("C:/R") #  doi thu muc hoat dong
getwd() #xem thu muc hoat dong

# mo file so lieu
trong = read.csv("All_GPUs")
head(trong)

# Hoi quy tuyen tinh da bien
fit1 = lm(data = trong, 
          Boost_Clock ~ Core_Speed)
summary(fit1)