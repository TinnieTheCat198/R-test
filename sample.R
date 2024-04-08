# Mo so lieu
setwd("D:/SECOND_YEAR/Outsider/repos/R-test") #  doi thu muc hoat dong
getwd() #xem thu muc hoat dong

# mo file so lieu
trong = read.csv("All_GPUs.csv")
head(trong)

# Hoi quy tuyen tinh da bien
fit1 = lm(Boost_Clock ~ Core_Speed,data = trong)
summary(fit1)