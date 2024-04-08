# import necessary libraries
library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(Metrics)
library(caret)
library(MASS)
library(ggplot2)
library(reshape2)
library(mltools)
library(plotly)

# Set working directory
setwd("D:/SECOND_YEAR/Outsider/repos/R-test")

#Read All_GPUs.csv
naStrings = c("\n- ", "", "N/A")
GPUs_data = read.csv("All_GPUs.csv", na.strings = naStrings)
head(GPUs_data)

#Summarise the dataset
summary(GPUs_data)

#Check the missing data
colSums(is.na(GPUs_data))
missing_counts <- colSums(is.na(GPUs_data))
barplot(missing_counts,main="Missing Values Count in Each Column",ylab="Count",col="skyblue",names.arg=names(missing_counts),las=2)
# # Choose key columns

GPUs_data = GPUs_data[,c("Name", "Best_Resolution","Core_Speed","Memory","Memory_Speed","Memory_Bandwidth","Manufacturer","Release_Date")]

# data = read.csv("All_GPUs.csv", head=TRUE)

# # chon cac cot quan trong
# new_DF <- data[,c("Name", "Best_Resolution","Core_Speed","Memory","Memory_Speed","Memory_Bandwidth","Manufacturer","Release_Date")]
# head (new_DF)

#Loai bo don vi "MB" tu cot "Memory"
GPUs_data$ Memory <- as.numeric ( sub (" MB", "", GPUs_data$Memory ))
head(GPUs_data)
#Loai bo don vi "GB/sec" tu cot "Memory _ Bandwidth"
GPUs_data$Memory_Bandwidth <- as.numeric( gsub ("GB/sec","", GPUs_data$Memory_Bandwidth ))

#Loai bo don vi "MHz" tu cot "Memory _Speed"
GPUs_data$Memory_Speed <- as.numeric ( sub (" MHz", "", GPUs_data$Memory_Speed ))
# head (new_DF)

# # 3.3
# #Kiem tra cac du lieu bi khuyet va de xuat cac phuong an
# #Kiem tra bao nhieu du lieu bi khuyet
apply (is.na(GPUs_data) ,2 ,sum )
# #Kiem tra vi tri chinh xac cua du lieu bi khuyet
apply (is.na(GPUs_data) ,2 , which )
# #tinh ti le du lieu khuyet
apply (is.na(GPUs_data) ,2 , mean )

# #Xoa cac du lieu bi khuyet cua cot "Memory _Speed " ," Memory_ Bandwidth"
GPUs_data <- subset ( GPUs_data ,!is.na(Memory_Speed) & !is.na(Memory_Bandwidth))

# # Tinh median cua cot "Memory" (bo qua gia tri NA)
median_memory <- median (GPUs_data$Memory , na.rm = TRUE )

# # Thay the gia tri NA trong cot "Memory" bang median
GPUs_data$Memory <- ifelse(is.na( GPUs_data$ Memory ) , median_memory , GPUs_data$Memory )

# #Kiem tra lai du lieu
apply (is.na(GPUs_data) ,2 ,sum)
