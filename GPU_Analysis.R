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
library(anytime)
library(knitr)

# Set working directory
setwd("./")

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

# Choose key columns
new_DF <- GPUs_data[,c("Name", "Best_Resolution","Core_Speed","Memory","Memory_Speed","Memory_Bandwidth","Manufacturer","Release_Date")]
head(new_DF);

# Eliminate "MB" in "Memory" column
new_DF$Memory <- as.numeric(sub (" MB", "", new_DF$Memory ))
# head(new_DF)

# Eliminate "GB/sec" in "Memory_Bandwidth" column
new_DF$Memory_Bandwidth <- as.numeric(gsub("GB/sec","", new_DF$Memory_Bandwidth))

# Eliminate "MHz" in "Memory_Speed" column
new_DF$Memory_Speed <- as.numeric(sub(" MHz", "", new_DF$Memory_Speed))

# Eliminate "MHz" in "Core_Speed" column
new_DF$Core_Speed <- as.numeric(sub(" MHz", "", new_DF$Core_Speed))

# 3.3
# Kiem tra cac du lieu bi khuyet va de xuat cac phuong an
#Kiem tra bao nhieu du lieu bi khuyet
apply (is.na(new_DF) ,2 ,sum )
# #Kiem tra vi tri chinh xac cua du lieu bi khuyet
apply (is.na(new_DF) ,2 , which )
# #tinh ti le du lieu khuyet
apply (is.na(new_DF) ,2 , mean )

#Xoa cac du lieu bi khuyet cua cot "Memory _Speed " ," Memory_ Bandwidth"
new_DF <- subset (new_DF,!is.na(Memory_Speed) & !is.na(Memory_Bandwidth))

# Tinh median cua cot "Memory" (bo qua gia tri NA)
median_memory <- median (new_DF$Memory , na.rm = TRUE )

# Thay the gia tri NA trong cot "Memory" bang median
new_DF$Memory <- ifelse(is.na( new_DF$Memory ) , median_memory , new_DF$Memory )

median_core_speed <- median(new_DF$Core_Speed , na.rm = TRUE )
new_DF$Core_Speed <- ifelse(is.na( new_DF$Core_Speed ) , median_core_speed , new_DF$Core_Speed )


# #Kiem tra lai du lieu
apply (is.na(new_DF) ,2 ,sum)

# 4.1 Memory_Speed
# tinh cac gia tri thong ke
install.packages("knitr")
library(dplyr)
library(knitr)
nF_summ <- new_DF %>% group_by(Manufacturer) %>% summarize (
sample_size = n() ,
mean = mean(Memory_Speed , na.rm = TRUE) ,
sd = sd(Memory_Speed , na.rm = TRUE) ,
minimum = min (Memory_Speed , na.rm = TRUE) ,
first_quantile = quantile(Memory_Speed , 0.25 , na.rm = TRUE) ,
median = median(Memory_Speed , na.rm = TRUE) ,
third_quantile = quantile(Memory_Speed , 0.75 , na.rm = TRUE) ,
maximum = max(Memory_Speed , na.rm = TRUE)
)

# ve do thi boxplot Memory_Speed tuong ung voi tung hang
ggplot(new_DF , aes(x=Manufacturer , y=Memory_Speed)) + geom_boxplot () + stat_summary(fun.y = "mean", geom = "point", color = "red") + theme_minimal()


# 4.2 Memory
# ve do thi scatterplot theo bien Memory cua cac hang qua cac nam

#Tai du lieu (ten la new_DF)
dataset <- new_DF

#Dinh nghia mot ham de dem so diem anh dua tren cot Best _Resolution
# countPixels <- function(x) {
# if (is.na(x)) {
# return(1024 * 768) # Kich thuoc mac dinh neu Best_Resolution bi thieu
# } else {
# values <- as.numeric (unlist(strsplit(x, ' x ')))
# return(values [1] * values [2])
# }
# }
#Ap dung ham countPixels de tao cot moi PixelNum
# dataset$PixelNum <- sapply(dataset$Best_Resolution ,countPixels)

#Chuyen cot Release_Date thanh dinh dang Date
dataset$Release_Date <- anydate(dataset$Release_Date)
dataset$Release_Date <- as.Date(dataset$Release_Date ,format = "%Y -%m -%d")

#Trich xuat Year tu cot Release_Date
dataset$Year <- format(dataset$Release_Date , "%Y")

#Khoi tao mot doi tuong plotly trong
fig <- plot_ly()

#Lap qua cac nha san xuat duy nhat de tao ra cac dong bieu do Scatter
for(manufacturer in unique(dataset$Manufacturer)) {

#Tach ra tap du lieu cho nha san xuat hien tai
trace_dataset <- subset (dataset , Manufacturer == manufacturer)

#Add a trace (scatter plot) for the current manufacturer
fig <- fig %>% add_trace (
x = trace_dataset$Year,
y = trace_dataset$Memory ,
type = 'scatter',
mode = 'markers',
name = manufacturer,
marker = list (
symbol = 'circle',
size = 10,
opacity = 0.1 ,
line = list (
width = 1,
color = 'rgb (255 , 255 , 255)'
 )
),
text = trace_dataset$Name #Label van ban cho moi diem danh dau
)
}

#Cai dat bo cuc cho bieu do
fig <- fig %>% layout (
title = 'GPU Memory vs Year of Release by Manufacturer',
paper_bgcolor = 'rgb (243 , 243 , 243)',
plot_bgcolor = 'rgb (243 , 243 , 243)',
yaxis = list (
title = 'GPUs Memory',
ticklen = 5,
gridcolor = 'rgb (255 , 255 , 255)',
gridwidth = 2
),
xaxis = list (
title = 'Year of Release',
ticklen = 5,
gridcolor = 'rgb (255 , 255 , 255)',
gridwidth = 2)
)
#In bieu do
fig


###### Multiple Linear Regression ############
dataset$Year <- as.numeric(dataset$Year)
dataset <- dataset[complete.cases(dataset$Year), ]

model1 <- lm(Memory_Bandwidth ~ Memory + Memory_Speed, data = dataset)
summary(model1)

model2 <- lm(Memory_Bandwidth ~ Memory + Memory_Speed + Core_Speed, data = dataset)
summary(model2)

model3 <- lm(Memory_Bandwidth ~ Memory + Memory_Speed + Core_Speed + Year, data = dataset)
summary(model3)

y_pred <- predict(model3, newdata = dataset)

# Plot the actual vs predicted values
plot(dataset$Memory_Bandwidth, y_pred, xlab='Actual Y', ylab='Predicted Y', main='Actual vs Predicted values')
abline(0, 1, col='red')  # Add a diagonal line for reference
grid()
