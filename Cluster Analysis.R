
## Installing and Loading Packages
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("HSAUR")
library(stringr)
library(readxl)
library(dplyr)

## Reading data files
laundet_drug <- read.delim("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/laundet_drug_1114_1165", 
                           header = T, sep = "", stringsAsFactors = FALSE)
laundet_groc <- read.delim("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/laundet_groc_1114_1165", 
                           header = T, sep = "", stringsAsFactors = FALSE)
#write.csv(laundet_groc, file = 'D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/laundet_groc.csv')

laundet_gr <- read.table("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/laundet_PANEL_GR_1114_1165.dat",
                         header = T)
demo <- read.csv("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/ads_demo.csv", header = T)


#laundet_dr <- read.table("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/laundet_PANEL_DR_1114_1165.dat",
#                         header = T)
#laundet_ma <- read.table("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/laundet_PANEL_MA_1114_1165.dat",
#                         header = T)

delivery_stores <- read.delim("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/Delivery_Stores.csv", 
                           header = T, sep = ",", stringsAsFactors = FALSE)

prod_laundet <- read_excel("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/prod_laundet.xls")

week_table <- read_excel("D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/IRI week translation.xls")

##################################### Cluster Analysis ##########################################
laundet_groc$SY <- sprintf("%02d", as.numeric(laundet_groc$SY))
laundet_groc$GE <- sprintf("%02d", as.numeric(laundet_groc$GE))
laundet_groc$VEND <- sprintf("%05d", as.numeric(laundet_groc$VEND))
laundet_groc$ITEM <- sprintf("%05d", as.numeric(laundet_groc$ITEM))
laundet_groc$UPC <- paste(laundet_groc$SY, laundet_groc$GE, laundet_groc$VEND, laundet_groc$ITEM, sep = "-")

laundet_groc$SY <- NULL
laundet_groc$GE <- NULL
laundet_groc$VEND <- NULL
laundet_groc$ITEM <- NULL

prod_laundet$COLUPC <- str_replace_all(prod_laundet$UPC, "[[:punct:]]", "")
prod_laundet$COLUPC <- sprintf("%014.000f", as.numeric(prod_laundet$COLUPC))

final_data <- merge(prod_laundet, laundet_groc, by = "UPC")
#final_data <- final_data%>%filter(FORM == "LIQUID" & L5 == "ARM & HAMMER")

sum(final_data$DOLLARS)

#final_data$Level <- NULL
final_data$L1 <- NULL
final_data$L2 <- NULL
final_data$L3 <- NULL
final_data$L4 <- NULL
final_data$SY <- NULL
final_data$GE <- NULL
final_data$VEND <- NULL
final_data$ITEM <- NULL

final_data$OZ <- word(final_data$L9, -1)

laundet_gr$COLUPC <- sprintf("%014.000f", as.numeric(laundet_gr$COLUPC))


data <- merge(final_data, laundet_gr, by = c("COLUPC", "IRI_KEY", "WEEK"))

colnames(data)[18] <- "DOLLARS"
colnames(data)[17] <- "UNITS"

data$DOLLARS.y <- NULL
data$UNITS.y <- NULL

data <- merge(data, demo, by.x = "PANID", by.y = "Panelist.ID")

unique(data$Level)
data$Level <- NULL
data$UPC <- NULL
data$COLUPC <- NULL
data$IRI_KEY <- NULL
data$PANID <- NULL
data$F <- NULL
data$D <- NULL
data$OUTLET <- NULL
data$COUNTY <- NULL
data$HISP_CAT <- NULL
data$HISP_FLAG <- NULL
data$ZIPCODE <- NULL
data$FIPSCODE <- NULL
data$Language <- NULL

######### Decision Tree
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(UNITS ~., data = data, method = 'class')
rpart.plot(fit, extra = 106)

test <- data
na_code <- "."
test <- as.data.frame(lapply(test, function(x) {
  levels(x)[levels(x) %in% na_code] <- 0 
  x
}))


test <- test[complete.cases(test), ]
test$PANID <- NULL
test$WEEK <- NULL
test$L9 <- NULL
test$X.STUBSPEC.1860RC.........................................................00004 <- NULL
test$L5 <- as.factor(test$L5)

unique(test$L5)
test <- test%>%filter(FORM == "LIQUID" & L5 == "ARM & HAMMER")

test$L5 <- NULL
test$FORM <- NULL
test$PACKAGE <- NULL
test$PRODUCT.TYPE <- NULL
test$FLAVOR.SCENT <- NULL
test$CONCENTRATION.LEVEL <- NULL
test$ADDITIVES <- NULL
test$TYPE.OF.FORMULA <- NULL
test$OZ <- NULL

str(test)
test$HH_RACE <- NULL
test$Microwave.Owned.by.HH <- as.numeric(test$Microwave.Owned.by.HH)
test$Number.of.TVs.Used.by.HH <- as.numeric(test$Number.of.TVs.Used.by.HH)
test$Number.of.TVs.Hooked.to.Cable <- as.numeric(test$Number.of.TVs.Hooked.to.Cable)
test$MALE_SMOKE <- as.numeric(test$MALE_SMOKE)
test$FEM_SMOKE <- as.numeric(test$FEM_SMOKE)
test$HH.Head.Race..RACE2. <- as.numeric(test$HH.Head.Race..RACE2.)
test$Panelist.Type <- NULL
test$market.based.upon.zipcode <- NULL
test$EXT_FACT <- NULL
test$VOL_EQ <- NULL
test$IRI.Geography.Number <- NULL
test$Year <- NULL

'''
impute.mean <- function(x) replace(x, is.na(x) | is.nan(x) | is.infinite(x), mean(x[!is.na(x) & !is.nan(x) & !is.infinite(x)]))
test1 <- apply(test, 2, impute.mean)

impute.mean <- function(x) replace(x, is.na(x) | is.nan(x) | is.infinite(x), mean(x, na.rm = TRUE))
test1 <- apply(test1, 2, impute.mean)

sum(apply( test1, 2, function(.) sum(is.infinite(.)) ))
'''
#write.csv(test, "D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/test_na.csv")
sum(is.na(test))
test <- test[complete.cases(test), ]
str(test)

#any(is.na(test) | is.infinite(test))
### Elbow Curve ###
clu_data <- scale(test)
clu_data <- na.omit(clu_data)
#kmeans <- kmeans(clu_data,6)

#kmeans$cluster

#cluster.df <- data.frame((kmeans$cluster))
#means <- kmeans$centers

#summary(test)
library(purrr)
tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(clu_data, centers = k)
  model$tot.withinss
})


elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)


library(ggplot2)
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

model <- kmeans(clu_data, centers = 2, nstart = 40)
result <- as.data.frame(clu_data)
result_final <- mutate(result, cluster = model$cluster)

test_final <- mutate(test, cluster = model$cluster)
table(result_final$cluster)
table(test_final$cluster)

#write.csv(test_final, "D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/test_final.csv")
library(factoextra)
fviz_cluster(object = model, data = result_final, geom = "point",
             choose.vars = c("Number.of.Dogs", "Family.Size"), stand = FALSE, 
             ellipse.type = "norm") + theme_bw()

result_final$Family.Size

clust1 <- test_final%>%filter(cluster == 2)
summary(clust1)

'''
library(cluster)
library(HSAUR)
#data(pottery)
#km    <- kmeans(pottery,3)
dissE <- daisy(result_final) 
dE2   <- dissE^2
sk2   <- silhouette(result_final$cluster, dE2)
plot(sk2)
'''
##################################### Time Series ##########################################
laundet_groc$SY <- sprintf("%02d", as.numeric(laundet_groc$SY))
laundet_groc$GE <- sprintf("%02d", as.numeric(laundet_groc$GE))
laundet_groc$VEND <- sprintf("%05d", as.numeric(laundet_groc$VEND))
laundet_groc$ITEM <- sprintf("%05d", as.numeric(laundet_groc$ITEM))
laundet_groc$UPC <- paste(laundet_groc$SY, laundet_groc$GE, laundet_groc$VEND, laundet_groc$ITEM, sep = "-")

laundet_groc$SY <- NULL
laundet_groc$GE <- NULL
laundet_groc$VEND <- NULL
laundet_groc$ITEM <- NULL

final_data <- merge(prod_laundet, laundet_groc, by = "UPC")
final_data <- final_data%>%filter(FORM == "LIQUID" & L5 == "ARM & HAMMER")
write.csv(final_data, "D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/final_data.csv")

colnames(week_table)[1] <- "WEEK"
final_data <- merge(final_data, week_table, by = "WEEK")

final_data$Level <- NULL
final_data$L1 <- NULL
final_data$L2 <- NULL
final_data$L3 <- NULL
final_data$L4 <- NULL
final_data$SY <- NULL
final_data$GE <- NULL
final_data$VEND <- NULL
final_data$ITEM <- NULL
#final_data$L3 <- NULL

colnames(delivery_stores)[1] <- "IRI_KEY"
final_data <- merge(final_data, delivery_stores, by = "IRI_KEY")

flavor_mkt <- final_data%>%filter(FORM == "LIQUID" & (L5 == "ARM & HAMMER" | L5 == "TIDE"))%>%group_by(L5, Market_Name, `FLAVOR/SCENT`)%>%summarise(Units = sum(UNITS), Dollars = sum(DOLLARS))%>%arrange(Market_Name, desc(Dollars))
  filter(L5 == "ARM & HAMMER" OR L5 == "TIDE")
unique(final_data$Level)

final_data_am <- final_data%>%filter(L5 == "ARM & HAMMER" & FORM == "LIQUID")

final_data_am$L5 <- NULL
final_data_am$`PRODUCT TYPE` <- NULL
final_data_am$WEEK <- NULL
final_data_am$UPC <- NULL
final_data_am$FORM <- NULL
final_data_am$`TYPE OF FORMULA` <- NULL

final_data_am$OZ <- word(final_data_am$L9, -1)

final_data_am$L9 <- NULL
unique(final_data_am$PACKAGE)
final_data_am$PACKAGE <- NULL
final_data_am$IRI_KEY <- NULL
final_data_am$`Calendar week starting on` <- NULL
final_data_am$`Calendar week ending on` <- NULL
final_data_am$F <- NULL
final_data_am$D <- NULL
final_data_am$`*STUBSPEC 1860RC                                                         00004` <- NULL
final_data_am$VOL_EQ <- NULL

lm_mod1 <- lm(DOLLARS ~ OZ + PR + ADDITIVES + `CONCENTRATION LEVEL` + `FLAVOR/SCENT`, data = final_data_am)
summary(lm_mod1)

final_data <- final_data%>%mutate(Brand = ifelse(
  L5 == "ARM & HAMMER", "A&M", "Other Brands"
))

final_data$Oz <- word(final_data$L9, -1)

lm_mod_data <- final_data%>%group_by(Brand, Oz, Brand, PR, ADDITIVES, `CONCENTRATION LEVEL`, `FLAVOR/SCENT`,
                                   FORM, PACKAGE, `PRODUCT TYPE`)%>%summarise(Dollars = sum(DOLLARS),
                                                                              Units = sum(UNITS))
lm_mod_data$Unit_price <- lm_mod_data$Dollars/lm_mod_data$Units

lm_mod_data$Dollars <- NULL
lm_mod_data$Units <- NULL

lm_mod2 <- lm(Unit_price ~ ., data = lm_mod_data)
summary(lm_mod2)

library(rpart)
library(rpart.plot)
fit <- rpart(Unit_price ~ ., data = lm_mod_data, method = 'class')
rpart.plot(fit, extra = 106)

#data("AirPassengers")
#AP <- AirPassengers
#View(AP)
#################################################################################################
test_final <- merge(laundet_gr, demo, by.x = "PANID", by.y = "Panelist.ID", all = TRUE)
test_final <- test_final[complete.cases(test_final), ]

prod_laundet$COLUPC <- str_replace_all(prod_laundet$UPC, "[[:punct:]]", "")
prod_laundet$COLUPC <- sprintf("%014.000f", as.numeric(prod_laundet$COLUPC))
test_final$COLUPC <- sprintf("%014.000f", as.numeric(test_final$COLUPC))

test2_final <- merge(test_final, prod_laundet, by = "COLUPC")



asd <- test2_final%>%
  filter(L5 == "ARM & HAMMER" & FORM == "LIQUID")
  

sum(asd$DOLLARS)





##################################### Final Version ##########################################
### Treating Laundet_groc

laundet_groc$SY <- sprintf("%02d", as.numeric(laundet_groc$SY))
laundet_groc$GE <- sprintf("%02d", as.numeric(laundet_groc$GE))
laundet_groc$VEND <- sprintf("%05d", as.numeric(laundet_groc$VEND))
laundet_groc$ITEM <- sprintf("%05d", as.numeric(laundet_groc$ITEM))
laundet_groc$UPC <- paste(laundet_groc$SY, laundet_groc$GE, laundet_groc$VEND, laundet_groc$ITEM, sep = "-")

laundet_groc$SY <- NULL
laundet_groc$GE <- NULL
laundet_groc$VEND <- NULL
laundet_groc$ITEM <- NULL

#max(length(laundet_groc$SY))

laundet_groc$COLUPC <- str_replace_all(laundet_groc$UPC, "[[:punct:]]", "")

prod_groc <- merge(laundet_groc, prod_laundet, by = "UPC")
prod_groc_final <- prod_groc%>%filter(L5 == "ARM & HAMMER" & FORM == "LIQUID")
sum(prod_groc_final$DOLLARS)

prod_groc_final$L1 <- NULL
prod_groc_final$L2 <- NULL
prod_groc_final$L3 <- NULL
prod_groc_final$L4 <- NULL
prod_groc_final$L5 <- NULL
prod_groc_final$FORM <- NULL
prod_groc_final$Level <- NULL
prod_groc_final$SY <- NULL
prod_groc_final$GE <- NULL
prod_groc_final$VEND <- NULL
prod_groc_final$ITEM <- NULL
prod_groc_final$`TYPE OF FORMULA` <- NULL
prod_groc_final$PACKAGE <- NULL
prod_groc_final$`PRODUCT TYPE` <- NULL
prod_groc_final$F <- NULL
prod_groc_final$D <- NULL

laundet_gr$COLUPC <- sprintf("%014.000f", as.numeric(laundet_gr$COLUPC))

gr_prod <- merge(prod_groc_final, laundet_gr, by = c("COLUPC", "WEEK", "IRI_KEY"), all.x = T)
sum(gr_prod$DOLLARS.x)
test <- gr_prod%>%distinct()

sum(test$DOLLARS.x)
#write.csv(prod_groc_final, file = 'D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/prod_groc_final.csv')
#count(laundet_gr%>%distinct(PANID))
#unique(prod_groc_final$`FLAVOR/SCENT`)


#substrRight <- function(x, n){
#  substr(x, nchar(x)-n+1, nchar(x))
#}

#laundet_groc$COLUPC <- substrRight(laundet_groc$COLUPC, 11)
#laundet_gr$COLUPC1 <- NULL
gr_prod <- merge(laundet_groc, laundet_gr, by = c("IRI_KEY", "WEEK", "COLUPC"), all.x = )
sum(gr_prod$DOLLARS.x)

sum(gr_data$DOLLARS.x)
sum(gr_data$DOLLARS.y)
'''
colnames(gr_data)[4] <- "UNITS"
colnames(gr_data)[5] <- "DOLLARS"
gr_data$UNITS.y <- NULL
gr_data$DOLLARS.y <- NULL
'''

prod_laundet$SY <- NULL
prod_laundet$GE <- NULL
prod_laundet$VEND <- NULL
prod_laundet$ITEM <- NULL
prod_laundet$Level <- NULL
#unique(prod_laundet$`TYPE OF FORMULA`)
prod_laundet%>%distinct(UPC)

final_data <- merge(gr_data, prod_laundet, by = "UPC")
final_data_am <- final_data%>%filter(L5 == "ARM & HAMMER" & FORM == "LIQUID")

sum(final_data_am$DOLLARS.y)
### Grouping gr_data at PANID level
gr_data_panel <- gr_data%>%group_by(PANID)%>%summarise(Units = sum(UNITS), Dollars = sum(DOLLARS))

sum(gr_data_panel$Dollars)

gr_data_panel <- merge(gr_data_panel, demo, by.x = "PANID", by.y = "Panelist.ID", all.x = T)
gr_data_panel <- gr_data_panel[complete.cases(gr_data_panel), ]

### Joining grocery data with Delivery Stores to obtain markt info
colnames(delivery_stores)[1] <- "IRI_KEY"
colnames(delivery_stores)[2] <- "OUTLET"
gr_data_stores <- merge(gr_data, delivery_stores, by = c("IRI_KEY", "OUTLET"))




#test <- merge(laundet_groc, prod_laundet, by = "UPC")
#test <- test%>%filter(L5 == "ARM & HAMMER" & FORM == "LIQUID")
#sum(test_final$DOLLARS)

test_final <- merge(test, gr_data, by = c("UPC"))






###### Final Code
### Treating Laundet_groc
laundet_groc$SY <- paste0("0", laundet_groc$SY)
laundet_groc$GE <- paste0("0", laundet_groc$GE)
laundet_groc$VEND <- sprintf("%05d", as.numeric(laundet_groc$VEND))
laundet_groc$ITEM <- sprintf("%05d", as.numeric(laundet_groc$ITEM))
laundet_groc$UPC <- paste(laundet_groc$SY, laundet_groc$GE, laundet_groc$VEND, laundet_groc$ITEM, sep = "-")

laundet_groc$SY <- NULL
laundet_groc$GE <- NULL
laundet_groc$VEND <- NULL
laundet_groc$ITEM <- NULL

########################### Processing code using senior's code as reference ###############################
demo_gr <- merge(laundet_gr, demo, by.x = "PANID", by.y = "Panelist.ID")
demo_gr <- demo_gr%>%distinct()

data <- merge(prod_laundet, laundet_groc, by = "UPC")
data$L1 <- NULL
data$L2 <- NULL
data$L3 <- NULL
data$L4 <- NULL
data$SY <- NULL
data$GE <- NULL
data$VEND <- NULL
data$ITEM <- NULL
data$Level <- NULL
data <- data%>%distinct()

data$OZ <- word(data$L9, -1)

data <- data%>%mutate(Brand = ifelse(
  L5 == "ARM & HAMMER", "A&M", "Other Brands"
))

data$unit_price <- data$DOLLARS / data$UNITS

### Treating Week table
colnames(week_table)[1] <- "WEEK"
colnames(week_table)[2] <- "Strt_date"
colnames(week_table)[3] <- "End_date"
colnames(week_table)[4] <- "C4"
colnames(week_table)[5] <- "C5"
colnames(week_table)[6] <- "C6"

data$WEEK <- as.numeric(data$WEEK)
week_table <- as.data.frame(week_table)

data <- merge(data, week_table, by = "WEEK")
data <- data%>%distinct()

#################################### Logistic Regression #########################################
log_data <- data[ , c("L5", "PRODUCT TYPE", "PACKAGE", "FORM", "FLAVOR/SCENT", "CONCENTRATION LEVEL", "ADDITIVES", 
                      "TYPE OF FORMULA", "UNITS", "unit_price", "OZ", "Brand", "Strt_date")]

log_data <- log_data%>%distinct()
write.csv(log_data, file = 'D:/UTD/Spring 20/Predictive Analytics using SAS/Project/laundet/log_data.csv')

#demo_gr <- merge(demo, laundet_gr, by.x = "Panelist.ID",  by.y = "PANID")
#demo_gr <- merge(demo_gr, laundet_groc, by = c("IRI_KEY", "WEEK"))

## Data Exploration
#colnames(demo)[colSums(is.na(demo)) > 0]

#grepl(".", laundet_drug, fixed = TRUE)

#laundet_drug$DOLLARS <- str_replace(laundet_drug$DOLLARS, ".", "0")

### Treating prod_laundet
data <- prod_laundet%>%filter(L5 == "ARM & HAMMER" & FORM == "LIQUID")
data$L1 <- NULL
data$L2 <- NULL
data$L3 <- NULL
data$L4 <- NULL
data$L5 <- NULL
data$SY <- NULL
data$GE <- NULL
data$VEND <- NULL
data$ITEM <- NULL
data$FORM <- NULL
data$Level <- NULL
data$`TYPE OF FORMULA` <- NULL

### Treating Laundet_groc
laundet_groc$SY <- paste0("0", laundet_groc$SY)
laundet_groc$GE <- paste0("0", laundet_groc$GE)
laundet_groc$VEND <- sprintf("%05d", as.numeric(laundet_groc$VEND))
laundet_groc$ITEM <- sprintf("%05d", as.numeric(laundet_groc$ITEM))
laundet_groc$UPC <- paste(laundet_groc$SY, laundet_groc$GE, laundet_groc$VEND, laundet_groc$ITEM, sep = "-")

laundet_groc$SY <- NULL
laundet_groc$GE <- NULL
laundet_groc$VEND <- NULL
laundet_groc$ITEM <- NULL

### Treating Laundet_drug
laundet_drug$SY <- paste0("0", laundet_drug$SY)
laundet_drug$GE <- paste0("0", laundet_drug$GE)
laundet_drug$VEND <- sprintf("%05d", as.numeric(laundet_drug$VEND))
laundet_drug$ITEM <- sprintf("%05d", as.numeric(laundet_drug$ITEM))
laundet_drug$UPC <- paste(laundet_drug$SY, laundet_drug$GE, laundet_drug$VEND, laundet_drug$ITEM, sep = "-")

laundet_drug$SY <- NULL
laundet_drug$GE <- NULL
laundet_drug$VEND <- NULL
laundet_drug$ITEM <- NULL

### Treating Delivery Stores
colnames(delivery_stores)[1] <- "IRI_KEY"

### Treating Week table
colnames(week_table)[1] <- "WEEK"
colnames(week_table)[2] <- "Strt_date"
colnames(week_table)[3] <- "End_date"
colnames(week_table)[4] <- "C4"
colnames(week_table)[5] <- "C5"
colnames(week_table)[6] <- "C6"

## Joining both data with groc and drug tables
data <- merge(data, laundet_groc, by = "UPC") 
#data <- merge(data, laundet_drug, by = "UPC")

## Joining both data and delivery stores tables
data <- merge(data, delivery_stores, by = "IRI_KEY") 

## Extracting features from thr dataset
data$OZ <- word(data$L9, -1)

data$OU <- NULL
data$PACKAGE <- NULL
data$`PRODUCT TYPE` <- NULL
unique(data$`PRODUCT TYPE`)

## Joining both data and week tables
str(data$WEEK)
data$WEEK <- as.numeric(data$WEEK)
str(data)

week_table <- as.data.frame(week_table)
data <- merge(data, week_table, by = "WEEK") ## Final Dataset

write.csv(data, file = 'data.csv')


#### Creating 2nd dataset
data2 <- merge(x = demo, y = laundet_gr, by.x = "Panelist.ID", by.y = "PANID", all = TRUE)
data2 <- data2[complete.cases(data2), ]


#### Final Dataset
final_dataset <- merge(data, data2, by = c("IRI_KEY", "WEEK"))
final_dataset <- final_dataset[complete.cases(final_dataset), ]
