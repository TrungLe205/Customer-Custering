### Load the ta-feng data set and cleaning
# Data Discription
# D11: Transaction data collected in November, 2000
# D12: Transaction data collected in December, 2000
# D01: Transaction data collected in January, 2001
# D02: Transaction data collected in February, 2001

# Format of Transaction Data
# --------------------------
# First line: Column definition in Traditional Chinese
#     §È¥¡;∑|≠˚•d∏π;¶~ƒ÷;∞œ∞Ï;∞”´~§¿√˛;∞”´~ΩsΩX;º∆∂q;¶®•ª;æP∞‚
# Second line and the rest: data columns separated by ";"

# Column definition
# -----------------
# 1: Transaction date and time (time invalid and useless)
# 2: Customer ID
# 3: Age: 10 possible values,
#    A <25,B 25-29,C 30-34,D 35-39,E 40-44,F 45-49,G 50-54,H 55-59,I 60-64,J >65
#    actually upon inspection there's 22362 rows with value K, will assume it's J+
# 4: Residence Area: 8 possible values, 
#    A-F: zipcode area: 105,106,110,114,115,221,G: others, H: Unknown
#    Distance to store, from the closest: 115,221,114,105,106,110
#    "E","F","D","A","B","C","G","H"
# 5: Product subclass
# 6: Product ID
# 7: Amount
# 8: Asset
# 9: Sales price

install.packages("scales")
library(scales)
library(ggplot2)
install.packages("grid")
library(grid)
install.packages("Hmisc")
library(Hmisc)
library(data.table)
library(reshape2)
install.packages("lubridate")
library(lubridate)
install.packages("mclust")
library(mclust)
install.packages("mix")
library(mix)
library(zoo)

# Read data
colNames <- c("trans_date", "cust_id", "age", "area", "prod_cat", "prod_id", "quantity", "asset", "price")
dat1 <- read.csv("D01.csv")
colnames(dat1) <- colNames
dat2 <- read.csv("D02.csv")
colnames(dat2) <- colNames
dat3 <- read.csv("D11.csv")
colnames(dat3) <- colNames
dat4 <- read.csv("D12.csv")
colnames(dat4) <- colNames
dat <- rbind(dat1, dat2, dat3, dat4)
str(dat)
dat[, 1:6] <- sapply(dat[, 1:6], as.character)
dat[, 7:9] <- sapply(dat[, 7:9], as.numeric)
dat$trans_date <- as.Date(dat$trans_date)
dat$age <- as.factor(dat$age)
dat$area <- as.factor(dat$area)
dat$prod_cat <- as.factor(dat$prod_cat)
dat$prod_id <- as.factor(dat$prod_id)
ageGroup <- ifelse(dat$age == "A", "<25", ifelse( dat$age == "B", "25-29", ifelse(dat$age == "C", "30-34", ifelse(dat$age == "D", "35-39", ifelse(dat$age == "E", "40-44", ifelse(dat$age == "F", "45-49", ifelse(dat$age == "G", "50-54", ifelse(dat$age == "H", "55-59", ifelse(dat$age == "I", "60-64", ">65")))))))))
dat <- cbind(dat,ageGroup)
str(dat)

### Exploration
library(ggplot2)
# How many transactions, dats, customers, product_id and product subclasses?
dat$trans_id <- seq(1, nrow(dat),1)
dt <- data.table(dat)
is.data.table(dt)
nrow(dt)  
num_trans <- nrow(dt[,1,by = trans_date])
nrow(dt[,1,by = cust_id])
nrow(dt[,1,by = prod_cat])
nrow(dt[,1,by = prod_id])

# Visualization
# transaction by date 
p1 <- ggplot(dt[,list(num_trans=length(trans_id)),by=trans_date]) + geom_bar(aes(x = trans_date, y = num_trans), stat = 'identity', alpha = 0.8)
plot(p1)
# histogram items per customers
p2 <- ggplot(dt[,list(num_items=length(trans_id)), by = cust_id]) + geom_histogram(aes(x = num_items), stat = 'bin', binwidth = 10, alpha = 0.8) + coord_cartesian(xlim = c(0,200))
plot(p2)
dt[,list(num_items=length(trans_id)), by = cust_id]

### Feature Engineer
# Convert age and area variable into numeric
dtnums <- dt[,list(cust_id, age = as.numeric(age), area = as.numeric(area))]
dtnums <- dtnums[,list(age = mean(age), area = mean(area)), by = cust_id]
setkey(dtnums, cust_id)

# Count total basket, total item, total spend per customer
basket <- dt[,list(num_bask = length(trans_id), num_item = sum(quantity), spend = sum(quantity*price)), by = list(cust_id, prod_cat, prod_id)]
basket <- basket[,list(num_bask = sum(num_bask), num_item = sum(num_item), spend = sum(spend), num_productId = length(prod_id)), by = list(cust_id, prod_cat)]
basket <- basket[,list(num_bask = as.numeric(sum(num_bask)), num_item = as.numeric(sum(num_item)), spend = sum(spend), num_productId = as.numeric(sum(num_productId)), num_productCat = as.numeric(length(prod_cat))), by = cust_id]
setkey(basket, cust_id)

# Distribution of items and spend per customer
dists_ispc <- dt[, list(num_item = sum(quantity), spend = sum(quantity*price)), by = list(trans_date,cust_id)]
dists_ispc <- dists_ispc[,list(ipc_max = max(num_item), ipc_med = median(num_item), ipc_min = min(num_item), spc_max = max(spend), spc_med = median(spend), spc_min = min(spend)), by = cust_id]
setkey(dists_ispc, cust_id)

# Distribution of product_id and product_subclass per customer
dists_pppc <- dt[, list(num_productId = length(prod_id)), by = list(cust_id,trans_date, prod_cat)]
dists_pppc <- dists_pppc[, list(num_productId = sum(num_productId), num_productCat = length(prod_cat)), by = list(trans_date,cust_id)]
dists_pppc[,c("num_productId","num_productCat"):=list(as.numeric(num_productId),as.numeric(num_productCat))]
dists_pppc <- dists_pppc[, list(productCat_min = min(num_productCat), productCat_med = median(num_productCat), productCat_max = max(num_productCat)), by = cust_id]
setkey(dists_pppc, cust_id)


# Finding prop. of baskets in N bands of products cats and ids by imtem and by price
# Create classification of product_id at product_cat level
popcatid <- dt[,list(nbask=length(trans_id)),by=list(prod_cat,prod_id)]
popcat <- popcatid[,list(nbask=sum(nbask),nprodid=length(prod_id)),by=prod_cat]
popcat[, popcat_nbask:= LETTERS[as.numeric(cut2(nbask, g=5))]]
popcat[, popcat_size:= LETTERS[as.numeric(cut2(nprodid, g=5))]]
# We classify the number of transactions and product_id in each of product subclass
# The letter A shows low and E shows high number of product_id and transactions 

# Create classification of number of transaction at product_id level
popid <- popcatid[, list(nbask = sum(nbask)), by = prod_id]
popid[, popid_nbask:= LETTERS[as.numeric(cut2(nbask, g=5))]]
# The letter A shows product that has low transaction, E has highest transaction

# Create classification of price at product_id level
priceid <- dt[, list(avrprice = median(price)), by = prod_id]
priceid[, priceRank:= LETTERS[as.numeric(cut2(avrprice, g =5))]]
# The letter A shows low price of product, E shows high price

# Merge these pop variable back to dt
dtp <- copy(dt)
# Merge popcat
setkey(dtp, prod_cat)
dtpop <- merge(dtp, popcat[,list(prod_cat, popcat_nbask, popcat_size)])
dtpop[, length(trans_id), by = popcat_size]

# Merge popid
setkey(dtp, prod_id)
setkey(dtpop, prod_id)
dtpop <- merge(dtpop, popid[, list(prod_id, popid_nbask)])

# Merge priceid
dtpop <- merge(dtpop,priceid[,list(prod_id,priceRank)])

# Group by various cat and id pops and preserve row totals margin
dtpop_popcat_nbask <- data.table(dcast(dtpop,cust_id~popcat_nbask,value.var="trans_id"))
setkey(dtpop_popcat_nbask,cust_id)
dtpop_popcat_size <- data.table(dcast(dtpop,cust_id~popcat_size,value.var="trans_id"))
setkey(dtpop_popcat_size,cust_id)
dtpop_popid_nbask <- data.table(dcast(dtpop,cust_id~popid_nbask,value.var="trans_id"))
setkey(dtpop_popid_nbask,cust_id)
dtpop_prodprice <- data.table(dcast(dtpop,cust_id~priceRank,value.var="trans_id"))
setkey(dtpop_prodprice,cust_id)
rm(dat1, dat2, dat3, dat4)
setnames(dtpop_popcat_nbask, c("cust_id", "popcat_nbaskA", "popcat_nbaskB", "popcat_nbaskC", "popcat_nbaskD", "popcat_nbaskE"))
setnames(dtpop_popcat_size, c("cust_id", "popcat_sizeA", "popcat_sizeB", "popcat_sizeC", "popcat_sizeD", "popcat_sizeE"))
setnames(dtpop_popid_nbask, c("cust_id", "popid_nbaskA", "popid_nbaskB", "popid_nbaskC", "popid_nbaskD", "popid_nbaskE"))
setnames(dtpop_prodprice, c("cust_id", "poppriceA", "poppriceB", "poppriceC", "poppriceD", "poppriceE"))

# Merge all data
tfcust <- merge(dtnums, basket)
tfcust <- merge(tfcust, dists_ispc)
tfcust <- merge(tfcust, dists_pppc)
tfcust <- merge(tfcust, dtpop_popcat_nbask)
tfcust <- merge(tfcust, dtpop_popcat_size)
tfcust <- merge(tfcust, dtpop_popid_nbask)
tfcust <- merge(tfcust, dtpop_prodprice)
write.csv(tfcust, file = "TaFeng.csv")
str(tfcust)

# Dimension reduction using PCA
library(stats)
cstZi <- data.table(scale(tfcust[,which(!colnames(tfcust) %in% c("cust_id")),with=F]))
pca_tfcust <- prcomp(cstZi, scale. = TRUE)
plot(pca_tfcust)
tfcustPCA <- data.table(tfcust[,list(cust_id)],pca_tfcust$x)
head(score_pca[1:2])
plot(score_pca)
summary(pca_tfcust)

# Clustering
rm(tfcust_group, tfcust_clustering)
# Initial plot and model
p1 <- ggplot(tfcust[seq(1, nrow(tfcust), 10)]) + geom_point(aes(x = productCat_max, y = popcat_nbaskD))
plot(p1)

sptclst <- Mclust(tfcust[seq(1, nrow(tfcust),40), list(productCat_max, popcat_nbaskD)], G = (1:6))
summary(sptclst, parameters = T)
plot(sptclst)

# Limit the tfcustPCA data, take first 5 PC, 10% of the data
moddata <- tfcustPCA[round(seq(1,nrow(tfcustPCA),10)),(2:6),with=F]

# Running Mclust
tfcustPCA_clst <- Mclust(moddata)
summary(tfcustPCA_clst)
plot(tfcustPCA_clst)

modclust <- (4)
modnames <- c("VVV")

tfClust <- Mclust(moddata, G = modclust, modelNames = modnames)
summary(tfClust)
plot(tfClust)

# Clustering by k-means
