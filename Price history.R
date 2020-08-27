library(trundler)
library(data.table)
library(tibble)
library(dplyr)

#================api key========================================================
set_api_key("##########################")
#================set working directory==========================================
filedir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(filedir)
#================Pull prices for analysis=======================================
#Check the price movements of Lindt Excellence chocolate
#---------Set variables----------------------------------------
string1 <- "Lindt Excellence"
string2 <- "100g"
string3 <- ""
regExpression <- paste("(?=.*?(",string1,"))(?=.*?(",string2,"))(?=.*?(",string3,"))",sep="")
# #--------all retailers and all sepecific products per string1-------------------------------------
all_retailers <- retailer()   #TRUNDLER function
products_found <- products(string1)  #TRUNDLER function
products_found <- as.data.frame(fread("products_found"))

#---Merge and clean up
retailer_products <- merge(products_found,all_retailers,by="retailer_id",all.x=TRUE)
retailer_products <- distinct(retailer_products, sku, .keep_all = TRUE) #remove duplicate product listing - assume SKU is unique
drops <- c("brand","model","sku","retailer_url","visible")
retailer_products <- retailer_products[,!(names(retailer_products) %in% drops)] #drop some columns
retailer_products <- retailer_products[complete.cases(retailer_products), ] #make sure there are no NA values - remove lines with NA values
#-----Filter more specific product-----------
retailer_products <- retailer_products[grepl(regExpression,retailer_products$product,ignore.case=TRUE, perl=TRUE),]
#---------Find price history of these products--------------------
all_prices <- tibble(Retailer=character(),Product=character(),Date=character(),Price=numeric(),Discounted=numeric())

for (i in 1:nrow(retailer_products)) {
  Sys.sleep(2) #delay to avoid getting blocked
  price_page <- product_prices(retailer_products[i,]$product_id)  #TRUNDLER function
  for (j in 1:nrow(price_page)) { 
    date <- format(price_page[j,]$time,format='%Y-%m-%d')
    discount <- price_page[j,]$price - price_page[j,]$price_effective
    price_row <- tibble_row(Retailer = retailer_products[i,]$retailer,
                            Product = retailer_products[i,]$product, Date = date, 
                            Price = price_page[j,]$price_effective,
                            Discounted = discount)
    all_prices <- rbind(all_prices,price_row)
  }
}

fwrite(all_prices,"all_prices")
