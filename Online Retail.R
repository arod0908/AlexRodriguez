library(bnlearn)
library(knitr)
library(tidyverse)
library(arulesViz)
library(arules)
library(lubridate)
library(plyr)
library(dplyr)
options(digits = 7)

online_retail <- readxl::read_excel("/Users/alexrodriguez/Desktop/Graduate Classes/Data Prep/Final Exam/Online Retail.xlsx")
online_retail_new <- online_retail[, c('InvoiceNo', 'Description', 'Quantity')]

#Ordering Data by decreasing Quantity
attach(online_retail_new)
df <- online_retail_new[order(-Quantity),]
detach(online_retail_new)

#Getting rid of rows with N/A and negative Quantities
str(df)
df <- na.omit(df)
df <- df[df$Quantity >= 0, ]

#Getting rid of Errors and noninformational data
df<-df[!(df$Description=="?" | df$Description=="came coded as 20713" 
         | df$Description=="incorrectly credited C550456 see 47" | df$Description=="Manual" 
         | df$Description=="Adjust bad debt" | df$Description=="incorrectly credited C550456 see 47" 
         | df$Description=="did  a credit  and did not tick ret"  | df$Description=="wrongly coded 23343" 
         | df$Description=="wrongly coded 20713" | df$Description=="wrongly sold (22719) barcode" 
         | df$Description=="found" | df$Description=="Found" | df$Description=="FOUND" 
         | df$Description=="Found in w/hse" | df$Description=="found box" | df$Description=="Found by jackie" 
         | df$Description=="wrongly marked" | df$Description=="Sale error" | df$Description=="check?" 
         | df$Description=="add stock to allocate online orders" | df$Description=="Adjustment" 
         | df$Description=="adjustment" | df$Description=="Adjust bad debt" | df$Description=="taig adjust" 
         | df$Description=="amazon adjust" | df$Description=="dotcom adjust" | df$Description=="Amazon Adjustment" 
         | df$Description=="wrongly marked 23343" | df$Description=="website fixed" 
         | df$Description=="to push order througha s stock was" | df$Description=="returned" 
         | df$Description=="test" | df$Description=="TRAVEL CARD WALLET DOTCOMGIFTSHOP" 
         | df$Description=="rcvd be air temp fix for dotcom sit" | df$Description=="dotcom" | df$Description=="DOTCOM POSTAGE" 
         | df$Description=="DOTCOMGIFTSHOP TEA TOWEL" | df$Description=="allocate stock for dotcom orders ta" 
         | df$Description=="dotcomstock" | df$Description=="Dotcomgiftshop Gift Voucher £20.00" 
         | df$Description=="MUG , DOTCOMGIFTSHOP.COM" | df$Description=="Dotcomgiftshop Gift Voucher £40.00" 
         | df$Description=="Dotcomgiftshop Gift Voucher £50.00" | df$Description=="Dotcomgiftshop Gift Voucher £30.00" 
         | df$Description=="Dotcomgiftshop Gift Voucher £10.00" | df$Description=="Dotcomgiftshop Gift Voucher £100.00" 
         | df$Description=="on cargo order" | df$Description=="mailout" | df$Description=="michel oops" 
         | df$Description=="had been put aside" | df$Description=="Had been put aside." 
         | df$Description=="damaged" | df$Description=="checked" 
         | df$Description=="amazon" | df$Description=="amazon sales" | df$Description=="AMAZON FEE"  
         | df$Description=="Amazon" | df$Description=="AMAZON" | df$Description=="alan hodge cant mamage this section" 
         | df$Description=="for online retail orders" | df$Description=="alan hodge cant mamage this section"),]

#Cleaning Up the Descriptions of the Data
df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "GLASS/",
  replacement = "GLASS / "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "S/",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "SET OF",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "SET/",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "SET",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "PACK OF",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "SMALL",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "LARGE",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "MEDIUM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "PINK",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "BLUE",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "RED",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "PURPLE",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "GREEN",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "YELLOW",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "BLACK",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "WHITE",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "ASSTD DESIGNS",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "ASSTD DESIGN",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "ASSORTED COLOURS",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "ASSORTED COLOUR",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "3D",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "125g",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "150g",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "3.5g",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "A5 SIZE",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "A6 SIZE",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "A7 SIZE",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "15CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "30CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "40CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "45CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "50CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "60CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "120CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "15C",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "45x30cm",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "45x45cm",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "40x40cm",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "30x30cm",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "30CMx30CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "65CMx65CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "25x24x12cm",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "34X20CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "16X16CM",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "60x40cm",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "11 PC",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "50'S",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "70'S",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "50g",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "500g",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "0",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "1",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "2",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "3",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "4",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "5",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "6",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "7",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "8",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "9",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "\\+",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "&",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "/",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = '"KEEP CLEAN"',
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = "[:punct:]",
  replacement = " "
)

df$Description <- stringr::str_replace_all(
  string = df$Description,
  pattern = ",",
  replacement = " "
)

df$Description <- trimws(stringr::str_replace_all(
  string = df$Description,
  pattern = "\\s+",
  replacement = " "
))

write.csv(df, "/Users/alexrodriguez/Desktop/Graduate Classes/Data Prep/Final Exam/df.csv")

df_set <- read.csv("/Users/alexrodriguez/Desktop/Graduate Classes/Data Prep/Final Exam/df_set.csv", header = TRUE)
df3 <- df_set
df3$InvoiceNo <- NULL

colnames(df3) <- make.names(colnames(df3))

df3 <- df3[
  order(rowSums(df3),decreasing = TRUE),
  order(colSums(df3),decreasing = TRUE)
]

v_sums <- colSums(df3)
mean(cumsum(v_sums)/sum(v_sums) <= 0.20)

sum(cumsum(v_sums)/sum(v_sums) <= 0.20)

v_names <- names(v_sums[cumsum(v_sums)/sum(v_sums) <= 0.2])
df3 <- df3[,v_names]

################################################################################################################################################
#Come back after you finish Model prediction
str(df3)
df3<- as.data.frame(df3)
col_names <- colnames(df3)
col_sums <- colSums(df3)
col_sums <- as.data.frame(col_sums)
library(data.table)
setDT(col_sums,keep.rownames = TRUE)[]
#Names of columns of dataframe
names(col_sums) <- c("Description", "Count In Invoice")

col_sums$Description <- stringr::str_replace_all(
  string = col_sums$Description,
  pattern = "[:punct:]",
  replacement = " "
)

col_sums$Description <- trimws(stringr::str_replace_all(
  string = col_sums$Description,
  pattern = "\\s+",
  replacement = " "
))
#bob <- table(Final_data$Description)
aa <- as.data.frame(table(Final_data$Description))
names(aa)[names(aa) == 'Var1'] <- 'Description'

Summary <- merge(
  x = col_sums,
  y = aa,
  by.x = "Description",
  by.y = "Description",
  all.x = TRUE,
  all.y = TRUE
)

Summary[is.na(Summary)] <- 0
names(Summary)[names(Summary) == 'Freq'] <- 'Count in Recommendation set'

write.csv(Summary,"/Users/alexrodriguez/Desktop/Graduate Classes/Data Prep/Final Exam/Summary Table Final.csv", row.names = FALSE)

############################################################################################################################################################

for(j in 1:ncol(df3)){
  df3[,j] <- factor(as.numeric(df3[,j]))
}

v_algorithms <- c(
  "iamb.fdr","fast.iamb","inter.iamb","gs","iamb","hpc","si.hiton.pc","mmpc","pc.stable",
  "hc","tabu",
  "h2pc","mmhc","rsmax2",
  "aracne","chow.liu"
)

library(bnlearn)
list_bnlearn <- list()
for(j in "hc") try({
  list_bnlearn[[j]] <- do.call(
    what = j,
    args = list(x = df3[,v_names])
  )
},silent = TRUE)


bn_retail <- bn.fit(
  x = list_bnlearn[["hc"]],
  data = df3[,v_names]
)

v_subset <- sort(sample.int(
  n = nrow(df3),
  size = 19886
))

subset_retail <- df3[v_subset,]
final <- list()
final$InvoiceNo <- rownames(subset_retail)
final <- as.data.frame(final)


predict_retail <- matrix(
  data = 0,
  nrow = nrow(subset_retail),
  ncol = ncol(subset_retail)
)
rownames(predict_retail) <- rownames(subset_retail)
colnames(predict_retail) <- colnames(subset_retail)
v_index <- 1:ncol(subset_retail)


for(j in 1:nrow(subset_retail)){
  for(k in v_index[subset_retail[j,] == "0"]){
    predict_retail[j,k] <- attr(
      x = predict(
        object = bn_retail,
        data = subset_retail[j,-k],
        node = v_names[k],
        prob = TRUE
      ),
      which = "prob"
    )["1",1]
  }
}
#predict_retail[,1:6]
list_predict <- list()
for(j in 1:nrow(subset_retail)){
  list_predict[[j]] <- list()
  list_predict[[j]]$In_Basket <- v_names[subset_retail[j,] == "1"]
  list_predict[[j]]$Promote <- sort(predict_retail[j,predict_retail[j,] > 0],decreasing = TRUE)
  list_predict[[j]]$Promoted <- which.max(sort(predict_retail[j,predict_retail[j,] > 0],decreasing = TRUE))
}

data <- as.data.frame(t(do.call(cbind,list_predict)))
data <- stack(unlist(data$Promoted))
data$InvoiceNo <- df3$InvoiceNo

names(data)[names(data) == 'ind'] <- 'Description'

data$Description <- stringr::str_replace_all(
  string = data$Description,
  pattern = "[:punct:]",
  replacement = " "
)

data$Description <- trimws(stringr::str_replace_all(
  string = data$Description,
  pattern = "\\s+",
  replacement = " "
))

Final_data <- data[, c("InvoiceNo", "Description")]
write.csv(Final_data,"/Users/alexrodriguez/Desktop/Graduate Classes/Data Prep/Final Exam/Recommendation Data Set.csv", row.names = FALSE)
#END