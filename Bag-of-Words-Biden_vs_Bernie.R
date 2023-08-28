library(plyr)
library(caret)

#Set
setwd("/Users/alexrodriguez/Desktop/Graduate Classes/Data Prep/Data")

file_path_tweets <- c(
  "Bernie tweets.csv",   
  "Biden tweets.csv"    
)

list_tweets <- lapply(
  X = file_path_tweets,
  FUN = read.csv,
  colClasses = "character"
)

names(list_tweets) <- file_path_tweets

for(j in 1:length(list_tweets)){
  names(list_tweets)[j] <- names(sort(
    x = table(make.names(list_tweets[[j]]$name)),
    decreasing = TRUE
  ))[1]
}
for(j in names(list_tweets)) list_tweets[[j]]$Target <- j
for(j in names(list_tweets)) list_tweets[[j]] <- list_tweets[[j]][,c("Target","text")]
tweets <- dplyr::bind_rows(list_tweets)

tweets$text <- stringr::str_to_lower(tweets$text)

tweets$text <- qdapRegex::rm_twitter_url(
  tweets$text,
  replacement = " ",
  clean = TRUE
)

tweets$text <- qdapRegex::rm_url(
  tweets$text,
  replacement = " ",
  clean = TRUE
)

tweets$text <- qdapRegex::rm_hash(
  tweets$text,
  replacement = " ",
  clean = TRUE
)

tweets$text <- qdapRegex::rm_tag(
  tweets$text,
  replacement = " ",
  clean = TRUE
)

tweets$text <- qdapRegex::rm_emoticon(
  tweets$text,
  replacement = " ",
  clean = TRUE
)

tweets$text <- qdapRegex::rm_email(
  tweets$text,
  replacement = " ",
  clean = TRUE
)

tweets$text <- qdapRegex::rm_between(
  tweets$text,
  left = "<",
  right = ">",
  replacement = " ",
  clean = TRUE
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "â€œ",
  replacement = " "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "â€™",
  replacement = "'"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "á",
  replacement = "a"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "ã",
  replacement = "a"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "š",
  replacement = "s"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "ñ",
  replacement = "n"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "â",
  replacement = "a"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "¿",
  replacement = "?"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "é",
  replacement = "e"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "ó",
  replacement = "o"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "í",
  replacement = "i"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "\\+",
  replacement = " plus "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "=",
  replacement = " equals "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "±",
  replacement = " plus minus "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "\u009d",
  replacement = " "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "¼",
  replacement = " quarter "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "½",
  replacement = " half "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "³",
  replacement = " 3 "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "º",
  replacement = " degree "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = " i'm ",
  replacement = " i am "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "'re ",
  replacement = " are "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "'t ",
  replacement = " not "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "'ve ",
  replacement = " have "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "'ll ",
  replacement = " will "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = " doesn't ",
  replacement = " does not "
)

tweets$text <- qdapRegex::rm_phone(
  tweets$text,
  clean = TRUE
)

tweets$text <- qdapRegex::rm_zip(
  tweets$text,
  clean = TRUE
)

tweets$text <- qdapRegex::rm_time(
  tweets$text,
  replacement = " ",
  clean = TRUE
)

tweets$text <- qdapRegex::rm_date(
  tweets$text,
  replacement = " ",
  clean = TRUE
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "â€™",
  replacement = "'"
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "â",
  replacement = " "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "€",
  replacement = " "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "™",
  replacement = " "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "[:punct:]",
  replacement = " "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "[:digit:]",
  replacement = " "
)

tweets$text <- stringr::str_replace_all(
  string = tweets$text,
  pattern = "\\W",
  replacement = " "
)

tweets$text <- tm::removeWords(
  x = tweets$text,
  words = tm::stopwords(kind = "SMART")
)

tweets$text <- tm::removeWords(
  x = tweets$text,
  words = tm::stopwords(kind = "english")
)

tweets$text <- tm::removeWords(
  x = tweets$text,
  words = qdapDictionaries::Top200Words
)

tweets$text <- trimws(stringr::str_replace_all(
  string = tweets$text,
  pattern = "\\s+",
  replacement = " "
))

tweets$text <- gsub(
  x = tweets$text,
  pattern = "aaaaand",
  replacement = "and"
)

tweets$text <- gsub(
  x = tweets$text,
  pattern = "american",
  replacement = "america"
)

tweets$text <- gsub(
  x = tweets$text,
  pattern = "thata",
  replacement = "that"
)

set.seed(2539)
strsplit_tweets <- strsplit(tweets$text," ")
dictionary_tweets <- sort(unique(unlist(strsplit_tweets)))
strsplit_tweets <- lapply(
  X = strsplit_tweets,
  FUN = tm::stemDocument
)

strsplit_tweets <- lapply(
  X = strsplit_tweets,
  FUN = tm::stemCompletion,
  dictionary = dictionary_tweets
)

strsplit_tweets <- lapply(
  X = strsplit_tweets,
  FUN = paste,
  collapse = " "
)
tweets$text <- unlist(strsplit_tweets)

Corpus_tweets <- tm::VCorpus(tm::VectorSource(tweets$text))

DocumentTermMatrix_tweets <- tm::DocumentTermMatrix(Corpus_tweets)

DocumentTermMatrix_tweets <- tm::removeSparseTerms(
  x = DocumentTermMatrix_tweets,
  sparse = 0.98
)
tm::inspect(DocumentTermMatrix_tweets)
dim(DocumentTermMatrix_tweets)

rowTotals <- apply(DocumentTermMatrix_tweets , 1, sum)
tweets <- tweets[rowTotals> 0, ]
DocumentTermMatrix_tweets <- DocumentTermMatrix_tweets[rowTotals> 0, ]


M <- as.matrix(DocumentTermMatrix_tweets)
dim(M)
M

term_frequency <- data.frame(
  Term = colnames(M),
  Frequency = colSums(M),
  stringsAsFactors = FALSE
)
term_frequency <- term_frequency[order(term_frequency$Frequency),]
tail(term_frequency)

wordcloud::wordcloud(
  words = term_frequency$Term,
  freq = term_frequency$Frequency,
  max.words = 25,
  random.order = FALSE,
  colors = viridis::viridis(100),
)
title(paste("Bernie Sanders & Joe Biden \n Most frequent Terms"))

aggregate_tweets <- aggregate(
  x = tweets$text,
  FUN = paste,
  collapse = " ",
  by = list(
    Target = tweets$Target
  )
)
colnames(aggregate_tweets) <- colnames(tweets)

tdm_aggregate <- tm::TermDocumentMatrix(tm::VCorpus(tm::VectorSource(aggregate_tweets$text)))
sqrt_n <- sqrt(nrow(aggregate_tweets))
par(
  oma = c(0,0,0,0),
  mai = c(0,0,0,0),
  mar = c(0,0,0,0)
)
set.seed(2539)
for(j in 1:nrow(aggregate_tweets)){
  wordcloud::commonality.cloud(
    term.matrix = tdm_aggregate[,j],
    max.words = 15,
    random.order = FALSE,
    colors = viridis::viridis(100),
    title.size = 1
    
  )
  title(aggregate_tweets$Target[j])
}


dtm_aggregate <- tm::DocumentTermMatrix(tm::VCorpus(tm::VectorSource(aggregate_tweets$text)))
M_aggregate <- as.matrix(dtm_aggregate)

T_M_aggregate <- t(M_aggregate)

M_toptwo <- as.data.frame(T_M_aggregate)
M_toptwo <- M_toptwo[rowSums(M_toptwo) > 0,]
# M_toptwo <- rename(M_toptwo,c("1" = "Bernie Sanders", "2" = "Joe Biden"))
M_toptwo$Difference <- M_toptwo[,"1"] - M_toptwo[,"2"]
M_toptwo$abs_Difference <- abs(M_toptwo[,"1"] - M_toptwo[,"2"])
for(j in 1:4) M_toptwo <- M_toptwo[order(M_toptwo[,j],decreasing = TRUE),]
M_toptwo <- M_toptwo[1:25,]
M_toptwo <- M_toptwo[order(M_toptwo$Difference),]
plotrix::pyramid.plot(
  lx = M_toptwo$"1",
  rx = M_toptwo$"2",
  labels = rownames(M_toptwo),
  top.labels = c("Bernie","","Biden"),
  lxcol = "red",
  rxcol = "blue",
  unit="count",
  gap = 100
)

Test_df <- cbind(tweets$Target,M)
Test_df <- as.data.frame(Test_df)
names(Test_df)[names(Test_df)=='V1']<-'Target'
Test_df$Target[Test_df$Target=='Bernie.Sanders']<-0
Test_df$Target[Test_df$Target=='Joe.Biden..Text.Join.to.30330.']<-1
df <- as.data.frame(sapply(Test_df, as.numeric))
df_new <- as.data.frame(sapply(Test_df, as.numeric))
dim(df)

set.seed(2539)
trainIndex <- createDataPartition(df_new$Target, p=.7, list = FALSE, times = 1)
Train <- df_new[ trainIndex,]
Test  <- df_new[-trainIndex,]
head(Train)
head()

model <- glm(formula = Target ~ ., family = binomial, data = Train)
model_selection <- step(model)
summary(model_selection)

x_test <- subset(Test, select = -c(Test$Target))
y_test <- subset(Test, select = c(Target))
pred <- predict(model, x_test)
#pred <- as.data.frame(pred)
confusionMatrix(pred, y_test)


str(predictions)
str(y_test)
levels(predictions)
levels(y_test)

model <- glm(formula = Target ~ ., family = binomial, data = df)
model_selection <- step(model)
summary(model_selection)
Forward_model <- stepAIC(model,direction="forward")
summary(Forward_model)


Train_model <- glm(
  formula = Target ~ .,
  data = list_M[["train"]],
  family = "binomial"
)
