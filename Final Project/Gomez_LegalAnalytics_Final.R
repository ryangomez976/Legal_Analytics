# Getting a TDM and predicting parties while retaining file names
library(XML)
library(SnowballC)
library(plyr)
library(class)
library(RCurl)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(RWeka)
library(R.utils)
library(reshape2)
library(rpart)

# Creating and cleaning the corpus
corpus <- VCorpus(DirSource("~/Desktop/Legal_Analytics_Final/plain_html/"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
myStopwords <- c(stopwords("english"), "p", "br", "td", "tr", "colspan", "d", "amp", "nbsp", "border", "<body>", "</body>", "<p>", "</p>")
corpus <- tm_map(corpus, removeWords, myStopwords)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus_clean <- corpus

# Creating a word cloud for high-level overview of the corpus
corpus <- tm_map(corpus, stemDocument, language = "english")
tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(3, 14)))
tdm_clean <- tdm
tdm <- removeSparseTerms(tdm, 0.50)
findFreqTerms(tdm, lowfreq = 1000)
matrix <- as.matrix(tdm)
wordFreq.sort <- sort(rowSums(matrix), decreasing=T)
pal <- brewer.pal(8,"Accent")
pal <- pal[-(1:4)]
word.cloud <- wordcloud(words = names(wordFreq.sort), freq = wordFreq.sort, min.freq = 1000, random.order = F, colors = pal)

# Finding "sideletter" clauses. A sideletter reflects terms that are important to the parties, but came to light outside of the normal bargaining process, or were important, but not important enough to include in the CBA.
options(mc.cores=1)
corpus <- corpus_clean
corpus <- tm_map(corpus, stemDocument, language = "english")

gram1Tokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

tdm1 <- TermDocumentMatrix(corpus, control = list(tokenize=gram1Tokenizer, wordLengths = c(3, 14)))
tdm1 <- removeSparseTerms(tdm1, 0.99)

df <- as.data.frame(inspect(tdm1))
dft <- t(df)
dft <- data.frame(dft)
sideletter <- c("Evergreen.htm", "2013-16_SEA_Agreement_ONLINE.htm", "ACTA Contract July 1 2009 - June 30 2012 FINAL JUNE 2011.htm", "CUTA_Agreement_2012-2013_through_2014-2015.htm", "Eureka 2014-15 Certificated Agreement (Contract) FINAL VERSION 9-12-14.htm", "GSCFT_2011_2014_Contract_revised_for_12_13_signature.htm", "hr_cert_agreement.htm", "MEA - Master Agreement 2014-2017.htm", "SJTA COLLECTIVE BARGAINING CONTRACT_1416.htm", "SLTA Contract 2012-2015 Final.htm")
sideletterpresence <- c()
sideletterpresence <- c(sideletterpresence,(ifelse((rownames(dft) %in% sideletter), 1, 0)))
dft <- cbind(dft, sideletterpresence)
dft$class <- as.factor(dft$sideletterpresence)

set.seed(5983)
training <- sample(nrow(dft), ceiling(nrow(dft) * 0.7))
test <- (1:nrow(dft))[-training]
training <- dft[training,]
test <- dft[test,]

patree <- rpart(sideletterpresence ~ ., data=training, method = "class")
patree.predictions.eval <- predict(patree, test, type = "class")

confusionmatrix <- table(test$sideletterpresence, patree.predictions.eval)
confusionmatrix

tn <- confusionmatrix[1,1] #true negative
fp <- confusionmatrix[1,2] #false positive
fn <- confusionmatrix[2,1] #false negative
tp <- confusionmatrix[2,2] #true positive

accuracy.rate <- (tp + tn) / (tp + tn + fp + fn)
accuracy.rate

# Professional workday vs. defined workday. The presence of a professional workday clause likely indicates more autonomy for teachers, whereas a defined workday likely indicates less teacher autonomy. 
# 1 indicates a professional workday, 0 indicates a defined workday. This will be switched for another classification to observe any differences.
options(mc.cores=1)
corpus <- corpus_clean
corpus <- tm_map(corpus, stemDocument, language = "english")

gram1Tokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

tdm1 <- TermDocumentMatrix(corpus, control = list(tokenize=gram1Tokenizer, wordLengths = c(3, 14)))
tdm1 <- removeSparseTerms(tdm1, 0.99)

df <- as.data.frame(inspect(tdm1))
dft <- t(df)
dft <- data.frame(dft)
profworkday <- c("2012-13 MCTA Agreement PDF.htm", "2013-16 contract.htm", "3244577001150048049.htm", "3350576201222205676.htm", "7543943393345802461.htm", "BEA Contract 2012-2015 with Appendices.htm", "BEA.htm", "contract 2014-2017.htm", "Evergreen.htm", "J.C.T.A. contract 2010-2013.htm", "MARFAC contract 2011-14 (FINAL) (1).htm", "scoeta_contract.htm", "SEA-Teachers-Contract-2013-2016.htm", "TAWC Contract - Master - 2011 - 2014.htm")

profworkdayclass <- c()
profworkdayclass <- c(profworkdayclass,(ifelse((rownames(dft) %in% profworkday), 1, 0)))
dft <- cbind(dft, profworkdayclass)
dft$class <- as.factor(dft$profworkdayclass)

set.seed(5983)
training <- sample(nrow(dft), ceiling(nrow(dft) * 0.7))
test <- (1:nrow(dft))[-training]
training <- dft[training,]
test <- dft[test,]

patree <- rpart(profworkdayclass ~ ., data=training, method = "class")
patree.predictions.eval <- predict(patree, test, type = "class")

confusionmatrix <- table(test$profworkdayclass, patree.predictions.eval)
confusionmatrix

tn <- confusionmatrix[1,1] #true negative
fp <- confusionmatrix[1,2] #false positive
fn <- confusionmatrix[2,1] #false negative
tp <- confusionmatrix[2,2] #true positive

accuracy.rate <- (tp + tn) / (tp + tn + fp + fn)
accuracy.rate

# 1 indicates a defined workday, 0 indicates a professional workday.
options(mc.cores=1)
corpus <- corpus_clean
corpus <- tm_map(corpus, stemDocument, language = "english")

gram1Tokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}

tdm1 <- TermDocumentMatrix(corpus, control = list(tokenize=gram1Tokenizer, wordLengths = c(3, 14)))
tdm1 <- removeSparseTerms(tdm1, 0.99)
df <- as.data.frame(inspect(tdm1))
dft <- t(df)
dft <- data.frame(dft)
defworkday <- c("4733679209303521488.htm", "5724342063445673460.htm", "7679200709597512380.htm", "APT-PUSD AGREEMENT 2014-2016-FINAL.htm", "cba020711.htm", "Certificated Contract 2013_2016_Board approved June 13_2013.htm", "FINAL_2015__2013-2016_Ratified_TTA_Contract.htm", "GSCFT_2011_2014_Contract_revised_for_12_13_signature.htm", "LSEA contract agreement 07-09.htm", "mhusd-mhft_contract_2012_-_2015_revised_july_1_2014_2.htm", "NTA-Contract-July-1-2014-June-20-2016-FINAL.htm", "PVTA Contract 2013-2016 FINAL.htm", "SDTA Collective Bargaining Agreement 2013-16.htm", "Tulare_CertificatedMasterContract20142017_020415.htm", "Woodville  teacher Contract  12-13.htm", "YCTA Contract.htm")

defworkdayclass <- c()
defworkdayclass <- c(defworkdayclass,(ifelse((rownames(dft) %in% defworkday), 1, 0)))
dft <- cbind(dft, defworkdayclass)
dft$class <- as.factor(dft$defworkdayclass)

set.seed(5983)
training <- sample(nrow(dft), ceiling(nrow(dft) * 0.7))
test <- (1:nrow(dft))[-training]
training <- dft[training,]
test <- dft[test,]

patree <- rpart(defworkdayclass ~ ., data=training, method = "class")
patree.predictions.eval <- predict(patree, test, type = "class")

confusionmatrix <- table(test$defworkdayclass, patree.predictions.eval)
confusionmatrix

tn <- confusionmatrix[1,1] #true negative
fp <- confusionmatrix[1,2] #false positive
fn <- confusionmatrix[2,1] #false negative
tp <- confusionmatrix[2,2] #true positive

accuracy.rate <- (tp + tn) / (tp + tn + fp + fn)
accuracy.rate