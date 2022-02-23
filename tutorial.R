#reading a text file
library(readtext)
intro=readtext(file="C:/Users/zacha/Documents/teaching/text_analysis/intro.txt")
intro=as.character(intro[,2])
intro

#LOADING AND ATTACHING DATA
setwd("C:/Users/zacha/Documents/teaching/text_analysis")
load(file="C:/Users/zacha/Documents/teaching/text_analysis/JOINTDATA.rda")
attach(JOINTDATAFRAME)

#exploring the data
colnames(JOINTDATAFRAME)

#basic stuff
test_text=JOINTDATAFRAME[500,2]
test_text
  #TRIMMING WHITE SPACE
test_text=gsub("\\s+"," ",test_text)
test_text
  #testing for presense of a particular word
grepl("mystery", test_text, ignore.case = TRUE)
grepl("mystery", test_text, ignore.case = FALSE)
grepl("Italy", test_text, ignore.case = TRUE)
  #replacing a word or string
gsub("London", "Rome", test_text)
gsub("London", "Rome", gsub("convicted", "found innocent", test_text, ignore.case = TRUE))

#tokenizing
library(quanteda)
tokens_test=tokens(test_text, what="word", remove_punct = TRUE, remove_symbols = TRUE)
tokens_test
ntoken(tokens_test) #this is counting the number of words
ntype(tokens_test) #this is the number of unique words

    #now lets try a bigger example
test_text=JOINTDATAFRAME[c(6:25),2] #bigger set of multiple texts
test_text=gsub("\\s+"," ",test_text)
          #Do any of the texts mention Italy?
grepl("Italy", test_text, ignore.case = TRUE)
                  #let's look at this in a clearer way
test=grepl("Italy", test_text, ignore.case = TRUE)
length(test[which(test==TRUE)])
length(test[which(test==FALSE)])
        #Do any of these cases mention London?
test=grepl("London", test_text, ignore.case = TRUE)
length(test[which(test==TRUE)])
    #How long is each text and how many unique words are there in each?
tokens_test=tokens(test_text, what="word", remove_punct = TRUE, remove_symbols = TRUE)
ntoken(tokens_test)
ntype(tokens_test)
    #Let's try and see the average number of times a word is repeated in each text, this can be a measure of 
          #linguistic uniqueness
ntoken(tokens_test)/ntype(tokens_test)
    #how many times does a word appear in each text?
corpus=corpus(test_text)
corpus
test_dfm=dfm(corpus)
list=test_dfm[,which(colnames(test_dfm)=="guilty")]
    #checking a particular text
list[20,]


#let's try the same excersize on our large set of texts!
big_corpus=corpus(JOINTDATAFRAME[,2])
big_test_dfm=dfm(big_corpus)
list=big_test_dfm[,which(colnames(big_test_dfm)=="tavern")]
    #So let's answer some questions with this
        #how many cases mention a tavern at least once?
list=cbind(as.data.frame(list), JOINTDATAFRAME$YEAR)
list$tavern_indicator=ifelse(list$tavern>0,1,0)
sum(list$tavern_indicator)
        #and as a percent of all cases
sum(list$tavern_indicator)/nrow(list)
        #how many cases in a particular year mention taverns?
sum(list[which(list[,3]==1861),4])
        #what is the earliest year we see taverns mentioned?
min(as.numeric(list[which(list[,4]==1),3]))
list_sub=subset(list, list$tavern_indicator==1)
min(list_sub$`JOINTDATAFRAME$YEAR`)
        #And what is the last year?
max(as.numeric(list[which(list[,4]==1),3]))

#let's revisit our smaller dataset to practice some other operations
    #removing stopwords
stopwords(language = "en")
stopwords=stopwords(language = "en")
stopwords_2=stopwords[c(1:14,16:length(stopwords))]
tokens_test_2=tokens_remove(tokens_test, stopwords)
ntoken(tokens_test_2)
ntype(tokens_test_2)
ntoken(tokens_test_2)/ntype(tokens_test_2)
ntoken(tokens_test)/ntype(tokens_test) #for comparison
    #removing capitalization
test_text[13]
tolower(test_text[13])
    #or alternatively, making everything capital
toupper(test_text[13])
    #creating bigrams and ngrams
tokens_ngrams(tokens_test_2, n=2L, concatenator = " ") #bigrams
ntype(tokens_ngrams(tokens_test_2, n=2L, concatenator = " "))
tokens_ngrams(tokens_test_2, n=5L, concatenator = " ") #bigger!
ntype(tokens_ngrams(tokens_test_2, n=5L, concatenator = " "))


#now let's try some fun stuff
kwic(corpus(test_text), "guilty", window=5) #context of keywords
kwic(corpus(test_text), "silver", window=5)
      #how similiar are documents?
simi_test=as.matrix(textstat_simil(test_dfm))
simi_test=as.data.frame(textstat_simil(test_dfm))
      #creating a word cloud
library(quanteda.textplots)
test_dfm_3=dfm(corpus, remove = stopwords)
quanteda.textplots::textplot_wordcloud(test_dfm_3, min_size=4)

#time for a big example!
attach(JOINTDATAFRAME)

CASETEXTVECTOR=as.vector(CASETEXT)
CASECORPUS=corpus(JOINTDATAFRAME$CASETEXT)
CASETOKENS=tokens(
  CASECORPUS,
  what = "word",
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = FALSE,
  include_docvars = TRUE,
  padding = FALSE,
  verbose = quanteda_options("verbose"))
PAWNdictionary=dictionary(list(pawn="pawn", 
                               pawn="pawn'd",
                               pawn="pawned",
                               pawn="pawnbroker",
                               pawn="pawnbroker's",
                               pawn="pledged",
                               pawn="pledge",
                               pawn="pawnbrokers",
                               pawn="redeem",
                               pawn="pawn-broker",
                               pawn="pawn-broker's",
                               pawn="pawnbrokers",
                               pawn="pawnd"
))
case_dfm=dfm(CASETOKENS, dictionary = PAWNdictionary)


JOINTDATAFRAME$pawn_indicator=ntype(case_dfm)
JOINTDATAFRAME$sentence_count=nsentence(JOINTDATAFRAME$CASETEXT)
JOINTDATAFRAME$CASETEXT=NULL
years=min(JOINTDATAFRAME$YEAR):max(JOINTDATAFRAME$YEAR)
year=as.vector(years)
collapse=matrix(nrow=length(years), ncol=4)
collapse[,1]=years
analysis_sub=JOINTDATAFRAME[c(6,8:9)]
analysis_sub$year=as.numeric(analysis_sub$YEAR)
analysis_sub$pawn_indicator=as.numeric(analysis_sub$pawn_indicator)
analysis_sub$sentence_count=as.numeric(analysis_sub$sentence_count)
timeNow <- Sys.time()
for (i in 1:nrow(collapse)){
  sum(analysis_sub[which(analysis_sub$year==collapse[i,1]),2])
  collapse[i,2]=nrow(subset(analysis_sub, analysis_sub$year==collapse[i,1] & analysis_sub$pawn_indicator>0))
  collapse[i,3]=nrow(subset(analysis_sub, analysis_sub$year==collapse[i,1]))
  collapse[i,4]=mean(analysis_sub[analysis_sub$year==collapse[i,1],"sentence_count"])
  cat("\r", round(i*100/nrow(collapse), 2), "% done in ", Sys.time() - timeNow, " ... ")
  
}



collapse=as.data.frame(collapse)
colnames(collapse)[1]="year"
colnames(collapse)[2]="pawn_instances"
colnames(collapse)[3]="total_cases"
colnames(collapse)[4]="average_sentence_count"

collapse$pawn_proportion=collapse$pawn_instances/collapse$total_cases


library(ggplot2)
time_series <- ggplot(collapse, aes(x=year, y=pawn_instances)) +
  geom_line() + 
  geom_vline(xintercept=1800)  +
  geom_vline(xintercept=1815)  +
  xlab("")
time_series

time_series_proportion <- ggplot(collapse, aes(x=year, y=pawn_proportion)) +
  geom_line() +
  geom_vline(xintercept=1800)  +
  geom_vline(xintercept=1815)  +
  xlab("")
time_series_proportion







