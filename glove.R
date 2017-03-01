library(text2vec)
library(SOAR)
Objects()


#https://cran.r-project.org/web/packages/text2vec/vignettes/glove.html


#install.packages("SOAR")
#install.packages("text2vec")


#---------------------------Read the data
dir<-getwd()
text8_file = "~/text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
  unzip ("~/text8.zip", files = "text8", exdir = dir)
}
wiki = readLines(text8_file, n = 1, warn = FALSE)


Store(wiki)

str(wiki)

#============Count how many words in the wiki==============
str1 <- wiki 
str2 <- gsub(' {2,}',' ',str1)
length(strsplit(str2,' ')[[1]] )#17 005 208

strsplit(str2,' ')[[1]] [1:100]

#==merge the characters
print(paste(strsplit(str2,' ')[[1]] [1:100], collapse = " "))




#<------------------------Vocab
# Create iterator over tokens
tokens <- space_tokenizer(wiki)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab, term_count_min = 5L)



# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab, 
                               # don't vectorize input
                               grow_dtm = FALSE, 
                               # use window of 5 for context words
                               skip_grams_window = 5L)
tcm <- create_tcm(it, vectorizer)

dim(tcm) #71290 x 71290






Store(tcm)

RcppParallel::setThreadOptions(numThreads = 4)


#GlobalVectors: Creates Global Vectors word-embeddings model
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)

glove$fit(tcm, n_iter = 20)




#Alternatively we can train model with R's S3 interface (but keep in mind that 
#all text2vec models are R6 classes and they are mutable! 
#So fit, fit_transform methods modify models!):
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
# `glove` object will be modified by `fit()` call !
fit(tcm, glove, n_iter = 20)


str(glove)

word_vectors <- glove$get_word_vectors()
dim(word_vectors)

Store(word_vectors) 


length(rownames(word_vectors) )



#--------------------To check how many spaces------------
countSpaces <- function(s) { sapply(gregexpr(" ", s), 
                                    function(p) { sum(p>=0) } ) }
t<-countSpaces(rownames(word_vectors))  #no space at all
sum(t)


berlin <- word_vectors["paris", , drop = FALSE] - 
  word_vectors["france", , drop = FALSE] + 
  word_vectors["germany", , drop = FALSE]


cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
dim(cos_sim)

head(sort(cos_sim[,1], decreasing = TRUE), 5)
# berlin     paris    munich    leipzig   germany 
# 0.8015347 0.7623165 0.7013252 0.6616945 0.6540700 
