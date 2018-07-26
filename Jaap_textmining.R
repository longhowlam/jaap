library(dplyr)
library(text2vec)
library(stringr)
library(glmnet)
library(xgboost)

#### import scraped house data #######################################################

## see JaapScraper.R in this same git repo project https://github.com/longhowlam/jaap

jaap = readRDS("JaapResultsLongDesc20170528.RDs")

## import postcode data, not really needed for text mining
PC6 = readRDS("postocdes_NL.RDs")

jaap = jaap %>% 
  left_join(
    PC6,
    by = c("PC6" = "Postcode_spatie")
  )

# median prijse per city
city = jaap %>%
  group_by(city) %>%
  summarise(
    median = median(prijs, na.rm=TRUE)
  )


#### Cleaning up text ####################################################

### remove empty descriptions, short descriptions outlying prices 
jaap = jaap %>%
  filter(
    longdescription != ""
  ) %>% 
  mutate(
    Nwoorden = str_count(longdescription, pattern = '[\\w]+'),
    huisbeschrijving = str_to_lower(longdescription)
  )  %>%
  filter(
    prijs > 50000,
    prijs < 1250000,
    Nwoorden > 20,
    Nwoorden < 1500
  )


# create an index column
jaap$id = 1:dim(jaap)[1]


#### Text mining part ###########################################################

stopwoorden_nl =c(
  stopwords::stopwords( language = "nl"),
  letters
  )

prep_fun = function(x) {
  x %>% 
    str_to_lower %>% 
    str_replace_all("[^[:alnum:]]", " ") %>% 
    str_replace_all("\\s+", " ")
}

jaap = jaap %>% 
  mutate(
    normalizedText = prep_fun(huisbeschrijving)
  )

## clean with prep_fun, tokennize, remove stopw and create pruned vocab
iter = jaap$huisbeschrijving %>% 
  word_tokenizer() %>%  
  itoken(
    preprocessor = prep_fun,
    progressbar = TRUE
  )

pruned_vocab = iter %>% 
  create_vocabulary(
    stopwords = stopwoorden_nl,
    ngram = c(ngram_min = 1L, ngram_max = 3L)
  ) %>% 
  prune_vocabulary(
    doc_proportion_min = 0.0035 
  )

#### create the document term matrix
vectorizer = vocab_vectorizer(pruned_vocab)
dtm = create_dtm(iter, vectorizer)


dim(dtm)

#eerste rij 
dtm[1,]


# define tfidf model (Inverse document frequency)
tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_tfidf = fit_transform(dtm, tfidf)
dim(dtm_tfidf)

#eerste rij 
dtm_tfidf[1,]


#### Fit price models ############################################################

#### glmnet ########################################################
## split into test and train
Ntrain = floor(0.8*nrow(dtm))
tr.idx = sample(1:nrow(dtm), size = Ntrain)

dtm_train = dtm[tr.idx,]
dtm_test = dtm[-tr.idx,]
target_train = jaap$prijs[tr.idx]
target_test = jaap$prijs[-tr.idx]

glmnet_prijsmodel = cv.glmnet(
  x = dtm_train,
  y = target_train, 
  # L1 penalty
  alpha = 1,
  # high value is less accurate, but has faster training
  thresh = 1e-4,
  # again lower number of iterations for faster training
  maxit = 1e4,
  type.measure="mae"
)

plot(glmnet_prijsmodel)
print(paste("mse =", round(min(glmnet_prijsmodel$cvm), 4)))

### extract terms with 'large' negative or positive value

tmp = coef(glmnet_prijsmodel)
wordvalue = as.numeric(tmp)
names(wordvalue) = tmp@Dimnames[[1]]

### 100 'largest' and 'smallest' terms
neg100 = sort(wordvalue)[1:100]
pos100 = sort(wordvalue, decreasing = TRUE)[1:100]

outterms = tibble::tibble(
  NegWords = names(neg100),
  NegValue = neg100,
  PosWords = names(pos100),
  PosValues = pos100
)

## R squared calculation on the hold out test set
test_huizenprijs = predict(glmnet_prijsmodel, newx = dtm_test)[,1]
R2 = 1 - sum((target_test - test_huizenprijs)^2) / sum((target_test - mean(target_test))^2)
R2

#### xgboost ###################################################################

param = list(
  max_depth = 12,
  nthread = 4
)

xgbmodel = xgboost(
  params = param,
  data = dtm_train,
  label = target_train,
  nrounds = 150,
  print_every_n = 5L
)


## R squared calculation on the hold out test set
test_huizenprijs = predict(xgbmodel, newdata =  dtm_test)
R2_xgb = 1 - sum((target_test - test_huizenprijs)^2) / sum((target_test - mean(target_test))^2)
R2_xgb

## some model diagnostics 
varimp = xgb.importance(colnames(dtm_train), model = xgbmodel)
head(varimp, 25)

### nice plots but not very useful at all.....
xgb.plot.tree(colnames(dtm_train), model = xgbmodel, n_first_tree = 3)

p = xgb.plot.multi.trees(
  model = xgbmodel,
  feature_names = colnames(dtm_train),
  features_keep = 30
)
print(p)


tmpjaap = jaap %>%  select(PC6, longdescription, prijs)
readr::write_csv(tmpjaap, "jaap_dss.csv")


########## LSA ##############################################

lsa = LSA$new(n_topics = 300)
lsa_doc = dtm %>% fit_transform(lsa)
dim(lsa_doc)

Ntrain = floor(0.8*nrow(lsa_doc))
tr.idx = sample(1:nrow(dtm), size = Ntrain)

lsa_train = lsa_doc[tr.idx,]
lsa_test = lsa_doc[-tr.idx,]
target_train = jaap$prijs[tr.idx]
target_test = jaap$prijs[-tr.idx]


xgbmodel_lsa = xgboost(
  params = param,
  data = lsa_train,
  label = target_train,
  nrounds = 150,
  print_every_n = 5L
)

## R squared calculation on the hold out test set
test_huizenprijs = predict(xgbmodel_lsa, newdata =  lsa_test)
R2_xgb_lsa = 1 - sum((target_test - test_huizenprijs)^2) / sum((target_test - mean(target_test))^2)
R2_xgb_lsa
