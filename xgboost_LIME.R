library(lime)
library(pROC)
library(ggplot2)

hist(jaap$prijs)


#### LIME : Local Interpretable Model-agnostic Explanations #####

#########################################################################################
##### binaire classificatie ##############################################

# kunnen we dure huizen voorspellen op basis van de beschrijvingen?

TRESHOLD = 750000

jaap = jaap %>% 
  mutate(
    label = ifelse(prijs > TRESHOLD, 1,0)
  )

########## split in train test ###########################################

Njaap = nrow(jaap)
Ntrain = floor( 0.8 * Njaap)
tr.idx = sample(1:Njaap, size = Ntrain)

jaaptrain = jaap[tr.idx,]
jaaptest = jaap[-tr.idx,]

get_matrix <- function(text) {
  iter = itoken(text, progressbar = FALSE)
  VV = create_vocabulary(
    iter, 
    stopwords = stw,
    ngram = c(ngram_min = 1L, ngram_max = 3L)
  )
  create_dtm(iter, vectorizer = VV)
}


train_matrix = get_matrix(jaaptrain$normalizedText)
test_matrix = get_matrix(jaaptest$normalizedText)

##### fit xgboost model op document term matrix #########################

xgb_model <- xgb.train(
  list(
    max_depth = 8,
    eta = 0.1, 
    objective = "binary:logistic",
    eval_metric = "error", 
    nthread = 4
  ),
  xgb.DMatrix(
    train_matrix, 
    label = jaaptrain$label
  ),
  nrounds = 50
)

##### evalueer xgboost op test set ######################################

predOnTest = data.frame(
  response = jaaptest$prijs > TRESHOLD,
  prijs = jaaptest$prijs,
  predictor = predict(xgb_model,  newdata = test_matrix) 
)

roc(response ~ predictor, data = predOnTest, plot = TRUE, ci = TRUE)

ggplot(predOnTest, aes(prijs, predictor)) + geom_point(alpha = 0.3) + geom_smooth()


###### LIME verklaringen ################################################

#####  Gebruik LIME om verklaring te vinden in de onderliggende termen

laag = jaaptest$prijs < 180000
hoog = jaaptest$prijs > 800000
sentences = c(
  jaaptest$normalizedText[laag][1:2],
  jaaptest$normalizedText[hoog][1:2]
)

explainer = lime(jaaptrain$normalizedText, xgb_model, get_matrix)
explanations = lime::explain(sentences, explainer, n_labels = 2, n_features = 10)
plot_features(explanations)

explanations_2 = lime::explain(sentences, explainer, n_labels = 1, n_features = 30)
plot_text_explanations(explanations_2)

interactive_text_explanations(explainer)



##############################################################################################
##### REGRESSIE  ######


########## split in train test ###########################################

Njaap = nrow(jaap)
Ntrain = floor( 0.8 * Njaap)
tr.idx = sample(1:Njaap, size = Ntrain)

jaaptrain = jaap[tr.idx,]
jaaptest = jaap[-tr.idx,]

get_matrix <- function(text) {
  iter = itoken(text, progressbar = FALSE)
  VV = create_vocabulary(
    iter, 
    stopwords = stw,
    ngram = c(ngram_min = 1L, ngram_max = 3L)
  )
  create_dtm(iter, vectorizer = VV)
}

train_matrix = get_matrix(jaaptrain$normalizedText)
test_matrix = get_matrix(jaaptest$normalizedText)

##### fit xgboost model op document term matrix #########################

xgb_modelREG <- xgb.train(
  list(
    max_depth = 8,
    eta = 0.1, 
    objective = "reg:linear",
    nthread = 4
  ),
  xgb.DMatrix(
    train_matrix, 
    label = jaaptrain$prijs
  ),
  nrounds = 50
)

##### evalueer xgboost op test set ######################################

predOnTestREG = data.frame(
  prijs = jaaptest$prijs,
  predictor = predict(xgb_modelREG,  newdata = test_matrix) 
)

ggplot(predOnTestREG, aes(prijs, predictor)) + geom_point(alpha = 0.3) + geom_smooth()

###### LIME verklaringen ################################################

#####  Gebruik LIME om verklaring te vinden in de onderliggende termen

laag = jaaptest$prijs < 180000
hoog = jaaptest$prijs > 800000
sentences = c(
  jaaptest$normalizedText[laag][1:2],
  jaaptest$normalizedText[hoog][1:2]
)
prijzen = c(jaaptest$prijs[laag][1:2], jaaptest$prijs[hoog][1:2])

explainerREG = lime(jaaptrain$normalizedText, xgb_modelREG, get_matrix)
explanationsREG = lime::explain(sentences, explainerREG, n_features = 15)
plot_features(explanationsREG)
prijzen




