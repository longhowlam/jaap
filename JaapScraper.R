library(dplyr)
library(rvest)
library(stringr)
library(anytime)

N = 4625
JaapResult = data.frame()
baseurl = "http://www.jaap.nl/koophuizen/p"


for( i in 1:N){
  
  tryCatch({
    print(i)
    URL = paste0(baseurl, i)
    out = read_html(URL)
    
    straat    = html_nodes(out,xpath='//h2[@class="property-address-street"]') %>% html_text()
    
    postcode  = html_nodes(out, xpath='//div[@class="property-address-zipcity"]') %>% html_text()
    
    prop1     = html_nodes(out, xpath='//div[@class="property-features"]') %>%
      html_text() %>% 
      str_replace_all("\r", "") %>%
      str_replace_all("\n", "") %>%
      str_trim()
    
    price     =  html_nodes(out, xpath='//div[@class="property-price"]') %>% html_text()
    scrapedate = Sys.Date()
    outdf = data.frame(straat, postcode, prop1, price, scrapedate, stringsAsFactors = FALSE)

    JaapResult = rbind(JaapResult, outdf)
        
  }, 
  error=function(e) {print("***")})
  if (i%%25 == 0 ){
    saveRDS(JaapResult, "JaapResult.RDs")
  }
}      


####  create some additional variables based on what is scraped

JaapResults = JaapResult %>%
  mutate(
    prijs = str_extract(price,"[:digit:]+[\\.][:digit:]+[\\.]*[:digit:]*") %>%
      str_replace_all("\\.","") %>% as.numeric(),
    VON = str_detect(price, "v.o.n."),
    PC6 = str_extract(postcode, "[:digit:]{4}[:space:]*[:alpha:]{2}"),
    kamers = str_extract(prop1, "[:digit:]+[:space:][k]") %>% str_replace("k", "") %>% as.numeric(),
    tmp = str_locate(prop1, "[:space:]{2,}")[,1],
    Type = str_sub(prop1, 1, tmp),
    Oppervlakte = str_extract(prop1, "[:digit:]+[ᵐ]") %>% str_replace("[ᵐ]", "") %>% as.numeric()
  )

hist(JaapResults$kamers[JaapResults$kamers < 20])
summary(JaapResults$VON)
table(JaapResults$Type)


hist(JaapResults$Oppervlakte[JaapResults$Oppervlakte < 1000])

saveRDS(JaapResults, "JaapResults.RDs")
