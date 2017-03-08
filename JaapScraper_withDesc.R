library(dplyr)
library(rvest)
library(stringr)
library(anytime)

N = 4625
JaapResult = data.frame()
baseurl = "http://www.jaap.nl/koophuizen/p"
mislukt = numeric()

for( i in 2302:1){
  

    print(i)
    URL = paste0(baseurl, i)
    out = read_html(URL)
    
    straat    = html_nodes(out,xpath='//h2[@class="property-address-street"]') %>% html_text()
    linkhuis =  html_nodes(out,xpath='//a[@class="property-inner"]/@href') %>% html_text()
    postcode  = html_nodes(out, xpath='//div[@class="property-address-zipcity"]') %>% html_text()
    
    prop1     = html_nodes(out, xpath='//div[@class="property-features"]') %>%
      html_text() %>% 
      str_replace_all("\r", "") %>%
      str_replace_all("\n", "") %>%
      str_trim()
    
    price     =  html_nodes(out, xpath='//div[@class="property-price"]') %>% html_text()
    scrapedate = Sys.Date()

    
    ## loop over the individual house by folloeing the link
    linkhuis =  html_nodes(out,xpath='//a[@class="property-inner"]/@href') %>% html_text()
    
    nhuizen = length(linkhuis)
    oorsprPrijs = longdescription = geplaatst = character(nhuizen)
    
    for(j in 1:nhuizen)
    {
      tryCatch({
        out2 = read_html(linkhuis[j])
        tmpdes = html_nodes(out2, xpath='//div[@id="long-description"]') %>% html_text
        longdescription[j] = ifelse(length(tmpdes) == 0, "", tmpdes)
        
        out3 = read_html(  str_replace(linkhuis[j], "overzicht", "woningwaarde")  ) 
        
        tmptables =   html_table(out3, fill=TRUE) 
        geplaatst[j] = tmptables[[1]][1,2]
        oorsprPrijs[j] = tmptables[[1]][3,2]
        cat ("     ")
        print(j)
        Sys.sleep(3+3*runif(1))
     
      }, 
      error=function(e) {
        print("***")
        mislukt = c(mislukt,i)
      }
      )
    }
    
    outdf = data.frame(
      straat,
      postcode,
      prop1,
      price,
      scrapedate,
      linkhuis,
      geplaatst,
      longdescription,
      oorsprPrijs,
      stringsAsFactors = FALSE
    )

    JaapResult = rbind(JaapResult, outdf)
    saveRDS(JaapResult, "JaapResult.RDs")
}      




#########################################################################################################################


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

#hist(JaapResults$kamers[JaapResults$kamers < 20])
#summary(JaapResults$VON)
#table(JaapResults$Type)
#hist(JaapResults$Oppervlakte[JaapResults$Oppervlakte < 1000])

filename = paste0("JaapResults", Sys.Date(), ".RDs")
saveRDS(JaapResults, filename)
