##### 
#SUBITO
#P fare lo scrape del titolo, posso provare a fare lo scrape di tutto il blocco dell'annuncio e poi manualmente rimuovere la prima parte della stringa fino a (TO)
#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scraped
url = 'https://www.subito.it/annunci-piemonte/vendita/appartamenti/torino/?q=casa'

#Reading the HTML code from the website
webpage_html = read_html(url)

#Using CSS selectors to scrape the rankings section
title_data_html = html_nodes(webpage_html,'.big')#(url, CSS)

#Converting the ranking data to text
title_data = html_text(title_data_html)

#Let's have a look at the rankings
head(title_data)

##### 
#IMMOBILIARE
#Per fare lo scrape del titolo, posso provare a fare lo scrape di tutto il blocco dell'annuncio e poi manualmente rimuovere la prima parte della stringa fino a (TO)
#Qui viene troppo sporco...ma funziona meglio se prendo il rettangolo senza immagine
#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scraped
url = 'https://www.immobiliare.it/vendita-case/torino/centro/?criterio=rilevanza'

#Reading the HTML code from the website
webpage_html = read_html(url)

#Using CSS selectors to scrape the rankings section
title_data_html = html_nodes(webpage_html,'.listing-item_body--content')#(url, CSS)

#Converting the ranking data to text
title_data = html_text(title_data_html)

#Let's have a look at the rankings
head(title_data)

#####
#Problema in immobiliare viene davvero scomodo fare in questo modo
#IMMOBILIARE
#Per fare lo scrape del titolo, posso provare a fare lo scrape di tutto il blocco dell'annuncio e poi manualmente rimuovere la prima parte della stringa fino a (TO)
#Qui viene troppo sporco...ma funziona meglio se prendo il rettangolo senza immagine

url = 'https://www.immobiliare.it/vendita-case/torino/centro/?criterio=rilevanza'
webpage_html = read_html(url)
title_data_html = html_nodes(webpage_html,'.lif__item')#(url, CSS)
title_data = html_text(title_data_html)
head(title_data)
title_data

#####
#Problema missing data, come inserire gli NA al posto giusto?
#SUBITO 
#potrei creare un indice sapendo quanti risultati ci sono in una pagina, poi trovare qualcosa che si ripete sempre e confrontarlo caso per caso per sapere se ci sono tutti e due gli "oggetti"
#è complicato...provare ad usare un altro comando diverso tipo https://rdrr.io/cran/rvest/man/html_table.html
#faccio lo scrape di solo i prezzi e di tutto il blocco, se la stringa del prezzo è presente nella stringa del blocco si procede, se no in quella posizione metto NA.
#devo poi fare un controllo perchè, per sfiga, non ci siano due prezzi uguali uno dietro l'altro
url = 'https://www.subito.it/annunci-piemonte/vendita/appartamenti/torino/?q=casa'
webpage_html = read_html(url)
#Qui faccio lo scraping dei blocchi 
blocco_data_html = html_nodes(webpage_html,'.upper-data-group')#(url, CSS)
blocco_data_text = html_text(blocco_data_html)
#Qui faccio lo scraping dei prezzi 
price_data_html = html_nodes(webpage_html,'.AdElements__ItemPrice--price-L2hvbWUv')#(url, CSS)
price_data_text = html_text(price_data_html)
price_data_text
#quanti blocchi ci sono per pagina?
length(blocco_data_text)
#cercare se il prezzo è presente nel blocco
grepl(pattern = price_data_text[5], blocco_data_text[5], fixed = TRUE)#(cosa cercare, dove, fixed true = cerco per un match esatto)
#mettere l'NA al posto giusto, notando che l'argument è after, non la posizione effettiva
price_list_appended = append(price_data_text, values = NA, after = 3)
#cercando di fare un ciclo for mettendo tutto insieme
price_list_appended = price_data_text
for (i in c(1:length(blocco_data_text))) {
  if(grepl(pattern = price_list_appended[i], blocco_data_text[i], fixed = TRUE)) {
    #print(price_data_text[i])
    #print(blocco_data_text[i])
    print("ok")
  } else {
    price_list_appended = append(price_list_appended, values = NA, after = i-1)#questo non funzione se manca il primo elemento?
  }
}
print(price_list_appended)

#PROBLEMA di fare lo scrape su pagine multiple
#vedo manualmente cosa cambia nell'url quando cambio pagina
#lapply returns a list of the same length as X, 
#each element of which is the result of applying FUN to the corresponding element of X.
#%>%What the function does is to pass the left hand side of the operator to the first argument of the right hand side of the operator
prices = list()
prices[[1]] <-  url %>% read_html() %>% 
              html_nodes(".AdElements__ItemPrice--price-L2hvbWUv") %>% 
              html_text()
for (i in 2:5) {
  
  prices[[i]] <- lapply(paste0('https://www.subito.it/annunci-piemonte/vendita/appartamenti/torino/?q=casa', '&o=', i), #qui la prima pagina va fatta a parte, automatizzare il 15
                function(url){
                  url %>% read_html() %>% 
                    html_nodes(".AdElements__ItemPrice--price-L2hvbWUv") %>% 
                    html_text()
                })
  }
prices

#problema - adattare il ciclo for alla struttura a lista
#prima di tutto devo creare il corrispettivo della lista di blocchi
blocco = list()
blocco[[1]] <-  url %>% read_html() %>% 
  html_nodes(".upper-data-group") %>% 
  html_text()
for (i in 2:15) {
  
  blocco[[i]] <- lapply(paste0('https://www.subito.it/annunci-piemonte/vendita/appartamenti/torino/?q=casa', '&o=', i), #qui la prima pagina va fatta a parte, automatizzare il 15
                        function(url){
                          url %>% read_html() %>% 
                            html_nodes(".upper-data-group") %>% 
                            html_text()
                        })
}
#cercando di impostare il ciclo for
price_list_appended  <-  prices
rm(j)
rm(i)
#j=pagine
#i=elemento nella pagina
for (j in 1:1) {
  for (i in c(1:length(blocco[[j]]))) {
    if(grepl(pattern = price_list_appended[[j]][[i]], blocco[[j]][[i]], fixed = TRUE)) {
      print("ok")
    } else {
      price_list_appended[[j]] <- append(price_list_appended[[j]], values = NA, after = i-1)#questo non funzione se manca il primo elemento?
      print(price_list_appended[[1]])
    }
  }
}

price_list_appended
#funziona per un giro, fare per gli altri senza incasinare tutto 
#copio per non incasinare il tutto

price_list_appended  <-  prices
rm(j)
rm(i)
#j=pagine
#i=elemento nella pagina
for (j in 1:2) {
  for (i in c(1:length(blocco[[j]]))) {
    if(grepl(pattern = price_list_appended[[j]][[i]], blocco[[j]][[i]], fixed = TRUE)) {
      print("ok")
    } else {
      price_list_appended[[j]] <- append(price_list_appended[[j]], values = NA, after = i-1)#questo non funzione se manca il primo elemento?
      print(price_list_appended[[1]])
    }
  }
}

price_list_appended
#####
#Alcuni siti rendono difficile isolare i tag -> immobiliare.it - si può utilizzare un sistema diverso per la selezione degli elementi da copiare?
#Su subito i titoli sono incasinati, ma i prezzi sono facilmente copiabili - si può fare per immobiliare?
#Alcuni siti sono protetti -> idealista - usare altre soluzioni 
