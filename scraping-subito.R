library("rvest")
library("stringr")
#inizializzazione####
#url prima pagina
url <-  'https://www.subito.it/annunci-piemonte/vendita/appartamenti/torino/?q=casa'
#url seconda pagina tranne l'ultimo numero
url2pg <- "https://www.subito.it/annunci-piemonte/vendita/appartamenti/torino/?q=casa&o="
#numero pagine da cercare
n_pag_da_cercare <- 10


#Qui faccio lo scraping dei prezzi di tutte le pagine ####
prices = list()
#i prezzi della prima pagina li devo fare manualmetne
prices[[1]] <-  
  url %>% 
  read_html() %>% 
  html_nodes(".AdElements__ItemPrice--price-L2hvbWUv") %>% 
  html_text()

# for (i in 2:n_pag_da_cercare) {
#   
#   prices[[i]] <- lapply(paste0(url2pg, i, sep = ""), 
#                         function(url){
#                           url %>% 
#                             read_html() %>% 
#                             html_nodes(".AdElements__ItemPrice--price-L2hvbWUv") %>% 
#                             html_text()
#                         })
# }


for (i in 2:n_pag_da_cercare) {
  url1 = paste(url2pg,i, sep="")
  prices[[i]] <- url1 %>% 
    read_html() %>% 
    html_nodes(".AdElements__ItemPrice--price-L2hvbWUv") %>% 
    html_text()
}

#sostituire m2superficie con uno spazio o con nulla
f <- function(x){str_replace_all(x, "€", "")}
prices2a <-lapply(prices, f)

# creazione del vettore prezzo 
vec_prezzo <- as.integer(unlist(prices2a, use.names = FALSE))

#mq####
mq = list()
mq[[1]] <-  
  url %>% 
  read_html() %>% 
  html_nodes(".AdElements__ItemAdditionalInfo--info-L2hvbWUv") %>% 
  html_text()
for (i in 2:n_pag_da_cercare) {
  url1 = paste(url2pg,i, sep="")
  mq[[i]] <- url1 %>% 
    read_html() %>% 
    html_nodes(".AdElements__ItemAdditionalInfo--info-L2hvbWUv") %>% 
    html_text()
}

#per mettere gli NA al posto giusto in mq e locali
f0 <- function(x){str_replace_all(x, "-", "999")}
mq <-lapply(mq, f0)
# mqprova[[1]][c(TRUE,FALSE)] #devo rimuovere tutti i pari o i dispari perchè il 999 potrebbe essere finito sia nei locali che nei mq
# for (i in 1:n_pag_da_cercare) {
#   mqprova[[i]] <- mqprova[[i]][c(TRUE,FALSE)]
# } 
mq1 <- mq
for (i in 1:n_pag_da_cercare) {
          mq1[[i]] <- mq1[[i]][c(TRUE,FALSE)]
            } 
f2 <- function(x){str_replace_all(x, "mq", "")}
mq2a <-lapply(mq1, f)

vec_mq <- as.integer(unlist(mq2a, use.names = FALSE))

vec_mqb <- na.omit(vec_mq)

#locali####
mq22 <- mq
#mq22[[1]][c(FALSE,TRUE)]
for (i in 1:n_pag_da_cercare) {
  mq22[[i]] <- mq22[[i]][c(FALSE,TRUE)]
} 

f3 <- function(x){str_replace_all(x, "locali", "")}
f5 <- function(x){str_replace_all(x, "locale", "")}
locali2a <-lapply(mq22, f3)
locali2b <-lapply(locali2a, f5)
vec_loc <- as.integer(unlist(locali2b, use.names = FALSE))

vec_loc2 <- na.omit(vec_loc)#non dovrebbe servire, anzi, se ci sono NA è un problema

#inserimento degli NA nel prezzo####
#creaiamo l'inserzione completa
blocco = list()
blocco[[1]] <-  
  url %>% 
  read_html() %>% 
  html_nodes(".upper-data-group") %>% 
  html_text()
for (i in 2:n_pag_da_cercare) {
  url1 = paste(url2pg,i, sep="")
  blocco[[i]] <- url1 %>% 
    read_html() %>% 
    html_nodes(".upper-data-group") %>% 
    html_text()
}

vec_blocco <- unlist(blocco, use.names = FALSE)


#cerchiamo quali record mancano
 
sum(grepl(pattern = "€", vec_blocco))
nana <- grep(pattern = "€", vec_blocco)
to_append <- setdiff(c(1:(30*n_pag_da_cercare)), nana)

vec_pre <- vec_prezzo
k <- 0
for (i in to_append) {
  vec_pre <- append(vec_pre, NA, i-1+k)
  k <- k+1
}

length(vec_pre)

# c <- c(1,2,3)
# c <- append(c,4,2)



#df####
length(vec_prezzo)
length(vec_mqb)
length(vec_loc2)

annuncio <- rep(1:30,n_pag_da_cercare)
pagina <- rep(1:n_pag_da_cercare,rep(30,n_pag_da_cercare))

df <- data.frame(vec_pre*1000,vec_mqb,vec_loc2,annuncio,pagina)
colnames(df) = c("prezzo","mq", "locali","annuncio","pagina")

#attenzione: sostituisce il file già esistente
write.csv(df, "/Users/tommasobassignana/Desktop/df_subito.csv")
