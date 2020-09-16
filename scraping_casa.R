library("rvest")
library("stringr")

#inizializzazione####
#url prima pagina
url <-  'https://www.casa.it/vendita/residenziale/torino/centro-giardini-reali-repubblica/'
#url seconda pagina tranne l'ultimo numero
url2pg <- "https://www.casa.it/vendita/residenziale/torino/centro-giardini-reali-repubblica?page="
#numero pagine da cercare
n_pag_da_cercare <- 15
#scraping dei prezzi+locali+mq = trio, di tutte le pagine####

trio <- list()


trio[[1]] <- url %>% 
  read_html() %>% 
  html_nodes(".features") %>% 
  html_text()


for (i in 2:n_pag_da_cercare) {
  url1 = paste(url2pg,i, sep="")
  trio[[i]] <- url1 %>% 
    read_html() %>% 
    html_nodes(".features") %>% 
    html_text()
}

#PREZZI####
#se nella stringa non c'è "Tua da" rimuovere la stringa
for (j in 1:length(trio)) {
  for (i in c(1:length(trio[[j]]))) {
    if(grepl(pattern = "Tua da", trio[[j]][[i]], fixed = TRUE)) {
      print("ok")
    } else {
      trio[[j]][[i]] <- NA
    }
  }
}
#così tolgo sia gli annunci delle case in costruzione che quelli in trattativa riservata

#estrazione del prezzo: ATTENZIONE nela ricerca nel sito escludere gli immobili con valore >= a 1000000
#prezzo <- str_match(trio[[1]][[1]], "([0-9]{1,})[- .]([0-9]{1,})Tua da")
#as.integer(prezzo[1,2])*1000

#numero di osservazioni, cioè di annunci di cui ho fatto lo scrape in tutte le pagine
n <- 0
for (j in 1:length(trio)) {
  n <- n + (length(trio[[j]]))
}

#creazione della matrice per mettere le stringhe dei prezzi "sporche"
mat_prezzo <- matrix(NA, nrow = n , ncol = 3)

#riempimento della matrice
k <- 1
for (j in 1:length(trio)) {
  for (i in c(1:length(trio[[j]]))) {
    mat_prezzo[k,] <- str_match(trio[[j]][[i]], "([0-9]{1,})[- .]([0-9]{1,})Tua da")
    k <- k+1
  }
}

#vettore dei prezzi pronto per il datafreame
prezzi_vec_int_casa <- c()
for (i in 1:n) {
  prezzi_vec_int_casa[i] <- as.integer(mat_prezzo[i,2])*1000
}

#MQ####
#n rimane quello di prima
#creazione della matrice per contenere le stringhe
mat_mq <- matrix(NA, nrow = n , ncol = 2)

#riempimento della matrice
k <- 1
for (j in 1:length(trio)) {
  for (i in c(1:length(trio[[j]]))) {
    mat_mq[k,] <- str_match(trio[[j]][[i]], "([0-9]{1,3})[- .]mq")
    k <- k+1
  }
}

#vettore dei mq pronto per il datafreame
mq_vec_int_casa <- c()
for (i in 1:n) {
  mq_vec_int_casa[i] <- as.integer(mat_mq[i,2])
}

#LOCALI####
#n rimane quello di prima
#creazione della matrice per contenere le stringhe
mat_locali <- matrix(NA, nrow = n , ncol = 2)

#riempimento della matrice
k <- 1
for (j in 1:length(trio)) {
  for (i in c(1:length(trio[[j]]))) {
    mat_locali[k,] <- str_match(trio[[j]][[i]], "([0-9]{1,1})[- .]locali")
    k <- k+1
  }
}

#vettore dei mq pronto per il datafreame
locali_vec_int_casa <- c()
for (i in 1:n) {
  locali_vec_int_casa[i] <- as.integer(mat_locali[i,2])
}

#df####

#aggiungere numero di pagina e numero annuncio <- NON PRECISO
m <- length(prezzi_vec_int_casa)
q <- m/n_pag_da_cercare

annuncio <- rep(1:q,n_pag_da_cercare)
pagina <- rep(1:n_pag_da_cercare,rep(q,n_pag_da_cercare))

#
df_casa <- data.frame(prezzi_vec_int_casa, mq_vec_int_casa[1:length(prezzi_vec_int_casa)], locali_vec_int_casa[1:length(prezzi_vec_int_casa)],annuncio,pagina)
colnames(df_casa) = c("prezzo","mq", "locali","annuncio","pagina")

write.csv(df_casa, "/Users/tommasobassignana/Desktop/df_casa.csv")

