library("rvest")
library("stringr")

#inizializzazione####
#url prima pagina
url <-  'https://www.immobiliare.it/vendita-case/torino/con-piani-intermedi/?criterio=rilevanza&prezzoMassimo=120000&fasciaPiano[]=30&idMZona[]=172&idMZona[]=173&idMZona[]=185'
#url seconda pagina tranne l'ultimo numero
url2pg <- "https://www.immobiliare.it/vendita-case/torino/con-piani-intermedi/?criterio=rilevanza&prezzoMassimo=120000&fasciaPiano[]=30&idMZona[]=172&idMZona[]=173&idMZona[]=185&pag="
#numero pagine da cercare
n_pag_da_cercare <- 10

#scraping del prezzo#### 
#crea la lista vuota per contenere in ogni sublista tutti i prezzi di una pagina 
prezzo <- list()

#metto i risultati della prima pagina nella prima sublista
prezzo[[1]] <- url %>% 
  read_html() %>% 
  html_nodes(".lif__pricing") %>% 
  html_text()


#attenzione a non mettere più pagine delle esistenti
try(
for (i in 2:n_pag_da_cercare) {
  url1 = paste(url2pg,i, sep="")
  prezzo[[i]] <- url1 %>% 
    read_html() %>% 
    html_nodes(".lif__pricing") %>% 
    html_text()
}
)
#sembra che il prezzo sia obbligatorio ma bisogna prestare attenzione a  "€ 145.000€ 159.500(-9.1%)"


#estrazione del prezzo
#numero di osservazioni
# n <- 0
# for (j in 1:length(prezzo)) {
#   n <- n + (length(prezzo[[j]]))
# }
# 
#  #creazione del vettore dei prezzi
 vec_prezzo <- c()

#riempimento della matrice versione 2
k <- 1
for (j in 1:length(prezzo)) {
  for (i in c(1:length(prezzo[[j]]))) {
    u <- str_match(prezzo[[j]][[i]], "(([0-9]{1,})[- .]([0-9]{1,})\n)|(([0-9]{1,})[- .]([0-9]{1,})€)")
    if (is.na(u[1,3])) {
      vec_prezzo[k] <- u[1,6]
    } else {
      vec_prezzo[k] <- u[1,3]  
    }
    k <- k+1
  }
}

vec_prezzo_int <- as.integer(vec_prezzo)*1000


#ATTENZIONE gli NA sono case all'asta

#blocco completo####
#crea la lista vuota per contenere in ogni sublista tutte le info di una pagina 
blocco <- list()

#metto i risultati della prima pagina nella prima sublista
blocco[[1]] <- url %>% 
  read_html() %>% 
  html_nodes(".lif__item") %>% 
  html_text()
#uso un ciclo per tutte le altre pagine
for (i in 2:n_pag_da_cercare) {
  url1 = paste(url2pg,i, sep="")
  blocco[[i]] <- url1 %>% 
    read_html() %>% 
    html_nodes(".lif__item") %>% 
    html_text()
}

blc <- blocco
#estrazione della superficie####

###per una sola pagina
for (i in c(1:length(blocco[[1]]))) {
  if(grepl(pattern = "m2superficie", blocco[[1]][i], fixed = TRUE)) {
    print("ok")
  } else {
    blocco[[1]][[i]] <- NA
    print("ora ok")
  }
}

#rimuovere gli NA dalla lista
#b è il blocco con gli NA
b <- blocco[[1]]
b1 <- na.omit(b)
b1


#sostituire m2superficie con uno spazio o con nulla
b2a <- str_replace_all(b1, "m2superficie", "")
b2 <- str_replace_all(b2a, "da ", "")
b2

#convertire tutto nel formato giusto
b3 <- as.integer(b2)
b3

###estensione a tutte le pagine
blocco1 <- blocco
try(
for (j in 1:length(blocco1[[j]])) {
  for (i in c(1:length(blocco1[[j]]))) {
    if(grepl(pattern = "m2superficie", blocco1[[j]][i], fixed = TRUE)) {
      print("ok")
    } else {
      blocco1[[j]][[i]] <- NA
      print("ora ok")
    }
  }
}, silent = TRUE)

#rimuovere tutti gli na da tutte le subliste di blocco1
blocco2 <-lapply(blocco1, function(x) x[!is.na(x)])

#sostituire m2superficie con uno spazio o con nulla
f <- function(x){str_replace_all(x, "m2superficie", "")}
f2 <- function(x){str_replace_all(x, "da", "")}
blocco2a <-lapply(blocco2, f)
blocco2b <-lapply(blocco2a, f2)

#convertire in integers
mqvecperdf <- as.integer(unlist(blocco2b, use.names = FALSE))
#locali####


# fixed = TRUE usa l'exact match'
for (i in c(1:length(blc[[1]]))) {
  s <- 0
  if(grepl(pattern = "([0-9]{1})-([0-9]{1})\\slocali", blc[[1]][i])) {
    blc[[1]][[i]]  <-  "99 locali\n" #non posso metterlo come NA se no poi mi rimuove il record
    s <- s+1
  } else {
    s <- s
  }
  print(sum(s))
}
for (i in c(1:length(blc[[1]]))) {
  if(grepl(pattern = "locali\n", blc[[1]][i], fixed = TRUE)) {
    print("ok")
  } else {
    blc[[1]][[i]] <- NA
    print("ora ok")
  }
}
blc[[1]]

                 
#rimuovere gli NA dalla lista
#b è il blocco con gli NA
b <- blc[[1]]
b1 <- na.omit(b)
b1


#sostituire le parti che non sono numero con nulla
b2a <- str_replace_all(b1, "locali\n", "")
b2b <- str_replace_all(b2a, "\n", "")

b2b

#ATTENZIONE: cosa succede ai casi dove i locali sono: n-m
#li rimuove, devo imputare qualcosa se non slitta tutto

#convertire tutto nel formato giusto
b3 <- as.integer(b2b) 
b3
# mi darà NA se ci sono n+ locali
length(b3)

try(
for (j in 2:length(blc)) { # due perchè la prima pagina l'ho fatta separatamente
  for (i in c(1:length(blc[[j]]))) {
  s <- 0
  if(grepl(pattern = "([0-9]{1})-([0-9]{1})\\slocali", blc[[j]][i])) {
    blc[[j]][[i]]  <-  "99 locali\n" #non posso metterlo come NA se no poi mi rimuove il record
    s <- s+1
  } else {
    s <- s
  }
  print(sum(s))
  }
  for (i in c(1:length(blc[[j]]))) {
    s <- 0
    if(grepl(pattern = "([0-9]{1})\\sunità", blc[[j]][i])) {
      blc[[j]][[i]]  <-  "99 locali\n" #non posso metterlo come NA se no poi mi rimuove il record
      s <- s+1
    } else {
      s <- s
    }
    print(sum(s))
  }
  for (i in c(1:length(blc[[j]]))) {
  if(grepl(pattern = "locali\n", blc[[j]][i], fixed = TRUE)) {
    print("ok")
  } else {
    blc[[j]][[i]] <- NA
    print("ora ok")
  }
}
}, silent = TRUE)

blc[[2]]

#rimuovere tutti gli na da tutte le subliste di blocco1
blc2 <-lapply(blc, function(x) x[!is.na(x)])
blc2[[2]]
blc2[[1]]

#sostituire le strinche con uno spazio o con nulla
f3 <- function(x){str_replace_all(x, "locali\n", "")}
f4 <- function(x){str_replace_all(x, "\n", "")}
blc2a <-lapply(blc2, f3)
blc2b <-lapply(blc2a, f4)
#convertire in integers
localivecperdf <- as.integer(unlist(blc2b, use.names = FALSE))

#df####
length(vec_prezzo_int)
length(mqvecperdf)
length(localivecperdf)

#aggiunge numero di pagina e numero annuncio
annuncio <- rep(1:25,n_pag_da_cercare)
pagina <- rep(1:n_pag_da_cercare,rep(25,n_pag_da_cercare))

df <- data.frame(vec_prezzo_int,mqvecperdf,localivecperdf,annuncio,pagina)
colnames(df) = c("prezzo","mq", "locali","annuncio","pagina")
df$prezzo_al_mq <- round(df$prezzo/df$mq, digits = 0)
df <- df[order(df$prezzo_al_mq, decreasing = FALSE),c(6,1,2,3,4,5)]
#criterio con penalty 
#caso 1
#prezzo > soglia, f <- soglia/mq, g <- q(prezzo-soglia)/mq, f+g, con q funzione lentamente crescente
#prezzo <= soglia, f <- prezzo/mq, g <- q(soglia-prezzo)/mq, f-g, con q funzione lentamente crescente
#22000^(0.9)
# mq <- 71
# prezzo <- 90000
# pr_al_mq <- c()
# soglia <- 100000
# if (prezzo > soglia) {
#   f <- soglia/mq
#   g <- ((prezzo-soglia)+(prezzo-soglia)^(0.9))/mq
#   pr_al_mq <- f+g
# }else{
#   f <- prezzo/mq
#   g <- ((soglia-prezzo)^(0.9))/mq
#   pr_al_mq <- f-g
# }
# pr_al_mq
# prezzo/mq
# View(df)
adj_pr_mq <- function(col_prezzo, col_mq, soglia) {
  prezzo_al_mq <- c()
  try(for(i in 1:length(col_prezzo)){
    if (col_prezzo[i] > soglia) {
    f <- soglia/col_mq[i]
    g <- ((col_prezzo[i]-soglia)+(col_prezzo[i]-soglia)^(0.9))/col_mq[i]
    pr_al_mq[i] <- f+g
  }else{
    f <- col_prezzo[i]/col_mq[i]
    g <- ((soglia-col_prezzo[i])^(0.9))/col_mq[i]
    pr_al_mq[i] <- f-g
  }})
  return(pr_al_mq)
}
prezzo_adj <- adj_pr_mq(col_prezzo = df$prezzo, col_mq = df$mq, soglia = 100000)
prezzo_adj[1:length(df$prezzo)]
df$prezzo_adj <- prezzo_adj[1:length(df$prezzo)]
#attenzione: sostituisce il file già esistente
write.csv(df, "/Users/tommasobassignana/Desktop/df_immobiliare.csv")






