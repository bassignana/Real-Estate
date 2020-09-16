library("rvest")
library("stringr")

#inizializzazione####
#url prima pagina
url <-  'https://www.immobiliare.it/ricerca.php?idCategoria=1&idContratto=1&idNazione=IT&prezzoMassimo=120000&criterio=rilevanza&ordine=desc&pag=1&vrt=45.07262063070585,7.7109137177467355;45.06898364567272,7.711857855319978;45.066801343562794,7.702502310276032;45.06358835827723,7.6964083313941964;45.058313821104434,7.69057184457779;45.058313821104434,7.686881124973298;45.06243648939038,7.67958551645279;45.063042739065914,7.677868902683259;45.051947352352975,7.667483389377594;45.054433391164046,7.6568403840065;45.06231523868382,7.650832235813142;45.06752878685693,7.654179632663727;45.06825622089359,7.662676870822907;45.06571016126377,7.671259939670564;45.065952648019206,7.678040564060211;45.07074155066961,7.682160437107087;45.07510577070316,7.690056860446931;45.078621150030415,7.6985540986061105;45.075166382523776,7.70155817270279;45.07219632771079,7.704991400241853&fasciaPiano%5B%5D=20&fasciaPiano%5B%5D=30'
#url seconda pagina tranne l'ultimo numero
url2pg <- "https://www.immobiliare.it/ricerca.php?idCategoria=1&idContratto=1&idNazione=IT&prezzoMassimo=120000&criterio=rilevanza&ordine=desc&pag=" 
url2pg2 <- "&vrt=45.07262063070585%2C7.7109137177467355%3B45.06898364567272%2C7.711857855319978%3B45.066801343562794%2C7.702502310276032%3B45.06358835827723%2C7.6964083313941964%3B45.058313821104434%2C7.69057184457779%3B45.058313821104434%2C7.686881124973298%3B45.06243648939038%2C7.67958551645279%3B45.063042739065914%2C7.677868902683259%3B45.051947352352975%2C7.667483389377594%3B45.054433391164046%2C7.6568403840065%3B45.06231523868382%2C7.650832235813142%3B45.06752878685693%2C7.654179632663727%3B45.06825622089359%2C7.662676870822907%3B45.06571016126377%2C7.671259939670564%3B45.065952648019206%2C7.678040564060211%3B45.07074155066961%2C7.682160437107087%3B45.07510577070316%2C7.690056860446931%3B45.078621150030415%2C7.6985540986061105%3B45.075166382523776%2C7.70155817270279%3B45.07219632771079%2C7.704991400241853&fasciaPiano[]=20&fasciaPiano[]=30"
#numero pagine da cercare
#TROVARE UN MODO PER CALCOLARE IL NUMERO DI PAGINE TOTALI
#attenzione a non mettere più pagine delle esistenti
n_pag_da_cercare <- 4

#scraping del prezzo#### 
#crea la lista vuota per contenere in ogni sublista tutti i prezzi di una pagina 
prezzo <- list()

#metto i risultati della prima pagina nella prima sublista
prezzo[[1]] <- url %>% 
  read_html() %>% 
  html_nodes(".lif__pricing") %>% 
  html_text()

#metto i risultati delle altre pagine nelle altre sbliste
try(
for (i in 2:n_pag_da_cercare) {
  url1 = paste(url2pg,i,url2pg2, sep="")
  prezzo[[i]] <- url1 %>% 
    read_html() %>% 
    html_nodes(".lif__pricing") %>% 
    html_text()
}
)
#sembra che il prezzo sia obbligatorio ma bisogna prestare attenzione a  "€ 145.000€ 159.500(-9.1%)"
###posso estrarre il prezzo giusto estraendo le cifre tra i due €€ nelle stringhe in cui è presente il simbolo %
###forse già risolto con la seconda parte della regex expression <- Già risolto

#creazione del vettore dei prezzi
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
  url1 = paste(url2pg,i,url2pg2, sep="")
  blocco[[i]] <- url1 %>% 
    read_html() %>% 
    html_nodes(".lif__item") %>% 
    html_text()
}

blc <- blocco
#estrazione della superficie####
#ATTENZIONE nela ricerca nel sito escludere gli immobili con valore >= a 1000000
#numero di osservazioni, cioè di annunci di cui ho fatto lo scrape in tuttel le pagine
# n <- 0
# for (j in 1:length(blocco)) {
#   n <- n + (length(blocco[[j]]))
# }

#creazione della matrice per mettere le stringhe dei prezzi "sporche" <- non più utile
#vec_superficie <- rep(NA, n)

# #riempimento della lista <- non funziona !?
# k <- 1
# for (j in 1:length(blocco)) {
#   for (i in c(1:length(blocco[[j]]))) {
#     vec_superficie[k] <- str_extract_all(blocco[[j]][[i]], "([0-9]{2,})[- .]m")
#     k <- k+1
#   }
# }
# vec_superficie

#provando il comando che mi serve su una singola stringa
# blocco[[1]][3]
# str_extract_all(blocco[[1]][[3]],  "\\d")

#voglio tenere solo le stringhe che mi interessano in cui c'è "m2superficie"
#quindi prima impongo NA su tutte le stringhe che non contengono m2superficie
#e poi le rimuovo con il comando apposito per gli NA

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
#ATTENZIONE in blc[[1]] le prime 50 righe, dopo il primo annuncio, non riesco a capire da dove 
#arrivino, ma da quando poi ricominciano gli annunci veri sono giuste
#sembra che sia un problema solo della prima pagina

#voglio tenere solo le stringhe che mi interessano in cui c'è "locali\n"
#quindi prima impongo NA su tutte le stringhe che non contengono m2superficie
#e poi le rimuovo con il comando apposito per gli NA

###per una sola pagina
#per ora i 2-5locali e 1-5 locali li gestisco così(perchè nei mq e nei prezzi ho il record)
#l'idea mi sembra giusta ma i primi due cicli for non fungono
#magari non mi trova il "-"
#provare ad usare una regex expression invece della ricerca sulla stringa
# blc <- blocco
# blc[[1]][2]
# grepl(pattern = "\\slocali", blc[[1]][2])
# grepl(pattern = "([0-9]{1,})\\slocali", blc[[1]][2])
# grepl(pattern = "-([0-9]{1})\\slocali", blc[[1]][2])
# grepl(pattern = "([0-9]{1})-([0-9]{1})\\slocali", blc[[1]][2])
# blc[[1]][61]
# grepl(pattern = "([0-9]{1})-([0-9]{1})\\slocali", blc[[1]][61])

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
#ATTENZIONE per sicurezza o ribuovere le variabili di prima dopo averle utilizzate o cambiare il 
# nome qui dopo
#rimuovere gli NA dalla lista
#b è il blocco con gli NA
b <- blc[[1]]
b1 <- na.omit(b)
b1


#sostituire le parti che non sono numero con nulla
b2a <- str_replace_all(b1, "locali\n", "")
b2b <- str_replace_all(b2a, "\n", "")
#b2c <- str_replace_all(b2b, "+", "") <- non si può dare
b2b

#ATTENZIONE: cosa succede ai casi dove i locali sono: n-m
#li rimuove, devo imputare qualcosa se non mi slitta tutto

#convertire tutto nel formato giusto
b3 <- as.integer(b2b) 
b3
# mi darà NA se ci sono n+ locali
length(b3)

###per tutte le altre pagine 
# try(
#   for (j in 1:length(blocco1[[j]])) {#perchè blocco1[[j]] e non blocco1?
#     for (i in c(1:length(blocco1[[j]]))) {
#       if(grepl(pattern = "m2superficie", blocco1[[j]][i], fixed = TRUE)) {
#         print("ok")
#       } else {
#         blocco1[[j]][[i]] <- NA
#         print("ora ok")
#       }
#     }
#   }, silent = TRUE)

try(
for (j in 2:length(blc)) { # due perchè la prima pagina l'ho fatta separatamente
  # controllare per questa cosa anche dalle altre parti
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

#aggiungere numero di pagina e numero annuncio
annuncio <- rep(1:25,n_pag_da_cercare)
pagina <- rep(1:n_pag_da_cercare,rep(25,n_pag_da_cercare))

#siccome le pagine finiscono corte se le prendo tutte in considerazione
vec_prezzo_int <-  vec_prezzo_int[1:(25*n_pag_da_cercare)]
mqvecperdf <- mqvecperdf[1:(25*n_pag_da_cercare)]
localivecperdf <- localivecperdf[1:(25*n_pag_da_cercare)]

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
  pr_al_mq <- c()
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
pr_mq_adj <- adj_pr_mq(col_prezzo = df$prezzo, col_mq = df$mq, soglia = 100000)
#prezzo_adj[1:length(df$prezzo)]
df$pr_mq_adj <- pr_mq_adj[1:length(df$prezzo)]
df <- df[order(df$pr_mq_adj, decreasing = FALSE),c(7,6,1,2,3,4,5)]
#attenzione: sostituisce il file già esistente
write.csv(df, "/Users/tommasobassignana/Desktop/df_immobiliare.csv")






