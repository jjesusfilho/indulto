library(httr)
library(xml2)
library(stringr)

tjsc<- function(word="",phrase=""){
  
phrase<-deparse(phrase)

  url<-"http://busca.tjsc.jus.br/jurisprudencia/buscaForm.do"
  url2<-"http://busca.tjsc.jus.br/jurisprudencia/buscaajax.do?&categoria=acordaos"
  
  
  a<-GET(url)
  
  body<-list(q = "", only_ementa = "", frase = "", excluir = "", 
             qualquer = "", `NA` = NA_character_, prox1 = "", prox2 = "", 
             proxc = "", sort = "dtJulgamento desc", ps = "50", busca = "avancada", 
             pg = "1", flapto = "1", radio_campo = "integra", `categoria[]` = "acordaos", 
             faceta = "false")
  
  body$`q`<-word
  body$`frase`<-phrase
  
  b<-POST(url2,body=body,encode="form",set_cookies(unlist(a$cookies)))
  
  num<- b %>% 
    httr::content() %>% 
    xml_find_all("//*[@class='texto_resultados']") %>%
    xml_text() %>% 
    str_extract("\\d+") %>% 
    as.numeric() %>% 
    `/`(50) %>% 
    ceiling()
  
  c<-NULL
  
  for(i in 1:num){
    body[[13]]<-i
    d<-url2 %>% POST(body=body,encode="form",set_cookies(unlist(a$cookies))) %>% 
      httr::content() %>% 
      xml_find_all("//*[@id='coluna_principal']/div/div[3]/a/@href") %>% 
      xml_text() %>% 
      paste0("http://busca.tjsc.jus.br/jurisprudencia/",.)
    c<-c(c,d)
  }
  
  c<-map_chr(c,URLencode)
  
  resultado<- map(c,function(x){
    x %>% GET() %>% 
      httr::content()
  })
  
  inteiroTeor<-resultado %>%
    map_chr(function(x){ 
      x %>% 
        xml_find_all("//*[@id='coluna_principal']/div/div[3]") %>% 
        xml_text(trim=T)
    })
  
  df<-map(resultado,function(x){ 
    x %>% xml_find_all("//*[@class='resultados']/strong/following-sibling::text()") %>% 
      xml_text(trim=T) %>% 
      .[1:8]
  })
  
  df1<-map(resultado, function(x){ 
    x %>% xml_find_all("//*[@class='resultados']/strong") %>% 
      xml_text(trim=T) %>% 
      .[1:8]
  })
  
  df2<-map2(df1,df,~paste(.x,.y)) %>% unlist()
  

  df3<-tibble(inteiroTeor,
              processo=df2 %>% stri_extract_first_regex("(?<=Processo:\\s).*") %>% na.omit(),
              relator=df2 %>% stri_extract_first_regex("(?<=Relator:\\s).*")  %>% na.omit(),
              origem=df2 %>% stri_extract_first_regex("(?<=Origem:\\s).*")  %>% na.omit(),
              orgaoJulgador=df2 %>% stri_extract_first_regex("(?<=Julgador:\\s).*")  %>% na.omit(),
              classe=df2 %>% stri_extract_first_regex("(?<=Classe:\\s).*")  %>% na.omit()
  )
  return(df3)
}

indulto<-tjsc(word="indulto")
comutacao<-tjsc(word="comutacao")


indulto<-rbind(indulto,comutacao)

