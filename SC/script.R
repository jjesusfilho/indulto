library(quanteda)
library(tibble)
agravo<-ind[str_which(ind$acao,"Recurso\\sde\\sAgravo|Agravo\\sde\\sExecução\\sPenal"),]

agravo$inteiro<-stringi::stri_trans_general(agravo$inteiro,"Latin-ASCII")
agravo$inteiro<-tolower(agravo$inteiro)
#agravo$processo<-str_replace_all(agravo$processo,"\\D+","")

duplicado<-duplicated(agravo$inteiro)
agravo<-agravo[!duplicated(agravo$inteiro),]



#agravo<-column_to_rownames(agravo,"processo")




indulto<-kwic(agravo$inteiro,"indulto|comutação",valuetype="regex")


