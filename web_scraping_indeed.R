
# Empleamos la biblioteca para leer c√≥digos web
library('rvest')
library("magrittr")
library("curl")

# Especificamos el sitio web que vamos a analizar
# Convertimos la informaci√≥n a texto

# puesto_dataf <- NULL
salario_dataf <- NULL
categoria_dataf <- NULL
direccion_url_dataf <- NULL
fecha_dataf <- NULL
lugar_dataf <- NULL
numvacantes <- NULL
liags <- NULL
sistiempo<-NULL

# Se extraen primero los vÌnculos de todas las vacantes de INDEED 
# for(i in 1:50) {
for(i in seq(0,500,10)) {
  if(i==0){
    url0 <- paste0("https://www.indeed.com/jobs?q=data+science&l=")
  } else {
    url0 <- paste0("https://www.indeed.com/jobs?q=data+science&start=",i)
  }
  
  #https://www.indeed.com/jobs?q=data+science&start=
  # url0 <- curl(paste0("https://www.computrabajo.com.mx/ofertas-de-trabajo/?p=",i,".html", i), handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  pagina<-read_html(url0)
  #Para el URL  
  
  
  url <-  pagina %>% html_nodes('h2.jobtitle') %>% html_nodes('a') %>% html_attr("href")
  #sitio<-grepl("/ofertas-de-trabajo/",url,ignore.case = TRUE)
  urls <- paste0("https://www.indeed.com",url)
  liags <- append(liags,urls)
  sistiempo <- append(sistiempo, as.character(Sys.time()))
  Sys.sleep(runif(1, .5, 1.5))
  cat(i, paste0(Sys.time()), length(urls), "\n")
  urls <- NULL
}


data_indeed <- as.data.frame(table(liags))
textos<-NULL
# Se abren todas las vacantes recabadas
for(vacante in liags){
  conexion<-curl(vacante, handle = curl::new_handle("useragent" = "LINE Lab - investigacion@crefal.org"))
  pagina<-tryCatch(read_html(conexion), error = function(e) NA)
  
  if(!is.na(pagina)){
    texto_vacante <- pagina %>% html_nodes(xpath="/html/body/div[1]/div[3]/div[3]/div/div[1]/div[1]") %>% html_text()
    if(identical(texto_vacante,character(0))){
      texto_vacante <- pagina %>% html_nodes(xpath="/html/body/div[1]/div[3]/div[3]/div/div/div[1]/div[1]") %>% html_text()
    }
    if(identical(texto_vacante,character(0))){
         texto_vacante <- pagina %>% html_nodes('div:contains("data")') %>% html_text()
    }
    
  } else {
    texto_vacante <- NA
  }
  
  textos<-append(textos,texto_vacante)
  sistiempo <- append(sistiempo, as.character(Sys.time()))
  Sys.sleep(runif(1, 10, 35))
  cat(vacante, paste0(Sys.time()), nchar(texto_vacante), substr(texto_vacante,1,30), "\n")
  
  
}

# Graba el resultado del raspado
write.table(data.frame(Textos=textos),paste0("web_scraping_data_Indeed_",substr(Sys.time(),1,10),".txt"),col.names = TRUE,row.names = FALSE,quote = FALSE,sep = "\t")
# Ojo: AquÌ se graba como data frame. Si se abre del archivo, hay que convertirlo
#      de nuevo en Array de tipo Character (NO FACTOR) para poder correr el resto del 
#      cÛdigo m·s abajo.

# Eliminar las observaciones repetidas
texto_limpio<-gsub("\\t|\\n","",textos)
# Identificar las copias idÈnticas
cuales<-duplicated(texto_limpio)
texto_limpio<-texto_limpio[!cuales] # Borrar duplicadas
textos<-textos[!cuales] # Borrar duplicadas

# FunciÛn que encuentra las lÌneas que son texto mayormente repetido en alguna de las 10 observaciones previas
# Es necesario enviar a esta funciÛn el Array con los textos ya limpios de \\n y \\t
cuales_semi_repetidas<-function(string_textos){
     tamano<-length(string_textos)
     culpables<-NULL
     for (i in 2:tamano) {
          if(is.na(string_textos[i])){
               string_textos[i]<-"             "
               culpables<-append(culpables,i)
          }
          caracteres<-nchar(string_textos[i])
          #if(substr(string_textos[i-1],1,caracteres)==string_textos[i]){culpables<-append(culpables,i)}
          for(pisos in 1:10){  # Busca 
               if(i-pisos>0){
                    if(substr(string_textos[i-pisos],1,caracteres)==string_textos[i]){
                         culpables<-append(culpables,i)
                         break
                    }
               } else {
                    break
               }
               
          }
          
          
     }
     return(culpables)     
     
}
repetidas<-cuales_semi_repetidas(texto_limpio)
texto_limpio<-texto_limpio[-repetidas]
textos<-textos[-repetidas]

# Juntar todo el texto
todo_texto<-paste0(textos,collapse = "\n")

### Corregir algunos problemas conocidos del corpus extraÌdo
# Insertar un espacio entre palabras pegadas: jobQualifications
todo_texto<-gsub("([a-z]{1})([A-Z]{1})","\\1 \\2",todo_texto)
# Todo a min˙sculas
todo_texto<-tolower(todo_texto)

# Quitar stopwords
library(tidytext)
library(stringr)
data("stop_words")
wordos<-stop_words$word[!stop_words$word %in% c("c","r")] # I want to be able to track C++ and R language
pals_a_quitar<-paste0("\\b",paste0(wordos,collapse = "\\b|\\b"),"\\b")
#todo_texto<-gsub(pals_a_quitar,"",todo_texto)
todo_texto<-stringr::str_replace_all(todo_texto,pals_a_quitar,"")
# Eliminamos los caracteres especiales
todo_texto <- gsub("htt\\S+", "", todo_texto)
todo_texto <- gsub("@\\S+", "", todo_texto)
todo_texto <- gsub("#\\S+", "", todo_texto)
# Removemos los emoticones
todo_texto <- gsub("<[^>]+>", "", todo_texto)
todo_texto <- gsub("[!#%&,-:;<=>?@_`{|}~\\(\\)\\$\\+\\'\\-]"," ",todo_texto)

# Dividir el texto en palabras, bi-gramas, trigramas y tetragramas
one_grams<-as.array(strsplit(todo_texto,"\\s",fixed = FALSE)[[1]]) # F = Regular Expression; T = Exact string match
# Limpiar
one_grams<-one_grams[!nchar(one_grams)<1]
desfasado<-c(one_grams," ")[2:(length(one_grams)+1)]
bi_grams<-paste0(one_grams," ",desfasado)
desfasado2<-c(desfasado," ")[2:(length(desfasado)+1)]
tri_grams<-paste0(bi_grams," ",desfasado2)
desfasado3<-c(desfasado2," ")[2:(length(desfasado2)+1)]
tetra_grams<-paste0(tri_grams," ",desfasado3)

# Encontrar las palabras m·s repetidas
one_freq<-as.data.frame.table(table(one_grams))
bi_freq<-as.data.frame.table(table(bi_grams))
tri_freq<-as.data.frame.table(table(tri_grams))
tetra_freq<-as.data.frame.table(table(tetra_grams))

# Put together and save
one_freq$tipo<-"one"
bi_freq$tipo<-"two"
tri_freq$tipo<-"three"
tetra_freq$tipo<-"four"
names(one_freq)<-c("gram","freq","type")
names(bi_freq)<-c("gram","freq","type")
names(tri_freq)<-c("gram","freq","type")
names(tetra_freq)<-c("gram","freq","type")
grams<-rbind(one_freq,bi_freq,tri_freq,tetra_freq)

# Grabar archivo con las tablas de frecuencia
nombre_out<-paste0("frecuencias_otras_vacantes",Sys.Date(),".txt")
write.table(grams,nombre_out,sep = "\t",quote = FALSE,row.names = FALSE,col.names = TRUE)


# Muchas palabras que no tienen sentido.
# Muchas palabras encimadas jobDescription, etc. Corregir desde antes de 
#  encontrar frecuencias.
# Generar tambiÈn trigramas y tetragramas.
# Quitar stopwords antes de cualquier cosa.
# Enviar todas a min˙sculas.
# Encontrar tambiÈn todas las palabras que son muy frecuentes en vacantes
#    de todo tipo (biologÌa, diseÒo, trabajo social, etc.) y eliminarlas
#    de la lista. Que queden solo las que son frecuentes en Data Science.

# Eliminar de la lista las palabras que son comunes en otro tipo de vacantes
pals_comparacion<-read.table(choose.files(),header = TRUE,sep = "\t",quote = "")

#### Limpiar n-grams
## One-grams
sensitivity<-25 # We'll discard any word that appeared more than 25 times
               # in the 560 comparison vacancies
other_ones<-as.character(pals_comparacion[pals_comparacion$type=="one" & 
                                  pals_comparacion$freq>sensitivity,'gram'])
clean_ones<-one_freq[!(as.character(one_freq$one_grams) %in% other_ones),]

## Bi-grams
sensitivity<-25 # We'll discard any word that appeared more than 25 times
               # in the 560 comparison vacancies
other_bis<-as.character(pals_comparacion[pals_comparacion$type=="two" & 
                                               pals_comparacion$freq>sensitivity,'gram'])
clean_twos<-bi_freq[!(as.character(bi_freq$bi_grams) %in% other_bis),]

## Tri-grams
sensitivity<-25 # We'll discard any word that appeared more than 25 times
               # in the 560 comparison vacancies
other_tris<-as.character(pals_comparacion[pals_comparacion$type=="three" & 
                                              pals_comparacion$freq>sensitivity,'gram'])
clean_threes<-tri_freq[!(as.character(tri_freq$tri_grams) %in% other_tris),]

# Four-grams
sensitivity<-25 # We'll discard any word that appeared more than 25 times
# in the 560 comparison vacancies
other_tetras<-as.character(pals_comparacion[pals_comparacion$type=="four" & 
                                               pals_comparacion$freq>sensitivity,'gram'])
clean_fours<-tetra_freq[!(as.character(tetra_freq$tetra_grams) %in% other_tetras),]



