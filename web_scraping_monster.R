
# Empleamos la biblioteca para leer códigos web
library('rvest')
library("magrittr")
library("curl")

# Espesificamos el sitio web que vamos a nalizar
# Convertimos la información a texto

# puesto_dataf <- NULL
salario_dataf <- NULL
categoria_dataf <- NULL
direccion_url_dataf <- NULL
fecha_dataf <- NULL
lugar_dataf <- NULL
numvacantes <- NULL
liags <- NULL
sistiempo<-NULL

# Se extraen primero los v�nculos de todas las vacantes de INDEED 
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
  } else {
    texto_vacante <- NA
  }
  
  textos<-append(textos,texto_vacante)
  sistiempo <- append(sistiempo, as.character(Sys.time()))
  Sys.sleep(runif(1, 10, 35))
  cat(vacante, paste0(Sys.time()), nchar(texto_vacante), substr(texto_vacante,1,30), "\n")
  
  
}


