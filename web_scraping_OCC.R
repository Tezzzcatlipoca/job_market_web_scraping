# Empleamos la biblioteca para leer códigos web
library('rvest')
#library('httr')
library('curl')
library('RSelenium')

# Espesificamos el sitio web que vamos a analizar
# Convertimos la información en texto

puesto_dataf <- NULL
salario_dataf <- NULL
categoria_dataf <- NULL
url_dataf <- NULL
url_pages_dataf<- NULL
puesto_pages_dataf<-NULL
     
# Open browser
our_browser <- rsDriver(browser = "chrome")
# Browser handler into variable
rem_browser<-our_browser[["client"]]
# Open page
rem_browser$navigate("https://www.occ.com.mx/empleos/")
# Get Source code
webpage<-rem_browser$getPageSource()
# Get text data from list variable and transform it into xml_document
web_html<-read_html(webpage[[1]]) # Parse
# Extract nodes from xml document
# New URLs
# Obtener URLs de cada oportunidad laboral
urls <- web_html %>% html_nodes('a') %>% html_attr("href")
job_urls <- paste0("https://www.occ.com.mx",
                   urls[grepl(pattern = "/empleo/oferta/",x = urls)])
url_dataf <- c(url_dataf, unique(job_urls))
# NEED TO CHANGE THESE: THEY WERE NOT DESIGNED FOR OCC !!!!
puesto <- html_nodes(web_html,'h2') # Job Titles
# Convertimos la información en texto
puesto_data <- html_text(puesto)
# Append values
puesto_dataf <- c(puesto_dataf, puesto_data)
pages_dataf<- c(pages_dataf, rep(1,length(unique(job_urls))))
puesto_pages_dataf<- c(puesto_pages_dataf,rep(pag,length(puesto_data)))

# ------------- Now the rest of the pages --------------------
     
for (pag in 2:30) {
     # Wait up some time in each loop
     tiempo<-75+rnorm(1,20,41.5)
     Sys.sleep(tiempo)
     # Retrieve next page
     curr_url<-paste0("https://www.occ.com.mx/empleos/?page=",pag)
     rem_browser$navigate(curr_url)
     # Let know where you are now
     cat(paste0("Carga exitosa de página ",pag," @-",Sys.time()))
     # Get Source code
     webpage<-rem_browser$getPageSource()
     # Get text data from list variable and transform it into xml_document
     web_html<-read_html(webpage[[1]]) # Parse
     # Obtener titulos de ofertas laborales
     puesto <- html_nodes(web_html,'h2') # Job Titles
     # Convertimos la información en texto
     puesto_data <- html_text(puesto)
     puesto_dataf <- c(puesto_dataf, puesto_data)
     # Obtener URLs de cada oportunidad laboral
     urls <- web_html %>% html_nodes('a') %>% html_attr("href")
     job_urls <- paste0("https://www.occ.com.mx",
                        urls[grepl(pattern = "/empleo/oferta/",x = urls)])
     url_dataf <- c(url_dataf, unique(job_urls))
     url_pages_dataf<- c(url_pages_dataf,rep(pag,length(unique(job_urls))))
     puesto_pages_dataf<- c(puesto_pages_dataf,rep(pag,length(puesto_data)))
}

vacancy_data<-data.frame(titulo=puesto_dataf , url=url_dataf, 
                         url_index=c( url_pages_dataf, 
                                      rep(NA,length(puesto_dataf)-length(url_pages_dataf) ) ), 
                         puesto_index=c( puesto_pages_dataf, 
                                      rep(NA,length(url_pages_dataf)-length(puesto_dataf) )) )
# Save output

# Date is also important

# Create function that visits each link from the results page


#       HERE !!!!










# Perhaps better if we visit the pages that appear in each result page
# Now loop to review the rest of the list


# To stop de browser (Needed orelse it stops responding!!!)
our_browser[["server"]]$stop()
# To terminate browser
rm(our_browser)
gc(our_browser)




#### ----------------- Sobrantes de código  ---------------------------

to_send <- list ( "Host" = "www.occ.com.mx",
                  "Connection" = "keep-alive",
                  "Accept" = 'application/json, text/plain, */*',
                  "authorization" = "M41nDY0uR0wNBu5S1n35",
                  "User-Agent" = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36',
                  "Referer" = "https://www.occ.com.mx/empleos-en-mexico-y-el-mundo/ventas",
                  "Accept-Encoding" = "gzip, deflate, br",
                  "Accept-Language" = "es,en-GB;q=0.9,en;q=0.8,pl;q=0.7,de;q=0.6,gl;q=0.5",
                  "Cookie" = "_gcl_au=1.1.1652098363.1542156766; occtrsid=058b2e58-b24c-468d-b71c-6dd0a7310ed4; _ga=GA1.3.1170647620.1542156767; _gid=GA1.3.1895139682.1542156767; _fbp=fb.2.1542156767841.550567228; _dc_gtm_UA-3096589-32=1"
)


#h <- new_handle()
#handle_setform(h,
#               Host = "www.occ.com.mx",
#               Connection = "keep-alive",
#               Accept = "application/json, text/plain, */*",
#               authorization = "M41nDY0uR0wNBu5S1n35",
#               User-Agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36",
#               Referer = "https://www.occ.com.mx/empleos-en-mexico-y-el-mundo/ventas",
#               Accept-Encoding = "gzip, deflate, br",
#               Accept-Language = "es,en-GB;q=0.9,en;q=0.8,pl;q=0.7,de;q=0.6,gl;q=0.5",
#               Cookie = "_gcl_au=1.1.1652098363.1542156766; occtrsid=058b2e58-b24c-468d-b71c-6dd0a7310ed4; _ga=GA1.3.1170647620.1542156767; _gid=GA1.3.1895139682.1542156767; _fbp=fb.2.1542156767841.550567228; _dc_gtm_UA-3096589-32=1"
#)

#salario <- html_nodes(web_html,'.salary') # Not every result has salary info
#categoria <- html_nodes(web_html,'.bottom-info') # Not easy to find on OCC
#salario_data <- html_text(salario)
#categoria_data <- html_text(categoria)
#puesto_data <- puesto_data[1:length(salario_data)]
#salario_dataf <- c(salario_dataf, salario_data)
#categoria_dataf <- c(categoria_dataf, categoria_data)

# Find link to next page:
#web_html %>% html_nodes(xpath = '//*[@id="search-results"]/div[3]/div/div[1]/div/div[3]/div/div/div/ul/li') %>% html_text
# Mucho pedo porque el div no contiene a href. El vínculo se hace de otra forma.
# Ver si funciona generando artificialmente el URL
# No funcionó lo siguiente:
#texto_vacante <- web_html %>% html_nodes('div:contains("Siguiente")')

for(i in 1:25) {
     url <- paste0("https://www.occ.com.mx/empleos-en-michoacan?tm=60&page=", i)
     # "https://www.occ.com.mx/empleos-en-mexico-y-el-mundo/en-la-ciudad-de-Morelia?tm=60"
     # https://www.occ.com.mx/empleos-en-mexico-y-el-mundo/en-la-ciudad-de-Morelia?tm=60&page=2
     tiempo<-75+rnorm(1,20,41.5)
     Sys.sleep(tiempo)
     
     # Leemos el código web del sitio
     #webpage <- read_html(url)
     #webpage <- curl()
     webpage <- GET("https://www.occ.com.mx/rest?server=jobs&service=jobs&query=%3Floc%3D%26categoria%3D19%26showseo%3Dtrue%26bdtype%3DOCCM%26f%3Dtrue", query = to_send)
     
     # Empleamos el complemeto SelectorGadget de Chrome para identificar en código CSS que necesitamos. Identificamos su parte del código correspondiente.
     puesto <- html_nodes(webpage,'h2')
     salario <- html_nodes(webpage,'.salary')
     categoria <- html_nodes(webpage,'.bottom-info')
     url <-  pagina %>% html_nodes('h2.jobtitle') %>% html_nodes('a') %>% html_attr("href")
     #sitio<-grepl("/ofertas-de-trabajo/",url,ignore.case = TRUE)
     urls <- paste0("https://www.indeed.com",url)
     liags <- append(liags,urls)
     
     
     # Convertimos la información en texto
     puesto_data <- html_text(puesto)
     salario_data <- html_text(salario)
     categoria_data <- html_text(categoria)
     
     puesto_data <- puesto_data[1:length(salario_data)]
     
     puesto_dataf <- c(puesto_dataf, puesto_data)
     salario_dataf <- c(salario_dataf, salario_data)
     categoria_dataf <- c(categoria_dataf, categoria_data)
     
}

data_final<-data.frame(puesto=puesto_dataf[1:length(salario_dataf)],salario=salario_dataf,categoria=categoria_dataf)
write.table(data_final,choose.files(),quote = FALSE,sep="\t",col.names = FALSE,row.names = TRUE)

# Contar los nombres de vacante que contengan la palabra "Programador"
data_final$programador<-grepl("programador|programaci",data_final$puesto,ignore.case = TRUE)

