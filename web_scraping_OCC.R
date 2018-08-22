# Empleamos la biblioteca para leer códigos web
library('rvest')

# Espesificamos el sitio web que vamos a analizar
# Convertimos la información en texto

puesto_dataf <- NULL
salario_dataf <- NULL
categoria_dataf <- NULL
for(i in 1:25) {
  url <- paste0("https://www.occ.com.mx/empleos-en-michoacan?tm=60&page=", i)
  # "https://www.occ.com.mx/empleos-en-mexico-y-el-mundo/en-la-ciudad-de-Morelia?tm=60"
  # https://www.occ.com.mx/empleos-en-mexico-y-el-mundo/en-la-ciudad-de-Morelia?tm=60&page=2
  tiempo<-75+rnorm(1,20,41.5)
  Sys.sleep(tiempo)
  
  # Leemos el código web del sitio
  webpage <- read_html(url)
  
  
  # Empleamos el complemeto SelectorGadget de Chrome para identificar en código CSS que necesitamos. Identificamos su parte del código correspondiente.
  puesto <- html_nodes(webpage,'h2')
  salario <- html_nodes(webpage,'.salary')
  categoria <- html_nodes(webpage,'.bottom-info')
  
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


