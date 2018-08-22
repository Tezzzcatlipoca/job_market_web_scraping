# En este script se creará una base de datos de twiter sobre algún tema específico

# Haremos un programa genérico para mapear lo que se dice de un tema o usuario
us_tem <- "@CREFAL_Oficial"
directorio <- "H:/Mi unidad/Análisis de Twiter/@CREFAL_Oficial/Base de Twits"

# Listamos las librerías que vamos a usar
library(twitteR)

# copus de stop words
library(tidytext) # Inglés
library(tm) # Español

library(SnowballC) # Librería para obtener las raíces de las palabras

# Librería para crear gráficas de nube de texto
library(wordcloud)

# librería para usar el operado %>%, el cuál ayuda a disminuir el tiempo de procesamiento y mejorar la lectura de código
library(magrittr)

# Librer?a para manipular data frames como objetos
library(dplyr)

#Librería para gráficas
library(ggplot2)
library(tidyquant) #Colores para las gráficas
library(RColorBrewer) #Colores para las gráficas

# Primero listamos las bibliotecas que vamos a usar
######################################################################################################
# Establecemos contacto con la API #
####################################

consumer_key <- "SK5ZpR8o90eWcPXxA5lpTI0g1"
consumer_secret <- "pAcb3AvZdB5SYtfqGyt9gMotgtjNM9v6RE4WFvotlefKwboFzD"
access_token <- "112462622-sTwto1vGLZwkOxjXyx36HrcCskclgwBGo3GgDy6d"
access_secret <- "wDA4rjp5ev1Q3T9LNo8A617JSBvaUgYW2ac0kXyg873Da"


options(httr_oauth_cache = TRUE)

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)



######################################################################################################

######################## Obtenemos los Tweets ###############################
tweets <- searchTwitter(paste0(us_tem), n=1500)
tweets_df <- twListToDF(tweets)

####################### Obtenemos los usuarios que twitean ##################

usuarios_id <- as.list(tweets_df$id) 
usuarios_nomb <- as.list(tweets_df$screenName)

# Buscamos los usuarios
usuarios_enc <- lookupUsers(usuarios_nomb)

DataUser <- lapply(usuarios_enc, function(x) getUser(x))

DataUser <- twListToDF(DataUser)
###########################################################################

# Salvamos la base
fecha <- Sys.Date()
fecha
setwd(directorio)
save.image(paste(us_tem,"(",fecha, ")", ".Rda" ))

#############################################################################
#############################################################################
