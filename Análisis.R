# Abrimos el corpus
load("H:/Mi unidad/Análisis de Twiter/@CREFAL_Oficial/Base de Twits/@CREFAL_Oficial ( 2017-12-06 ) .Rda")


# copus de stop words
library(tidytext) # Inglés
library(tm) # Español
library(SnowballC) # Librería para obtener las raíces de las palabras
# Librería para crear gráficas de nube de texto
library(wordcloud)
# librería para usar el operado %>%, el cuál ayuda a disminuir el tiempo de procesamiento y mejorar la lectura de código
library(magrittr)
# Librería para manipular data frames como objetos
library(dplyr)

#Librería para gráficas
library(ggplot2)
library(tidyquant) #Colores para las gráficas
library(RColorBrewer) #Colores para las gráficas

############################################################################################################
########################### Text Mining ###################################################################

# Creamos un diccionario de stop words
data("stop_words")
word <- (c(stop_words$word, stopwords("spanish"), "NA", "na", "in", "for", "with","at", "the", "to", "on", "si", "et", "rt","die", us_tem))
stop_words <- as.data.frame(word)

# Eliminamos los caracteres especiales
tweets_df$text <- gsub("htt\\S+", "", tweets_df$text)
tweets_df$text <- gsub("@\\S+", "", tweets_df$text)
tweets_df$text <- gsub("#\\S+", "", tweets_df$text)
# Removemos los emoticones
tweets_df$text <- gsub("<[^>]+>", NA, tweets_df$text)

# Obtenemos los tokens de la base tanto en palabra, como en palabra-raíz
tidy_descr <- tweets_df %>%
  unnest_tokens(word, text) %>%
  mutate(word_stem = wordStem(word, language = "spanish")) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl("\\.|htt|@|_", word))

# Removemos los restos de los emoticones, que est?n registrados como caracteres del idioma japones
tidy_descr$word<- gsub('\\p{Hiragana}|\\p{Han}|\\p{Katakana}', NA, tidy_descr$word, perl = TRUE)
tidy_descr$word_stem<- gsub('\\p{Hiragana}|\\p{Han}|\\p{Katakana}', NA, tidy_descr$word_stem, perl = TRUE)

####################################################################################################
# Gráficas

# Etiqueta para las gráficas
texto <- "Recuento de las palabras más repetidas en "
texto <- paste(texto, us_tem)

# Graficamos las palabras más repetidas
tidy_descr %>%
  count(word, sort = TRUE) %>%
  filter(n > 20 & word != "na") %>% # qu? se debe poner NA pero no est? funcionando.
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col( alpha = 0.8) +
  coord_flip() +
  labs(x = "",
       y = texto)


palabras <- table(tidy_descr$word)

palabras2 <- cbind(names(palabras),as.integer(palabras))

palabras3 <- palabras2[,1]

frecuencia <- as.numeric(palabras2[,2])



palabras2
str(palabras2)






p1 <- as.list(tidy_descr$word)

palabras <- as.matrix(TermDocumentMatrix(palabras2))

tweets_tex <- data.frame(palabra = rownames(palabras3), Freq = rowSums(frecuencia), row.names = NULL)


wordcloud(words=palabras3, freq=frecuencia, max.words=100, colors=brewer.pal(8, "Dark2"))



