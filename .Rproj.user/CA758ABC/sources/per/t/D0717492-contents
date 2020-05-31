
library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)

## LIBS PARA EXPORTAR COMO HTML 
library(plotly);
library(htmlwidgets);

# LIBRERÍA PARA FETCH DE IMAGEN PNG DE EMOJI DESDE https://abs.twimg.com
library(ggimage)

# LEEMOS EL CHAT A TRAVÉS DEL TXT EXPORTADO DESDE LA APP
miChat <- rwa_read("miChat_1.txt")


# PREPARACIÓN DE DATOS PARA ANÁLIIS POR DATE/TIME
miChat <- miChat %>% 
  mutate(day = date(time)) %>% 
  mutate(
    # SEGMENTACIÓN POR MES
    estacion = case_when(
      day >= dmy(18082018) & day <= dmy(22092018) ~ "Verano 2018",
      day >= dmy(23092018) & day <= dmy(20122018) ~ "Otoño 2018",
      day >= dmy(21122018) & day <= dmy(20032019) ~ "Invierno 2018",
      day >= dmy(21032019) & day <= dmy(21062019) ~ "Primavera 2019",
      day >= dmy(22062019) & day <= dmy(23092019) ~ "Verano 2019",
      day >= dmy(23092019) & day <= dmy(20122019) ~ "Otoño 2019",
      day >= dmy(21122019) & day <= dmy(20032020) ~ "Invierno 2020",
      day >= dmy(21032020) ~ "Primavera 2020",
      T ~ "Fuera de rango"
    )
  ) %>% 
  mutate( estacion = factor(estacion) ) %>% 
  filter(!is.na(author))

# PALETA DE COLORES
paleta.estaciones <- brewer.pal(8,"Set1")[c(7,5,1,3,4,2,6,8)]


# CAMBIEMOS EL NOMBRE DE LOS USUARIOS POR CONFIDENCIALIDAD
levels(miChat$author)[2] <- "Ella"
levels(miChat$author)[1] <- "Él"


library(tidytext)
library(stopwords)

# REMOVEMOS PALABRAS SIN SIGNIFICADO RELEVANTE, COMO ARTÍCULOS, PRONOMBRES, ETC.
remover_palabras <- c(stopwords(language = "pt"),
                      "multimedia", "y", "la", "el","en", "es", "si", "lo", "ya", "pero", "esa",
                      "los","yo","mi", "un", "con", "las", "omitido", "más","eso", "al", "una",
                      "del", "qué", "todo", "así", "le", "su", "va", "porque", "todos", "hay", "les",
                      "pue", "ese", "son", "está", "pues", "ahí", "sí","ver", "estás", "algo", "vas",
                      "ir","voy", "creo","fue","solo", "ni","sólo","nada", "aqui", "aa", "q", "tú", "fez")


# DIVERSIDAD DE LÉXICO
miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% remover_palabras) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("Diversidad léxica") +
  xlab("Usuario") +
  ggtitle("Diversidad de léxico en la conversación") +
  coord_flip()

ggplotly()


# PALABRAS ÚNICAS POR ELLA
palabras_unicas_ella <- miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Ella") %>%  
  count(word, sort = TRUE) 

miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Ella") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% palabras_unicas_ella$word) %>% # SELECCIONAR SÓLO PALABRAS QUE NADIE MÁS USA
  top_n(n = 15, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("Número de veces que se usó la palabra") + xlab("Palabras") +
  coord_flip() +
  ggtitle("Top de palabras únicas usadas por Ella")

ggplotly()


# PALABRAS ÚNICAS POR ÉL
palabras_unicas_el <- miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Él") %>%  
  count(word, sort = TRUE) 

miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Él") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% palabras_unicas_el$word) %>% # SELECCIONAR SÓLO PALABRAS QUE NADIE MÁS USA
  top_n(n = 15, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("Número de veces que se usó la palabra") + xlab("Palabras") +
  coord_flip() +
  ggtitle("Top de palabras únicas usadas por Él")

ggplotly()




# ANÁLISIS DE SENTIMIENTOS

library(rvest)

# FETCH DE PÁGINA HTML EMOJI SENTIMENT RANKING 1.0
url_base <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
doc <- read_html(url_base)

# BUSCAR TABLA DE EMOJI Y PROCESO
tabla_emojis <- doc %>% 
  html_node("#myTable") %>% 
  html_table() %>% 
  as_tibble()

# UN VISTAZO PREVIO AL RESULTADO
tabla_emojis %>% 
  head(5) %>% 
  kable() %>% 
  kable_styling(font_size = 10)


# OBTENER PUNTAJE DE SENTIMIENTO Y LIMPIAR LOS NOMBRES DE LA TABLAS DE EMOJI
sentimiento_emoji <- tabla_emojis %>% 
  select(1,6:9) %>% 
  set_names("char", "negativo","neutral","positivo","sent.score")

# EXTRAER EMOJI Y UNIR CON SENTIMIENTO
emoji_chat <- miChat %>% 
  unnest(emoji, emoji_name) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% # Remover ligaduras
  inner_join(sentimiento_emoji, by=c("emoji"="char")) 

# VISUALIZACIÓN 
emoji_chat %>% 
  select(-source, -day, -estacion) %>% 
  slice(1207:1219) %>% 
  head(10) %>%
  kable() %>% 
  kable_styling(font_size = 10)


# OCURRENCIAS DE SENTIMIENTOS POR EMOJIS, POR USUARIO
emoji_sentimiento_usuarios <- emoji_chat %>% 
  group_by(author) %>% 
  summarise(
    positivo=mean(positivo),
    negativo=mean(negativo),
    neutral=mean(neutral),
    balance=mean(sent.score)
  ) %>% 
  arrange(desc(balance))

# FORMATO DE DATOS PARA REALIZAR PLOT
emoji_sentimiento_usuarios %>% 
  mutate( negativo  = -negativo,
          neutral.positivo =  neutral/2,
          neutral.negativo = -neutral/2) %>% 
  select(-neutral) %>% 
  gather("sentiment","mean", -author, -balance) %>% 
  mutate(sentiment = factor(sentiment, levels = c("negativo", "neutral.negativo", "positivo", "neutral.positivo"), ordered = T)) %>% 
  ggplot(aes(x=reorder(author,balance), y=mean, fill=sentiment)) +
  geom_bar(position="stack", stat="identity", show.legend = F, width = .5) +
  scale_fill_manual(values = brewer.pal(4,"RdYlGn")[c(1,2,4,2)]) +
  ylab(" - Negativo / Neutral / Positivo +") + xlab("Usuario") +
  ggtitle("Análisis de sentimientos por usuario","Basado en el puntaje promedio de sentimientos por emojis") +
  coord_flip() +
  theme_minimal() 

ggplotly()




# ANÁLISIS DE SENTIMIENTOS CON LEXICON

library(textdata)

# OBTENER LÉXICO POSITIVO/NEGATIVO DEL PACKAGE DE LEXICON 
lexico_negpos  <- get_sentiments("afinn") # INTENSIDAD DE VALOR

# PREVIEW DEL FORMATO DE LEXICON
lexico_negpos %>% 
  head(10) %>% 
  kable() %>%
  kable_styling(full_width = F, font_size = 11)

# PREVIEW CUÁLES SON LOS VALORES POSIBLES
table(lexico_negpos$value) %>% 
  head(10) %>% 
  kable() %>%
  kable_styling(full_width = F, font_size = 11) 

# EXTRAER EMOJIS
emoji_sentimiento_score <- miChat %>%
  select( emoji, emoji_name) %>% 
  unnest( emoji, emoji_name) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>%  # REMOVER LIGADURAS
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%  # REMOVER NOMBRES DE LIGADURA
  distinct() %>% 
  unnest_tokens(input=emoji_name, output=emoji_words) %>% 
  inner_join(lexico_negpos, by=c("emoji_words"="word"))

# CREAR TABLA DE 3 COLUMNAS
bind_cols(
  slice(emoji_sentimiento_score, 01:10),
  slice(emoji_sentimiento_score, 11:20),
  slice(emoji_sentimiento_score, 21:30)
) %>% 
  kable() %>% 
  kable_styling(full_width = F, font_size = 11) 

# EXTRAER EMOJIS
emoji_chat <- miChat %>% 
  unnest(emoji, emoji_name) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>%  # REMOVER LIGADURAS
  mutate( emoji_name = str_remove(emoji_name, ":.*")) # REMOVER NOMBRES DE LIGADURA

# TOKENIZAR EL NOMBRE DE EMOJI
emoji_chat <- emoji_chat %>% 
  select(author, emoji_name) %>% 
  unnest_tokens(input=emoji_name, output=emoji_words)

# JOIN CON LEXICON
usuario_summary <- emoji_chat %>% 
  inner_join(lexico_negpos, by=c("emoji_words"="word")) %>% 
  count(author, value) %>% 
  group_by(author) %>% 
  mutate(mean=n/sum(n)) %>% 
  ungroup()

# COLORES Y GRÁFICA
reordenar_niveles <- c(-3,-2,-1,3,2,1)
colores <- c("#d7191c","#fdae61","#ffffbf","#1a9641","#a6d96a","#ffffbf")
mis_colores <- brewer.pal(5,"RdYlGn")[c(1,2,3,5,4,3)]

# PLOT DE LA GRÁFICA
usuario_summary %>% 
  mutate( mean = ifelse(value<0, -mean, mean)) %>% 
  group_by(author) %>% 
  mutate( balance = sum(mean)) %>% 
  ungroup() %>% 
  mutate( value = factor(value, levels = reordenar_niveles, ordered=T)) %>% 
  ggplot(aes(x=reorder(author,balance), y=mean, fill=value)) +
  geom_bar(stat="identity",position="stack", show.legend = F, width = .5) +
  scale_fill_manual(values = mis_colores) +
  xlab("Usuario") + ylab("Escala de netagivo a positivo") +
  coord_flip() +
  ggtitle("Análisis de sentimientos por usuario", "Basado en el léxico AFINN") +
  theme_minimal()


ggplotly()



# EMOCIÓN MÁS FRECUENTE

# OBTENER OTRO LÉXICO CON NOMBRE DE SENTIMIENTOS
lexico_sentimientos <- get_sentiments("nrc") # NOMBRE DEL SENTIMIENTO

# PREVIEW DE SENTIMIENTOS
lexico_sentimientos %>% 
  head(10) %>% 
  kable() %>%
  kable_styling(full_width = F, font_size = 11) 


# EXTRAER EMOJIS
emoji_emocion <- miChat %>%
  select( emoji, emoji_name) %>% 
  unnest( emoji, emoji_name) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>%  # REMOVER LIGADURAS
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%  # REMOVER NOMBRES DE LIGADURA
  unnest_tokens(input=emoji_name, output=emoji_words) %>% 
  inner_join(lexico_sentimientos, by=c("emoji_words"="word")) %>% 
  filter(!sentiment %in% c("negative","positive")) %>% # REMOVER CLASIFICACIÓN NEGATIVA/POSITIVA
  # MANTENER SÓLO LOS 4 EMOJI MÁS FRECUENTES PARA CADA SENTIMIENTO
  count(emoji, emoji_words, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(4,n) %>% 
  slice(1:4) %>% 
  ungroup() %>% 
  select(-n)

# PONER TABLAS JUNTAS
bind_cols(
  slice(emoji_emocion, 01:16),
  slice(emoji_emocion, 17:32)
) %>% 
  kable() %>% 
  kable_styling(full_width = F, font_size = 11) 


# JOIN CON EMOJIS
sentimiento_chat <- emoji_chat %>% 
  inner_join(lexico_sentimientos, by=c("emoji_words"="word")) %>% 
  filter(!sentiment %in% c("negative","positive")) # REMOVER CLASIFICACIÓN POITIVA/NEGATIVA

# PLOT DE EMOCIONES MAYORMENTE EXPRESADAS
sentimiento_chat %>% 
  count(sentiment) %>% 
  ggplot(aes(x=reorder(sentiment,n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .1) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  coord_flip() +
  ylab("Número de veces expresado") + xlab("Emoción") +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
  scale_color_gradient(low="#2b83ba",high="#d7191c") +
  ggtitle("Emoción expresada con mayor frecuencia","Expresado por uso de emojis") +
  theme_minimal()

ggplotly()



# PLOT DE EMOCIONES POR USUARIO
sentimiento_chat %>% 
  count(author, sentiment) %>% 
  left_join(filter(lexico_sentimientos, sentiment %in% c("negative","positive")),by=c("sentiment"="word")) %>% 
  rename( sentimiento = sentiment.y) %>% 
  mutate( sentimiento = ifelse(is.na(sentimiento), "neutral", sentimiento)) %>% 
  mutate( sentimiento = factor(sentimiento, levels = c("negative", "neutral", "positive"), ordered=T) ) %>% 
  group_by(author) %>%
  top_n(n = 8, n) %>%
  slice(1:8) %>% 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentimiento)) +
  geom_col() +
  scale_fill_manual(values = c("#d7191c","#fdae61", "#1a9641")) +
  ylab("Número de veces expresado") +
  xlab("Emoción") +
  coord_flip() +
  facet_wrap(~author, ncol = 3, scales = "free_x") +
  ggtitle("Emociones mayormente expresadas por usuario", "Expresado por uso de emojis") + 
  theme_minimal() + theme(legend.position = "bottom")


ggplotly()

