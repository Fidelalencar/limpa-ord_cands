

# Lista de candidatos
library(tidyverse)
library(ggplot2)

########## ANALISTA TRE
# Carregar o arquivo de texto
caminho_arquivo <- file.choose() # Escolher o arquivo

texto <- readLines(caminho_arquivo) # carrega 
texto <- paste(texto, collapse = "") # ignora as linhas, criando um elemento so

linhas <- unlist(strsplit(texto, "/")) # quebra as linhas com base na barra

df <- data.frame(Linhas = linhas, stringsAsFactors = FALSE) # organiza o DF
df_separado <- df %>%
  separate(Linhas, into = c("inscr", "cand", "obj", "disc"), sep = ",")


df_separado$obj <- as.numeric(df_separado$obj)
df_separado$disc <- as.numeric(df_separado$disc)
# AQUI, um experimento: transformando as notas da discursiva para o mesmo range de valores da objetiva
# df_separado$disc <- (df_separado$disc / 50) * 190
#
df_separado$tot <- df_separado$obj + df_separado$disc # soma das notaas

df_separado <- df_separado[order(-df_separado$obj),] # ordem pela objetiva
df_separado$coloc_obj <- 1:nrow(df)

df_separado <- df_separado[order(-df_separado$disc),] # ordem pela discursiva
df_separado$coloc_disc <- 1:nrow(df)

df_separado <- df_separado[order(-df_separado$tot),] # ordem pelo total
df_separado$coloc_tot <- 1:nrow(df)

# write.csv(df_separado, file = "anal.adm.tre.PE.csv", row.names = FALSE)

#notas da discursiva
ggplot(df_separado, aes(x = disc)) +
  geom_histogram(binwidth = .5, fill = "blue", color = "black", alpha = 0.7) +
  geom_text(stat = "bin", 
            aes(label = ..x..), 
            vjust = -0.2, 
            binwidth = .5,
            size=2) +
  labs(title = "Histograma",
       x = "Notas",
       y = "Frequência") +
  theme_minimal()



## analise
cor(df_separado$obj, df_separado$coloc_tot, use = "complete.obs") # 69%
cor(df_separado$disc, df_separado$coloc_tot, use = "complete.obs")  # 80%

x <- subset(df_separado, coloc_tot < 20)  # aparentemente a correlação da discursiva não é forte entre os primeiros colocados
cor(x$obj, x$coloc_tot, use = "complete.obs") # 70%
cor(x$disc, x$coloc_tot, use = "complete.obs")  # 20%
# Ou seja, apesar de a discursiva em média fazer um candidato qualquer subir mais posições, os primeiros colocados no concurso tiraram as maiores notas na objetiva





########## TECNICO TRE
# Carregar o arquivo de texto
caminho_arquivo <- file.choose() # Escolher o arquivo

texto <- readLines(caminho_arquivo)  # carrega 
texto <- paste(texto, collapse = "") # ignora as linhas, criando um elemento so

linhas <- unlist(strsplit(texto, "/"))  # quebra as linhas com base na barra

df <- data.frame(Linhas = linhas, stringsAsFactors = FALSE)  # organiza o DF
df_separado <- df %>%
  separate(Linhas, into = c("inscr", "cand", "obj"), sep = ",")


df_separado$obj <- as.numeric(df_separado$obj)

df_separado <- df_separado[order(-df_separado$obj),]
df_separado$coloc <- 1:nrow(df)
# write.csv(df_separado, file = "tec.adm.tre.PE.csv", row.names = FALSE)




########## ANALISTA GESTÃO TCE-PE 2017
# A nota máxima da objetiva era 120.
# Carregar o arquivo de texto
caminho_arquivo <- file.choose() # Escolher o arquivo

texto <- readLines(caminho_arquivo) # carrega 
texto <- paste(texto, collapse = "") # ignora as linhas, criando um elemento so

linhas <- unlist(strsplit(texto, "/")) # quebra as linhas com base na barra

df <- data.frame(Linhas = linhas, stringsAsFactors = FALSE) # organiza o DF
df_separado <- df %>%
  separate(Linhas, into = c("inscr", "cand", "obj"), sep = ",")

df_separado$obj <- as.numeric(df_separado$obj)

# 
df_separado <- df_separado[order(-df_separado$obj),] # ordem pela objetiva
df_separado$coloc_obj <- 1:nrow(df)


#notas da discursiva
ggplot(df_separado, aes(x = obj)) +
  geom_histogram(binwidth = .5, fill = "blue", color = "black", alpha = 0.7) +
  geom_text(stat = "bin", 
            aes(label = ..x..), 
            vjust = -0.2, 
            binwidth = .5,
            size=2) +
  labs(title = "Histograma",
       x = "Notas",
       y = "Frequência") +
  theme_minimal()



########## ANALISTA GESTÃO TCE-PE 2017
# A nota máxima da objetiva era 120... 
# A discursiva valia 40 e a nota foi divulgada tb seu reurso (ver abaixo)

# Carregar o arquivo de texto
caminho_arquivo <- file.choose() # Escolher o arquivo

texto <- readLines(caminho_arquivo) # carrega 
texto <- paste(texto, collapse = "") # ignora as linhas, criando um elemento so

linhas <- unlist(strsplit(texto, "/")) # quebra as linhas com base na barra

df <- data.frame(Linhas = linhas, stringsAsFactors = FALSE) # organiza o DF
df_separado <- df %>%
  separate(Linhas, into = c("inscr", "cand", "obj", "disc"), sep = ",")

df_separado$obj <- as.numeric(df_separado$obj)
df_separado$disc <- as.numeric(df_separado$disc)


df_separado$tot <- df_separado$obj + df_separado$disc # soma das notaas
# 
df_separado <- df_separado[order(-df_separado$obj),] # ordem pela objetiva
df_separado$coloc_obj <- 1:nrow(df)

df_separado <- df_separado[order(-df_separado$disc),] # ordem pela discursiva
df_separado$coloc_disc <- 1:nrow(df)

df_separado <- df_separado[order(-df_separado$tot),] # ordem pelo total
df_separado$coloc_tot <- 1:nrow(df)

# # Carregar a nota da discursiva apos recurso - o arquivo de texto
# caminho_arquivo <- file.choose() # Escolher o arquivo
# texto <- readLines(caminho_arquivo) # carrega 
# texto <- paste(texto, collapse = "") # ignora as linhas, criando um elemento so
# linhas <- unlist(strsplit(texto, "/")) # quebra as linhas com base na barra
# dfREC <- data.frame(Linhas = linhas, stringsAsFactors = FALSE) # organiza o DF
# dfREC_separado <- dfREC %>%
#   separate(Linhas, into = c("inscr", "cand", "rec"), sep = ",")
# dfREC_separado$rec <- as.numeric(dfREC_separado$rec)
# df_separado$inscr <- as.numeric(df_separado$inscr)
# dfREC_separado$inscr <- as.numeric(dfREC_separado$inscr)
# df_sep.final <- full_join(df_separado, dfREC_separado, by = "inscr")
# recursos <- df_sep.final[(df_sep.final$disc != df_sep.final$rec),]
# # CONCLUSÃO: Só 6 recursos foram aceitos e nenhum alcançou nem 1 décimo





