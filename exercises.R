install.packages("ggplot2")
library(ggplot2)


library(readr)
library(dplyr)
install.packages("readr")
install.packages('dplyr')
getwd()


imdb = readr::read_rds('imdb.rds')

#1
filme_ordenado = imdb %>%
                  arrange(ano, desc(receita))

#2
titulo_orc = imdb %>%
  select(titulo,orcamento) %>% arrange(desc(orcamento))

#3
filmes_pb = imdb %>%
  filter(cor == "Black and White")

#4
curtos_legais = imdb %>% 
    filter(duracao <= 90) %>%  filter(nota_imdb > 8.5)

#5
imdb_prejuizo = mutate(imdb,
              prejuizo = orcamento - receita) %>% filter(prejuizo < 0) %>% arrange(prejuizo)

#6
imdb_lucros = mutate(imdb,
                     lucro = receita - orcamento,
                     lucro_medio = mean(lucro, na.rm = TRUE),
                     lucro_relativo = (lucro - lucro_medio)/lucro_medio,
                     houve_lucro = ifelse(lucro > 0, "sim", "não")
                     )



medias_por_diretor = 
  imdb %>% group_by(diretor) %>% summarise(media = mean(nota_imdb))

media = left_join(imdb, medias_por_diretor, by="diretor")



