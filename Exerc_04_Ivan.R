# Exercícios aula 04
install.packages("tidyverse")
install.packages("lubridate")
install.packages("stringr")

library(tidyverse)
library(lubridate)
library(stringr)

# Carregue o arquivo `decisoes.rds` em um objeto chamado `decisoes`. ----
decisoes <- readRDS("C:/Users/aluno.ENAP/Downloads/decisoes.rds")

# Crie um objeto contendo o tempo m?dio entre decis?o e registro por juiz, apenas para processos relacionados a drogas nos munic?pios de Campinas ou Limeira. ----
## Obs.: a nova "singularidade" da base de dados ser? o `juiz`. Na base original, a singularidade era o `processo`

juiz_drogas_CL <- decisoes %>% 
  select(juiz, municipio, txt_decisao, data_registro, data_decisao) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao, "droga|entorpecente|coca[?i]na|maconha"),
         tempo = dmy(data_decisao) - dmy(data_registro)) %>%
  filter(droga == TRUE, municipio %in% c("Campinas", "Limeira")) %>%
  group_by(juiz) %>%
  summarise(tempo_medio = mean(tempo, na.rm = T))

# Salve o objeto resultante em um arquivo chamado `juizes_drogas_CL.rds` ----

write_rds(juiz_drogas_CL, "C:/Users/aluno.ENAP/Downloads/Juiz_drogas")

# Faça commit e push do script e do arquivo `.rds` ----
git config --global user.email 

#Usando o gather

dec_gather <- decisoes %>%
  filter(!is.na(id_decisao)) %>%
  select(id_decisao:data_registro) %>%
  gather(key="variavel", value="valor",-id_decisao) %>%
  arrange(id_decisao)

# Qual juiz julga a maior proporção de processos que tratam de drogas ----


# Crie um objeto contendo informações sobre os tamanhos das bancadas dos ----
# partidos (arquivo `bancadas.rds`), suas respectivas coligações 
# eleitorais para 2018 (arquivo `coligacoes.xlsx`) e o 
# grau de concordância com a agenda do Gov 
# Temer (arquivo `governismo_temer.xlsx`). 

# Bônus: use `group_by` e `summarise` para identificar qual candidato tem a ----
# coligação com menor média de concordância e qual candidato 
# tem a maior proporção total de assentos.