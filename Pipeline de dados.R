# No meu repositório (Estudo_linguagem_R) eu descrevo como instalar os pacotes basicos e preparar o ambiente para fazer analises.

# Carreguei pacotes essenciais

library(dplyr)
library(ggplot2)
library(tidyverse)
library(skimr)
library(askpass)
library(DescTools)
library(microdatasus)
library(readr)


install.packages("remotes")  # se ainda não tiver
remotes::install_github("rfsaldanha/microdatasus") # se ainda não tiver


# A EXTRAÇÃO E TRANSFORMAÇÃO DOS DADOS - esta descrito nos arquivos ETL_sobre_os_dados e ETL_db_comorbidades

#Importe o CSV
library(readr)
db_geral_covid_2025 <- read_csv2("C:/Users/miche/Documents/Projetos GitHub/Casos_COVID10_geral_2025/db_geral_covid_2025.csv")
View(db_geral_covid_2025)

#Explorando os dados
View(db_geral_covid_2025)
str(db_geral_covid_2025)
head(db_geral_covid_2025)
dim(db_geral_covid_2025)
summary(db_geral_covid_2025)
glimpse(db_geral_covid_2025)
skim(db_geral_covid_2025)

#Limpeza e transformação dados - Não foi necessario, pela analise exploratorio foi visto que os dados estão corretos

#Análises Descritivas

Desc(db_geral_covid_2025$faixa_etaria)
Desc(db_geral_covid_2025$estadoIBGE)
Desc(db_geral_covid_2025$municipio)

db <- db_geral_covid_2025 |>
  mutate(casos = as.numeric(casos))

# 1. Total geral, média e mediana de casos por linha

total_geral <- sum(db$casos, na.rm = TRUE)
cat("Total geral de casos:", total_geral, "\n")

print("Dimensões do dataset:")
print(dim(db))
print("Colunas:")
print(names(db))
print("Total de casos:")
total_casos <- sum(db$casos)
print(total_casos)

# 2. Total e percentual de casos por estado

tab_estado <- db |>
  group_by(estadoIBGE) |>
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
  mutate(
    perc = total_casos / sum(total_casos)
  ) |>
  arrange(desc(total_casos))

cat("=== Total e percentual de casos por estado ===\n")
print(tab_estado)
cat("\n")

# 3. Top 10 municípios com mais casos (total, percentual, média, mediana)

tab_mun <- db |>
  group_by(municipio) |>
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
  mutate(
    perc = total_casos / sum(total_casos)
  ) |>
  arrange(desc(total_casos))

top10_mun <- tab_mun |>
  slice_max(order_by = total_casos, n = 10)

cat("=== Top 10 municípios com mais casos ===\n")
print(top10_mun)
cat("\n")


# 4. Total e percentual por faixa etária

tab_faixa <- db |>
  group_by(faixa_etaria) |>
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
  mutate(
    perc = total_casos / sum(total_casos)
  ) |>
  arrange(desc(total_casos))

cat("=== Total e percentual de casos por faixa etária ===\n")
print(tab_faixa)
cat("\n")

# 5. Total de casos por estado em idosos


df_idosos <- db %>% filter(faixa_etaria == "70 anos e mais")

total_casos_idosos <- sum(df_idosos$casos)
print(paste("Total de casos para 70 anos e mais (nacional):", total_casos_idosos))


print("=== TOTAL DE CASOS POR ESTADO - FAIXA ETÁRIA 70 ANOS E MAIS ===")

#Por estado

analise_estado_idosos <- df_idosos %>%
  group_by(estadoIBGE) %>%
  summarise(
    total_casos = sum(casos),
    n_pacientes = n(),
    media_casos_por_paciente = round(mean(casos), 2),
    .groups = 'drop'
  ) %>%
  mutate(
    perc_nacional = round(total_casos / total_casos_idosos * 100, 2),
    perc_pacientes = round(n_pacientes / nrow(df_idosos) * 100, 2)
  ) %>%
  arrange(desc(total_casos))

print(analise_estado_idosos)

# GRÁFICOS

# 1. Estados com maior número total de casos

# total de casos por estado
casos_estado <- db_geral_covid_2025 |>
  group_by(estadoIBGE) |>
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop")

ggplot(casos_estado,
       aes(x = reorder(estadoIBGE, total_casos),
           y = total_casos)) +
  geom_col(fill = "#1f78b4") +
  coord_flip() +
  labs(
    title = "Número total de casos de COVID-19 por estado (menor para maior)",
    x = "Estado",
    y = "Total de casos"
  ) +
  theme_minimal()

print(analise_estado_idosos)


# Top 5 estados com mais casos em idosos
print("=== TOP 5 ESTADOS COM MAIS CASOS (70+ ANOS) ===")
print(head(analise_estado_idosos, 5))

# Gráfico simples no console (opcional)
barplot(analise_estado_idosos$total_casos[1:10], 
        names.arg = analise_estado_idosos$estadoIBGE[1:10],
        main = "Top 10 Estados - Casos COVID 70+ Anos",
        las = 2, cex.names = 0.8)

# 2. Municipios com mais casos, no Brasil.

#Gráfico de barras com o “top município” de cada estado
mun_top <- db_geral_covid_2025 |>
  group_by(estadoIBGE, municipio) |>
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
  group_by(estadoIBGE) |>
  slice_max(order_by = total_casos, n = 1, with_ties = FALSE) |>
  ungroup()

head(mun_top)

ggplot(mun_top,
       aes(x = reorder(estadoIBGE, total_casos),
           y = total_casos,
           fill = municipio)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Municípios com maior número de casos em cada estado",
    x = "Estado",
    y = "Total de casos",
    fill = "Município"
  ) +
  theme_minimal()

#3. Municípios por estado (top 3)

mun_top3 <- db_geral_covid_2025 |>
  group_by(estadoIBGE, municipio) |>
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop") |>
  group_by(estadoIBGE) |>
  slice_max(order_by = total_casos, n = 3, with_ties = FALSE) |>
  ungroup()


ggplot(mun_top3,
       aes(x = reorder(municipio, total_casos),
           y = total_casos,
           fill = estadoIBGE)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ estadoIBGE, scales = "free_y") +
  labs(
    title = "Top 3 municípios em número de casos por estado",
    x = "Município",
    y = "Total de casos",
    fill = "Estado"
  ) +
  theme_minimal()

# 4. Analise de faixa etária por estado e BRASIL

#Gráfico de barras com total de casos por faixa etaria no Brasil
casos_br_faixa <- db_geral_covid_2025 |>
  group_by(faixa_etaria) |>
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop")

ggplot(casos_br_faixa,
       aes(x = faixa_etaria,
           y = total_casos)) +
  geom_col(fill = "#1f78b4") +
  labs(
    title = "Total de casos de COVID-19 por faixa etária no Brasil (2025)",
    x = "Faixa etária",
    y = "Total de casos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Distribuição etária por estado:
distri_estado <- db_geral_covid_2025 %>%
  group_by(estadoIBGE, faixa_etaria) %>%
  summarise(total_casos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
  group_by(estadoIBGE) %>%
  mutate(prop = total_casos / sum(total_casos))

ggplot(distri_estado,
       aes(x = faixa_etaria,
           y = prop,
           fill = faixa_etaria)) +
  geom_col() +
  facet_wrap(~ estadoIBGE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribuição proporcional de casos por faixa etária, por estado",
    x = "Faixa etária",
    y = "Proporção de casos"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# 6. Casos em idosos (≥ 70 anos) por estado e município
idosos_estado <- db_geral_covid_2025 %>%
  filter(faixa_etaria == "70 anos e mais") %>%
  group_by(estadoIBGE) %>%
  summarise(total_idosos = sum(casos, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_idosos))

ggplot(idosos_estado,
       aes(x = reorder(estadoIBGE, total_idosos), y = total_idosos)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(x = "Estado", y = "Casos (70+)",
       title = "Casos de COVID-19 em idosos (70+) por estado") +
  theme_minimal()
ggsave("fig_idosos_por_estado.png", width = 8, height = 6)



# supondo um data frame chamado minha_tabela
total_linhas <- nrow(db_geral_covid_2025)

cat("Total de linhas da tabela:", total_linhas, "\n")
# ou simplesmente:
nrow(db_geral_covid_2025)
