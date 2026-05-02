#==============================================================================
#
# Proporção de eleitores por habitantes nos municípios do Brasil
# Análise Exploratória
# Escopo: Brasil, 2010
#
# Autor: Marcus Vinicius Chevitarese Alves (mchevita@gmail.com)
#
#==============================================================================

#
# Preparação do ambiente
#

## Limpa memória
rm(list=ls())

## Desabilita notação científica para valores numéricos
options(scipen=999)

## Carrega bibliotecas necessárias
library(censobr)
library(geobr)
library(tidyverse)
library(dplyr)
library(stringi)
library(ggplot2)
library(flextable)
library(gdtools)
library(car)
library(report)
library(emmeans)

#
# Dados de população
#
# Fonte: IBGE, via pacotes {censobr} e {geobr}
#

## Carrega dados de população

### Lê arquivo com dados "brutos" do IBGE usando o pacote {censobr}
pop_br_inicial <- censobr::read_population(
  year = 2010,
  showProgress = FALSE
)
dplyr::glimpse(pop_br_inicial)

### Gera um dataset com a população agrupada por sigla da UF e código do 
### município
df_pop_mun <- pop_br_inicial |>
  compute() |>
  group_by(code_region, name_region, abbrev_state, code_muni) |>
  summarize(pop = sum(V0010) ) |> # V0010 = população
  collect()
dplyr::glimpse(df_pop_mun)

### Gera um dataset com o nome dos municípios a partir do arquivo 
### georreferenciado do pacote {censobr}
df_mun_geo <- geobr::read_municipality(
  year = 2010,
  showProgress = FALSE
)
dplyr::glimpse(df_mun_geo)

### Cruza dados dos dois datasets para obter o nome do município
### Obs.: bastaria fazer o cruzamento pela variável "code_muni";
### no entanto, usei também "abrev_state" para evitar duplicidade
### de variáveis.
df_pop_mun <- left_join(
  df_pop_mun, df_mun_geo, 
  by = join_by(
    abbrev_state == abbrev_state,
    code_muni == code_muni))
dplyr::glimpse(df_pop_mun)

#
# Dados do eleitorado
#
# Fonte: TSE
#

## Carga dos dados de eleitorado de 2010
url_eleitorado_2010 = "https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2010.zip"
downloader::download(url_eleitorado_2010, "arq_eleitorado_2010", mode = "wb")
unzip("arq_eleitorado_2010")
df_eleit_br_inicial <- read.csv(
  "perfil_eleitorado_2010.csv",
  fileEncoding = "ISO-8859-1",
  sep = ";")

## Agrupa dados iniciais por município
## (também normaliza nomes das variáveis)
df_eleit_br <- df_eleit_br_inicial |>
  select(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, QT_ELEITORES_PERFIL) |>
  rename(
    ano_eleicao = ANO_ELEICAO,
    sg_uf = SG_UF,
    cd_municipio_tse = CD_MUNICIPIO,
    nm_municipio_tse = NM_MUNICIPIO) |>
  group_by(ano_eleicao, sg_uf, cd_municipio_tse, nm_municipio_tse) |>
  summarise(qt_eleitores_mun = sum(QT_ELEITORES_PERFIL))
dplyr::glimpse(df_eleit_br)

#
# Análise dos dados
#

##
## Preparação dos dados para análise
##

### Uniformiza dados: converte nomes dos municípios do dataset de população
### para letras maiúsculas
df_pop_mun <- df_pop_mun |>
  mutate(name_muni = toupper(name_muni))

### Cruza dados de população com dados de municípios
df_pop_eleit_mun <- df_pop_mun |>
  inner_join(
    df_eleit_br, 
    by = join_by(
      abbrev_state == sg_uf,
      name_muni == nm_municipio_tse))

### Verifica se todos os municípios do dataset de população
### tem correspondência no dataset de eleitores
compara_datasets <- function(df_pop, df_eleit,
                             uf_pop, uf_eleit,
                             mun_pop, mun_eleit) {
  df_mun_nao_correspondentes <- tibble()
  if (nrow(df_pop) == nrow(df_eleit)) {
    print("Todos os municípios foram localizados.")
  } else {
    df_mun_nao_correspondentes <- df_pop %>%
      anti_join(df_eleit, 
                by = join_by({{uf_pop}} == {{uf_eleit}},
                             {{mun_pop}} == {{mun_eleit}}))
    cat(sprintf("Há %d municípios sem correspondência.", 
                nrow(df_mun_nao_correspondentes)))
  }
  return(df_mun_nao_correspondentes)
}

df_mun_nao_correspondentes <- compara_datasets(
  df_pop_mun, df_pop_eleit_mun,
  abbrev_state, abbrev_state,
  name_muni, name_muni)
df_mun_nao_correspondentes[, c("abbrev_state", "name_muni")]

### Faz a transliteração dos nomes dos municípios para o padrão Latin-ASCII
df_pop_mun <- df_pop_mun |> 
  mutate(nm_mun_ibge_tran = stri_trans_general(name_muni, "Latin-ASCII"))
df_eleit_br <- df_eleit_br |> 
  mutate(nm_mun_tse_tran = stri_trans_general(nm_municipio_tse, "Latin-ASCII"))

### Cruza dados de população com dados de eleitorado
### após a transliteração dos nomes dos municípios
df_pop_eleit_mun <- df_pop_mun |>
  inner_join(
    df_eleit_br, 
    by = join_by(
      abbrev_state == sg_uf,
      nm_mun_ibge_tran == nm_mun_tse_tran))
df_mun_nao_correspondentes <- compara_datasets(
  df_pop_mun, df_pop_eleit_mun,
  abbrev_state, abbrev_state,
  nm_mun_ibge_tran, nm_mun_ibge_tran)
df_mun_nao_correspondentes[, c("abbrev_state", "nm_mun_ibge_tran")]

### Calcula proporção de eleitores da população
df_pop_eleit_mun <- df_pop_eleit_mun |>
  mutate(pr_eleit = qt_eleitores_mun / pop)

### Enriquecimento dos dados de municípios (população vs. eleitorado)
df_pop_eleit_mun <- df_pop_eleit_mun |>
  mutate(tp_porte_mun = case_when(
    pop <= 20000 ~ "Pequeno Porte I",
    pop >= 20001 & pop <= 50000 ~ "Pequeno Porte II",
    pop >= 50001 & pop <= 100000 ~ "Médio Porte",
    pop >= 100001 ~ "Grande Porte")) |>
  mutate(tp_porte_mun = fct_reorder(tp_porte_mun,
                                    pop,
                                    .fun = "length"))

##
## Análise exploratória dos dados
##

### Resumo dos dados
summary(df_pop_eleit_mun$pr_eleit)

### Configura tabelas
set_flextable_defaults(
  font.size = 10,
  padding = 6)

### Estatísticas descritivas do número de municípios
### por porte do município (tabela 1)

#### Constrói tabela 1
tab_1 <- df_pop_eleit_mun |>
  group_by(tp_porte_mun) |>
  summarise(n_mun = as.numeric(n_distinct(code_muni)))
total_mun <- sum(tab_1$n_mun)

tab_1 <- tab_1 |>
  as_flextable() |>
  set_header_labels(
    tab_1, 
    tp_porte_mun = "Porte do município", 
    n_mun = "Nº de municípios")

tab_1 <- tab_1 |> 
  add_footer_row(
    values = c("Total", format(total_mun, big.mark = ".")),
    colwidths = c(1, 1)) %>%
    align(i = NULL, j = 2, align = "right", part = "footer")

tab_1 <- tab_1 |>
  colformat_num(
    j = c("n_mun"),
    decimal.mark = ",",    # separador de decimal
    big.mark = ".",    # separador de milhar
    digits = 0
  )

tab_1 <- tab_1 |>
  bg(bg = "white", part = "body")

tab_1 <- delete_rows(tab_1, i = 1, part = "header")
tab_1 <- delete_rows(tab_1, i = 2, part = "footer")
tab_1 <- set_table_properties(tab_1, layout = "autofit")

#### Visualiza tabela 1
tab_1

#### Exporta tabela 1 para formato PNG
tf <- tempfile(fileext = ".png")
save_as_image(x = tab_1, path = tf)
init_flextable_defaults()

### Visualização da distribuição da proporção de eleitores
### em relação ao número de habitantes
fig_1 <- ggplot(df_pop_eleit_mun,
            aes(x = tp_porte_mun, y = pr_eleit, fill = tp_porte_mun)) +
  geom_violin() +
  labs(
    x = "Porte do município",
    y = "Proporção eleitores/habitantes",
    fill = "Porte do município") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = 'gray'),
        panel.grid.minor.y = element_line(colour = 'lightgray'))
fig_1

### Constrói tabela com estatísticas descritivas da proporção de eleitores por 
### porte do município

#### Constrói tabela
tab_2 <- df_pop_eleit_mun |>
  group_by(tp_porte_mun) |>
  summarise(
    n_prop_acima_de_80 = n_distinct(code_muni[pr_eleit > 0.8]),
    n_prop_acima_de_90 = n_distinct(code_muni[pr_eleit > 0.9]),
    n_prop_acima_de_100 = n_distinct(code_muni[pr_eleit > 1.0])) |>
  as_flextable()
tab_2 <- set_header_labels(
  tab_2, 
  tp_porte_mun = "Porte do município", 
  n_prop_acima_de_80 = "Acima de 80% de proporção",
  n_prop_acima_de_90 = "Acima de 90% de proporção",
  n_prop_acima_de_100 = "Acima de 100% de proporção")
tab_2 <- delete_rows(tab_2, i = 1, part = "header")
tab_2 <- delete_rows(tab_2, part = "footer")
tab_2 <- set_table_properties(tab_2, layout = "autofit")

#### Visualiza tabela 2
tab_2

##
## Análise confirmatória
##

###
### Visualmente, há uma diferença entre os grupos no que
### se refere ao percentual de eleitores por habitante. Municípios de
### menor porte tendem a ter um percentual maior. Mas será que essa
### diferença é estatisticamente significativa? Para investigar essa hipótese, 
### iremos usar um teste do tipo ANOVA one-way (ou seja, com apenas um fator
### explicativo).
###
### No entanto, temos que tomar cuidado porque os grupos são **desbalanceados**,
### isto é, possuem tamanhos diferentes. De fato, há muito mais municípios
### de menor porte do que porte médio ou grande. Nesse caso, é necessário usar
### um teste ANOVA específico, com a soma de quadrados tipo II ou III. 
### Nesse trabalho, usaremos o tipo II (*).
###
### (*) Para entender mais sobre ANOVA e seus tipos específicos, consultar:
### https://nathanieldphillips-yarrr.share.connect.posit.cloud/anova.html
###
porte_municipio_modelo <- lm(pr_eleit ~ tp_porte_mun, data = df_pop_eleit_mun) 
porte_municipio_anova <- car::Anova(porte_municipio_modelo, type = 2)
report(porte_municipio_anova)

### O teste sugere que existe um efeito da variável tipo de porte do município,
### ou seja, existe uma variação significativa no valor do percentual de
### eleitores por habitante de acordo com o tipo de município.
### No entanto, o teste ANOVA, isoladamente, não aponta em qual dos grupos há a
### diferença, apenas que existe uma diferença. Precisamos então de uma análise
### à posteriori. No caso, vamos fazer uma comparação par a par, usando médias
### marginais estimadas (EMMs) e valor-p ajustado pelo método de Tukey. Para
### isso, vamos usar o pacote {emmeans}.

#### Comparação par a par por porte de município
options(contrasts = c("contr.sum", "contr.poly"))
porte_municipio_emms <- emmeans::emmeans(porte_municipio_modelo, 
                                   specs = pairwise ~ tp_porte_mun,
                                   type = "response")
porte_municipio_emms$contrasts

#### Esse resultado pode ser visto também de forma compacta e matricial,
#### por meio da função pwpm().
emmeans::pwpm(porte_municipio_emms)

### O teste indica que há uma diferença significativa entre todos os
### pares de grupos, exceto entre municípios de médio e grande porte.
### É possível visualizar essas diferenças por meio de um gráfico
### com as médias marginais estimadas e seus respectivos intervalos de 
### confiança.
### *Precisamos usar médias marginais estimadas porque se trata de grupos
### desbalanceados, conforme mencionado.

#### Visualização tabular das médias marginais estimadas e seus intervalos de 
#### confiança
porte_municipio_emms$emmeans

#### Visualização gráfica das médias marginais estimadas, intervalos de 
#### confiança e comparação entre grupos
####
#### As barras azuis são os intervalos de confiança para as EMMs, e as setas
#### vermelhas são para a comparação entre os grupos. Se uma seta de uma média
#### se sobrepor a uma seta de outro grupo, a diferença não é "significativa",
#### baseada nas configurações de ajuste (cujo padrão é "tukey") e o valor de
#### alfa (cujo padrão é 0,05).
#### Fonte: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html#pairwise
par(mar = c(5, 10, 4, 2) + 0.1) 
plot(porte_municipio_emms, comparisons = TRUE,
     las=1, cex.axis = 0.7,
     xlab = "Média marginal estimada",
     ylab = "Porte do município")

### Podemos ver que as setas associadas às médias marginais estimadas
### dos grupos Grande Porte e Médio Porte possuem uma sobreposição. Isso não 
### ocorre na comparação entre os demais grupos. Podemos ver ainda que o grupo 
### mais "separado" é o de menor porte (Pequeno Porte I), o que era esperado 
### devido aos achados da análise exploratória.

#### Outra visualização, com os p-valores comparativos:
emmeans::pwpp(porte_municipio_emms,
              xlab = "Valor-p ajustado pelo método de Tukey",
              ylab = "Porte do município")

#### Curiosidade: o cálculo do contraste é feito simplesmente fazendo a
#### a diferença entre as médias marginais estimadas. 
#### Exemplos:

##### Grande Porte x Médio Porte
GP_MP_contraste <- 0.6833 - 0.6907
GP_MP_contraste

##### Grande Porte x Pequeno Porte I
GP_PP1_contraste <- 0.6833 - 0.7911
GP_PP1_contraste

#### Se compararmos, esses são quase exatamente os valores mostrados na tabela
#### de comparação par a par, que mostra os contrastes (existe uma pequena
#### diferença por causa de arredondamento).

###
### **Conclusão**
###
### As evidências indicam diferenças significativas entre a proporção de
### eleitores de todos os pares de grupos, exceto entre os municípios de 
### médio e grande porte. Além disso, as maiores diferenças são entre os 
### municípios do grupo de menor porte e os do grupo de maior porte. O teste 
### considerou um nível de confiança de 95%.
###