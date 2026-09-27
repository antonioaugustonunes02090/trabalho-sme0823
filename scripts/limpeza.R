#Antonio Augusto Nunes de Souza NUSP 15440698
# Limpeza dos Dados e Criação da base de dados final
rm(list=ls(all=T))

library(arrow)
library(readr)
library(readxl)

df_muni <- read_excel("data\\raw\\indicadorAids_dados.xlsx") #variável resposta
df_cod <- read_excel('data\\raw\\indicadorAIDS_codigos.xlsx') #Variável auxiliar

ufs <- df_cod %>%
  dplyr::select( #Selecionando as seguintes colunas
    ibge_code = 'Código', #Código de Cada Município
    uf = 'Unidade da Federação (SIGLA)', #Estado
    regiao = 'Região' #Região
  )

hiv_2024 <- df_muni %>%
  dplyr::select( #Selecionando apenas as seguintes colunas
    ibge_code = "Código", #Código de Cada Município
    city_name = "Nome Município", #Nome do Município
    new_cases_2024 = 'Casos 2024' #Número de Novas Notificações/Casos de HIV
  ) %>% left_join(ufs, by = 'ibge_code') %>% #Unindo por Código de cada município (tabela com informação regional)
  mutate(ibge_code=as.character(ibge_code)) #Transformando o Código em String

hiv_2024 <- hiv_2024 %>%
  relocate(new_cases_2024, .after = last_col()) %>% #Colocando a variável resposta como ultima
  dplyr::select(code_ibge=ibge_code, #selecionando as colunas principais: codigo ibge, nome da cidade e
                city_name = city_name, #numero de casos de HIV
                new_cases_2024=new_cases_2024)

head(hiv_2024) 


pop_sidra=read_csv("data\\raw\\pop_data.csv") #Dados de totais populacionais

pop_clean <- pop_sidra %>%
  dplyr::select( #Selecionando as colunas 
    code_ibge_7 = `Município (Código)`, #Codigo IBGE com digito de validação
    pop_total = Valor  #Quantidade total da população da cidade
  ) %>%
  mutate(code_ibge = substr(code_ibge_7, 1, 6)) %>% #Retirando o digito de validação do código do IBGE
  dplyr::select(code_ibge=`code_ibge`, #Selecionando apenas o novo código IBGE e os totais
                pop_total=`pop_total`)

head(pop_clean)


cnes = read_parquet("data/raw/cnes_data.parquet") #Dados de unidades de saúde CTA e SAE

# Contagem de unidades de saúde especializadaas (CTA/SAE) e unidades primárias por cidade
#SAE: Serviço de Atenção Especializada
#CTA: Centro de Testagem e Acolhimento
health_units <- cnes %>%
  group_by(CODUFMUN) %>% #Selecionando por cidade
  summarise(
    total_health_units = n(), #total de unidades de saúde de cada cidade
    # 2. Contagem de clínicas especializadas (CTA/Policlínicas, tipo 36)
    specialized_clinics = sum(TP_UNID == "36", na.rm = TRUE),
  ) %>%
  rename(code_ibge = CODUFMUN) %>% #Renomeando para code_ibge
  mutate(has_specialized_clinics=ifelse(specialized_clinics>0,1,0)) %>% #adicionando a indicadora de clínicas especializadas
  mutate(propor_specialized_clinics=has_specialized_clinics/total_health_units) %>% #adicionando a proporção de clinicas especializadas por total de unidades de saúde
  dplyr::select(
    code_ibge=code_ibge, #Selecionando as colunas de interesse: codigo ibge, proporção, total de clinicas especilizadas e
    total_health_units = total_health_units, #o total de clinicas
    propor_specialized_clinics = propor_specialized_clinics,
    has_specialized_clinics = has_specialized_clinics
  )

head(health_units)

lit_rate_raw = read_csv("data\\raw\\literacy_data.csv")

lit_rate = lit_rate_raw %>%
  dplyr::select(code_ibge=`Município (Código)`, #selecionando as colunas de interesse: codigo do municipio
                lit_ratio=`Valor`) %>% #porcentagem x 100 de pessoas alfabetizadas
  mutate(lit_ratio = lit_ratio/100, #mudando a porcentagem para a escala entre 0-1
         code_ibge = as.character(code_ibge), #mudando o codigo ibge para string
         code_ibge = substr(code_ibge, 1, 6)) #retirando o digito de validação do codigo

head(lit_rate)


cad_unico_raw <- read.csv("data\\raw\\cadunico_data.csv") #Variável Cadastros no CadUnico


cad_unc <- cad_unico_raw %>%
  group_by(codigo_ibge) %>% #grupando por cidade
  summarise(
    avg_cadun_registers = mean(cadun_qtd_pessoas_cadastradas_i) #calculando a média anual de pessoas cadastradas
  ) %>% #para aliviar o sobrecadastramento
  mutate(
    code_ibge = as.character(codigo_ibge), #Mudando o code_ibge para string
    code_ibge = substr(code_ibge, 1, 6) #Retirando o digito de validação do código ibge
  ) %>%
  left_join(pop_clean, by = "code_ibge") %>% #Utilizando o total populacional
  mutate(
    cadun_ratio = pmin(avg_cadun_registers / pop_total,1.0) #calculando a proporção de registros no cadunico
  ) %>% # setando o maxímo para 1
  dplyr::select(code_ibge, cadun_ratio) #selecionando apenas as colunas de interesse: codigo ibge e a razão de registros no CadUnico

#cat(sum(cad_unc[,2]==1)) >>>58 (58 cidades com todos residentes com cadunico ou sobrecadastramento)



aps_coverage_raw <- read_xlsx("data\\raw\\cobertura-aps.xlsx") #Variável Cobertura APS (Atenção Primária a Saúde)


aps_cover = aps_coverage_raw %>%
  dplyr::select(
    code_ibge=`Código IBGE`, #selecionando apenas as colunas de interesse: codigo ibge e
    aps_coverage = `Cobertura APS`#a cobertura potencial do APS (atenção primária da saúde)
  )

pib_raw = read_excel('data\\raw\\pib_per_municipality.xlsx') #Variável PIB per Capita (Municipio)

pib = pib_raw %>%
  mutate(year = as.numeric(Ano), #Transformar ano em numero
         code_ibge = as.character(`Código do Município`), #transformar codigo ibge em string
         code_ibge = substr(code_ibge,1,6))%>% #retirando o digito de validação do codigo ibge
  group_by(code_ibge) %>% #agrupando por cidade
  slice_max(order_by = year, n=1, with_ties = F) %>% #pegando informação do maior ano
  ungroup() %>%
  select(code_ibge=code_ibge, #selecionando apenas as variaveis de interesse
         pib_per_capita = `Produto Interno Bruto per capita, \r\na preços correntes\r\n(R$ 1,00)`)

avg_study_raw = read_excel('data\\raw\\average_years_of_study.xlsx') #Variável Número médio de anos de estudo

avg_study = avg_study_raw %>% 
  mutate(code_ibge=as.character(Codigo), #transformando codigo ibge em string
         code_ibge=substr(code_ibge,1,6)) %>% #retirando o digito de validação do código ibge
  select(code_ibge=code_ibge, #selecionando as variáveis de interesse
         avg_years_study=`Anos de Estudo Médio`)


people_in_bf_raw = read.csv('data\\raw\\people_in_bolsa_familia.csv',fileEncoding = 'latin1') #Número de pessoas inscritas no BF

bf_people_data=people_in_bf_raw %>% 
  mutate(ano=as.Date(paste0('01/',Referência),format="%d/%m/%Y"), #transformar em objeto data
         code_ibge=as.character(Código)) %>% #transformar codigo ibge em string
  group_by(Código) %>%
  slice_max(order_by = ano, n = 1, with_ties = FALSE) %>% #selecionar a data mais atual (até 20244)
  ungroup() %>%
  select(code_ibge = code_ibge,
         bf_people = `Pessoas.beneficiárias.no.PBF..a.partir.de.Mar.2023.`) #selecionando as colunas de interesse


df = hiv_2024 %>% #Juntando todos os dados por código ibge (cidade)
  left_join(pop_clean,by='code_ibge') %>%
  left_join(health_units,by='code_ibge') %>%
  left_join(lit_rate,by='code_ibge') %>%
  left_join(cad_unc,by='code_ibge') %>%
  left_join(aps_cover,by='code_ibge') %>%
  left_join(pib,by='code_ibge') %>%
  left_join(avg_study,by='code_ibge')%>%
  left_join(bf_people_data,by='code_ibge') %>%
  drop_na() #Retirando as cidades que não possuem todas informações

#write_csv(df,"data\\processed\\processed_df.csv") exportando os dados processados

#dim(df)  >>> 5568 13  | 5568 cidades 
