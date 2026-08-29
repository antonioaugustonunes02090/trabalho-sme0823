#Antonio Augusto Nunes de Souza NUSP 15440698
# Download e exportação das bases de dados utilizadas no trabalho
rm(list=ls(all=T)) 
library(renv)

renv::init()
library(arrow) #Exportar dados parquet 
library(readxl) #Ler arquivos excel 
library(skimr) #Visualização e descrição de dataframes
library(microdatasus) #Dados do SUS
library(dplyr) # Operações com DataFrames
library(sidrar) # Dados do IBGE
library(readr) # Exportar dados csv
library(writexl) #Exportar dados xlsx
library(tidyr) # 
renv::snapshot()




setwd("C:\\Users\\follo\\OneDrive\\Documents\\Big Brain\\6th Semester\\GLM\\Trabalho\\repository\\trabalho-sme0823")

df_muni <- read_excel("data\\raw\\indicadorAids_dados.xlsx") #variável resposta
df_cod <- read_excel('data\\raw\\indicadorAIDS_codigos.xlsx') #variável auxiliar


pop_sidra <- get_sidra( #totais populacional
  x = 4714,         # ID do censo 2022
  variable = 93,    # variável população residente
  period = "2022",  # Período
  geo = "City"      # Por nível municipal
)
#pop_data = write_csv(pop_sidra,"data\\raw\\pop_data.csv") exportando os dados de totais populacionais

cnes <- fetch_datasus( #Variável Unidades CTA/SAE
  year_start = 2024, month_start = 12, #Quantidade no final do ano de 2024
  year_end = 2024, month_end = 12, #Quantidade no final do ano de 2024
  uf = "all", information_system = "CNES-ST" #
)

#write_parquet(cnes,"data/raw/cnes_data.parquet") exportando os dados de unidades CTA/SAE

lit_rate_raw <- get_sidra(api = "/t/9543/n6/all/v/all/p/2022") #Variável de Contagem de Alfabetizados

#write_csv(lit_rate_raw,'data/raw/literacy_data.csv') exportando os dados de alfabetizados

cad_unico_raw <- read.csv("Raw Data\\cadunico_data.csv") #Variável Cadastros no CadUnico

aps_coverage_raw <- read_xlsx("Raw Data\\cobertura-aps.xlsx") #Variável Cobertura APS (Atenção Primária a Saúde)
