require(AER)
require(plm)
require(janitor)
library(esquisse)
library(ggplot2)
library(geobr)
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(rio)
library(readxl)
library(maps)
library(mapdata)
library(ggpubr)
library(ggspatial)
library(sandwich)

IGP_DI<-IGP_DI|>janitor::clean_names()
dados_precos_fisicos_cepea<-dados_precos_fisicos_cepea|>janitor::clean_names()
igpdi<- IGP_DI |>
  dplyr::mutate(mes_ano=as.factor(mes_ano))
base1<- dados_precos_fisicos_cepea|>
  dplyr::mutate(mes_ano=as.factor(mes_ano))
base1<-base1%>%inner_join(igpdi, by=c('mes_ano'='mes_ano'))

base1$vd_bezerro_ims<-base1$indicador_cepea_bezerro_ms*base1$multiplicador
base1$vd_bgordo_bolsa<-base1$valor_boi_gordo_bolsa*base1$multiplicador
base1$vd_bmagro_spot<-base1$valor_boi_magro_spot*base1$multiplicador
base1$vd_milho_bolsa<-base1$valor_milho_bolsa*base1$multiplicador

base1$vd_bgordo_sp<-base1$boi_gordo_sp*base1$multiplicador
base1$vd_bmagro_sp<-base1$boi_magro_sp*base1$multiplicador
base1$vd_milho_sp<-base1$milho_sp*base1$multiplicador
base1$vd_bezerro_sp<-base1$bezerro_sp*base1$multiplicador

base1$vd_bgordo_ms<-base1$boi_gordo_ms*base1$multiplicador
base1$vd_bmagro_ms<-base1$boi_magro_ms*base1$multiplicador
base1$vd_milho_ms<-base1$milho_ms*base1$multiplicador
base1$vd_bezerro_ms<-base1$bezerro_ms*base1$multiplicador


#dados bolsa/seguro de margem
vd_bezerro_sp<-ts(base1$vd_bezerro_sp, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_bezerro_ims<-ts(base1$vd_bezerro_ims, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_bezerro_ms<-ts(base1$vd_bezerro_ms, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_bgordo_bolsa<-ts(base1$vd_bgordo_bolsa, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_milho_bolsa<-ts(base1$vd_milho_bolsa, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_bmagro_spot<-ts(base1$vd_bmagro_spot, start=c(2014,1), end=c(2023,12), frequency = 12)

#dados SP mercado spot
vd_bgordo_sp<-ts(base1$vd_bgordo_sp, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_milho_sp<-ts(base1$vd_milho_sp, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_bmagro_sp<-ts(base1$vd_bmagro_sp, start=c(2014,1), end=c(2023,12), frequency = 12)

vd_bgordo_ms<-ts(base1$vd_bgordo_ms, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_milho_ms<-ts(base1$vd_milho_ms, start=c(2014,1), end=c(2023,12), frequency = 12)
vd_bmagro_ms<-ts(base1$vd_bmagro_ms, start=c(2014,1), end=c(2023,12), frequency = 12)

##Variaçao da base boi gordo SP
base_bgordo_sp<-vd_bgordo_sp-vd_bgordo_bolsa
mean(base_bgordo_sp, na.rm = T)
var(base_bgordo_sp, na.rm = T)
sd(base_bgordo_sp, na.rm = T)

summary(base_bgordo_sp)

base_bgordo_sp
# Converter para data.frame e criar coluna de ano
base_bgordo_sp <- data.frame(
  ano = floor(time(base_bgordo_sp)), # Extrai o ano
  variacao_base = as.numeric(base_bgordo_sp) # Converte a série ts para numérica
)

# Calcular o coeficiente de variação (CV) por ano
library(dplyr)
var_por_ano_boi <- base_bgordo_sp %>%
  group_by(ano) %>%
  summarise(
    media = mean(variacao_base, na.rm = TRUE),
    var =  var(variacao_base, na.rm = TRUE),
    desvio_padrao = sd(variacao_base, na.rm = TRUE)
  )
write.csv(var_por_ano_boi, "var_por_ano_boi.csv")

library(dplyr)
library(lubridate)

# Extrair ano e mês da série temporal
base_bgordo_sp <- data.frame(
  data = as.Date(time(base_bgordo_sp)), # Converte índice da série temporal para data
  variacao_base = as.numeric(base_bgordo_sp)
) %>%
  mutate(
    ano = year(data),
    mes = month(data)
  )

# Calcular estatísticas por mês (ano + mês)
var_por_mes_boi <- base_bgordo_sp %>%
  group_by(mes) %>%
  summarise(
    media = mean(variacao_base, na.rm = TRUE),
    variancia = var(variacao_base, na.rm = TRUE),
    desvio_padrao = sd(variacao_base, na.rm = TRUE),
    coef_var = ifelse(media != 0, (desvio_padrao / media) * 100, NA_real_)
  )

library(ggplot2)

# Criando um data frame com os preços spot e futuros
df <- data.frame(
  Tempo = time(vd_bgordo_sp),  # Assume que 'precos_spot' e 'precos_futuros' são séries temporais
  Preco_Spot = as.numeric(vd_bgordo_sp),
  Preco_Futuro = as.numeric(vd_bgordo_bolsa)
)

# Plotando com ggplot
ggplot(df, aes(x = Tempo)) +
  geom_line(aes(y = Preco_Spot, color = "Preço Spot"), size = 1) +  # Linha dos preços spot
  geom_line(aes(y = Preco_Futuro, color = "Preço Futuro"), size = 1, linetype = "dashed") +  # Linha dos preços futuros
  labs( x = "Tempo", y = "Preço") +
  scale_color_manual(values = c("Preço Spot" = "blue", "Preço Futuro" = "red")) +  # Definindo cores
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remover título da legenda
        legend.position = "bottom",  # Posicionando a legenda no topo
        axis.line = element_line(color = "black"),  # Garantir linhas dos eixos
        axis.ticks = element_line(color = "black"),  # Garantir os ticks dos eixos
        axis.text = element_text(color = "black"))  # Garantir que o texto dos eixos seja visível

##Variaçao da base boi gordo MS
base_bgordo_ms<-vd_bgordo_ms-vd_bgordo_bolsa
mean(base_bgordo_ms, na.rm = T)
var(base_bgordo_ms, na.rm = T)
sd(base_bgordo_ms, na.rm = T)

summary(base_bgordo_ms)

# Converter para data.frame e criar coluna de ano
base_bgordo_ms <- data.frame(
  ano = floor(time(base_bgordo_ms)), # Extrai o ano
  variacao_base = as.numeric(base_bgordo_ms) # Converte a série ts para numérica
)

# Calcular o coeficiente de variação (CV) por ano
library(dplyr)
var_por_ano_boi_ms <- base_bgordo_ms %>%
  group_by(ano) %>%
  summarise(
    media = mean(variacao_base, na.rm = TRUE),
    var =  var(variacao_base, na.rm = TRUE),
    desvio_padrao = sd(variacao_base, na.rm = TRUE)
  )
write.csv(var_por_ano_boi_ms, "var_por_ano_boi_ms.csv")

library(ggplot2)

# Criando um data frame com os preços spot e futuros
df <- data.frame(
  Tempo = time(vd_bgordo_ms),  # Assume que 'precos_spot' e 'precos_futuros' são séries temporais
  Preco_Spot_MS = as.numeric(vd_bgordo_ms),
  Preco_Spot_SP = as.numeric(vd_bgordo_sp),
  Preco_Futuro = as.numeric(vd_bgordo_bolsa)
)

# Plotando com ggplot
g1<-ggplot(df, aes(x = Tempo)) +
  geom_line(aes(y = Preco_Spot_MS, color = "Preço Spot Mato Grosso do Sul"), size = 1) +  # Linha dos preços spot
  geom_line(aes(y = Preco_Spot_SP, color = "Preço Spot São Paulo"), size = 1) +
  geom_line(aes(y = Preco_Futuro, color = "Preço Futuro"), size = 1, linetype = "dashed") +  # Linha dos preços futuros
  labs( x = "Tempo", y = "Preço") +
  scale_color_manual(values = c("Preço Spot Mato Grosso do Sul" = "blue", "Preço Spot São Paulo" = "darkgreen", "Preço Futuro" = "red")) +  # Definindo cores
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remover título da legenda
        legend.position = "bottom",  # Posicionando a legenda no topo
        axis.line = element_line(color = "black"),  # Garantir linhas dos eixos
        axis.ticks = element_line(color = "black"),  # Garantir os ticks dos eixos
        axis.text = element_text(color = "black"))  # Garantir que o texto dos eixos seja visível
ggsave("g1.svg", plot = g1, width = 16, height = 9, dpi=300)



##Variaçao da base milho SP
base_milho_sp<-vd_milho_sp-vd_milho_bolsa
mean(base_milho_sp)
var(base_milho_sp)
sd(base_milho_sp)

summary(base_milho_sp)

base_milho_sp <- data.frame(
  ano = floor(time(base_milho_sp)), # Extrai o ano
  variacao_base = as.numeric(base_milho_sp) # Converte a série ts para numérica
)

# Calcular o coeficiente de variação (CV) por ano
var_por_ano_milho <- base_milho_sp %>%
  group_by(ano) %>%
  summarise(
    media = mean(variacao_base, na.rm = TRUE),
    var = var(variacao_base, na.rm = TRUE),
    desvio_padrao = sd(variacao_base, na.rm = TRUE)
  )

write.csv(var_por_ano_milho, "var_por_ano_milho.csv")

# Criando um data frame com os preços spot e futuros
df <- data.frame(
  Tempo = time(vd_milho_sp),  # Assume que 'precos_spot' e 'precos_futuros' são séries temporais
  Preco_Spot = as.numeric(vd_milho_sp),
  Preco_Futuro = as.numeric(vd_milho_bolsa)
)

# Plotando com ggplot
ggplot(df, aes(x = Tempo)) +
  geom_line(aes(y = Preco_Spot, color = "Preço Spot"), size = 1) +  # Linha dos preços spot
  geom_line(aes(y = Preco_Futuro, color = "Preço Futuro"), size = 1, linetype = "dashed") +  # Linha dos preços futuros
  labs( x = "Tempo", y = "Preço") +
  scale_color_manual(values = c("Preço Spot" = "blue", "Preço Futuro" = "red")) +  # Definindo cores
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remover título da legenda
        legend.position = "bottom",  # Posicionando a legenda no topo
        axis.line = element_line(color = "black"),  # Garantir linhas dos eixos
        axis.ticks = element_line(color = "black"),  # Garantir os ticks dos eixos
        axis.text = element_text(color = "black"))  # Garantir que o texto dos eixos seja visível


##Variaçao da base milho MS
base_milho_ms<-vd_milho_ms-vd_milho_bolsa
mean(base_milho_ms)
var(base_milho_ms)
sd(base_milho_ms)

summary(base_milho_ms)

base_milho_ms <- data.frame(
  ano = floor(time(base_milho_ms)), # Extrai o ano
  variacao_base = as.numeric(base_milho_ms) # Converte a série ts para numérica
)

# Calcular o coeficiente de variação (CV) por ano
var_por_ano_milho_ms <- base_milho_ms %>%
  group_by(ano) %>%
  summarise(
    media = mean(variacao_base, na.rm = TRUE),
    var = var(variacao_base, na.rm = TRUE),
    desvio_padrao = sd(variacao_base, na.rm = TRUE)
  )

write.csv(var_por_ano_milho_ms, "var_por_ano_milho_ms.csv")

# Criando um data frame com os preços spot e futuros
df <- data.frame(
  Tempo = time(vd_milho_ms),  # Assume que 'precos_spot' e 'precos_futuros' são séries temporais
  Preco_Spot_MS = as.numeric(vd_milho_ms),
  Preco_Spot_SP = as.numeric(vd_milho_sp),
  Preco_Futuro = as.numeric(vd_milho_bolsa)
)

# Plotando com ggplot
g2<-ggplot(df, aes(x = Tempo)) +
  geom_line(aes(y = Preco_Spot_MS, color = "Preço Spot Mato Grosso do Sul"), size = 1) +  # Linha dos preços spot
  geom_line(aes(y = Preco_Spot_SP, color = "Preço Spot São Paulo"), size = 1) +  # Linha dos preços spot
  geom_line(aes(y = Preco_Futuro, color = "Preço Futuro"), size = 1, linetype = "dashed") +  # Linha dos preços futuros
  labs( x = "Tempo", y = "Preço") +
  scale_color_manual(values = c("Preço Spot Mato Grosso do Sul" = "blue", "Preço Spot São Paulo" = "darkgreen", "Preço Futuro" = "red")) +  # Definindo cores
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remover título da legenda
        legend.position = "bottom",  # Posicionando a legenda no topo
        axis.line = element_line(color = "black"),  # Garantir linhas dos eixos
        axis.ticks = element_line(color = "black"),  # Garantir os ticks dos eixos
        axis.text = element_text(color = "black"))  # Garantir que o texto dos eixos seja visível
ggsave("g2.svg", plot = g2, width = 16, height = 9, dpi=300)


##Variaçao da base boi magro SP
modelo_proxy_boi_magro_sp <- lm(vd_bmagro_spot ~ log(vd_bezerro_sp) + vd_milho_bolsa)
options(scipen=9999)
summary(modelo_proxy_boi_magro_sp)
shapiro.test(modelo_proxy_boi_magro_sp$residuals)
hist(modelo_proxy_boi_magro_sp$residuals, main = "Histograma dos Resíduos", xlab = "Resíduos")
qqnorm(modelo_proxy_boi_magro_sp$residuals)
qqline(modelo_proxy_boi_magro_sp$residuals)

library(lmtest)
bptest(modelo_proxy_boi_magro_sp)
dwtest(modelo_proxy_boi_magro_sp)

intercepto_sp <- coef(modelo_proxy_boi_magro_sp)[1]
coef_bezerro_sp <- coef(modelo_proxy_boi_magro_sp)[2]
coef_milho_sp <- coef(modelo_proxy_boi_magro_sp)[3]

proxy_boi_magro_previsto_sp <- intercepto_sp + coef_bezerro_sp*log(vd_bezerro_sp)+ coef_milho_sp*vd_milho_bolsa


base_bmagro_sp<-vd_bmagro_sp-proxy_boi_magro_previsto_sp
mean(base_bmagro_sp)
var(base_bmagro_sp)
sd(base_bmagro_sp)
summary(base_bmagro_sp)

base_bmagro_sp <- data.frame(
  ano = floor(time(base_bmagro_sp)), # Extrai o ano
  variacao_base = as.numeric(base_bmagro_sp) # Converte a série ts para numérica
)

# Calcular o coeficiente de variação (CV) por ano
library(dplyr)
var_por_ano_bmagro <- base_bmagro_sp %>%
  group_by(ano) %>%
  summarise(
    media = mean(variacao_base, na.rm = TRUE),
    var = var(variacao_base, na.rm = TRUE),
    desvio_padrao = sd(variacao_base, na.rm = TRUE)
  )
var_por_ano_bmagro
write.csv(var_por_ano_bmagro, "var_por_ano_bmagro.csv")

# Criando um data frame com os preços spot e futuros
df_sp <- data.frame(
  Tempo = time(vd_bmagro_sp),  # Assume que 'precos_spot' e 'precos_futuros' são séries temporais
  Preco_Spot_sp = as.numeric(vd_bmagro_sp),
  Preco_Futuro_sp = as.numeric(proxy_boi_magro_previsto_sp)
)

# Plotando com ggplot
g3a<-ggplot(df_sp, aes(x = Tempo)) +
  geom_line(aes(y = Preco_Spot_sp, color = "Preço Spot São Paulo"), size = 1) +  # Linha dos preços spot
  geom_line(aes(y = Preco_Futuro_sp, color = "Proxy Preço Futuro São Paulo"), size = 1, linetype = "dashed") +  # Linha dos preços futuros
  labs( x = "Tempo", y = "Preço") +
  scale_color_manual(values = c("Preço Spot São Paulo" = "blue", "Proxy Preço Futuro São Paulo" = "red")) +  # Definindo cores
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remover título da legenda
        legend.position = "bottom",  # Posicionando a legenda no topo
        axis.line = element_line(color = "black"),  # Garantir linhas dos eixos
        axis.ticks = element_line(color = "black"),  # Garantir os ticks dos eixos
        axis.text = element_text(color = "black"))  # Garantir que o text
ggsave("g3a.svg", plot = g3a, width = 16, height = 9, dpi=300)


##Variaçao da base boi magro MS
modelo_proxy_boi_magro_ms <- lm(vd_bmagro_spot ~ log(vd_bezerro_ms) + vd_milho_bolsa)
options(scipen=9999)
summary(modelo_proxy_boi_magro_ms)
shapiro.test(modelo_proxy_boi_magro_ms$residuals)
hist(modelo_proxy_boi_magro_ms$residuals, main = "Histograma dos Resíduos", xlab = "Resíduos")
qqnorm(modelo_proxy_boi_magro_ms$residuals)
qqline(modelo_proxy_boi_magro_ms$residuals)

library(lmtest)
bptest(modelo_proxy_boi_magro_ms)
dwtest(modelo_proxy_boi_magro_ms)

intercepto_ms <- coef(modelo_proxy_boi_magro_ms)[1]
coef_bezerro_ms <- coef(modelo_proxy_boi_magro_ms)[2]
coef_milho_ms <- coef(modelo_proxy_boi_magro_ms)[3]

proxy_boi_magro_previsto_ms <- intercepto_ms + coef_bezerro_ms*log(vd_bezerro_ms)+ coef_milho_ms*vd_milho_bolsa
proxy_boi_magro_previsto_ms

base_bmagro_ms<-vd_bmagro_ms-proxy_boi_magro_previsto_ms
mean(base_bmagro_ms)
var(base_bmagro_ms)
sd(base_bmagro_ms)
summary(base_bmagro_ms)

base_bmagro_ms <- data.frame(
  ano = floor(time(base_bmagro_ms)), # Extrai o ano
  variacao_base = as.numeric(base_bmagro_ms) # Converte a série ts para numérica
)

# Calcular o coeficiente de variação (CV) por ano
library(dplyr)
var_por_ano_bmagro_ms <- base_bmagro_ms %>%
  group_by(ano) %>%
  summarise(
    media = mean(variacao_base, na.rm = TRUE),
    var = var(variacao_base, na.rm = TRUE),
    desvio_padrao = sd(variacao_base, na.rm = TRUE)
  )
var_por_ano_bmagro_ms
write.csv(var_por_ano_bmagro_ms, "var_por_ano_bmagro_ms.csv")

# Criando um data frame com os preços spot e futuros
df_ms <- data.frame(
  Tempo = time(vd_bmagro_ms),  # Assume que 'precos_spot' e 'precos_futuros' são séries temporais
  Preco_Spot_MS = as.numeric(vd_bmagro_ms),
  Preco_Futuro_MS = as.numeric(proxy_boi_magro_previsto_ms)
)

# Plotando com ggplot
g4a<-ggplot(df_ms, aes(x = Tempo)) +
  geom_line(aes(y = Preco_Spot_MS, color = "Preço Spot Mato Grosso do Sul"), size = 1) +  # Linha dos preços spot
  geom_line(aes(y = Preco_Futuro_MS, color = "Proxy Preço Futuro Mato Grosso do Sul"), size = 1, linetype = "dashed") +  # Linha dos preços futuros
  labs( x = "Tempo", y = "Preço") +
  scale_color_manual(values = c("Preço Spot Mato Grosso do Sul" = "blue", "Proxy Preço Futuro Mato Grosso do Sul" = "red")) +  # Definindo cores
  theme_minimal() +
  theme(legend.title = element_blank(),  # Remover título da legenda
        legend.position = "bottom",  # Posicionando a legenda no topo
        axis.line = element_line(color = "black"),  # Garantir linhas dos eixos
        axis.ticks = element_line(color = "black"),  # Garantir os ticks dos eixos
        axis.text = element_text(color = "black"))  # Garantir que o text
ggsave("g4a.svg", plot = g4a, width = 16, height = 9, dpi=300)
g4a

library(gridExtra)
grid.arrange(g1, g2, ncol = 2) # lado a lado
grid.arrange(g1, g2, ncol = 1) # um embaixo do outro

#simulação
base2<-data.frame(vd_bezerro_sp,vd_bezerro_ims, vd_bezerro_ms, vd_bgordo_bolsa,vd_milho_bolsa,vd_bmagro_spot,vd_bgordo_sp,vd_milho_sp,vd_bmagro_sp, vd_bgordo_ms,vd_milho_ms,vd_bmagro_ms)
base2

# Transformar série temporal em data.frame
base2 <- as.data.frame(base2) # Converte o objeto `ts` para um data.frame
base2$date <- seq(as.Date("2014-01-01"), by = "month", length.out = nrow(base2)) # Cria a coluna de datas

# Verifique os nomes das colunas
colnames(base2)

# Criando as variáveis com defasagem
base3 <- base2 %>%
  arrange(date) %>%
  mutate(
    PBGordo_t_bolsa = vd_bgordo_bolsa,                        # Preço do boi gordo no tempo atual
    PBMagro_t_5_bolsa = dplyr::lag(vd_bmagro_spot, 5),       # Preço do boi magro com defasagem de 5 meses
    PBezerro_t_5_sp = dplyr::lag(vd_bezerro_sp, 5),     # Preço do bezerro com defasagem de 5 meses
    PBezerro_t_5_ims = dplyr::lag(vd_bezerro_ims, 5),
    PBezerro_t_5_ms = dplyr::lag(vd_bezerro_ms, 5),
    PMilho_t_2_bolsa = dplyr::lag(vd_milho_bolsa, 2),          # Preço do milho com defasagem de 2 meses
    PBGordo_t_spot_sp = vd_bgordo_sp,                        # Preço do boi gordo no tempo atual
    PBMagro_t_5_spot_sp = dplyr::lag(vd_bmagro_sp, 5),       # Preço do boi magro com defasagem de 5 meses
    PMilho_t_2_spot_sp = dplyr::lag(vd_milho_sp, 2),
    PBGordo_t_spot_ms = vd_bgordo_ms,                        # Preço do boi gordo no tempo atual
    PBMagro_t_5_spot_ms = dplyr::lag(vd_bmagro_ms, 5),       # Preço do boi magro com defasagem de 5 meses
    PMilho_t_2_spot_ms = dplyr::lag(vd_milho_ms, 2),
  ) %>%
  filter(!is.na(PBMagro_t_5_bolsa) & !is.na(PBezerro_t_5_sp) & !is.na(PBezerro_t_5_ims)& !is.na(PBezerro_t_5_ms)& !is.na(PMilho_t_2_bolsa)& !is.na(PMilho_t_2_spot_sp)& !is.na(PMilho_t_2_spot_ms)& !is.na(PBMagro_t_5_spot_sp)& !is.na(PBMagro_t_5_spot_ms)) # Remove observações com NA

# Visualizando a nova base
head(base3)

# Pacotes necessários
library(forecast)
library(rugarch)
#install.packages("rugarch")
require(urca)

PBGordo_t_bolsa<-ts(base3$PBGordo_t_bolsa, start=c(2014,6), end=c(2023,12), frequency = 12)
PBezerro_t_5_sp<-ts(base3$PBezerro_t_5_sp, start=c(2014,6), end=c(2023,12), frequency = 12)
PBezerro_t_5_ms<-ts(base3$PBezerro_t_5_ms, start=c(2014,6), end=c(2023,12), frequency = 12)
PBezerro_t_5_ims<-ts(base3$PBezerro_t_5_ims, start=c(2014,6), end=c(2023,12), frequency = 12)
PMilho_t_2_bolsa<-ts(base3$PMilho_t_2_bolsa, start=c(2014,6), end=c(2023,12), frequency = 12)
PBMagro_t_5_bolsa<-ts(base3$PBMagro_t_5_bolsa, start=c(2014,6), end=c(2023,12), frequency = 12)
PBGordo_t_spot_sp<-ts(base3$PBGordo_t_spot_sp, start=c(2014,6), end=c(2023,12), frequency = 12)
PBMagro_t_5_spot_sp<-ts(base3$PBMagro_t_5_spot_sp, start=c(2014,6), end=c(2023,12), frequency = 12)
PMilho_t_2_spot_sp<-ts(base3$PMilho_t_2_spot_sp, start=c(2014,6), end=c(2023,12), frequency = 12)
PBGordo_t_spot_ms<-ts(base3$PBGordo_t_spot_ms, start=c(2014,6), end=c(2023,12), frequency = 12)
PBMagro_t_5_spot_ms<-ts(base3$PBMagro_t_5_spot_ms, start=c(2014,6), end=c(2023,12), frequency = 12)
PMilho_t_2_spot_ms<-ts(base3$PMilho_t_2_spot_ms, start=c(2014,6), end=c(2023,12), frequency = 12)

adf_test_magro_sp <- ur.df(PBMagro_t_5_spot_sp, type = "trend", lags = 1); summary(adf_test_magro_sp)  # type pode ser "none", "drift", ou "trend" ##nao estacionaria
adf_test_boi_sp <- ur.df(PBGordo_t_spot_sp, type = "trend", lags = 1); summary(adf_test_boi_sp) # type pode ser "none", "drift", ou "trend ##nao estacionaria
adf_test_milho_sp <- ur.df(PMilho_t_2_spot_sp, type = "trend", lags = 1); summary(adf_test_milho_sp)  # type pode ser "none", "drift", ou "trend ##nao estacionaria
adf_test_bezerro_sp<-ur.df(PBezerro_t_5_sp, type = "trend", lags = 1); summary(adf_test_bezerro_sp)  # type pode ser "none", "drift", ou "trend ##nao estacionaria

adf_test_magro_ms <- ur.df(PBMagro_t_5_spot_ms, type = "trend", lags = 1); summary(adf_test_magro_ms)  # type pode ser "none", "drift", ou "trend" ##nao estacionaria
adf_test_boi_ms <- ur.df(PBGordo_t_spot_ms, type = "trend", lags = 1); summary(adf_test_boi_ms) # type pode ser "none", "drift", ou "trend ##nao estacionaria
adf_test_milho_ms <- ur.df(PMilho_t_2_spot_ms, type = "trend", lags = 1); summary(adf_test_milho_ms)  # type pode ser "none", "drift", ou "trend ##nao estacionaria
adf_test_bezerro_ms<-ur.df(PBezerro_t_5_ms, type = "trend", lags = 1); summary(adf_test_bezerro_ms)  # type pode ser "none", "drift", ou "trend ##nao estacionaria

#testanddo normalidade (não necessário)
shapiro_test_boi_sp <- shapiro.test(PBGordo_t_spot_sp); shapiro_test_boi_sp
shapiro_test_milho_sp <- shapiro.test(PMilho_t_2_spot_sp); shapiro_test_milho_sp
shapiro_test_magro_sp <- shapiro.test(PBMagro_t_5_spot_sp); shapiro_test_magro_sp
shapiro_test_bezerro_sp <- shapiro.test(PBezerro_t_5_sp); shapiro_test_bezerro_sp

shapiro_test_boi_ms <- shapiro.test(PBGordo_t_spot_ms); shapiro_test_boi_ms
shapiro_test_milho_ms <- shapiro.test(PMilho_t_2_spot_ms); shapiro_test_milho_ms
shapiro_test_magro_ms <- shapiro.test(PBMagro_t_5_spot_ms); shapiro_test_magro_ms
shapiro_test_bezerro_ms <- shapiro.test(PBezerro_t_5_ms); shapiro_test_bezerro_ms
#não são normais

#testando cointegração (não necessário)
dados_precos <- data.frame(PBGordo_t_spot_sp, PMilho_t_2_spot_sp, PBezerro_t_5_sp)
dados_precos <- data.frame(PBGordo_t_spot_ms, PMilho_t_2_spot_ms, PBezerro_t_5_ms)
dados_precos <- data.frame(PBMagro_t_5_bolsa, PMilho_t_2_bolsa, PBezerro_t_5_ms)
dados_precos <- data.frame(PBMagro_t_5_bolsa, PMilho_t_2_bolsa, PBezerro_t_5_sp)

# Aplicar teste de Johansen
johansen_test <- ca.jo(dados_precos, type = "trace", ecdet = "const", K = 2)

options(scipen = 999)
# Resultado do teste
summary(johansen_test)

teste_independencia_kendall <- cor.test(PBGordo_t_spot_sp, PMilho_t_2_spot_sp, method = "kendall") #sao dependentes
teste_independencia_kendall <- cor.test(PBGordo_t_spot_sp, PBezerro_t_5_sp, method = "kendall")#sao dependentes
teste_independencia_kendall <- cor.test(PMilho_t_2_spot_sp,PBezerro_t_5_sp, method = "kendall")#sao dependentes

teste_independencia_kendall <- cor.test(PBGordo_t_spot_ms, PMilho_t_2_spot_ms, method = "kendall") #sao dependentes
teste_independencia_kendall <- cor.test(PBGordo_t_spot_ms, PBezerro_t_5_ms, method = "kendall")#sao dependentes
teste_independencia_kendall <- cor.test(PMilho_t_2_spot_ms,PBezerro_t_5_ms, method = "kendall")#sao dependentes
# Exibir o resultado do teste
print(teste_independencia_kendall)


##bezerro SP
acf(diff(PBezerro_t_5_sp), lag.max = 115, col="red", lwd=2)
pacf(diff(PBezerro_t_5_sp), lag.max = 115, col="red", lwd=2)
plot(diff(PBezerro_t_5_sp))
teste_za <- ur.za(PBezerro_t_5_sp, model = "both", lag = 12)
# Exibindo o resultado do teste
summary(teste_za)
plot(teste_za)

arima_model_bezerro_sp <- auto.arima(PBezerro_t_5_sp, seasonal = TRUE, trace = TRUE); arima_model_bezerro_sp
modelo_arima_bezerro_sp <- Arima(PBezerro_t_5_sp, order = c(1, 1, 0))
summary(modelo_arima_bezerro_sp)

#checando os residuos
checkresiduals(modelo_arima_bezerro_sp)
residuos_bezerro_sp <- residuals(modelo_arima_bezerro_sp)
hist(residuos_bezerro_sp,freq=F)
qqnorm(residuos_bezerro_sp)
qqline(residuos_bezerro_sp)
acf(residuos_bezerro_sp, lag.max = 115, col="red", lwd=1)
pacf(residuos_bezerro_sp, lag.max = 115, col="red", lwd=1)
#testando normalidade
shapiro.test(residuos_bezerro_sp)

#testandoautocorrelação
Box.test(residuos_bezerro_sp, lag = 24, type = "Ljung-Box")
Box.test(residuos_bezerro_sp, lag = 10, type = "Ljung-Box")

#install.packages("FinTS")
library(FinTS)
# Realize o teste ARCH nos resíduos
ArchTest(residuos_bezerro_sp)

bezerro_2024_sp <- ts(base1$vd_bezerro_sp, start = c(2024, 1), end = c(2024, 12),frequency = 12)
previsao <- forecast(modelo_arima_bezerro_sp, h = length(bezerro_2024_sp))
rmse <- sqrt(mean((bezerro_2024_sp - previsao$mean)^2))
print(rmse)

##bezerro MS
acf(diff(PBezerro_t_5_ms), lag.max = 115, col="red", lwd=2)
pacf(diff(PBezerro_t_5_ms), lag.max = 115, col="red", lwd=2)
plot(diff(PBezerro_t_5_ms))

teste_za <- ur.za(PBezerro_t_5_ms, model = "both", lag = 12)
# Exibindo o resultado do teste
summary(teste_za)
plot(teste_za)

arima_model_bezerro_ms <- auto.arima(PBezerro_t_5_ms, seasonal = TRUE, trace = TRUE); arima_model_bezerro_ms
modelo_arima_bezerro_ms <- Arima(PBezerro_t_5_ms, order = c(1, 1, 0), seasonal=c(0, 0, 1))
summary(modelo_arima_bezerro_ms)

#checando os residuos
checkresiduals(modelo_arima_bezerro_ms)
residuos_bezerro_ms <- residuals(modelo_arima_bezerro_ms)
hist(residuos_bezerro_ms,freq=F)
qqnorm(residuos_bezerro_ms)
qqline(residuos_bezerro_ms)
acf(residuos_bezerro_ms, lag.max = 115, col="red", lwd=1)
pacf(residuos_bezerro_ms, lag.max = 115, col="red", lwd=1)
#testando normalidade
shapiro.test(residuos_bezerro_ms)

#testandoautocorrelação
Box.test(residuos_bezerro_ms, lag = 24, type = "Ljung-Box")
Box.test(residuos_bezerro_ms, lag = 10, type = "Ljung-Box")

#install.packages("FinTS")
library(FinTS)
# Realize o teste ARCH nos resíduos
ArchTest(residuos_bezerro_ms)

bezerro_2024_ms <- ts(base1$vd_bezerro_ms, start = c(2024, 1), end = c(2024, 12),frequency = 12)
previsao <- forecast(modelo_arima_bezerro_ms, h = length(bezerro_2024_sp))
rmse <- sqrt(mean((bezerro_2024_ms - previsao$mean)^2))
print(rmse)

# Ajuste de um modelo ARIMA para Boi Gordo Bolsa
# Criar uma variável dummy que é 1 apenas em outubro de 2021
dummy_quebra <- as.numeric(time(PBGordo_t_bolsa) == 2021 + 9/12)  # Outubro = 9/12
teste_za <- ur.za(PBGordo_t_bolsa, model = "both", lag = 12)
# Exibindo o resultado do teste
summary(teste_za)
plot(teste_za)

arima_model_boi_b <- auto.arima(PBGordo_t_bolsa, seasonal = TRUE,trace = TRUE, xreg = dummy_quebra); arima_model_boi_b
modelo_arima_boi_b <- Arima(PBGordo_t_bolsa, order = c(2, 1, 2), xreg = dummy_quebra)
summary(modelo_arima_boi_b)
#checando os residuos
checkresiduals(modelo_arima_boi_b)
residuos_boi_b <- residuals(modelo_arima_boi_b)
hist(residuos_boi_b,freq=F)
qqnorm(residuos_boi_b)
qqline(residuos_boi_b)
acf(residuos_boi_b, lag.max = 115, col="red", lwd=1)
pacf(residuos_boi_b, lag.max = 115, col="red", lwd=1)
#testando normalidade
shapiro.test(residuos_boi_b)
#testandoautocorrelação
Box.test(residuos_boi_b, lag = 24, type = "Ljung-Box")
Box.test(residuos_boi_b, lag = 10, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuos_boi_b)

boi_2024 <- ts(base1$vd_bgordo_bolsa, start = c(2024, 1), end = c(2024, 12),frequency = 12)
dummy_quebra_futuro <- rep(0, length(boi_2024))
previsao <- forecast(modelo_arima_boi_b, h = length(boi_2024), xreg=dummy_quebra_futuro)
rmse <- sqrt(mean((boi_2024 - previsao$mean)^2))
print(rmse)

#ajuste milho bolsa
arima_model_milho_b <- auto.arima(PMilho_t_2_bolsa,seasonal = TRUE,trace = TRUE); arima_model_milho_b
modelo_arima_milho_b <- Arima(PMilho_t_2_bolsa, order = c(0, 1, 0)) 

summary(modelo_arima_milho_b)
checkresiduals(modelo_arima_milho_b)
residuos_milho_b <- modelo_arima_milho_b$residuals
hist(residuos_milho_b,freq=F)
qqnorm(residuos_milho_b)
qqline(residuos_milho_b)
acf(residuos_milho_b, lag.max = 32, col="red", lwd=1)
pacf(residuos_milho_b, lag.max = 32, col="red", lwd=1)
shapiro.test(residuos_milho_b)
Box.test(residuos_milho_b, lag = 24, type = "Ljung-Box")
Box.test(residuos_milho_b, lag = 10, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuos_milho_b)

milho_2024 <- ts(base1$vd_milho_bolsa, start = c(2024, 1), end = c(2024, 12),frequency = 12)
previsao <- forecast(modelo_arima_milho_b, h = length(milho_2024))
rmse <- sqrt(mean((milho_2024 - previsao$mean)^2))
print(rmse)

#Ajuste arima para boi magro SP
acf(diff(PBMagro_t_5_spot_sp), lag.max = 115, col="red", lwd=2)
pacf(diff(PBMagro_t_5_spot_sp), lag.max = 115, col="red", lwd=2)
plot(PBMagro_t_5_spot_sp)
decomposicao <- stl(PBMagro_t_5_spot_sp, s.window = "periodic")
plot(decomposicao)
#teste de quebra estrutural
teste_za <- ur.za(PBMagro_t_5_spot_sp, model = "both", lag = 12)
# Exibindo o resultado do teste
summary(teste_za)
plot(teste_za)
PBMagro_t_5_spot_sp
dummy_quebra2 <- as.numeric(time(PBMagro_t_5_spot_sp) == 2021 + 3/12)  # Outubro = 9/12

?Arima
arima_model_magro_sp <- auto.arima(PBMagro_t_5_spot_sp, seasonal = TRUE, trace = TRUE); arima_model_magro_sp
modelo_arima_magro_sp <- Arima(PBMagro_t_5_spot_sp, order = c(2, 1, 2))
summary(modelo_arima_magro_sp)

#checando os residuos
checkresiduals(modelo_arima_magro_sp)
residuos_magro_sp <- residuals(modelo_arima_magro_sp)
hist(residuos_magro_sp,freq=F)
qqnorm(residuos_magro_sp)
qqline(residuos_magro_sp)
acf(residuos_magro_sp, lag.max = 115, col="red", lwd=1)
pacf(residuos_magro_sp, lag.max = 115, col="red", lwd=1)
#testando normalidade
shapiro.test(residuos_magro_sp)

#testandoautocorrelação
Box.test(residuos_magro_sp, lag = 24, type = "Ljung-Box")
Box.test(residuos_magro_sp, lag = 12, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuos_magro_sp)

magro_2024_sp <- ts(base1$vd_bmagro_sp, start = c(2024, 1), end = c(2024, 12),frequency = 12)
dummy_quebra_futuro <- rep(0, length(magro_2024_sp))
previsao <- forecast(modelo_arima_magro_sp, h = length(magro_2024_sp))
rmse <- sqrt(mean((magro_2024_sp - previsao$mean)^2))
print(rmse)

#Ajuste arima para boi magro MS
acf(diff(PBMagro_t_5_spot_ms), lag.max = 115, col="red", lwd=2)
pacf(diff(PBMagro_t_5_spot_ms), lag.max = 115, col="red", lwd=2)
plot(PBMagro_t_5_spot_ms)
decomposicao <- stl(PBMagro_t_5_spot_ms, s.window = "periodic")
plot(decomposicao)
#teste de quebra estrutural
teste_za <- ur.za(PBMagro_t_5_spot_ms, model = "both", lag = 12)
# Exibindo o resultado do teste
summary(teste_za)
plot(teste_za)
PBMagro_t_5_spot_ms
dummy_quebra3 <- as.numeric(time(PBMagro_t_5_spot_ms) == 2021 + 3/12)  # Outubro = 9/12

?Arima
arima_model_magro_ms <- auto.arima(PBMagro_t_5_spot_ms, seasonal = TRUE, trace = TRUE); arima_model_magro_ms
modelo_arima_magro_ms <- Arima(PBMagro_t_5_spot_ms, order = c(2, 1, 0))
summary(modelo_arima_magro_ms)

#checando os residuos
checkresiduals(modelo_arima_magro_ms)
residuos_magro_ms <- residuals(modelo_arima_magro_ms)
hist(residuos_magro_ms,freq=F)
qqnorm(residuos_magro_ms)
qqline(residuos_magro_ms)
acf(residuos_magro_ms, lag.max = 115, col="red", lwd=1)
pacf(residuos_magro_ms, lag.max = 115, col="red", lwd=1)
#testando normalidade
shapiro.test(residuos_magro_ms)

#testandoautocorrelação
Box.test(residuos_magro_ms, lag = 24, type = "Ljung-Box")
Box.test(residuos_magro_ms, lag = 10, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuos_magro_ms)

magro_2024_ms <- ts(base1$vd_bmagro_ms, start = c(2024, 1), end = c(2024, 12),frequency = 12)
dummy_quebra_futuro <- rep(0, length(magro_2024_ms))
previsao <- forecast(modelo_arima_magro_ms, h = length(magro_2024_ms))
rmse <- sqrt(mean((magro_2024_ms - previsao$mean)^2))
print(rmse)

#Arima para Boi SP
acf(diff(PBGordo_t_spot_sp), lag.max = 115, col="red", lwd=2)
pacf(diff(PBGordo_t_spot_sp), lag.max = 115, col="red", lwd=2)
plot(PBGordo_t_spot_sp)

teste_za <- ur.za(PBGordo_t_spot_sp, model = "both", lag = 12)
# Exibindo o resultado do teste
summary(teste_za)
plot(teste_za)

arima_model_boi_sp <- auto.arima(PBGordo_t_spot_sp, seasonal = TRUE, trace = TRUE); arima_model_boi_sp
modelo_arima_boi_sp <- Arima(PBGordo_t_spot_sp, order = c(1, 1, 1))
summary(modelo_arima_boi_sp)

#checando os residuos
checkresiduals(modelo_arima_boi_sp)
residuos_boi_sp <- residuals(modelo_arima_boi_sp)
hist(residuos_boi_sp,freq=F)
qqnorm(residuos_boi_sp)
qqline(residuos_boi_sp)
acf(residuos_boi_sp, lag.max = 115, col="red", lwd=1)
pacf(residuos_boi_sp, lag.max = 115, col="red", lwd=1)
#testando normalidade
shapiro.test(residuos_boi_sp)

#testandoautocorrelação
Box.test(residuos_boi_sp, lag = 24, type = "Ljung-Box")
Box.test(residuos_boi_sp, lag = 10, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuos_boi_sp)

boi_2024_sp <- ts(base1$vd_bgordo_sp, start = c(2024, 1), end = c(2024, 12),frequency = 12)
previsao <- forecast(modelo_arima_boi_sp, h = length(boi_2024))
rmse <- sqrt(mean((boi_2024_sp - previsao$mean)^2))
print(rmse)

#Arima para Boi MS
acf(diff(PBGordo_t_spot_ms), lag.max = 115, col="red", lwd=2)
pacf(diff(PBGordo_t_spot_ms), lag.max = 115, col="red", lwd=2)
plot(PBGordo_t_spot_ms)

teste_za <- ur.za(PBGordo_t_spot_ms, model = "both", lag = 12)
# Exibindo o resultado do teste
summary(teste_za)
plot(teste_za)

arima_model_boi_ms <- auto.arima(PBGordo_t_spot_ms, seasonal = TRUE, trace = TRUE); arima_model_boi_ms
modelo_arima_boi_ms <- Arima(PBGordo_t_spot_ms, order = c(1, 1, 1))
summary(modelo_arima_boi_ms)

#checando os residuos
checkresiduals(modelo_arima_boi_ms)
residuos_boi_ms <- residuals(modelo_arima_boi_ms)
hist(residuos_boi_ms,freq=F)
qqnorm(residuos_boi_ms)
qqline(residuos_boi_ms)
acf(residuos_boi_ms, lag.max = 115, col="red", lwd=1)
pacf(residuos_boi_ms, lag.max = 115, col="red", lwd=1)
#testando normalidade
shapiro.test(residuos_boi_ms)

#testandoautocorrelação
Box.test(residuos_boi_ms, lag = 24, type = "Ljung-Box")
Box.test(residuos_boi_ms, lag = 10, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuos_boi_ms)

boi_2024_ms <- ts(base1$vd_bgordo_ms, start = c(2024, 1), end = c(2024, 12),frequency = 12)
previsao <- forecast(modelo_arima_boi_ms, h = length(boi_2024))
rmse <- sqrt(mean((boi_2024_ms - previsao$mean)^2))
print(rmse)

# Ajuste de um modelo ARIMA para Milho MS
acf(diff(PMilho_t_2_spot_ms), lag.max = 115, col="red", lwd=2)
pacf(diff(PMilho_t_2_spot_ms), lag.max = 115, col="red", lwd=2)
plot(PMilho_t_2_spot_ms)
arima_model_milho_ms <- auto.arima(PMilho_t_2_spot_ms,seasonal = TRUE,trace = TRUE); arima_model_milho_ms
modelo_arima_milho_ms <- Arima(PMilho_t_2_spot_ms, order = c(0, 1, 1))

summary(modelo_arima_milho_ms)
checkresiduals(modelo_arima_milho_ms)
residuos_milho_ms <- modelo_arima_milho_ms$residuals
hist(residuos_milho_ms,freq=F)
qqnorm(residuos_milho_ms)
qqline(residuos_milho_ms)
acf(residuos_milho_ms, lag.max = 32, col="red", lwd=1)
pacf(residuos_milho_ms, lag.max = 32, col="red", lwd=1)
shapiro.test(residuos_milho_ms)
Box.test(residuos_milho_ms, lag = 24, type = "Ljung-Box")
Box.test(residuos_milho_ms, lag = 10, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuos_milho_ms)

milho_2024_ms <- ts(base1$vd_milho_ms, start = c(2024, 1), end = c(2024, 12),frequency = 12)
previsao <- forecast(modelo_arima_milho_ms, h = length(milho_2024_ms))
rmse <- sqrt(mean((milho_2024_ms - previsao$mean)^2))
print(rmse)

# Ajuste de um modelo ARIMA para Milho SP
acf(diff(PMilho_t_2_spot_sp), lag.max = 115, col="red", lwd=2)
pacf(diff(PMilho_t_2_spot_sp), lag.max = 115, col="red", lwd=2)
plot(PMilho_t_2_spot_sp)
arima_model_milho_sp <- auto.arima(PMilho_t_2_spot_sp,seasonal = TRUE,trace = TRUE); arima_model_milho_sp
modelo_arima_milho_sp <- Arima(PMilho_t_2_spot_sp, order = c(1, 1, 0))

summary(modelo_arima_milho_sp)
checkresiduals(modelo_arima_milho_sp)
residuos_milho_sp <- modelo_arima_milho_sp$residuals
hist(residuos_milho_sp,freq=F)
qqnorm(residuos_milho_sp)
qqline(residuos_milho_sp)
acf(residuos_milho_sp, lag.max = 32, col="red", lwd=1)
pacf(residuos_milho_sp, lag.max = 32, col="red", lwd=1)
shapiro.test(residuos_milho_sp)
Box.test(residuos_milho_sp, lag = 24, type = "Ljung-Box")
Box.test(residuos_milho_sp, lag = 10, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuos_milho_sp)

milho_2024_sp <- ts(base1$vd_milho_sp, start = c(2024, 1), end = c(2024, 12),frequency = 12)
previsao <- forecast(modelo_arima_milho_sp, h = length(milho_2024_sp))
rmse <- sqrt(mean((milho_2024_sp - previsao$mean)^2))
print(rmse)

#forecast.Arima# Previsão para os próximos 12 meses
bezerro_forecast_sp <- forecast(modelo_arima_bezerro_sp, h = 6)
preco_bezerro_previsto_sp <- (bezerro_forecast_sp$mean)
preco_bezerro_previsto_sp
bezerro_forecast_ms <- forecast(modelo_arima_bezerro_ms, h = 6)
preco_bezerro_previsto_ms <- (bezerro_forecast_ms$mean) # Valores previstos para 12 meses

preco_bezerro_previsto_sp <- as.vector(preco_bezerro_previsto_sp)
preco_bezerro_previsto_ms <- as.vector(preco_bezerro_previsto_ms)

# Previsão do preço do boi magro usando os valores previstos para o preço do bezerro e milho nos próximos 12 meses
p_milho_g<-rbind(70.28, 72.3, 74.31, 74.09, 73.86, 73.36)


# Calcular o preço do boi magro para os dados simulados
proxy_boi_magro_previsto_sp <- intercepto_sp + coef_bezerro_sp*log(preco_bezerro_previsto_sp) + coef_milho_sp*p_milho_g
proxy_boi_magro_previsto_sp
library(copula)
library(MASS)

# Ajustar os modelos para o log dos preços
boi_gordo_log_params <- fitdistr(log(PBGordo_t_bolsa), "normal")
milho_log_params <- fitdistr(log(PMilho_t_2_bolsa), "normal")

# Extrair os parâmetros (média e desvio padrão) para cada distribuição log-normal
mu_boi_gordo <- boi_gordo_log_params$estimate[1]
sigma_boi_gordo <- boi_gordo_log_params$estimate[2]

mu_milho <- milho_log_params$estimate[1]
sigma_milho <- milho_log_params$estimate[2]

# Passo 2: Transformação para uniforme nos resíduos (pobs)
dados_residuos <- data.frame(residuos_bezerro_sp, residuos_boi_b, residuos_milho_b)
dados_residuos <- pobs(dados_residuos)  # Transformação para uniforme
correlacoes <- cor(dados_residuos, method = "spearman")
correlacoes

# Criando a cópula normal com as correlações
#Passo 1: Ajustar a cópula nos resíduos
# Usando fitCopula para ajustar a cópula aos resíduos
copula_model1_f <- fitCopula(normalCopula(dim = 3), dados_residuos, method = "ml")
copula_model2_f <- fitCopula(tCopula(dim = 3), dados_residuos, method = "ml")
copula_model3_f <- fitCopula(claytonCopula(dim = 3), dados_residuos, method = "ml")
copula_model4_f <- fitCopula(gumbelCopula(dim = 3), dados_residuos, method = "ml") #nao e boa para correlacao negativa
copula_model5_f<- fitCopula(frankCopula(dim = 3), dados_residuos, method = "ml")

AIC(copula_model1_f, copula_model2_f, copula_model3_f, copula_model5_f)
summary(copula_model3)

?simulate.Arima
#Passo 2: Simular amostras da cópula para os resíduos
# Simulação com inclusão de correlações e saída dos preços simulados
# Ajuste no replicate para evitar simplificação
set.seed(12345)
simulacoes_sp_f <- replicate(12, {
  
  # Geração de amostras da cópula para os resíduos ajustados
  copula_samples <- rCopula(5000, copula_model3_f@copula)
  
  # Simulação do preço do boi gordo e milho com log-normal (usando a abordagem logarítmica)
  preco_boi_gordo_sim <- exp(qnorm(copula_samples[, 2], mean = mu_boi_gordo, sd = sigma_boi_gordo))
  preco_milho_sim <- exp(qnorm(copula_samples[, 3], mean = mu_milho, sd = sigma_milho))
  
  # Simulação dos resíduos para o preço do bezerro a partir da cópula
  residuos_bezerro_sim_sp <- qnorm(copula_samples[, 1])
  
  # Simulação do preço do bezerro com base no modelo ARIMA e nos resíduos simulados
  
  bezerro_sim_sp <- simulate(modelo_arima_bezerro_sp, nsim = 5000, future=TRUE, innov = residuos_bezerro_sim_sp)
  
  boi_magro_proxy_sim_sp <- intercepto_sp + coef_bezerro_sp*log(bezerro_sim_sp) + coef_milho_sp*preco_milho_sim
  
  # Calcular a margem bruta para cada simulação
  margem_bruta_sim <- preco_boi_gordo_sim * 18 - boi_magro_proxy_sim_sp - 11 * preco_milho_sim
  
  # Retornar uma lista com os resultados simulados
  list(
    margem_bruta = margem_bruta_sim,
    preco_boi_gordo = preco_boi_gordo_sim,
    preco_milho = preco_milho_sim,
    preco_bezerro_sp = bezerro_sim_sp,
    preco_boi_magro_sp = boi_magro_proxy_sim_sp
  )
}, simplify = FALSE) 

# Acessar as margens brutas simuladas
margem_futuro_sp <- sapply(simulacoes_sp_f, function(x) x$margem_bruta)
margem_futuro_sp <- data.frame(margem_futuro_sp)
margem_futuro_sp <- data.frame(
  margem_futuro_sp = unlist(margem_futuro_sp),  # Unifica as margens brutas
  mes = rep(1:12, each = length(margem_futuro_sp[[1]]))  # Repete os meses
)


p_magro_sp<- intercepto_sp + coef_bezerro_sp * log(vd_bezerro_sp) + coef_milho_sp * vd_milho_bolsa
p_magro_sp
p_magro_g1_sp<-p_magro_sp[117:120]
p_magro_g1_sp<-t(p_magro_g1_sp)
p_magro_g1_sp
proxy_boi_magro_previsto_sp<-t(proxy_boi_magro_previsto_sp)
proxy_boi_magro_previsto_sp

p_gordo_g<-rbind(246.55, 244.7, 242.67, 242.93, 240.77, 241.73, 250.22, 251.67, 261, 247.62)
p_milho_g<-rbind(65.32, 70.28, 72.3, 74.31, 74.09, 73.86, 73.36, 72.85, 72.36, 71.86)
p_magro_g_sp<-rbind(p_magro_g1_sp,proxy_boi_magro_previsto_sp);p_magro_g_sp

margem_garantida_sp<-(p_gordo_g * 18) - p_magro_g_sp - (11 * p_milho_g)
margem_garantida_sp<-rbind(NA,margem_garantida_sp, NA)
mes<-c(1,2,3,4,5,6,7,8,9,10,11,12)
mg_sp<-cbind(margem_garantida_sp,mes)
mg_sp
margem_garantida_df_sp <- as.data.frame(mg_sp)
colnames(margem_garantida_df_sp) <- c("margem_garantida_sp", "mes")

# Filtrando apenas os meses com valores válidos de margem garantida
margem_garantida_df_sp <- margem_garantida_df_sp[!is.na(margem_garantida_df_sp$margem_garantida_sp), ]

margem_futuro_sp <- margem_futuro_sp %>%left_join(margem_garantida_df_sp, by = "mes")

# Criando variáveis de decisão do seguro e da realidade
# O seguro indeniza se a margem com preço futuro for menor que a garantida
indenizado_pelo_seguro_sp<-ifelse(margem_futuro_sp < margem_garantida_sp, 1, 0)
margem_futuro_sp <- margem_futuro_sp %>%
  mutate(indenizacao = ifelse(margem_futuro_sp < margem_garantida_sp, margem_garantida_sp - margem_futuro_sp, 0))

# Calcule a indenização em cada simulação
# Onde a indenização é positiva se margem garantida for maior que margem simulada
margem_futuro_sp <- margem_futuro_sp %>%
  mutate(indenizacao = ifelse(margem_futuro_sp < margem_garantida_sp, margem_garantida_sp - margem_futuro_sp, 0))

# Calcule a indenização média em cada mês
indenizacoes_mensais <- margem_futuro_sp %>%
  group_by(mes) %>%
  summarize(media_indenizacao = mean(indenizacao, na.rm = TRUE))

# Calcule o prêmio para cada mês com os fatores 1.03 e 2
premios <- indenizacoes_mensais %>%
  mutate(premio_1_03 = media_indenizacao * 1.03,
         premio_2 = media_indenizacao * 2)

# Visualize o resultado
print(premios)

# Ajustar os modelos para o log dos preços
boi_gordo_log_params_sp <- fitdistr(log(PBGordo_t_spot_sp), "normal")
milho_log_params_sp <- fitdistr(log(PMilho_t_2_spot_sp), "normal")
magro_log_params_sp <- fitdistr(log(PBMagro_t_5_spot_sp), "normal")

# Extrair os parâmetros (média e desvio padrão) para cada distribuição log-normal
mu_boi_gordo_sp <- boi_gordo_log_params_sp$estimate[1]
sigma_boi_gordo_sp <- boi_gordo_log_params_sp$estimate[2]

mu_milho_sp <- milho_log_params_sp$estimate[1]
sigma_milho_sp <- milho_log_params_sp$estimate[2]

mu_magro_sp <- magro_log_params_sp$estimate[1]
sigma_magro_sp <- magro_log_params_sp$estimate[2]

# Passo 2: Transformação para uniforme nos resíduos (pobs)
dados_residuos_sp <- data.frame(residuos_magro_sp, residuos_boi_sp, residuos_milho_sp)
dados_residuos_sp <- pobs(dados_residuos_sp)  # Transformação para uniforme
correlacoes_sp <- cor(dados_residuos_sp, method = "spearman")
correlacoes_sp

# Criando a cópula normal com as correlações
#Passo 1: Ajustar a cópula nos resíduos
# Usando fitCopula para ajustar a cópula aos resíduos
copula_model1_s <- fitCopula(normalCopula(dim = 3), dados_residuos_sp, method = "ml")
copula_model2_s <- fitCopula(tCopula(dim = 3, df.fixed = TRUE, df = 4), dados_residuos_sp, method = "ml")
copula_model3_s <- fitCopula(claytonCopula(dim = 3), dados_residuos_sp, method = "ml", start = c(0.1))
copula_model4_s <- fitCopula(gumbelCopula(dim = 3), dados_residuos_sp, method = "ml", start = c(1.1))
copula_model5_s<- fitCopula(frankCopula(dim = 3), dados_residuos_sp, method = "ml",start = c(0.1))
AIC(copula_model1_s, copula_model2_s,copula_model3_s)
summary(copula_model3)

?simulate.Arima
#Passo 2: Simular amostras da cópula para os resíduos
# Simulação com inclusão de correlações e saída dos preços simulados
# Ajuste no replicate para evitar simplificação
set.seed(12345)
simulacoes_sp_s <- replicate(12, {
  
  # Geração de amostras da cópula para os resíduos ajustados
  copula_samples <- rCopula(5000, copula_model1_s@copula)
  
  # Simulação do preço do boi gordo e milho com log-normal (usando a abordagem logarítmica)
  preco_boi_gordo_sim <- exp(qnorm(copula_samples[, 2], mean = mu_boi_gordo_sp, sd = sigma_boi_gordo_sp))
  preco_milho_sim <- exp(qnorm(copula_samples[, 3], mean = mu_milho_sp, sd = sigma_milho_sp))
  preco_magro_sim <- exp(qnorm(copula_samples[, 1], mean = mu_magro_sp, sd = sigma_magro_sp))
  
  
  # Calcular a margem bruta para cada simulação
  margem_bruta_sim <- preco_boi_gordo_sim * 18 -  preco_magro_sim - 11 * preco_milho_sim
  
  # Retornar uma lista com os resultados simulados
  list(
    margem_bruta = margem_bruta_sim,
    preco_boi_gordo = preco_boi_gordo_sim,
    preco_milho = preco_milho_sim,
    preco_boi_magro = preco_magro_sim
  )
}, simplify = FALSE) 

# Acessar as margens brutas simuladas
margem_spot_sp <- sapply(simulacoes_sp_s, function(x) x$margem_bruta)
margem_spot_sp <- data.frame(margem_spot_sp)
margem_spot_sp <- data.frame(
  margem_spot_sp = unlist(margem_spot_sp),  # Unifica as margens brutas
  mes = rep(1:12, each = length(margem_spot_sp[[1]]))  # Repete os meses
)

margem_spot_sp <- margem_spot_sp %>%left_join(margem_garantida_df_sp, by = "mes")

# O produtor realmente deveria ser indenizado se a margem spot for menor que a garantida
sinistro_real_sp<-ifelse(margem_spot_sp < margem_garantida_sp, 1, 0)
margem_spot_sp <- margem_spot_sp %>%
  mutate(indenizacao = ifelse(margem_spot_sp < margem_garantida_sp, margem_garantida_sp - margem_spot_sp, 0))

margem_futuro_sp$id <- ave(margem_futuro_sp$mes, margem_futuro_sp$mes, FUN = seq_along)
margem_spot_sp$id <- ave(margem_spot_sp$mes, margem_spot_sp$mes, FUN = seq_along)
df_comparado_sp <- merge(margem_futuro_sp, margem_spot_sp, by = c("mes", "id"), suffixes = c("_futuro_sp", "_spot_sp"))
df_comparado_sp$erro_tipo_I <- ifelse(df_comparado_sp$indenizacao_futuro_sp > 0 & df_comparado_sp$indenizacao_spot_sp == 0, 1, 0)
df_comparado_sp$erro_tipo_II <- ifelse(df_comparado_sp$indenizacao_futuro_sp == 0 & df_comparado_sp$indenizacao_spot_sp > 0, 1, 0)
taxa_erro_I_sp <- mean(df_comparado_sp$erro_tipo_I, na.rm = TRUE); taxa_erro_I_sp
taxa_erro_II_sp <- mean(df_comparado_sp$erro_tipo_II,na.rm = TRUE); taxa_erro_II_sp

df_erros_mensal_sp <- df_comparado_sp %>%
  group_by(mes) %>%
  summarise(
    taxa_erro_I_sp = mean(erro_tipo_I, na.rm = TRUE),
    taxa_erro_II_sp = mean(erro_tipo_II, na.rm = TRUE)
  )
df_erros_mensal_sp

library(dplyr)
library(tidyr)

df_long_sp <- df_comparado_sp %>%
  dplyr::select(mes, id, margem_futuro_sp, margem_spot_sp, erro_tipo_I, erro_tipo_II) %>%
  pivot_longer(
    cols = c(margem_futuro_sp, margem_spot_sp),
    names_to = "mercado",
    values_to = "margem"
  ) %>%
  mutate(
    mercado = ifelse(mercado == "margem_futuro_sp", "Futuro", "Spot")
  )
df_long_sp

hist_arima_sp<-ggplot(df_long_sp, aes(x = margem, fill = mercado)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
  facet_wrap(~mes, scales = "free") +
  scale_fill_manual(values = c("Futuro" = "blue", "Spot" = "darkorange")) +
  labs(
    title = "Distribuição das Margens Simuladas por Mês",
    x = "Margem",
    y = "Frequência",
    fill = "Mercado"
  ) +
  theme_minimal()
hist_arima_sp
library(svglite)
ggsave("hist_arima_sp.svg", plot=hist_arima_sp, width = 16, height = 9,dpi=300)
install.packages("writexl")  # se ainda não tiver
library(writexl)

write_xlsx(df_erros_mensal_sp, "df_erros_mensal_sp.xlsx")


# Previsão do preço do boi magro usando os valores previstos para o preço do bezerro e milho nos próximos 12 meses
p_milho_g<-rbind(70.28, 72.3, 74.31, 74.09, 73.86, 73.36)


# Calcular o preço do boi magro para os dados simulados
proxy_boi_magro_previsto_ms <- intercepto_ms + coef_bezerro_ms*log(preco_bezerro_previsto_ms) + coef_milho_ms*p_milho_g
proxy_boi_magro_previsto_ms
library(copula)
library(MASS)

# Ajustar os modelos para o log dos preços
boi_gordo_log_params <- fitdistr(log(PBGordo_t_bolsa), "normal")
milho_log_params <- fitdistr(log(PMilho_t_2_bolsa), "normal")

# Extrair os parâmetros (média e desvio padrão) para cada distribuição log-normal
mu_boi_gordo <- boi_gordo_log_params$estimate[1]
sigma_boi_gordo <- boi_gordo_log_params$estimate[2]

mu_milho <- milho_log_params$estimate[1]
sigma_milho <- milho_log_params$estimate[2]

# Passo 2: Transformação para uniforme nos resíduos (pobs)
dados_residuos_fut_ms <- data.frame(residuos_bezerro_ms, residuos_boi_b, residuos_milho_b)
dados_residuos_fut_ms <- pobs(dados_residuos_fut_ms)  # Transformação para uniforme
correlacoes_futuro_ms <- cor(dados_residuos_fut_ms, method = "spearman")
correlacoes_futuro_ms

# Criando a cópula normal com as correlações
#Passo 1: Ajustar a cópula nos resíduos
# Usando fitCopula para ajustar a cópula aos resíduos
copula_model1_f <- fitCopula(normalCopula(dim = 3), dados_residuos_fut_ms, method = "ml")
copula_model2_f <- fitCopula(tCopula(dim = 3), dados_residuos_fut_ms, method = "ml")
copula_model3_f <- fitCopula(claytonCopula(dim = 3), dados_residuos_fut_ms, method = "ml")
copula_model4_f <- fitCopula(gumbelCopula(dim = 3), dados_residuos_fut_ms, method = "ml")
copula_model5_f<- fitCopula(frankCopula(dim = 3), dados_residuos_fut_ms, method = "ml")

AIC(copula_model1_f, copula_model2_f, copula_model3_f, copula_model5_f)
summary(copula_model)

?simulate.Arima
#Passo 2: Simular amostras da cópula para os resíduos
# Simulação com inclusão de correlações e saída dos preços simulados
# Ajuste no replicate para evitar simplificação
set.seed(12345)
simulacoes_ms_f <- replicate(12, {
  
  # Geração de amostras da cópula para os resíduos ajustados
  copula_samples <- rCopula(5000, copula_model5_f@copula)
  
  # Simulação do preço do boi gordo e milho com log-normal (usando a abordagem logarítmica)
  preco_boi_gordo_sim <- exp(qnorm(copula_samples[, 2], mean = mu_boi_gordo, sd = sigma_boi_gordo))
  preco_milho_sim <- exp(qnorm(copula_samples[, 3], mean = mu_milho, sd = sigma_milho))
  
  # Simulação dos resíduos para o preço do bezerro a partir da cópula
  residuos_bezerro_sim_ms <- qnorm(copula_samples[, 1])
  
  # Simulação do preço do bezerro com base no modelo ARIMA e nos resíduos simulados
  
  bezerro_sim_ms <- simulate(modelo_arima_bezerro_ms, nsim = 5000, future=TRUE, innov = residuos_bezerro_sim_ms)
  
  boi_magro_proxy_sim_ms <- intercepto_ms + coef_bezerro_ms*log(bezerro_sim_ms) + coef_milho_ms*preco_milho_sim
  
  # Calcular a margem bruta para cada simulação
  margem_bruta_sim <- preco_boi_gordo_sim * 18 - boi_magro_proxy_sim_ms - 11 * preco_milho_sim
  
  # Retornar uma lista com os resultados simulados
  list(
    margem_bruta = margem_bruta_sim,
    preco_boi_gordo = preco_boi_gordo_sim,
    preco_milho = preco_milho_sim,
    preco_bezerro_ms = bezerro_sim_ms,
    preco_boi_magro_ms = boi_magro_proxy_sim_ms
  )
}, simplify = FALSE) 

# Acessar as margens brutas simuladas
margem_futuro_ms <- sapply(simulacoes_ms_f, function(x) x$margem_bruta)
margem_futuro_ms <- data.frame(margem_futuro_ms)
margem_futuro_ms <- data.frame(
  margem_futuro_ms = unlist(margem_futuro_ms),  # Unifica as margens brutas
  mes = rep(1:12, each = length(margem_futuro_ms[[1]]))  # Repete os meses
)


p_magro_ms<- intercepto_ms + coef_bezerro_ms * log(vd_bezerro_ms) + coef_milho_ms * vd_milho_bolsa
p_magro_g1_ms<-p_magro_ms[117:120]
p_magro_g1_ms<-t(p_magro_g1_ms)
p_magro_g1_ms
proxy_boi_magro_previsto_ms<-t(proxy_boi_magro_previsto_ms)
proxy_boi_magro_previsto_ms

p_gordo_g<-rbind(246.55, 244.7, 242.67, 242.93, 240.77, 241.73, 250.22, 251.67, 261, 247.62)
p_milho_g<-rbind(65.32, 70.28, 72.3, 74.31, 74.09, 73.86, 73.36, 72.85, 72.36, 71.86)
p_magro_g_ms<-rbind(p_magro_g1_ms,proxy_boi_magro_previsto_ms);p_magro_g_ms

margem_garantida_ms<-(p_gordo_g * 18) - p_magro_g_ms - (11 * p_milho_g)
margem_garantida_ms<-rbind(NA,margem_garantida_ms, NA)
mes<-c(1,2,3,4,5,6,7,8,9,10,11,12)
mg_ms<-cbind(margem_garantida_ms,mes)
mg_ms
margem_garantida_df_ms <- as.data.frame(mg_ms)
colnames(margem_garantida_df_ms) <- c("margem_garantida_ms", "mes")

# Filtrando apenas os meses com valores válidos de margem garantida
margem_garantida_df_ms <- margem_garantida_df_ms[!is.na(margem_garantida_df_ms$margem_garantida_ms), ]

margem_futuro_ms <- margem_futuro_ms %>%left_join(margem_garantida_df_ms, by = "mes")

# Criando variáveis de decisão do seguro e da realidade
# O seguro indeniza se a margem com preço futuro for menor que a garantida
indenizado_pelo_seguro_ms<-ifelse(margem_futuro_ms < margem_garantida_ms, 1, 0)
margem_futuro_ms <- margem_futuro_ms %>%
  mutate(indenizacao = ifelse(margem_futuro_ms < margem_garantida_ms, margem_garantida_ms - margem_futuro_ms, 0))

# Calcule a indenização em cada simulação
# Onde a indenização é positiva se margem garantida for maior que margem simulada
margem_futuro_ms <- margem_futuro_ms %>%
  mutate(indenizacao = ifelse(margem_futuro_ms < margem_garantida_ms, margem_garantida_ms - margem_futuro_ms, 0))

# Calcule a indenização média em cada mês
indenizacoes_mensais <- margem_futuro_ms %>%
  group_by(mes) %>%
  summarize(media_indenizacao = mean(indenizacao, na.rm = TRUE))

# Calcule o prêmio para cada mês com os fatores 1.03 e 2
premios <- indenizacoes_mensais %>%
  mutate(premio_1_03 = media_indenizacao * 1.03,
         premio_2 = media_indenizacao * 2)

# Visualize o resultado
print(premios)


# Ajustar os modelos para o log dos preços
boi_gordo_log_params_ms <- fitdistr(log(PBGordo_t_spot_ms), "normal")
milho_log_params_ms <- fitdistr(log(PMilho_t_2_spot_ms), "normal")
magro_log_params_ms <- fitdistr(log(PBMagro_t_5_spot_ms), "normal")

# Extrair os parâmetros (média e desvio padrão) para cada distribuição log-normal
mu_boi_gordo_ms <- boi_gordo_log_params_ms$estimate[1]
sigma_boi_gordo_ms <- boi_gordo_log_params_ms$estimate[2]

mu_milho_ms <- milho_log_params_ms$estimate[1]
sigma_milho_ms <- milho_log_params_ms$estimate[2]

mu_magro_ms <- magro_log_params_ms$estimate[1]
sigma_magro_ms<- magro_log_params_ms$estimate[2]

# Passo 2: Transformação para uniforme nos resíduos (pobs)
dados_residuos_ms <- data.frame(residuos_magro_ms, residuos_boi_ms, residuos_milho_ms)
dados_residuos_ms <- pobs(dados_residuos_ms)  # Transformação para uniforme
correlacoes_ms <- cor(dados_residuos_ms, method = "spearman")
correlacoes_ms

# Criando a cópula normal com as correlações
#Passo 1: Ajustar a cópula nos resíduos
# Usando fitCopula para ajustar a cópula aos resíduos
copula_model1_s <- fitCopula(normalCopula(dim = 3), dados_residuos_ms, method = "ml")
copula_model2_s <- fitCopula(tCopula(dim = 3), dados_residuos_ms, method = "ml")
copula_model3_s <- fitCopula(claytonCopula(dim = 3), dados_residuos_ms, method = "ml")
copula_model4_s <- fitCopula(gumbelCopula(dim = 3), dados_residuos_ms, method = "ml")
copula_model5_s<- fitCopula(frankCopula(dim = 3), dados_residuos_ms, method = "ml")

AIC(copula_model1_s, copula_model2_s,copula_model3_s,copula_model5_s)
summary(copula_model3)

?simulate.Arima
#Passo 2: Simular amostras da cópula para os resíduos
# Simulação com inclusão de correlações e saída dos preços simulados
# Ajuste no replicate para evitar simplificação
set.seed(12345)
simulacoes_ms_s <- replicate(12, {
  
  # Geração de amostras da cópula para os resíduos ajustados
  copula_samples <- rCopula(5000, copula_model3_s@copula)
  
  # Simulação do preço do boi gordo e milho com log-normal (usando a abordagem logarítmica)
  preco_boi_gordo_sim <- exp(qnorm(copula_samples[, 2], mean = mu_boi_gordo_ms, sd = sigma_boi_gordo_ms))
  preco_milho_sim <- exp(qnorm(copula_samples[, 3], mean = mu_milho_ms, sd = sigma_milho_ms))
  preco_magro_sim <- exp(qnorm(copula_samples[, 1], mean = mu_magro_ms, sd = sigma_magro_ms))
  
  
  # Calcular a margem bruta para cada simulação
  margem_bruta_sim <- preco_boi_gordo_sim * 18 -  preco_magro_sim - 11 * preco_milho_sim
  
  # Retornar uma lista com os resultados simulados
  list(
    margem_bruta = margem_bruta_sim,
    preco_boi_gordo = preco_boi_gordo_sim,
    preco_milho = preco_milho_sim,
    preco_boi_magro = preco_magro_sim
  )
}, simplify = FALSE) 

# Acessar as margens brutas simuladas
margem_spot_ms <- sapply(simulacoes_ms_s, function(x) x$margem_bruta)
margem_spot_ms <- data.frame(margem_spot_ms)
margem_spot_ms <- data.frame(
  margem_spot_ms = unlist(margem_spot_ms),  # Unifica as margens brutas
  mes = rep(1:12, each = length(margem_spot_ms[[1]]))  # Repete os meses
)

margem_spot_ms <- margem_spot_ms%>%left_join(margem_garantida_df_ms, by = "mes")

# O produtor realmente deveria ser indenizado se a margem spot for menor que a garantida
sinistro_real_ms<-ifelse(margem_spot_ms < margem_garantida_ms, 1, 0)
margem_spot_ms <- margem_spot_ms %>%
  mutate(indenizacao = ifelse(margem_spot_ms < margem_garantida_ms, margem_garantida_ms - margem_spot_ms, 0))

margem_futuro_ms$id <- ave(margem_futuro_ms$mes, margem_futuro_ms$mes, FUN = seq_along)
margem_spot_ms$id <- ave(margem_spot_ms$mes, margem_spot_ms$mes, FUN = seq_along)
df_comparado_ms <- merge(margem_futuro_ms, margem_spot_ms, by = c("mes", "id"), suffixes = c("_futuro_ms", "_spot_ms"))
df_comparado_ms$erro_tipo_I <- ifelse(df_comparado_ms$indenizacao_futuro_ms > 0 & df_comparado_ms$indenizacao_spot_ms == 0, 1, 0)
df_comparado_ms$erro_tipo_II <- ifelse(df_comparado_ms$indenizacao_futuro_ms == 0 & df_comparado_ms$indenizacao_spot_ms > 0, 1, 0)
taxa_erro_I_ms <- mean(df_comparado_ms$erro_tipo_I, na.rm = TRUE); taxa_erro_I_ms
taxa_erro_II_ms <- mean(df_comparado_ms$erro_tipo_II,na.rm = TRUE); taxa_erro_II_ms

df_erros_mensal_ms <- df_comparado_ms %>%
  group_by(mes) %>%
  summarise(
    taxa_erro_I_ms = mean(erro_tipo_I, na.rm = TRUE),
    taxa_erro_II_ms = mean(erro_tipo_II, na.rm = TRUE)
  )
df_erros_mensal_ms

library(dplyr)
library(tidyr)

df_long_ms <- df_comparado_ms %>%
  dplyr::select(mes, id, margem_futuro_ms, margem_spot_ms, erro_tipo_I, erro_tipo_II) %>%
  pivot_longer(
    cols = c(margem_futuro_ms, margem_spot_ms),
    names_to = "mercado",
    values_to = "margem"
  ) %>%
  mutate(
    mercado = ifelse(mercado == "margem_futuro_ms", "Futuro", "Spot")
  )
df_long_ms

hist_arima_ms<-ggplot(df_long_ms, aes(x = margem, fill = mercado)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 50) +
  facet_wrap(~mes, scales = "free") +
  scale_fill_manual(values = c("Futuro" = "blue", "Spot" = "darkorange")) +
  labs(
    title = "Distribuição das Margens Simuladas por Mês",
    x = "Margem",
    y = "Frequência",
    fill = "Mercado"
  ) +
  theme_minimal()
hist_arima_ms
library(svglite)
ggsave("hist_arima_ms_2.svg", plot=hist_arima_ms, width = 16, height = 9,dpi=300)
install.packages("writexl")  # se ainda não tiver
library(writexl)

write_xlsx(df_erros_mensal_ms, "df_erros_mensal_ms_2.xlsx")