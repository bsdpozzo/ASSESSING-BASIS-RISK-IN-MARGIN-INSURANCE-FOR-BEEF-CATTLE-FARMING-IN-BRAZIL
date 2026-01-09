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

IGP_DI_en<-IGP_DI_en|>janitor::clean_names()
data_prices<-data_prices|>janitor::clean_names()

# ===============================
# Deflating prices using IGP-DI
# ===============================

igp_di <- IGP_DI_en %>%
  dplyr::mutate(month_year = as.factor(month_year))

price_data <- data_prices %>%
  dplyr::mutate(month_year = as.factor(month_year)) %>%
  inner_join(igp_di, by = "month_year")

# ===============================
# Deflated prices (real values)
# ===============================

price_data <- price_data %>%
  mutate(
    real_calf_price_ms_cepea   = cepea_ms_calf_index * multiplier,
    real_fed_price_future     = future_fed_price * multiplier,
    real_feeder_price_federal  = federal_spot_feeder_price * multiplier,
    real_corn_price_future    = future_corn_price * multiplier,
    
    real_fed_price_sp         = spot_fed_price_sp * multiplier,
    real_feeder_price_sp      = state_spot_feeder_price_sp * multiplier,
    real_corn_price_sp        = spot_corn_price_sp * multiplier,
    real_calf_price_sp        = spot_calf_price_sp * multiplier,
    
    real_fed_price_ms         = spot_fed_price_ms * multiplier,
    real_feeder_price_ms      = state_spot_feeder_price_ms * multiplier,
    real_corn_price_ms        = spot_corn_price_ms * multiplier,
    real_calf_price_ms        = spot_calf_price_ms * multiplier
  )

# ===============================
# Time series settings
# ===============================

start_period <- c(2014, 1)
end_period   <- c(2023, 12)
freq_monthly <- 12

# =========================================
# Futures / margin insurance related series
# =========================================

# Calf / feeder cattle
ts_calf_price_sp <- ts(
  price_data$real_calf_price_sp,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

ts_calf_price_ms <- ts(
  price_data$real_calf_price_ms,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

ts_calf_price_ms_cepea <- ts(
  price_data$real_calf_price_ms_cepea,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

# Futures prices
ts_fed_price_future <- ts(
  price_data$real_fed_price_future,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

ts_corn_price_future <- ts(
  price_data$real_corn_price_future,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

# Feeder cattle – federal proxy (used as futures proxy)
ts_feeder_price_federal <- ts(
  price_data$real_feeder_price_federal,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

# ===============================
# Spot market – São Paulo
# ===============================

ts_fed_price_sp <- ts(
  price_data$real_fed_price_sp,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

ts_corn_price_sp <- ts(
  price_data$real_corn_price_sp,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

ts_feeder_price_sp <- ts(
  price_data$real_feeder_price_sp,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

# ===============================
# Spot market – Mato Grosso do Sul
# ===============================

ts_fed_price_ms <- ts(
  price_data$real_fed_price_ms,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

ts_corn_price_ms <- ts(
  price_data$real_corn_price_ms,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)

ts_feeder_price_ms <- ts(
  price_data$real_feeder_price_ms,
  start = start_period,
  end   = end_period,
  frequency = freq_monthly
)


# ===============================
# Packages
# ===============================
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(lmtest)

# ===============================
# Helper functions
# ===============================

# Convert a monthly ts object into a tidy data.frame
ts_to_df <- function(ts_object){
  data.frame(
    date = as.Date(as.yearmon(time(ts_object))),
    value = as.numeric(ts_object)
  ) %>%
    mutate(
      year = year(date),
      month = month(date)
    )
}

# Summary statistics of the basis
basis_stats <- function(basis_ts){
  data.frame(
    mean = mean(basis_ts, na.rm = TRUE),
    variance = var(basis_ts, na.rm = TRUE),
    sd = sd(basis_ts, na.rm = TRUE)
  )
}

# Annual basis statistics
annual_basis_stats <- function(basis_ts){
  ts_to_df(basis_ts) %>%
    group_by(year) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      variance = var(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .groups = "drop"
    )
}

# ===============================
# Fed cattle basis – São Paulo
# ===============================

basis_fed_sp <- ts_fed_price_sp - ts_fed_price_future

# Descriptive statistics
basis_stats(basis_fed_sp)
summary(basis_fed_sp)

# Annual statistics
annual_fed_sp <- annual_basis_stats(basis_fed_sp)
write.csv(annual_fed_sp, "annual_basis_fed_sp.csv", row.names = FALSE)

# Boxplot by year
fed_sp_df <- ts_to_df(basis_fed_sp)

g_fed_sp_box <- ggplot(fed_sp_df, aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Basis (BRL/@)",
    title = "Distribution of fed cattle basis – São Paulo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fed_basis_sp_boxplot.svg", g_fed_sp_box, width = 16, height = 9, dpi = 600)

# ===============================
# Fed cattle basis – Mato Grosso do Sul
# ===============================

basis_fed_ms <- ts_fed_price_ms - ts_fed_price_future

basis_stats(basis_fed_ms)
summary(basis_fed_ms)

annual_fed_ms <- annual_basis_stats(basis_fed_ms)
write.csv(annual_fed_ms, "annual_basis_fed_ms.csv", row.names = FALSE)

fed_ms_df <- ts_to_df(basis_fed_ms)

g_fed_ms_box <- ggplot(fed_ms_df, aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Basis (BRL/@)",
    title = "Distribution of fed cattle basis – Mato Grosso do Sul"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fed_basis_ms_boxplot.svg", g_fed_ms_box, width = 16, height = 9, dpi = 600)

fed_prices_df <- data.frame(
  date = as.Date(as.yearmon(time(ts_fed_price_ms))),
  spot_price_ms = as.numeric(ts_fed_price_ms),
  spot_price_sp = as.numeric(ts_fed_price_sp),
  futures_price = as.numeric(ts_fed_price_future)
)

g_fed_prices <- ggplot(fed_prices_df, aes(x = date)) +
  geom_line(
    aes(y = spot_price_ms, color = "Spot price – Mato Grosso do Sul"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = spot_price_sp, color = "Spot price – São Paulo"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = futures_price, color = "Futures price"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Time",
    y = "Price (BRL/@)",
    title = "Fed Cattle prices: spot and futures markets"
  ) +
  scale_color_manual(
    values = c(
      "Spot price – Mato Grosso do Sul" = "blue",
      "Spot price – São Paulo" = "darkgreen",
      "Futures price" = "red"
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = "fed_prices_sp_ms_futures.svg",
  plot = g_fed_prices,
  width = 16,
  height = 9,
  dpi = 300
)

# ===============================
# Corn basis – São Paulo
# ===============================

basis_corn_sp <- ts_corn_price_sp - ts_corn_price_future
basis_stats(basis_corn_sp)
summary(basis_corn_sp)

annual_corn_sp <- annual_basis_stats(basis_corn_sp)
write.csv(annual_corn_sp, "annual_basis_corn_sp.csv", row.names = FALSE)

corn_sp_df <- ts_to_df(basis_corn_sp)

g_corn_sp_box <- ggplot(corn_sp_df, aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Basis (BRL/bag)",
    title = "Distribution of corn basis – São Paulo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("corn_basis_sp_boxplot.svg", g_corn_sp_box, width = 16, height = 9, dpi = 600)

# ===============================
# Corn basis – Mato Grosso do Sul
# ===============================

basis_corn_ms <- ts_corn_price_ms - ts_corn_price_future
basis_stats(basis_corn_ms)
summary(basis_corn_ms)

annual_corn_ms <- annual_basis_stats(basis_corn_ms)
write.csv(annual_corn_ms, "annual_basis_corn_ms.csv", row.names = FALSE)

corn_ms_df <- ts_to_df(basis_corn_ms)

g_corn_ms_box <- ggplot(corn_ms_df, aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Basis (BRL/bag)",
    title = "Distribution of corn basis – Mato Grosso do Sul"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("corn_basis_ms_boxplot.svg", g_corn_ms_box, width = 16, height = 9, dpi = 600)

corn_prices_df <- data.frame(
  date = as.Date(as.yearmon(time(ts_corn_price_ms))),
  spot_price_ms = as.numeric(ts_corn_price_ms),
  spot_price_sp = as.numeric(ts_corn_price_sp),
  futures_price = as.numeric(ts_corn_price_future)
)

g_corn_prices <- ggplot(corn_prices_df, aes(x = date)) +
  geom_line(
    aes(y = spot_price_ms, color = "Spot price – Mato Grosso do Sul"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = spot_price_sp, color = "Spot price – São Paulo"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = futures_price, color = "Futures price"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Time",
    y = "Price (BRL/bag)",
    title = "Corn prices: spot and futures markets"
  ) +
  scale_color_manual(
    values = c(
      "Spot price – Mato Grosso do Sul" = "blue",
      "Spot price – São Paulo" = "darkgreen",
      "Futures price" = "red"
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = "corn_prices_sp_ms_futures.svg",
  plot = g_corn_prices,
  width = 16,
  height = 9,
  dpi = 300
)


# ===============================
# Feeder cattle proxy – São Paulo (Federal)
# ===============================

model_feeder_sp_federal <- lm(
  ts_feeder_price_federal ~ log(ts_calf_price_sp) + ts_corn_price_future
)
options(scipen=9999)
summary(model_feeder_sp_federal)
shapiro.test(residuals(model_feeder_sp_federal))
bptest(model_feeder_sp_federal)
dwtest(model_feeder_sp_federal)

feeder_proxy_sp_federal <- fitted(model_feeder_sp_federal)
intercept_sp_federal <- coef(model_feeder_sp_federal)[1]
coef_calf_sp_federal <- coef(model_feeder_sp_federal)[2]
coef_corn_sp_federal <- coef(model_feeder_sp_federal)[3]

basis_feeder_sp_federal <- ts_feeder_price_sp - feeder_proxy_sp_federal
basis_stats(basis_feeder_sp_federal)
summary(basis_feeder_sp_federal)

annual_feeder_sp_federal <- annual_basis_stats(basis_feeder_sp_federal)
write.csv(annual_feeder_sp_federal, "annual_basis_feeder_sp_federal.csv", row.names = FALSE)

feeder_sp_df_federal  <- ts_to_df(basis_feeder_sp_federal)

g_feeder_sp_box_federal  <- ggplot(feeder_sp_df_federal , aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Basis (BRL/head)",
    title = "Distribution of feeder basis – São Paulo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("feeder_basis_sp_boxplot_federal.svg", g_feeder_sp_box_federal, width = 16, height = 9, dpi = 600)

feeder_sp_df_federal <- data.frame(
  date = as.Date(as.yearmon(time(ts_feeder_price_sp))),
  spot_price_sp = as.numeric(ts_feeder_price_sp),
  futures_proxy_sp = as.numeric(feeder_proxy_sp_federal)
)

g_feeder_sp_federal <- ggplot(feeder_sp_df_federal, aes(x = date)) +
  geom_line(
    aes(y = spot_price_sp, color = "Spot price – São Paulo"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = futures_proxy_sp, color = "Futures price proxy – São Paulo"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Time",
    y = "Price (BRL/head)",
    title = "Feeder cattle prices: spot market and futures proxy (São Paulo)"
  ) +
  scale_color_manual(
    values = c(
      "Spot price – São Paulo" = "blue",
      "Futures price proxy – São Paulo" = "red"
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = "feeder_prices_spot_vs_proxy_sp_federal.svg",
  plot = g_feeder_sp_federal,
  width = 16,
  height = 9,
  dpi = 300
)


# ===============================
# Feeder cattle proxy – Mato Grosso do Sul (Federal)
# ===============================

model_feeder_ms_federal <- lm(
  ts_feeder_price_federal ~ log(ts_calf_price_ms) + ts_corn_price_future
)

summary(model_feeder_ms_federal)
shapiro.test(residuals(model_feeder_ms_federal))
bptest(model_feeder_ms_federal)
dwtest(model_feeder_ms_federal)

feeder_proxy_ms_federal <- fitted(model_feeder_ms_federal)
intercept_ms_federal <- coef(model_feeder_ms_federal)[1]
coef_calf_ms_federal <- coef(model_feeder_ms_federal)[2]
coef_corn_ms_federal <- coef(model_feeder_ms_federal)[3]

basis_feeder_ms_federal <- ts_feeder_price_ms - feeder_proxy_ms_federal
basis_stats(basis_feeder_ms_federal)
summary(basis_feeder_ms_federal)

annual_feeder_ms_federal <- annual_basis_stats(basis_feeder_ms_federal)
write.csv(annual_feeder_ms_federal, "annual_basis_feeder_ms_federal.csv", row.names = FALSE)

feeder_ms_df_federal  <- ts_to_df(basis_feeder_ms_federal)

g_feeder_ms_box_federal  <- ggplot(feeder_ms_df_federal , aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Basis (BRL/head)",
    title = "Distribution of feeder basis – Mato Grosso do Sul"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("feeder_basis_ms_boxplot_federal.svg", g_feeder_ms_box_federal, width = 16, height = 9, dpi = 600)

feeder_ms_df_federal <- data.frame(
  date = as.Date(as.yearmon(time(ts_feeder_price_ms))),
  spot_price_ms = as.numeric(ts_feeder_price_ms),
  futures_proxy_ms = as.numeric(feeder_proxy_ms_federal)
)

g_feeder_ms_federal <- ggplot(feeder_ms_df_federal, aes(x = date)) +
  geom_line(
    aes(y = spot_price_ms, color = "Spot price – Mato Grosso do Sul"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = futures_proxy_ms, color = "Futures price proxy – Mato Grosso do Sul"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Time",
    y = "Price (BRL/head)",
    title = "Feeder cattle prices: spot market and futures proxy (Mato Grosso do Sul)"
  ) +
  scale_color_manual(
    values = c(
      "Spot price – Mato Grosso do Sul" = "blue",
      "Futures price proxy – Mato Grosso do Sul" = "red"
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = "feeder_prices_spot_vs_proxy_ms_federal.svg",
  plot = g_feeder_ms_federal,
  width = 16,
  height = 9,
  dpi = 300
)

# ===============================
# Feeder cattle proxy – São Paulo (State)
# ===============================

model_feeder_sp_state <- lm(
  ts_feeder_price_sp ~ log(ts_calf_price_sp) + ts_corn_price_future
)
options(scipen=9999)
summary(model_feeder_sp_state)
shapiro.test(residuals(model_feeder_sp_state))
bptest(model_feeder_sp_state)
dwtest(model_feeder_sp_state)

feeder_proxy_sp_state <- fitted(model_feeder_sp_state)
intercept_sp_state <- coef(model_feeder_sp_state)[1]
coef_calf_sp_state <- coef(model_feeder_sp_state)[2]
coef_corn_sp_state <- coef(model_feeder_sp_state)[3]

basis_feeder_sp_state <- ts_feeder_price_sp - feeder_proxy_sp_state
basis_stats(basis_feeder_sp_state)
summary(basis_feeder_sp_state)

annual_feeder_sp_state <- annual_basis_stats(basis_feeder_sp_state)
write.csv(annual_feeder_sp_state, "annual_basis_feeder_sp_state.csv", row.names = FALSE)

feeder_sp_df_state  <- ts_to_df(basis_feeder_sp_state)

g_feeder_sp_box_state  <- ggplot(feeder_sp_df_state , aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Basis (BRL/head)",
    title = "Distribution of feeder basis – São Paulo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("feeder_basis_sp_boxplot_state.svg", g_feeder_sp_box_state, width = 16, height = 9, dpi = 600)

feeder_sp_df_state <- data.frame(
  date = as.Date(as.yearmon(time(ts_feeder_price_sp))),
  spot_price_sp = as.numeric(ts_feeder_price_sp),
  futures_proxy_sp = as.numeric(feeder_proxy_sp_state)
)

g_feeder_sp_state <- ggplot(feeder_sp_df_state, aes(x = date)) +
  geom_line(
    aes(y = spot_price_sp, color = "Spot price – São Paulo"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = futures_proxy_sp, color = "Futures price proxy – São Paulo"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Time",
    y = "Price (BRL/head)",
    title = "Feeder cattle prices: spot market and futures proxy (São Paulo)"
  ) +
  scale_color_manual(
    values = c(
      "Spot price – São Paulo" = "blue",
      "Futures price proxy – São Paulo" = "red"
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = "feeder_prices_spot_vs_proxy_sp_state.svg",
  plot = g_feeder_sp_state,
  width = 16,
  height = 9,
  dpi = 300
)


# ===============================
# Feeder cattle proxy – Mato Grosso do Sul (State)
# ===============================

model_feeder_ms_state <- lm(
  log(ts_feeder_price_ms) ~ log(ts_calf_price_ms) + log(ts_corn_price_future)
)

summary(model_feeder_ms_state)
shapiro.test(residuals(model_feeder_ms_state))
bptest(model_feeder_ms_state)
dwtest(model_feeder_ms_state)

feeder_proxy_ms_state <- fitted(model_feeder_ms_state)
intercept_ms_state <- coef(model_feeder_ms_state)[1]
coef_calf_ms_state <- coef(model_feeder_ms_state)[2]
coef_corn_ms_state <- coef(model_feeder_ms_state)[3]

basis_feeder_ms_state <- ts_feeder_price_ms - exp(feeder_proxy_ms_state)
basis_stats(basis_feeder_ms_state)
summary(basis_feeder_ms_state)

annual_feeder_ms_state <- annual_basis_stats(basis_feeder_ms_state)
write.csv(annual_feeder_ms_state, "annual_basis_feeder_ms_state.csv", row.names = FALSE)

feeder_ms_df_state  <- ts_to_df(basis_feeder_ms_state)

g_feeder_ms_box_state  <- ggplot(feeder_ms_df_state, aes(x = factor(year), y = value)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.size = 0.8) +
  labs(
    x = "Year",
    y = "Basis (BRL/head)",
    title = "Distribution of feeder basis – Mato Grosso do Sul"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("feeder_basis_ms_boxplot_state.svg", g_feeder_ms_box_state, width = 16, height = 9, dpi = 600)

feeder_ms_df_state <- data.frame(
  date = as.Date(as.yearmon(time(ts_feeder_price_ms))),
  spot_price_ms = as.numeric(ts_feeder_price_ms),
  futures_proxy_ms = as.numeric(exp(feeder_proxy_ms_state))
)

g_feeder_ms_state <- ggplot(feeder_ms_df_state, aes(x = date)) +
  geom_line(
    aes(y = spot_price_ms, color = "Spot price – Mato Grosso do Sul"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = futures_proxy_ms, color = "Futures price proxy – Mato Grosso do Sul"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  labs(
    x = "Time",
    y = "Price (BRL/head)",
    title = "Feeder cattle prices: spot market and futures proxy (Mato Grosso do Sul)"
  ) +
  scale_color_manual(
    values = c(
      "Spot price – Mato Grosso do Sul" = "blue",
      "Futures price proxy – Mato Grosso do Sul" = "red"
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = "feeder_prices_spot_vs_proxy_ms_federal.svg",
  plot = g_feeder_ms_federal,
  width = 16,
  height = 9,
  dpi = 300
)

# ===============================
# Simulation dataset
# ===============================

simulation_data <- data.frame(
  calf_price_sp      = ts_calf_price_sp,
  calf_price_ims     = ts_calf_price_ms_cepea,
  calf_price_ms      = ts_calf_price_ms,
  fed_cattle_futures = ts_fed_price_future,
  corn_futures       = ts_corn_price_future,
  feeder_spot_federal        = ts_feeder_price_federal,
  fed_cattle_spot_sp   = ts_fed_price_sp,
  corn_spot_sp         = ts_corn_price_sp,
  feeder_spot_sp       = ts_feeder_price_sp,
  fed_cattle_spot_ms   = ts_fed_price_ms,
  corn_spot_ms         = ts_corn_price_ms,
  feeder_spot_ms       = ts_feeder_price_ms
)

# Convert time series object to data frame and create date variable
simulation_data <- as.data.frame(simulation_data)
simulation_data$date <- seq(
  from = as.Date("2014-01-01"),
  by = "month",
  length.out = nrow(simulation_data)
)

colnames(simulation_data)

simulation_data_lag <- simulation_data %>%
  arrange(date) %>%
  mutate(
    fed_cattle_t_futures   = fed_cattle_futures,
    feeder_t5_federal      = dplyr::lag(feeder_spot_federal, 5),
    calf_t5_sp             = dplyr::lag(calf_price_sp, 5),
    calf_t5_ims            = dplyr::lag(calf_price_ims, 5),
    calf_t5_ms             = dplyr::lag(calf_price_ms, 5),
    corn_t2_futures        = dplyr::lag(corn_futures, 2),
    
    fed_cattle_t_spot_sp     = fed_cattle_spot_sp,
    feeder_t5_spot_sp        = dplyr::lag(feeder_spot_sp, 5),
    corn_t2_spot_sp          = dplyr::lag(corn_spot_sp, 2),
    
    fed_cattle_t_spot_ms     = fed_cattle_spot_ms,
    feeder_t5_spot_ms        = dplyr::lag(feeder_spot_ms, 5),
    corn_t2_spot_ms          = dplyr::lag(corn_spot_ms, 2)
  ) %>%
  filter(
    !is.na(feeder_t5_federal),
    !is.na(calf_t5_sp),
    !is.na(calf_t5_ims),
    !is.na(calf_t5_ms),
    !is.na(corn_t2_futures),
    !is.na(corn_t2_spot_sp),
    !is.na(corn_t2_spot_ms),
    !is.na(feeder_t5_spot_sp),
    !is.na(feeder_t5_spot_ms)
  )

head(simulation_data_lag)

library(forecast)
library(rugarch)
library(urca)

fed_cattle_futures_ts <- ts(simulation_data_lag$fed_cattle_t_futures,
                            start = c(2014, 6), frequency = 12)

calf_t5_sp_ts <- ts(simulation_data_lag$calf_t5_sp,
                    start = c(2014, 6), frequency = 12)

calf_t5_ms_ts <- ts(simulation_data_lag$calf_t5_ms,
                    start = c(2014, 6), frequency = 12)

calf_t5_ims_ts <- ts(simulation_data_lag$calf_t5_ims,
                     start = c(2014, 6), frequency = 12)

corn_t2_futures_ts <- ts(simulation_data_lag$corn_t2_futures,
                         start = c(2014, 6), frequency = 12)

feeder_t5_federal_ts <- ts(simulation_data_lag$feeder_t5_federal,
                           start = c(2014, 6), frequency = 12)

fed_cattle_spot_sp_ts <- ts(simulation_data_lag$fed_cattle_t_spot_sp,
                          start = c(2014, 6), frequency = 12)

feeder_sp_spot_ts <- ts(simulation_data_lag$feeder_t5_spot_sp,
                      start = c(2014, 6), frequency = 12)

corn_sp_spot_ts <- ts(simulation_data_lag$corn_t2_spot_sp,
                    start = c(2014, 6), frequency = 12)

fed_cattle_spot_ms_ts <- ts(simulation_data_lag$fed_cattle_t_spot_ms,
                          start = c(2014, 6), frequency = 12)

feeder_spot_ms_ts <- ts(simulation_data_lag$feeder_t5_spot_ms,
                      start = c(2014, 6), frequency = 12)

corn_spot_ms_ts <- ts(simulation_data_lag$corn_t2_spot_ms,
                    start = c(2014, 6), frequency = 12)

# Augmented Dickey–Fuller tests (non-stationarity expected)

summary(ur.df(feeder_sp_spot_ts, type = "trend", lags = 1))
summary(ur.df(fed_cattle_spot_sp_ts, type = "trend", lags = 1))
summary(ur.df(corn_sp_spot_ts, type = "trend", lags = 1))
summary(ur.df(calf_t5_sp_ts, type = "trend", lags = 1))

summary(ur.df(feeder_spot_ms_ts, type = "trend", lags = 1))
summary(ur.df(fed_cattle_spot_ms_ts, type = "trend", lags = 1))
summary(ur.df(corn_spot_ms_ts, type = "trend", lags = 1))
summary(ur.df(calf_t5_ms_ts, type = "trend", lags = 1))

shapiro.test(fed_cattle_spot_sp_ts)
shapiro.test(corn_sp_spot_ts)
shapiro.test(feeder_sp_spot_ts)
shapiro.test(calf_t5_sp_ts)

shapiro.test(fed_cattle_spot_ms_ts)
shapiro.test(corn_spot_ms_ts)
shapiro.test(feeder_spot_ms_ts)
shapiro.test(calf_t5_ms_ts)

# Kendall rank correlation tests (evidence of dependence)

cor.test(fed_cattle_spot_sp_ts, corn_sp_spot_ts, method = "kendall")
cor.test(fed_cattle_spot_sp_ts, calf_t5_sp_ts, method = "kendall")
cor.test(corn_sp_spot_ts, calf_t5_sp_ts, method = "kendall")

cor.test(fed_cattle_spot_ms_ts, corn_spot_ms_ts, method = "kendall")
cor.test(fed_cattle_spot_ms_ts, calf_t5_ms_ts, method = "kendall")
cor.test(corn_spot_ms_ts, calf_t5_ms_ts, method = "kendall")

## Calf price – São Paulo

# Identification using first differences
acf(diff(calf_t5_sp_ts), lag.max = 115, col = "red", lwd = 2)
pacf(diff(calf_t5_sp_ts), lag.max = 115, col = "red", lwd = 2)
plot(diff(calf_t5_sp_ts))

# Zivot-Andrews unit root test with structural break
za_test <- ur.za(calf_t5_sp_ts, model = "both", lag = 12)
summary(za_test)
plot(za_test)

# Automatic ARIMA selection
auto.arima(calf_t5_sp_ts, seasonal = TRUE, trace = TRUE)

# Selected ARIMA specification
arima_calf_sp <- Arima(calf_t5_sp_ts, order = c(1, 1, 0))
summary(arima_calf_sp)

# Residual diagnostics
checkresiduals(arima_calf_sp)
residuals_calf_sp <- residuals(arima_calf_sp)

# Normality and autocorrelation diagnostics
shapiro.test(residuals_calf_sp)
Box.test(residuals_calf_sp, lag = 24, type = "Ljung-Box")
Box.test(residuals_calf_sp, lag = 10, type = "Ljung-Box")

# ARCH effects
library(FinTS)
ArchTest(residuals_calf_sp)

# Out-of-sample forecast evaluation (2024)
calf_2024_sp <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_calf_price_sp) %>%
  ts(start = c(2024, 1), frequency = 12)

forecast_sp <- forecast(arima_calf_sp, h = length(calf_2024_sp))
rmse <- sqrt(mean((calf_2024_sp - forecast_sp$mean)^2))
rmse

## Calf price – Mato Grosso do Sul

acf(diff(calf_t5_ms_ts), lag.max = 115, col = "red", lwd = 2)
pacf(diff(calf_t5_ms_ts), lag.max = 115, col = "red", lwd = 2)
plot(diff(calf_t5_ms_ts))

za_test <- ur.za(calf_t5_ms_ts, model = "both", lag = 12)
summary(za_test)
plot(za_test)

auto.arima(calf_t5_ms_ts, seasonal = TRUE, trace = TRUE)

arima_calf_ms <- Arima(calf_t5_ms_ts, order = c(1, 1, 0), seasonal = c(0, 0, 1))
summary(arima_calf_ms)

# Residual diagnostics
checkresiduals(arima_calf_ms)
residuals_calf_ms <- residuals(arima_calf_ms)

shapiro.test(residuals_calf_ms)
Box.test(residuals_calf_ms, lag = 24, type = "Ljung-Box")
Box.test(residuals_calf_ms, lag = 10, type = "Ljung-Box")
ArchTest(residuals_calf_ms)

# Forecast accuracy (2024)
calf_2024_ms <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_calf_price_ms) %>%
  ts(start = c(2024, 1), frequency = 12)

forecast_ms <- forecast(arima_calf_ms, h = length(calf_2024_ms))
rmse <- sqrt(mean((calf_2024_ms - forecast_ms$mean)^2))
rmse

## Fed cattle futures price (exchange-traded)

# Structural break dummy (October 2021)
break_dummy <- as.numeric(time(fed_cattle_futures_ts) == 2021 + 9/12)

za_test <- ur.za(fed_cattle_futures_ts, model = "both", lag = 12)
summary(za_test)
plot(za_test)

auto.arima(fed_cattle_futures_ts, seasonal = TRUE, trace = TRUE, xreg = break_dummy)

arima_fed_futures <- Arima(
  fed_cattle_futures_ts,
  order = c(2, 1, 2),
  xreg = break_dummy
)
summary(arima_fed_futures)

# Residual diagnostics
checkresiduals(arima_fed_futures)
residuals_fed_futures <- residuals(arima_fed_futures)

shapiro.test(residuals_fed_futures)
Box.test(residuals_fed_futures, lag = 24, type = "Ljung-Box")
Box.test(residuals_fed_futures, lag = 10, type = "Ljung-Box")
ArchTest(residuals_fed_futures)

# Forecast accuracy
fed_2024 <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_fed_price_future) %>%
  ts(start = c(2024, 1), frequency = 12)

forecast_fed <- forecast(
  arima_fed_futures,
  h = length(fed_2024),
  xreg = rep(0, length(fed_2024))
)

rmse <- sqrt(mean((fed_2024 - forecast_fed$mean)^2))
rmse

## Corn futures price (exchange-traded)
auto.arima(corn_t2_futures_ts,seasonal = TRUE,trace = TRUE)
arima_corn_future <- Arima(corn_t2_futures_ts, order = c(0, 1, 0)) 

summary(arima_corn_future)
checkresiduals(arima_corn_future)
residuals_corn_futures <- arima_corn_future$residuals
hist(residuals_corn_futures,freq=F)
qqnorm(residuals_corn_futures)
qqline(residuals_corn_futures)
acf(residuals_corn_futures, lag.max = 32, col="red", lwd=1)
pacf(residuals_corn_futures, lag.max = 32, col="red", lwd=1)
shapiro.test(residuals_corn_futures)
Box.test(residuals_corn_futures, lag = 24, type = "Ljung-Box")
Box.test(residuals_corn_futures, lag = 10, type = "Ljung-Box")

# Realize o teste ARCH nos resíduos
ArchTest(residuals_corn_futures)

corn_2024 <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_corn_price_future) %>%
  ts(start = c(2024,1), frequency = 12)
forecast_corn <- forecast(arima_corn_future, h = length(corn_2024))
rmse_corn <- sqrt(mean((corn_2024 - forecast_corn$mean)^2))
print(rmse_corn)

## ===============================
## Feeder cattle (boi magro) – São Paulo
## ===============================

# Identification
acf(diff(feeder_sp_spot_ts), lag.max = 115, col = "red", lwd = 2)
pacf(diff(feeder_sp_spot_ts), lag.max = 115, col = "red", lwd = 2)
plot(feeder_sp_spot_ts)

# Structural break test
za_test_feeder_sp <- ur.za(feeder_sp_spot_ts, model = "both", lag = 12)
summary(za_test_feeder_sp)
plot(za_test_feeder_sp)

# Automatic ARIMA selection
auto.arima(feeder_sp_spot_ts, seasonal = TRUE, trace = TRUE)

# Final model
arima_feeder_sp <- Arima(feeder_sp_spot_ts, order = c(2, 1, 2))
summary(arima_feeder_sp)

# Residual diagnostics
checkresiduals(arima_feeder_sp)
res_feeder_sp <- residuals(arima_feeder_sp)


shapiro.test(res_feeder_sp)
Box.test(res_feeder_sp, lag = 24, type = "Ljung-Box")
Box.test(res_feeder_sp, lag = 12, type = "Ljung-Box")
ArchTest(res_feeder_sp)

# Forecast evaluation (2024)
feeder_2024_sp <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_feeder_price_sp) %>%
  ts(start = c(2024, 1), frequency = 12)

fc_feeder_sp <- forecast(arima_feeder_sp, h = length(feeder_2024_sp))
rmse_feeder_sp <- sqrt(mean((feeder_2024_sp - fc_feeder_sp$mean)^2))
rmse_feeder_sp


## ===============================
## Feeder cattle (boi magro) – Mato Grosso do Sul
## ===============================

acf(diff(feeder_spot_ms_ts), lag.max = 115, col = "red", lwd = 2)
pacf(diff(feeder_spot_ms_ts), lag.max = 115, col = "red", lwd = 2)
plot(feeder_spot_ms_ts)

za_test_feeder_ms <- ur.za(feeder_spot_ms_ts, model = "both", lag = 12)
summary(za_test_feeder_ms)
plot(za_test_feeder_ms)

auto.arima(feeder_spot_ms_ts, seasonal = TRUE, trace = TRUE)

arima_feeder_ms <- Arima(feeder_spot_ms_ts, order = c(2, 1, 0))
summary(arima_feeder_ms)

checkresiduals(arima_feeder_ms)
res_feeder_ms <- residuals(arima_feeder_ms)

shapiro.test(res_feeder_ms)
Box.test(res_feeder_ms, lag = 24, type = "Ljung-Box")
Box.test(res_feeder_ms, lag = 10, type = "Ljung-Box")
ArchTest(res_feeder_ms)

feeder_2024_ms <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_feeder_price_ms) %>%
  ts(start = c(2024, 1), frequency = 12)

fc_feeder_ms <- forecast(arima_feeder_ms, h = length(feeder_2024_ms))
rmse_feeder_ms <- sqrt(mean((feeder_2024_ms - fc_feeder_ms$mean)^2))
rmse_feeder_ms

## ===============================
## Fed cattle (boi gordo) – São Paulo
## ===============================

acf(diff(fed_cattle_spot_sp_ts), lag.max = 115, col = "red", lwd = 2)
pacf(diff(fed_cattle_spot_sp_ts), lag.max = 115, col = "red", lwd = 2)
plot(fed_cattle_spot_sp_ts)

za_test_fed_sp <- ur.za(fed_cattle_spot_sp_ts, model = "both", lag = 12)
summary(za_test_fed_sp)
plot(za_test_fed_sp)

auto.arima(fed_cattle_spot_sp_ts, seasonal = TRUE, trace = TRUE)

arima_fed_sp <- Arima(fed_cattle_spot_sp_ts, order = c(1, 1, 1))
summary(arima_fed_sp)

checkresiduals(arima_fed_sp)
res_fed_sp <- residuals(arima_fed_sp)

shapiro.test(res_fed_sp)
Box.test(res_fed_sp, lag = 24, type = "Ljung-Box")
Box.test(res_fed_sp, lag = 10, type = "Ljung-Box")
ArchTest(res_fed_sp)

fed_2024_sp <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_fed_price_sp) %>%
  ts(start = c(2024, 1), frequency = 12)

fc_fed_sp <- forecast(arima_fed_sp, h = length(fed_2024_sp))
rmse_fed_sp <- sqrt(mean((fed_2024_sp - fc_fed_sp$mean)^2))
rmse_fed_sp


## ===============================
## Fed cattle (boi gordo) – Mato Grosso do Sul
## ===============================

acf(diff(fed_cattle_spot_ms_ts), lag.max = 115, col = "red", lwd = 2)
pacf(diff(fed_cattle_spot_ms_ts), lag.max = 115, col = "red", lwd = 2)
plot(fed_cattle_spot_ms_ts)

za_test_fed_ms <- ur.za(fed_cattle_spot_ms_ts, model = "both", lag = 12)
summary(za_test_fed_ms)
plot(za_test_fed_ms)

auto.arima(fed_cattle_spot_ms_ts, seasonal = TRUE, trace = TRUE)

arima_fed_ms <- Arima(fed_cattle_spot_ms_ts, order = c(1, 1, 1))
summary(arima_fed_ms)

checkresiduals(arima_fed_ms)
res_fed_ms <- residuals(arima_fed_ms)

shapiro.test(res_fed_ms)
Box.test(res_fed_ms, lag = 24, type = "Ljung-Box")
Box.test(res_fed_ms, lag = 10, type = "Ljung-Box")
ArchTest(res_fed_ms)

fed_2024_ms <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_fed_price_ms) %>%
  ts(start = c(2024, 1), frequency = 12)

fc_fed_ms <- forecast(arima_fed_ms, h = length(fed_2024_ms))
rmse_fed_ms <- sqrt(mean((fed_2024_ms - fc_fed_ms$mean)^2))
rmse_fed_ms

## ===============================
## Corn price – São Paulo
## ===============================

acf(diff(corn_sp_spot_ts), lag.max = 115, col = "red", lwd = 2)
pacf(diff(corn_sp_spot_ts), lag.max = 115, col = "red", lwd = 2)
plot(corn_sp_spot_ts)

auto.arima(corn_sp_spot_ts, seasonal = TRUE, trace = TRUE)

arima_corn_sp <- Arima(corn_sp_spot_ts, order = c(2, 1, 0))
summary(arima_corn_sp)

checkresiduals(arima_corn_sp)
res_corn_sp <- residuals(arima_corn_sp)

shapiro.test(res_corn_sp)
Box.test(res_corn_sp, lag = 24, type = "Ljung-Box")
Box.test(res_corn_sp, lag = 10, type = "Ljung-Box")
ArchTest(res_corn_sp)

corn_2024_sp <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_corn_price_sp) %>%
  ts(start = c(2024, 1), frequency = 12)

fc_corn_sp <- forecast(arima_corn_sp, h = length(corn_2024_sp))
rmse_corn_sp <- sqrt(mean((corn_2024_sp - fc_corn_sp$mean)^2))
rmse_corn_sp

## ===============================
## Corn price – Mato Grosso do Sul
## ===============================

acf(diff(corn_spot_ms_ts), lag.max = 115, col = "red", lwd = 2)
pacf(diff(corn_spot_ms_ts), lag.max = 115, col = "red", lwd = 2)
plot(corn_spot_ms_ts)

auto.arima(corn_spot_ms_ts, seasonal = TRUE, trace = TRUE)

arima_corn_ms <- Arima(corn_spot_ms_ts, order = c(0, 1, 1), seasonal = c(1, 0, 0))
summary(arima_corn_ms)

checkresiduals(arima_corn_ms)
res_corn_ms <- residuals(arima_corn_ms)

shapiro.test(res_corn_ms)
Box.test(res_corn_ms, lag = 24, type = "Ljung-Box")
Box.test(res_corn_ms, lag = 10, type = "Ljung-Box")
ArchTest(res_corn_ms)

corn_2024_ms <- price_data %>%
  filter(lubridate::year(month_year) == 2024) %>%
  arrange(month_year) %>%
  pull(real_corn_price_ms) %>%
  ts(start = c(2024, 1), frequency = 12)

fc_corn_ms <- forecast(arima_corn_ms, h = length(corn_2024_ms))
rmse_corn_ms <- sqrt(mean((corn_2024_ms - fc_corn_ms$mean)^2))
rmse_corn_ms


# ======================================
# Out-of-sample test (proxy performance federal)
# ======================================
## São Paulo
feeder_proxy_sp_federal_2024 <- intercept_sp_federal +
  coef_calf_sp_federal * log(calf_2024_sp) +
  coef_corn_sp_federal * corn_2024


# Root Mean Squared Error (RMSE)
rmse_sp <- sqrt(mean(
  (feeder_2024_sp - feeder_proxy_sp_federal_2024)^2,
  na.rm = TRUE
))
rmse_sp

# Mean Absolute Error (MAE)
mae_sp <- mean(
  abs(feeder_2024_sp - feeder_proxy_sp_federal_2024),
  na.rm = TRUE
)
mae_sp

# Bias (mean error)
bias_sp <- mean(
  feeder_2024_sp - feeder_proxy_sp_federal_2024,
  na.rm = TRUE
)
bias_sp

# Basis risk (standard deviation of the error)
sd(feeder_2024_sp - feeder_proxy_sp_federal_2024)

## Mato Grosso do Sul

feeder_proxy_ms_federal_2024 <- intercept_ms_federal +
  coef_calf_ms_federal * log(calf_2024_ms) +
  coef_corn_ms_federal * corn_2024

# RMSE
rmse_ms <- sqrt(mean(
  (feeder_2024_ms - feeder_proxy_ms_federal_2024)^2,
  na.rm = TRUE
))
rmse_ms

# MAE
mae_ms <- mean(
  abs(feeder_2024_ms - feeder_proxy_ms_federal_2024),
  na.rm = TRUE
)
mae_ms

# Bias
bias_ms <- mean(
  feeder_2024_ms - feeder_proxy_ms_federal_2024,
  na.rm = TRUE
)
bias_ms

# Basis risk
sd(feeder_2024_ms - feeder_proxy_ms_federal_2024)

# ======================================
# Out-of-sample test (proxy performance state)
# ======================================
## São Paulo
feeder_proxy_sp_state_2024 <- intercept_sp_state +
  coef_calf_sp_state * log(calf_2024_sp) +
  coef_corn_sp_state * corn_2024


# Root Mean Squared Error (RMSE)
rmse_sp <- sqrt(mean(
  (feeder_2024_sp - feeder_proxy_sp_state_2024)^2,
  na.rm = TRUE
))
rmse_sp

# Mean Absolute Error (MAE)
mae_sp <- mean(
  abs(feeder_2024_sp - feeder_proxy_sp_state_2024),
  na.rm = TRUE
)
mae_sp

# Bias (mean error)
bias_sp <- mean(
  feeder_2024_sp - feeder_proxy_sp_state_2024,
  na.rm = TRUE
)
bias_sp

# Basis risk (standard deviation of the error)
sd(feeder_2024_sp - feeder_proxy_sp_state_2024)

## Mato Grosso do Sul

feeder_proxy_ms_state_2024 <- intercept_ms_state +
  coef_calf_ms_state * log(calf_2024_ms) +
  coef_corn_ms_state * log(corn_2024)

# RMSE
rmse_ms <- sqrt(mean(
  (feeder_2024_ms - exp(feeder_proxy_ms_state_2024))^2,
  na.rm = TRUE
))
rmse_ms

# MAE
mae_ms <- mean(
  abs(feeder_2024_ms - exp(feeder_proxy_ms_state_2024)),
  na.rm = TRUE
)
mae_ms

# Bias
bias_ms <- mean(
  feeder_2024_ms - exp(feeder_proxy_ms_state_2024),
  na.rm = TRUE
)
bias_ms

# Basis risk
sd(feeder_2024_ms - exp(feeder_proxy_ms_state_2024))


# ======================================
# ARIMA forecasts
# Forecast for the next 6 months
# ======================================

calf_forecast_sp <- forecast(arima_calf_sp, h = 6)
price_calf_forecast_sp <- calf_forecast_sp$mean
price_calf_forecast_sp

calf_forecast_ms <- forecast(arima_calf_ms, h = 6)
price_calf_forecast_ms <- calf_forecast_ms$mean  # Forecasted values

price_calf_forecast_sp <- as.vector(price_calf_forecast_sp)
price_calf_forecast_ms <- as.vector(price_calf_forecast_ms)

# Assumed corn price path for the forecast horizon
p_corn_g <- rbind(70.28, 72.3, 74.31, 74.09, 73.86, 73.36)


# Forecasted feeder cattle price using projected calf and corn prices
feeder_proxy_sp_federal_forecast <- intercept_sp_federal +
  coef_calf_sp_federal * log(price_calf_forecast_sp) +
  coef_corn_sp_federal * p_corn_g

feeder_proxy_sp_federal_forecast

feeder_proxy_sp_state_forecast <- intercept_sp_state +
  coef_calf_sp_state * log(price_calf_forecast_sp) +
  coef_corn_sp_state * p_corn_g

feeder_proxy_sp_state_forecast

# ======================================
# Copula modeling
# ======================================

library(copula)
library(MASS)

# Fit distributions to log prices
fed_log_params <- fitdistr(log(fed_cattle_futures_ts), "normal")
corn_log_params <- fitdistr(log(corn_t2_futures_ts), "normal")

# Extract parameters (mean and standard deviation)
mu_fed_cattle <- fed_log_params$estimate[1]
sigma_fed_cattle <- fed_log_params$estimate[2]

mu_corn <- corn_log_params$estimate[1]
sigma_corn <- corn_log_params$estimate[2]

# Step 2: Transform residuals to uniform margins (pseudo-observations)
data_residuals_sp <- data.frame(
  residuals_calf_sp,
  residuals_fed_futures,
  residuals_corn_futures
)

data_residuals_sp <- pobs(data_residuals_sp)
correlation <- cor(data_residuals_sp, method = "spearman")
correlation

# Step 3: Copula estimation
# Fitting different copula families using maximum likelihood

copula_model1_f <- fitCopula(
  normalCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
)

copula_model2_f <- fitCopula(
  tCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
)

copula_model3_f <- fitCopula(
  claytonCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
)

copula_model4_f <- fitCopula(
  gumbelCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
) # Not suitable for negative dependence

copula_model5_f <- fitCopula(
  frankCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
)

# Model comparison
AIC(
  copula_model1_f,
  copula_model2_f,
  copula_model3_f,
  copula_model5_f
)

# ======================================================
# Step 4: Copula-based simulation of margins (São Paulo) - Federal
# ======================================================

set.seed(12345)

simulations_sp_federal <- replicate(12, {
  
  # Generate correlated residuals from the selected copula
  copula_samples <- rCopula(
    n = 5000,
    copula = copula_model3_f@copula
  )
  
  # Simulate fed cattle and corn prices (log-normal distributions)
  fed_cattle_sim <- exp(
    qnorm(copula_samples[, 2],
          mean = mu_fed_cattle,
          sd   = sigma_fed_cattle)
  )
  
  corn_price_sim <- exp(
    qnorm(copula_samples[, 3],
          mean = mu_corn,
          sd   = sigma_corn)
  )
  
  # Simulate calf price innovations from the copula
  calf_residuals_sim <- qnorm(copula_samples[, 1])
  
  # Simulate calf prices using the ARIMA model
  calf_price_sim_sp <- simulate(
    arima_calf_sp,
    nsim   = 5000,
    future = TRUE,
    innov  = calf_residuals_sim
  )
  
  # Feeder cattle price proxy (federal)
  feeder_price_proxy_sim <- intercept_sp_federal +
    coef_calf_sp_federal * log(calf_price_sim_sp) +
    coef_corn_sp_federal * corn_price_sim
  
  # Gross margin simulation
  gross_margin_sim <- fed_cattle_sim * 18 -
    feeder_price_proxy_sim -
    11 * corn_price_sim
  
  # Output
  list(
    gross_margin = gross_margin_sim,
    fed_cattle   = fed_cattle_sim,
    corn_price   = corn_price_sim,
    calf_price   = calf_price_sim_sp,
    feeder_price = feeder_price_proxy_sim
  )
  
}, simplify = FALSE)

# Extract simulated gross margins
gross_margin_future_sp <- sapply(
  simulations_sp_federal,
  function(x) x$gross_margin
)
gross_margin_future_sp <- data.frame(gross_margin_future_sp)

gross_margin_future_sp <- data.frame(
  gross_margin_future = unlist(gross_margin_future_sp),
  month = rep(1:12, each = length(gross_margin_future_sp[[1]]))
)

# Observed feeder cattle price (last months)
feeder_price_hist_sp <- intercept_sp_federal +
  coef_calf_sp_federal * log(ts_calf_price_sp) +
  coef_corn_sp_federal * ts_corn_price_future

feeder_price_hist_sp<-feeder_price_hist_sp[117:120]
feeder_price_hist_sp<-t(feeder_price_hist_sp)
feeder_price_hist_sp
feeder_proxy_sp_federal_forecast<-t(feeder_proxy_sp_federal_forecast)
feeder_proxy_sp_federal_forecast


# Forecasted feeder cattle price

feeder_price_guaranteed_sp <- rbind(
  feeder_price_hist_sp,
  feeder_proxy_sp_federal_forecast
)

# Assumed price paths
fed_price_guaranteed <- rbind(
  246.55, 244.70, 242.67, 242.93, 240.77, 241.73,
  250.22, 251.67, 261.00, 247.62
)

corn_price_guaranteed <- rbind(
  65.32, 70.28, 72.30, 74.31, 74.09,
  73.86, 73.36, 72.85, 72.36, 71.86
)

# Guaranteed margin
guaranteed_margin_sp <- (fed_price_guaranteed * 18) -
  feeder_price_guaranteed_sp -
  (11 * corn_price_guaranteed)

guaranteed_margin_sp<-rbind(NA,guaranteed_margin_sp, NA)
month<-c(1,2,3,4,5,6,7,8,9,10,11,12)
gm_sp<-cbind(guaranteed_margin_sp,month)
gm_sp
guaranteed_margin_df_sp <- as.data.frame(gm_sp)
colnames(guaranteed_margin_df_sp) <- c("guaranteed_margin_sp", "month")
guaranteed_margin_df_sp <- guaranteed_margin_df_sp[!is.na(guaranteed_margin_df_sp$guaranteed_margin_sp), ]

gross_margin_future_sp <- gross_margin_future_sp %>%
  left_join(guaranteed_margin_df_sp, by = "month") %>%
  mutate(
    indemnity = ifelse(
      gross_margin_future < guaranteed_margin_sp,
      guaranteed_margin_sp - gross_margin_future,
      0
    ),
    insured_event = ifelse(indemnity > 0, 1, 0)
  )


# ======================================================
# Spot margin simulation (realized margin)
# ======================================================

# Fit distributions to log prices
fed_log_params <- fitdistr(log(fed_cattle_spot_sp_ts), "normal")
corn_log_params <- fitdistr(log(corn_sp_spot_ts), "normal")
feeder_log_params_sp <- fitdistr(log(feeder_sp_spot_ts), "normal")

# Extract parameters (mean and standard deviation)
mu_fed_cattle <- fed_log_params$estimate[1]
sigma_fed_cattle <- fed_log_params$estimate[2]

mu_corn <- corn_log_params$estimate[1]
sigma_corn <- corn_log_params$estimate[2]

mu_feeder_sp <- feeder_log_params_sp$estimate[1]
sigma_feeder_sp <- feeder_log_params_sp$estimate[2]

# Step 2: Transform residuals to uniform margins (pseudo-observations)
data_residuals_sp <- data.frame(
  res_feeder_sp,
  res_fed_sp,
  res_corn_sp
)

data_residuals_sp <- pobs(data_residuals_sp)
correlation <- cor(data_residuals_sp, method = "spearman")
correlation

# Step 3: Copula estimation
# Fitting different copula families using maximum likelihood

copula_model1_s <- fitCopula(normalCopula(dim = 3), data_residuals_sp, method = "ml")
copula_model2_s <- fitCopula(tCopula(dim = 3, df.fixed = TRUE, df = 4), data_residuals_sp, method = "ml")
copula_model3_s <- fitCopula(claytonCopula(dim = 3), data_residuals_sp, method = "ml", start = c(0.1))
copula_model4_s <- fitCopula(gumbelCopula(dim = 3), data_residuals_sp, method = "ml", start = c(1.1))
copula_model5_s<- fitCopula(frankCopula(dim = 3), data_residuals_sp, method = "ml",start = c(0.1))
AIC(copula_model1_s, copula_model2_s,copula_model3_s)


set.seed(12345)

simulations_spot_sp <- replicate(12, {
  
  copula_samples <- rCopula(
    n = 5000,
    copula = copula_model1_s@copula
  )
  
  fed_cattle_sim <- exp(
    qnorm(copula_samples[, 2], mu_fed_cattle, sigma_fed_cattle)
  )
  
  corn_price_sim <- exp(
    qnorm(copula_samples[, 3], mu_corn, sigma_corn)
  )
  
  feeder_price_sim <- exp(
    qnorm(copula_samples[, 1], mu_feeder_sp, sigma_feeder_sp)
  )
  
  gross_margin_sim <- fed_cattle_sim * 18 -
    feeder_price_sim -
    11 * corn_price_sim
  
  list(gross_margin = gross_margin_sim)
  
}, simplify = FALSE)

gross_margin_spot_sp <- data.frame(
  gross_margin_spot = unlist(
    lapply(simulations_spot_sp, function(x) x$gross_margin)
  ),
  month = rep(1:12, each = length(simulations_spot_sp[[1]]$gross_margin))
)

gross_margin_spot_sp <- gross_margin_spot_sp %>%
  left_join(guaranteed_margin_df_sp, by = "month") %>%
  mutate(
    indemnity_real = ifelse(
      gross_margin_spot < guaranteed_margin_sp,
      guaranteed_margin_sp - gross_margin_spot,
      0
    ),
    loss_event = ifelse(indemnity_real > 0, 1, 0)
  )

deductibles <- c(0.00, 0.01, 0.05)

calculate_results_sp <- function(df_future, df_spot, deductible){
  
  df_future <- df_future %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  df_spot <- df_spot %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  trigger_level <- (1 - deductible)
  
  df <- df_future %>%
    dplyr::mutate(
      trigger = trigger_level * guaranteed_margin_sp,
      indemnity_future = ifelse(
        gross_margin_future < trigger,
        trigger - gross_margin_future, 0
      )
    ) %>%
    dplyr::select(month, id, indemnity_future, gross_margin_future) %>%
    dplyr::left_join(
      df_spot %>%
        dplyr::mutate(
          trigger = trigger_level * guaranteed_margin_sp,
          indemnity_spot = ifelse(
            gross_margin_spot < trigger,
            trigger - gross_margin_spot, 0
          )
        ) %>%
        dplyr::select(month, id, indemnity_spot),
      by = c("month", "id")
    ) %>%
    dplyr::mutate(
      false_positive = ifelse(indemnity_future > 0 & indemnity_spot == 0, 1, 0),
      false_negative = ifelse(indemnity_future == 0 & indemnity_spot > 0, 1, 0)
    )
  
  df %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      actuarial_premium = mean(indemnity_future),
      expected_margin  = mean(gross_margin_future),
      premium_ratio    = actuarial_premium / expected_margin,
      FPP = mean(false_positive),
      FNP = mean(false_negative),
      .groups = "drop"
    ) %>%
    dplyr::mutate(deductible = deductible)
}


results_deductibles_sp <- bind_rows(
  lapply(deductibles, function(d){
    calculate_results_sp(
      gross_margin_future_sp,
      gross_margin_spot_sp,
      d
    )
  })
)
results_deductibles_sp
write_xlsx(
  results_deductibles_sp,
  "results_deductibles_sp_federal.xlsx"
)


# ======================================================
# Step 5: Copula-based simulation of margins (São Paulo) - State
# ======================================================
# Fit distributions to log prices
fed_log_params <- fitdistr(log(fed_cattle_futures_ts), "normal")
corn_log_params <- fitdistr(log(corn_t2_futures_ts), "normal")

# Extract parameters (mean and standard deviation)
mu_fed_cattle <- fed_log_params$estimate[1]
sigma_fed_cattle <- fed_log_params$estimate[2]

mu_corn <- corn_log_params$estimate[1]
sigma_corn <- corn_log_params$estimate[2]

# Step 2: Transform residuals to uniform margins (pseudo-observations)
data_residuals_sp <- data.frame(
  residuals_calf_sp,
  residuals_fed_futures,
  residuals_corn_futures
)

data_residuals_sp <- pobs(data_residuals_sp)
correlation <- cor(data_residuals_sp, method = "spearman")
correlation

# Step 3: Copula estimation
# Fitting different copula families using maximum likelihood

copula_model1_f <- fitCopula(
  normalCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
)

copula_model2_f <- fitCopula(
  tCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
)

copula_model3_f <- fitCopula(
  claytonCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
)

copula_model4_f <- fitCopula(
  gumbelCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
) # Not suitable for negative dependence

copula_model5_f <- fitCopula(
  frankCopula(dim = 3),
  data_residuals_sp,
  method = "ml"
)

# Model comparison
AIC(
  copula_model1_f,
  copula_model2_f,
  copula_model3_f,
  copula_model5_f
)
set.seed(12345)

simulations_sp_estate <- replicate(12, {
  
  # Generate correlated residuals from the selected copula
  copula_samples <- rCopula(
    n = 5000,
    copula = copula_model3_f@copula
  )
  
  # Simulate fed cattle and corn prices (log-normal distributions)
  fed_cattle_sim <- exp(
    qnorm(copula_samples[, 2],
          mean = mu_fed_cattle,
          sd   = sigma_fed_cattle)
  )
  
  corn_price_sim <- exp(
    qnorm(copula_samples[, 3],
          mean = mu_corn,
          sd   = sigma_corn)
  )
  
  # Simulate calf price innovations from the copula
  calf_residuals_sim <- qnorm(copula_samples[, 1])
  
  # Simulate calf prices using the ARIMA model
  calf_price_sim_sp <- simulate(
    arima_calf_sp,
    nsim   = 5000,
    future = TRUE,
    innov  = calf_residuals_sim
  )
  
  # Feeder cattle price proxy (federal)
  feeder_price_proxy_sim <- intercept_sp_state +
    coef_calf_sp_state * log(calf_price_sim_sp) +
    coef_corn_sp_state * corn_price_sim
  
  # Gross margin simulation
  gross_margin_sim <- fed_cattle_sim * 18 -
    feeder_price_proxy_sim -
    11 * corn_price_sim
  
  # Output
  list(
    gross_margin = gross_margin_sim,
    fed_cattle   = fed_cattle_sim,
    corn_price   = corn_price_sim,
    calf_price   = calf_price_sim_sp,
    feeder_price = feeder_price_proxy_sim
  )
  
}, simplify = FALSE)

# Extract simulated gross margins
gross_margin_future_sp <- sapply(
  simulations_sp_estate,
  function(x) x$gross_margin
)
gross_margin_future_sp <- data.frame(gross_margin_future_sp)

gross_margin_future_sp <- data.frame(
  gross_margin_future = unlist(gross_margin_future_sp),
  month = rep(1:12, each = length(gross_margin_future_sp[[1]]))
)

# Observed feeder cattle price (last months)
feeder_price_hist_sp <- intercept_sp_state +
  coef_calf_sp_state * log(ts_calf_price_sp) +
  coef_corn_sp_state * ts_corn_price_future

feeder_price_hist_sp<-feeder_price_hist_sp[117:120]
feeder_price_hist_sp<-t(feeder_price_hist_sp)
feeder_price_hist_sp
feeder_proxy_sp_state_forecast<-t(feeder_proxy_sp_state_forecast)
feeder_proxy_sp_state_forecast


# Forecasted feeder cattle price

feeder_price_guaranteed_sp <- rbind(
  feeder_price_hist_sp,
  feeder_proxy_sp_state_forecast
)

# Assumed price paths
fed_price_guaranteed <- rbind(
  246.55, 244.70, 242.67, 242.93, 240.77, 241.73,
  250.22, 251.67, 261.00, 247.62
)

corn_price_guaranteed <- rbind(
  65.32, 70.28, 72.30, 74.31, 74.09,
  73.86, 73.36, 72.85, 72.36, 71.86
)

# Guaranteed margin
guaranteed_margin_sp <- (fed_price_guaranteed * 18) -
  feeder_price_guaranteed_sp -
  (11 * corn_price_guaranteed)

guaranteed_margin_sp<-rbind(NA,guaranteed_margin_sp, NA)
month<-c(1,2,3,4,5,6,7,8,9,10,11,12)
gm_sp<-cbind(guaranteed_margin_sp,month)
gm_sp
guaranteed_margin_df_sp <- as.data.frame(gm_sp)
colnames(guaranteed_margin_df_sp) <- c("guaranteed_margin_sp", "month")
guaranteed_margin_df_sp <- guaranteed_margin_df_sp[!is.na(guaranteed_margin_df_sp$guaranteed_margin_sp), ]

gross_margin_future_sp <- gross_margin_future_sp %>%
  left_join(guaranteed_margin_df_sp, by = "month") %>%
  mutate(
    indemnity = ifelse(
      gross_margin_future < guaranteed_margin_sp,
      guaranteed_margin_sp - gross_margin_future,
      0
    ),
    insured_event = ifelse(indemnity > 0, 1, 0)
  )

# ======================================================
# Spot margin simulation (realized margin)
# ======================================================

# Fit distributions to log prices
fed_log_params <- fitdistr(log(fed_cattle_spot_sp_ts), "normal")
corn_log_params <- fitdistr(log(corn_sp_spot_ts), "normal")
feeder_log_params_sp <- fitdistr(log(feeder_sp_spot_ts), "normal")

# Extract parameters (mean and standard deviation)
mu_fed_cattle <- fed_log_params$estimate[1]
sigma_fed_cattle <- fed_log_params$estimate[2]

mu_corn <- corn_log_params$estimate[1]
sigma_corn <- corn_log_params$estimate[2]

mu_feeder_sp <- feeder_log_params_sp$estimate[1]
sigma_feeder_sp <- feeder_log_params_sp$estimate[2]

# Step 2: Transform residuals to uniform margins (pseudo-observations)
data_residuals_sp <- data.frame(
  res_feeder_sp,
  res_fed_sp,
  res_corn_sp
)

data_residuals_sp <- pobs(data_residuals_sp)
correlation <- cor(data_residuals_sp, method = "spearman")
correlation

# Step 3: Copula estimation
# Fitting different copula families using maximum likelihood

copula_model1_s <- fitCopula(normalCopula(dim = 3), data_residuals_sp, method = "ml")
copula_model2_s <- fitCopula(tCopula(dim = 3, df.fixed = TRUE, df = 4), data_residuals_sp, method = "ml")
copula_model3_s <- fitCopula(claytonCopula(dim = 3), data_residuals_sp, method = "ml", start = c(0.1))
copula_model4_s <- fitCopula(gumbelCopula(dim = 3), data_residuals_sp, method = "ml", start = c(1.1))
copula_model5_s<- fitCopula(frankCopula(dim = 3), data_residuals_sp, method = "ml",start = c(0.1))
AIC(copula_model1_s, copula_model2_s,copula_model3_s)


set.seed(12345)

simulations_spot_sp <- replicate(12, {
  
  copula_samples <- rCopula(
    n = 5000,
    copula = copula_model1_s@copula
  )
  
  fed_cattle_sim <- exp(
    qnorm(copula_samples[, 2], mu_fed_cattle, sigma_fed_cattle)
  )
  
  corn_price_sim <- exp(
    qnorm(copula_samples[, 3], mu_corn, sigma_corn)
  )
  
  feeder_price_sim <- exp(
    qnorm(copula_samples[, 1], mu_feeder_sp, sigma_feeder_sp)
  )
  
  gross_margin_sim <- fed_cattle_sim * 18 -
    feeder_price_sim -
    11 * corn_price_sim
  
  list(gross_margin = gross_margin_sim)
  
}, simplify = FALSE)

gross_margin_spot_sp <- data.frame(
  gross_margin_spot = unlist(
    lapply(simulations_spot_sp, function(x) x$gross_margin)
  ),
  month = rep(1:12, each = length(simulations_spot_sp[[1]]$gross_margin))
)

gross_margin_spot_sp <- gross_margin_spot_sp %>%
  left_join(guaranteed_margin_df_sp, by = "month") %>%
  mutate(
    indemnity_real = ifelse(
      gross_margin_spot < guaranteed_margin_sp,
      guaranteed_margin_sp - gross_margin_spot,
      0
    ),
    loss_event = ifelse(indemnity_real > 0, 1, 0)
  )

deductibles <- c(0.00, 0.01, 0.05)

calculate_results_sp <- function(df_future, df_spot, deductible){
  
  df_future <- df_future %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  df_spot <- df_spot %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  trigger_level <- (1 - deductible)
  
  df <- df_future %>%
    dplyr::mutate(
      trigger = trigger_level * guaranteed_margin_sp,
      indemnity_future = ifelse(
        gross_margin_future < trigger,
        trigger - gross_margin_future, 0
      )
    ) %>%
    dplyr::select(month, id, indemnity_future, gross_margin_future) %>%
    dplyr::left_join(
      df_spot %>%
        dplyr::mutate(
          trigger = trigger_level * guaranteed_margin_sp,
          indemnity_spot = ifelse(
            gross_margin_spot < trigger,
            trigger - gross_margin_spot, 0
          )
        ) %>%
        dplyr::select(month, id, indemnity_spot),
      by = c("month", "id")
    ) %>%
    dplyr::mutate(
      false_positive = ifelse(indemnity_future > 0 & indemnity_spot == 0, 1, 0),
      false_negative = ifelse(indemnity_future == 0 & indemnity_spot > 0, 1, 0)
    )
  
  df %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      actuarial_premium = mean(indemnity_future),
      expected_margin  = mean(gross_margin_future),
      premium_ratio    = actuarial_premium / expected_margin,
      FPP = mean(false_positive),
      FNP = mean(false_negative),
      .groups = "drop"
    ) %>%
    dplyr::mutate(deductible = deductible)
}


results_deductibles_sp <- bind_rows(
  lapply(deductibles, function(d){
    calculate_results_sp(
      gross_margin_future_sp,
      gross_margin_spot_sp,
      d
    )
  })
)
results_deductibles_sp
write_xlsx(
  results_deductibles_sp,
  "results_deductibles_sp_state.xlsx"
)

# ======================================
# Mato grosso do sul
# ======================================

# Forecasted feeder cattle price using projected calf and corn prices
feeder_proxy_ms_federal_forecast <- intercept_ms_federal +
  coef_calf_ms_federal * log(price_calf_forecast_ms) +
  coef_corn_ms_federal * p_corn_g

feeder_proxy_ms_federal_forecast

feeder_proxy_ms_state_forecast <- intercept_ms_state +
  coef_calf_ms_state * log(price_calf_forecast_ms) +
  coef_corn_ms_state * p_corn_g

feeder_proxy_ms_state_forecast

# Assumed corn price path for the forecast horizon
p_corn_g <- rbind(70.28, 72.3, 74.31, 74.09, 73.86, 73.36)


# ======================================
# Copula modeling
# ======================================

library(copula)
library(MASS)

# Fit distributions to log prices
fed_log_params <- fitdistr(log(fed_cattle_futures_ts), "normal")
corn_log_params <- fitdistr(log(corn_t2_futures_ts), "normal")

# Extract parameters (mean and standard deviation)
mu_fed_cattle <- fed_log_params$estimate[1]
sigma_fed_cattle <- fed_log_params$estimate[2]

mu_corn <- corn_log_params$estimate[1]
sigma_corn <- corn_log_params$estimate[2]

# Step 2: Transform residuals to uniform margins (pseudo-observations)
data_residuals_ms <- data.frame(
  residuals_calf_ms,
  residuals_fed_futures,
  residuals_corn_futures
)

data_residuals_ms <- pobs(data_residuals_ms)
correlation <- cor(data_residuals_ms, method = "spearman")
correlation

# Step 3: Copula estimation
# Fitting different copula families using maximum likelihood

copula_model1_f <- fitCopula(
  normalCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
)

copula_model2_f <- fitCopula(
  tCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
)

copula_model3_f <- fitCopula(
  claytonCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
)

copula_model4_f <- fitCopula(
  gumbelCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
) # Not suitable for negative dependence

copula_model5_f <- fitCopula(
  frankCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
)

# Model comparison
AIC(
  copula_model1_f,
  copula_model2_f,
  copula_model3_f,
  copula_model5_f
)

# ======================================================
# Step 4: Copula-based simulation of margins (Mato Grosso Do Sul) - Federal
# ======================================================

set.seed(12345)

simulations_ms_federal <- replicate(12, {
  
  # Generate correlated residuals from the selected copula
  copula_samples <- rCopula(
    n = 5000,
    copula = copula_model5_f@copula
  )
  
  # Simulate fed cattle and corn prices (log-normal distributions)
  fed_cattle_sim <- exp(
    qnorm(copula_samples[, 2],
          mean = mu_fed_cattle,
          sd   = sigma_fed_cattle)
  )
  
  corn_price_sim <- exp(
    qnorm(copula_samples[, 3],
          mean = mu_corn,
          sd   = sigma_corn)
  )
  
  # Simulate calf price innovations from the copula
  calf_residuals_sim <- qnorm(copula_samples[, 1])
  
  # Simulate calf prices using the ARIMA model
  calf_price_sim_ms <- simulate(
    arima_calf_ms,
    nsim   = 5000,
    future = TRUE,
    innov  = calf_residuals_sim
  )
  
  # Feeder cattle price proxy (federal)
  feeder_price_proxy_sim <- intercept_ms_federal +
    coef_calf_ms_federal * log(calf_price_sim_ms) +
    coef_corn_ms_federal * corn_price_sim
  
  # Gross margin simulation
  gross_margin_sim <- fed_cattle_sim * 18 -
    feeder_price_proxy_sim -
    11 * corn_price_sim
  
  # Output
  list(
    gross_margin = gross_margin_sim,
    fed_cattle   = fed_cattle_sim,
    corn_price   = corn_price_sim,
    calf_price   = calf_price_sim_ms,
    feeder_price = feeder_price_proxy_sim
  )
  
}, simplify = FALSE)

# Extract simulated gross margins
gross_margin_future_ms <- sapply(
  simulations_ms_federal,
  function(x) x$gross_margin
)
gross_margin_future_ms <- data.frame(gross_margin_future_ms)

gross_margin_future_ms <- data.frame(
  gross_margin_future = unlist(gross_margin_future_ms),
  month = rep(1:12, each = length(gross_margin_future_ms[[1]]))
)

# Observed feeder cattle price (last months)
feeder_price_hist_ms <- intercept_ms_federal +
  coef_calf_ms_federal * log(ts_calf_price_ms) +
  coef_corn_ms_federal * ts_corn_price_future

feeder_price_hist_ms<-feeder_price_hist_ms[117:120]
feeder_price_hist_ms<-t(feeder_price_hist_ms)
feeder_price_hist_ms
feeder_proxy_ms_federal_forecast<-t(feeder_proxy_ms_federal_forecast)
feeder_proxy_ms_federal_forecast


# Forecasted feeder cattle price

feeder_price_guaranteed_ms <- rbind(
  feeder_price_hist_ms,
  feeder_proxy_ms_federal_forecast
)

# Assumed price paths
fed_price_guaranteed <- rbind(
  246.55, 244.70, 242.67, 242.93, 240.77, 241.73,
  250.22, 251.67, 261.00, 247.62
)

corn_price_guaranteed <- rbind(
  65.32, 70.28, 72.30, 74.31, 74.09,
  73.86, 73.36, 72.85, 72.36, 71.86
)

# Guaranteed margin
guaranteed_margin_ms <- (fed_price_guaranteed * 18) -
  feeder_price_guaranteed_ms -
  (11 * corn_price_guaranteed)

guaranteed_margin_ms<-rbind(NA,guaranteed_margin_ms, NA)
month<-c(1,2,3,4,5,6,7,8,9,10,11,12)
gm_ms<-cbind(guaranteed_margin_ms,month)
gm_ms
guaranteed_margin_df_ms <- as.data.frame(gm_ms)
colnames(guaranteed_margin_df_ms) <- c("guaranteed_margin_ms", "month")
guaranteed_margin_df_ms <- guaranteed_margin_df_ms[!is.na(guaranteed_margin_df_ms$guaranteed_margin_ms), ]

gross_margin_future_ms <- gross_margin_future_ms %>%
  left_join(guaranteed_margin_df_ms, by = "month") %>%
  mutate(
    indemnity = ifelse(
      gross_margin_future < guaranteed_margin_ms,
      guaranteed_margin_ms - gross_margin_future,
      0
    ),
    insured_event = ifelse(indemnity > 0, 1, 0)
  )


# ======================================================
# Spot margin simulation (realized margin)
# ======================================================

# Fit distributions to log prices
fed_log_params <- fitdistr(log(fed_cattle_spot_ms_ts), "normal")
corn_log_params <- fitdistr(log(corn_ms_spot_ts), "normal")
feeder_log_params_ms <- fitdistr(log(feeder_ms_spot_ts), "normal")

# Extract parameters (mean and standard deviation)
mu_fed_cattle <- fed_log_params$estimate[1]
sigma_fed_cattle <- fed_log_params$estimate[2]

mu_corn <- corn_log_params$estimate[1]
sigma_corn <- corn_log_params$estimate[2]

mu_feeder_ms <- feeder_log_params_ms$estimate[1]
sigma_feeder_ms <- feeder_log_params_ms$estimate[2]

# Step 2: Transform residuals to uniform margins (pseudo-observations)
data_residuals_ms <- data.frame(
  res_feeder_ms,
  res_fed_ms,
  res_corn_ms
)

data_residuals_ms <- pobs(data_residuals_ms)
correlation <- cor(data_residuals_ms, method = "spearman")
correlation

# Step 3: Copula estimation
# Fitting different copula families using maximum likelihood

copula_model1_s <- fitCopula(normalCopula(dim = 3), data_residuals_ms, method = "ml")
copula_model2_s <- fitCopula(tCopula(dim = 3, df.fixed = TRUE, df = 4), data_residuals_ms, method = "ml")
copula_model3_s <- fitCopula(claytonCopula(dim = 3), data_residuals_ms, method = "ml", start = c(0.1))
copula_model4_s <- fitCopula(gumbelCopula(dim = 3), data_residuals_ms, method = "ml", start = c(1.1))
copula_model5_s<- fitCopula(frankCopula(dim = 3), data_residuals_ms, method = "ml",start = c(0.1))
AIC(copula_model1_s, copula_model2_s, copula_model3_s, copula_model5_s)


set.seed(12345)

simulations_spot_ms <- replicate(12, {
  
  copula_samples <- rCopula(
    n = 5000,
    copula = copula_model3_s@copula
  )
  
  fed_cattle_sim <- exp(
    qnorm(copula_samples[, 2], mu_fed_cattle, sigma_fed_cattle)
  )
  
  corn_price_sim <- exp(
    qnorm(copula_samples[, 3], mu_corn, sigma_corn)
  )
  
  feeder_price_sim <- exp(
    qnorm(copula_samples[, 1], mu_feeder_ms, sigma_feeder_ms)
  )
  
  gross_margin_sim <- fed_cattle_sim * 18 -
    feeder_price_sim -
    11 * corn_price_sim
  
  list(gross_margin = gross_margin_sim)
  
}, simplify = FALSE)

gross_margin_spot_ms <- data.frame(
  gross_margin_spot = unlist(
    lapply(simulations_spot_ms, function(x) x$gross_margin)
  ),
  month = rep(1:12, each = length(simulations_spot_ms[[1]]$gross_margin))
)

gross_margin_spot_ms <- gross_margin_spot_ms %>%
  left_join(guaranteed_margin_df_ms, by = "month") %>%
  mutate(
    indemnity_real = ifelse(
      gross_margin_spot < guaranteed_margin_ms,
      guaranteed_margin_ms - gross_margin_spot,
      0
    ),
    loss_event = ifelse(indemnity_real > 0, 1, 0)
  )

deductibles <- c(0.00, 0.01, 0.05)

calculate_results_ms <- function(df_future, df_spot, deductible){
  
  df_future <- df_future %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  df_spot <- df_spot %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  trigger_level <- (1 - deductible)
  
  df <- df_future %>%
    dplyr::mutate(
      trigger = trigger_level * guaranteed_margin_ms,
      indemnity_future = ifelse(
        gross_margin_future < trigger,
        trigger - gross_margin_future, 0
      )
    ) %>%
    dplyr::select(month, id, indemnity_future, gross_margin_future) %>%
    dplyr::left_join(
      df_spot %>%
        dplyr::mutate(
          trigger = trigger_level * guaranteed_margin_ms,
          indemnity_spot = ifelse(
            gross_margin_spot < trigger,
            trigger - gross_margin_spot, 0
          )
        ) %>%
        dplyr::select(month, id, indemnity_spot),
      by = c("month", "id")
    ) %>%
    dplyr::mutate(
      false_positive = ifelse(indemnity_future > 0 & indemnity_spot == 0, 1, 0),
      false_negative = ifelse(indemnity_future == 0 & indemnity_spot > 0, 1, 0)
    )
  
  df %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      actuarial_premium = mean(indemnity_future),
      expected_margin  = mean(gross_margin_future),
      premium_ratio    = actuarial_premium / expected_margin,
      FPP = mean(false_positive),
      FNP = mean(false_negative),
      .groups = "drop"
    ) %>%
    dplyr::mutate(deductible = deductible)
}


results_deductibles_ms <- bind_rows(
  lapply(deductibles, function(d){
    calculate_results_ms(
      gross_margin_future_ms,
      gross_margin_spot_ms,
      d
    )
  })
)
results_deductibles_ms
write_xlsx(
  results_deductibles_ms,
  "results_deductibles_ms_federal.xlsx"
)

# ======================================================
# Step 5: Copula-based simulation of margins Mato Grosso do Sul - State
# ======================================================

# Fit distributions to log prices
fed_log_params <- fitdistr(log(fed_cattle_futures_ts), "normal")
corn_log_params <- fitdistr(log(corn_t2_futures_ts), "normal")

# Extract parameters (mean and standard deviation)
mu_fed_cattle <- fed_log_params$estimate[1]
sigma_fed_cattle <- fed_log_params$estimate[2]

mu_corn <- corn_log_params$estimate[1]
sigma_corn <- corn_log_params$estimate[2]

# Step 2: Transform residuals to uniform margins (pseudo-observations)
data_residuals_ms <- data.frame(
  residuals_calf_ms,
  residuals_fed_futures,
  residuals_corn_futures
)

data_residuals_ms <- pobs(data_residuals_ms)
correlation <- cor(data_residuals_ms, method = "spearman")
correlation

# Step 3: Copula estimation
# Fitting different copula families using maximum likelihood

copula_model1_f <- fitCopula(
  normalCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
)

copula_model2_f <- fitCopula(
  tCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
)

copula_model3_f <- fitCopula(
  claytonCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
)

copula_model4_f <- fitCopula(
  gumbelCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
) # Not suitable for negative dependence

copula_model5_f <- fitCopula(
  frankCopula(dim = 3),
  data_residuals_ms,
  method = "ml"
)

# Model comparison
AIC(
  copula_model1_f,
  copula_model2_f,
  copula_model3_f,
  copula_model5_f
)

set.seed(12345)

simulations_ms_state <- replicate(12, {
  
  # Generate correlated residuals from the selected copula
  copula_samples <- rCopula(
    n = 5000,
    copula = copula_model5_f@copula
  )
  
  # Simulate fed cattle and corn prices (log-normal distributions)
  fed_cattle_sim <- exp(
    qnorm(copula_samples[, 2],
          mean = mu_fed_cattle,
          sd   = sigma_fed_cattle)
  )
  
  corn_price_sim <- exp(
    qnorm(copula_samples[, 3],
          mean = mu_corn,
          sd   = sigma_corn)
  )
  
  # Simulate calf price innovations from the copula
  calf_residuals_sim <- qnorm(copula_samples[, 1])
  
  # Simulate calf prices using the ARIMA model
  calf_price_sim_ms <- simulate(
    arima_calf_ms,
    nsim   = 5000,
    future = TRUE,
    innov  = calf_residuals_sim
  )
  
  # Feeder cattle price proxy (federal)
  feeder_price_proxy_sim <- intercept_ms_state +
    coef_calf_ms_state * log(calf_price_sim_ms) +
    coef_corn_ms_state * log(corn_price_sim)
  
  feeder_price_proxy_sim <- exp(feeder_price_proxy_sim)
  # Gross margin simulation
  gross_margin_sim <- fed_cattle_sim * 18 -
    feeder_price_proxy_sim -
    11 * corn_price_sim
  
  # Output
  list(
    gross_margin = gross_margin_sim,
    fed_cattle   = fed_cattle_sim,
    corn_price   = corn_price_sim,
    calf_price   = calf_price_sim_ms,
    feeder_price = feeder_price_proxy_sim
  )
  
}, simplify = FALSE)

# Extract simulated gross margins
gross_margin_future_ms <- sapply(
  simulations_ms_estate,
  function(x) x$gross_margin
)
gross_margin_future_ms <- data.frame(gross_margin_future_ms)

gross_margin_future_ms <- data.frame(
  gross_margin_future = unlist(gross_margin_future_ms),
  month = rep(1:12, each = length(gross_margin_future_ms[[1]]))
)

# Observed feeder cattle price (last months)
feeder_price_hist_ms <- intercept_ms_state +
  coef_calf_ms_state * log(ts_calf_price_ms) +
  coef_corn_ms_state * log(ts_corn_price_future)

feeder_price_hist_ms<-exp(feeder_price_hist_ms[117:120])
feeder_price_hist_ms<-t(feeder_price_hist_ms)
feeder_price_hist_ms
feeder_proxy_ms_state_forecast<-t(feeder_proxy_ms_state_forecast)
feeder_proxy_ms_state_forecast


# Forecasted feeder cattle price

feeder_price_guaranteed_ms <- rbind(
  feeder_price_hist_ms,
  feeder_proxy_ms_state_forecast
)

# Assumed price paths
fed_price_guaranteed <- rbind(
  246.55, 244.70, 242.67, 242.93, 240.77, 241.73,
  250.22, 251.67, 261.00, 247.62
)

corn_price_guaranteed <- rbind(
  65.32, 70.28, 72.30, 74.31, 74.09,
  73.86, 73.36, 72.85, 72.36, 71.86
)

# Guaranteed margin
guaranteed_margin_ms <- (fed_price_guaranteed * 18) -
  feeder_price_guaranteed_ms -
  (11 * corn_price_guaranteed)

guaranteed_margin_ms<-rbind(NA,guaranteed_margin_ms, NA)
month<-c(1,2,3,4,5,6,7,8,9,10,11,12)
gm_ms<-cbind(guaranteed_margin_ms,month)
gm_ms
guaranteed_margin_df_ms <- as.data.frame(gm_sp)
colnames(guaranteed_margin_df_ms) <- c("guaranteed_margin_sp", "month")
guaranteed_margin_df_ms <- guaranteed_margin_df_ms[!is.na(guaranteed_margin_df_ms$guaranteed_margin_ms), ]

gross_margin_future_ms <- gross_margin_future_ms %>%
  left_join(guaranteed_margin_df_ms, by = "month") %>%
  mutate(
    indemnity = ifelse(
      gross_margin_future < guaranteed_margin_ms,
      guaranteed_margin_ms - gross_margin_future,
      0
    ),
    insured_event = ifelse(indemnity > 0, 1, 0)
  )

# ======================================================
# Spot margin simulation (realized margin)
# ======================================================

# Fit distributions to log prices
fed_log_params <- fitdistr(log(fed_cattle_spot_ms_ts), "normal")
corn_log_params <- fitdistr(log(corn_ms_spot_ts), "normal")
feeder_log_params_ms <- fitdistr(log(feeder_ms_spot_ts), "normal")

# Extract parameters (mean and standard deviation)
mu_fed_cattle <- fed_log_params$estimate[1]
sigma_fed_cattle <- fed_log_params$estimate[2]

mu_corn <- corn_log_params$estimate[1]
sigma_corn <- corn_log_params$estimate[2]

mu_feeder_ms <- feeder_log_params_ms$estimate[1]
sigma_feeder_ms <- feeder_log_params_ms$estimate[2]

# Step 2: Transform residuals to uniform margins (pseudo-observations)
data_residuals_ms <- data.frame(
  res_feeder_ms,
  res_fed_ms,
  res_corn_ms
)

data_residuals_ms <- pobs(data_residuals_ms)
correlation <- cor(data_residuals_ms, method = "spearman")
correlation

# Step 3: Copula estimation
# Fitting different copula families using maximum likelihood

copula_model1_s <- fitCopula(normalCopula(dim = 3), data_residuals_ms, method = "ml")
copula_model2_s <- fitCopula(tCopula(dim = 3, df.fixed = TRUE, df = 4), data_residuals_ms, method = "ml")
copula_model3_s <- fitCopula(claytonCopula(dim = 3), data_residuals_ms, method = "ml", start = c(0.1))
copula_model4_s <- fitCopula(gumbelCopula(dim = 3), data_residuals_ms, method = "ml", start = c(1.1))
copula_model5_s<- fitCopula(frankCopula(dim = 3), data_residuals_ms, method = "ml",start = c(0.1))
AIC(copula_model1_s, copula_model2_s, copula_model3_s, copula_model5_s)


set.seed(12345)

simulations_spot_ms <- replicate(12, {
  
  copula_samples <- rCopula(
    n = 5000,
    copula = copula_model1_s@copula
  )
  
  fed_cattle_sim <- exp(
    qnorm(copula_samples[, 2], mu_fed_cattle, sigma_fed_cattle)
  )
  
  corn_price_sim <- exp(
    qnorm(copula_samples[, 3], mu_corn, sigma_corn)
  )
  
  feeder_price_sim <- exp(
    qnorm(copula_samples[, 1], mu_feeder_ms, sigma_feeder_ms)
  )
  
  gross_margin_sim <- fed_cattle_sim * 18 -
    feeder_price_sim -
    11 * corn_price_sim
  
  list(gross_margin = gross_margin_sim)
  
}, simplify = FALSE)

gross_margin_spot_ms <- data.frame(
  gross_margin_spot = unlist(
    lapply(simulations_spot_ms, function(x) x$gross_margin)
  ),
  month = rep(1:12, each = length(simulations_spot_ms[[1]]$gross_margin))
)

gross_margin_spot_ms <- gross_margin_spot_ms %>%
  left_join(guaranteed_margin_df_ms, by = "month") %>%
  mutate(
    indemnity_real = ifelse(
      gross_margin_spot < guaranteed_margin_ms,
      guaranteed_margin_ms - gross_margin_spot,
      0
    ),
    loss_event = ifelse(indemnity_real > 0, 1, 0)
  )

deductibles <- c(0.00, 0.01, 0.05)

calculate_results_ms <- function(df_future, df_spot, deductible){
  
  df_future <- df_future %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  df_spot <- df_spot %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  trigger_level <- (1 - deductible)
  
  df <- df_future %>%
    dplyr::mutate(
      trigger = trigger_level * guaranteed_margin_ms,
      indemnity_future = ifelse(
        gross_margin_future < trigger,
        trigger - gross_margin_future, 0
      )
    ) %>%
    dplyr::select(month, id, indemnity_future, gross_margin_future) %>%
    dplyr::left_join(
      df_spot %>%
        dplyr::mutate(
          trigger = trigger_level * guaranteed_margin_ms,
          indemnity_spot = ifelse(
            gross_margin_spot < trigger,
            trigger - gross_margin_spot, 0
          )
        ) %>%
        dplyr::select(month, id, indemnity_spot),
      by = c("month", "id")
    ) %>%
    dplyr::mutate(
      false_positive = ifelse(indemnity_future > 0 & indemnity_spot == 0, 1, 0),
      false_negative = ifelse(indemnity_future == 0 & indemnity_spot > 0, 1, 0)
    )
  
  df %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      actuarial_premium = mean(indemnity_future),
      expected_margin  = mean(gross_margin_future),
      premium_ratio    = actuarial_premium / expected_margin,
      FPP = mean(false_positive),
      FNP = mean(false_negative),
      .groups = "drop"
    ) %>%
    dplyr::mutate(deductible = deductible)
}


results_deductibles_ms <- bind_rows(
  lapply(deductibles, function(d){
    calculate_results_ms(
      gross_margin_future_ms,
      gross_margin_spot_ms,
      d
    )
  })
)
results_deductibles_ms
write_xlsx(
  results_deductibles_ms,
  "results_deductibles_ms_state.xlsx"
)