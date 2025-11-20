library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1) PARÁMETROS DEL MODELO
# ------------------------------------------------------------

params <- list(
  A_F = 1,
  A_I = 1.25,
  alpha = 0.7,
  gamma = 0.3,
  C = ((1-alpha)*A_F)*((1-gamma)*A_I)^(-1),
  kappa = 0,
  beta = 0.7
)

# ------------------------------------------------------------
# 2) FUNCIONES DEL MODELO
# ------------------------------------------------------------

l_exact <- function(h, alpha, gamma, C){
  target <- function(l) l^alpha / ((1-l)^gamma) - C*h^(alpha-gamma)
  eps <- 1e-9
  f_lo <- target(eps)
  f_hi <- target(1-eps)
  
  if(f_lo * f_hi > 0){
    vals <- c(eps, 1-eps)
    res <- sapply(vals, function(l) abs(target(l)))
    return(vals[which.min(res)])
  }
  
  uniroot(target, lower = eps, upper = 1 - eps)$root
}

y_c_exact <- function(h, A_F, A_I, alpha, gamma, kappa, C){
  l <- l_exact(h, alpha, gamma, C)
  A_F*h^alpha * l^(1-alpha) +
    A_I*h^gamma * (1-l)^(1-gamma) - kappa
}

U_p <- function(h, y_p, beta, A_F, A_I, alpha, gamma, kappa, C){
  if(h <= 0 || h >= y_p) return(-Inf)
  log(y_p - h) + beta * y_c_exact(h, A_F, A_I, alpha, gamma, kappa, C)
}

h_opt_parent <- function(y_p, params){
  optimize(
    f = function(h) U_p(
      h, y_p,
      params$beta, params$A_F, params$A_I,
      params$alpha, params$gamma,
      params$kappa, params$C
    ),
    interval = c(1e-8, y_p - 1e-8),
    maximum = TRUE
  )$maximum
}

# ------------------------------------------------------------
# 3) SIMULAR INGRESO DEL PADRE (log-normal)
# ------------------------------------------------------------

set.seed(123)
n <- 10000
sigma <- 0.7
mu = 1
y_p <- rlnorm(n, meanlog = mu, sdlog = sigma)

# ------------------------------------------------------------
# 4) CALCULAR h*, l(h*) y los ingresos del hijo
# ------------------------------------------------------------

h_star <- sapply(y_p, h_opt_parent, params = params)
l_star <- sapply(h_star, l_exact,
                 alpha = params$alpha,
                 gamma = params$gamma,
                 C = params$C)

# INGRESOS SIN RUIDO
y_formal <- params$A_F * h_star^params$alpha * l_star^(1 - params$alpha)
y_informal <- params$A_I * h_star^params$gamma * (1 - l_star)^(1 - params$gamma)

# ----------------------------------------------
#  🔥  AQUI AGREGO EL RUIDO QUE PEDISTE
# ----------------------------------------------

# ruido informal: mayor varianza
ruido_informal <- rnorm(n, mean = 0, sd = 0.015 )   ### <-- AQUI

# ruido formal: menor varianza
ruido_formal   <- rnorm(n, mean = 0, sd = 0.01 )     ### <-- AQUI

# aplicar
y_formal_ruido   <- pmax(y_formal   + ruido_formal,   0)            ### <-- AQUI
y_informal_ruido <- pmax(y_informal + ruido_informal, 0)            ### <-- AQUI

# total hijo
y_c <- y_formal_ruido + y_informal_ruido

# ------------------------------------------------------------
# 5) DATAFRAME FINAL
# ------------------------------------------------------------

df <- data.frame(
  y_p = y_p,
  h = h_star,
  l = l_star,
  y_c = y_c,
  y_formal = y_formal_ruido,
  y_informal = y_informal_ruido
)


# ------------------------------------------------------------
# 6) GRAFICOS
# ------------------------------------------------------------

# ----------------------------------------------
# GRAFICO: formal, informal y total del hijo
# ----------------------------------------------

df <- df %>%
  mutate(y_total = y_formal + y_informal)

df_long <- df %>%
  pivot_longer(
    cols = c(y_formal, y_informal, y_total),
    names_to = "tipo",
    values_to = "ingreso"
  )


ggplot(df_long, aes(x = y_p, y = ingreso, color = tipo)) +
  geom_point(alpha = 0.25) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Father's income",
    y = "Child's income",
    title = "Child's formal, informal and total income",
    color = NULL   # <-- elimina el título en la leyenda
  ) +
  theme_minimal(base_size = 14)




# ----------------------------------------------
# 1) Calcular percentiles del padre e hijo
# ----------------------------------------------

df <- df %>%
  mutate(
    p_parent = percent_rank(y_p),          # percentil del ingreso del padre
    p_formal = percent_rank(y_formal),     # percentil del ingreso formal
    p_informal = percent_rank(y_informal), # percentil del ingreso informal
    p_total = percent_rank(y_total)        # percentil del ingreso total
  )

# Pasar a formato largo
df_long_p <- df %>%
  pivot_longer(
    cols = c(p_formal, p_informal, p_total),
    names_to = "tipo",
    values_to = "percentil_hijo"
  )

# ----------------------------------------------
# 2) Gráfico percentil vs percentil
# ----------------------------------------------

ggplot(df_long_p, aes(x = p_parent, y = percentil_hijo, color = tipo)) +
  geom_point(alpha = 0.25, size = 1) +
  labs(
    x = "Percentil del ingreso del padre",
    y = "Percentil del ingreso del hijo",
    title = "Movilidad intergeneracional: percentil del hijo vs padre"
  ) +
  scale_color_manual(
    values = c("p_formal" = "blue", "p_informal" = "red"),
    labels = c("Formal", "Informal")
  ) +
  theme_minimal(base_size = 14)

df_long_p = df_long_p %>% filter(tipo == "p_formal")

ggplot(df_long_p, aes(x = p_parent, y = percentil_hijo, color = tipo)) +
  geom_point(alpha = 0.25, size = 1) +
  labs(
    x = "Percentil del ingreso del padre",
    y = "Percentil del ingreso del hijo",
    title = "Movilidad intergeneracional: percentil del hijo vs padre"
  ) +
  scale_color_manual(
    values = c("p_total" = "black"),
    labels = c("Total")
  ) +
  theme_minimal(base_size = 14)



# df_long tiene y_p = rank del padre, ingreso = rank del hijo

df_bins <- df_long_p %>%
  mutate(
    bin = ntile(y_p, 100)   # cambia 20 a 100 si quieres percentiles individuales
  ) %>%
  group_by(bin) %>%
  summarise(
    mean_parent = mean(p_parent, na.rm = TRUE),
    mean_child  = mean(percentil_hijo, na.rm = TRUE),
    n = n()
  )

ggplot(df_bins, aes(x = mean_parent, y = mean_child)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "Parent income percentile rank",
    y = "Child formal income percentile rank",
    title = "Binned scatterplot: Parent vs Child formal income ranks"
  ) +
  theme_minimal(base_size = 14)



ggplot(df_bins, aes(x = mean_parent, y = mean_child)) +
  geom_point(size = 3) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    x = "Parent income percentile rank",
    y = "Child income percentile rank",
    title = "Binned scatterplot with linear fit"
  ) +
  theme_minimal(base_size = 14)

