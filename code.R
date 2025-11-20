library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1) PARÁMETROS DEL MODELO
# ------------------------------------------------------------

params <- list(
  A_F = 1,
  A_I = 1.25,
  alpha = 0.9,
  gamma = 0.2,
  C = 0.25, #((1-alpha)*A_F)*((1-gamma)*A_I)^(-1)
  kappa = 0, # 200000
  beta = 0.7
)

# ------------------------------------------------------------
# 2) FUNCIONES DEL MODELO
# ------------------------------------------------------------

# l_exact: solución numérica interior de la FOC del hijo
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

# ingreso exacto del hijo
y_c_exact <- function(h, A_F, A_I, alpha, gamma, kappa, C){
  l <- l_exact(h, alpha, gamma, C)
  A_F*h^alpha * l^(1-alpha) +
    A_I*h^gamma * (1-l)^(1-gamma) - kappa
}

# utilidad del padre
U_p <- function(h, y_p, beta, A_F, A_I, alpha, gamma, kappa, C){
  if(h <= 0 || h >= y_p) return(-Inf)
  log(y_p - h) + beta * y_c_exact(h, A_F, A_I, alpha, gamma, kappa, C)
}

# solución numérica del padre
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
mu = 1
sigma <- 0.7
meanlog <- log(mu_target) - sigma^2/2
y_p <- rlnorm(n, meanlog = mu, sdlog = sigma)

# ------------------------------------------------------------
# 4) CALCULAR h*, l(h*) y los ingresos del hijo
# ------------------------------------------------------------

h_star <- sapply(y_p, h_opt_parent, params = params)
l_star <- sapply(h_star, l_exact,
                 alpha = params$alpha,
                 gamma = params$gamma,
                 C = params$C)

# ingreso formal/informal y total del hijo
y_formal <- params$A_F * h_star^params$alpha * l_star^(1 - params$alpha)
y_informal <- params$A_I * h_star^params$gamma * (1 - l_star)^(1 - params$gamma)
y_c <- y_formal + y_informal

# ------------------------------------------------------------
# 5) DATAFRAME FINAL
# ------------------------------------------------------------

df <- data.frame(
  y_p = y_p,
  h = h_star,
  l = l_star,
  y_c = y_c,
  y_formal = y_formal,
  y_informal = y_informal
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
    x = "Ingreso del padre",
    y = "Ingreso del hijo",
    title = "Ingreso formal, informal y total del hijo"
  ) +
  theme_minimal(base_size = 14)





