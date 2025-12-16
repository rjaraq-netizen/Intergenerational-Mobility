library(tidyverse)
library(ggplot2)

# =========================================================
# 1. PARÁMETROS DEL PROBLEMA (Nuevos Valores)
# =========================================================

y_tm1 <- 100   # Ingreso inicial (limite de I_tm1)
e_t   <- 20    # Shock en t
r1    <- 0.05  # Tasa r1
r2    <- 0.2  # Tasa r2
Hstar <- 61.59    # Umbral H*

# Punto de Transición (Kink)
I_kink <- Hstar - e_t
cat(paste("Punto de Kink I_t-1 =", I_kink, "\n")) # I_kink = 40

# =========================================================
# 2. DEFINICIÓN DE LAS FUNCIONES DE INGRESO (Y_T)
# =========================================================

# 1. Ingreso REAL (Y_t): La función con el quiebre
y_t_real <- function(I_tm1) {
  return((1 + r1) * I_tm1 + e_t + (r2 - r1) * pmax(0, I_tm1 + e_t - Hstar))
}

# 2. Ingreso Extensión R2 (Y_t_R2): Rendimiento r2 siempre (CORREGIDA)
# y_t = (1 + r2) * I_tm1 + e_t - (r2 - r1) * Hstar
y_t_r2_ext <- function(I_tm1) {
  # Esta es la extensión lineal de la sección con alto rendimiento.
  return((1 + r2) * I_tm1 + e_t + (r2-r1)*e_t - (r2 - r1) * Hstar)
}

# 3. Ingreso Extensión R1 (Y_t_R1): Rendimiento r1 siempre
y_t_r1_ext <- function(I_tm1) {
  return((1 + r1) * I_tm1 + e_t)
}

# =========================================================
# 3. DEFINICIÓN DE LAS FUNCIONES DE UTILIDAD (U)
# =========================================================

# Nota: Estas funciones son necesarias para encontrar los I*
u <- function(c) {
  if (c <= 0) return(-Inf)
  return(log(c))
}

utilidad1 <- function(I_tm1) {
  if (I_tm1 <= 0 || I_tm1 >= y_tm1) return(-Inf)
  U <- u(y_tm1 - I_tm1) + u(y_t_real(I_tm1))
  return(U)
}

utilidad2 <- function(I_tm1) {
  if (I_tm1 <= 0 || I_tm1 >= y_tm1) return(-Inf)
  U <- u(y_tm1 - I_tm1) + u(y_t_r2_ext(I_tm1))
  return(U)
}

utilidad3 <- function(I_tm1) {
  if (I_tm1 <= 0 || I_tm1 >= y_tm1) return(-Inf)
  U <- u(y_tm1 - I_tm1) + u(y_t_r1_ext(I_tm1))
  return(U)
}

# =========================================================
# 4. CÁLCULO DE LOS ÓPTIMOS (I*)
# =========================================================

opt1 <- optimize(utilidad1, interval = c(0.001, y_tm1 - 0.001), maximum = TRUE)
opt2 <- optimize(utilidad2, interval = c(0.001, y_tm1 - 0.001), maximum = TRUE)
opt3 <- optimize(utilidad3, interval = c(0.001, y_tm1 - 0.001), maximum = TRUE)

optimos <- tibble(
  Etiqueta = c("1. Ingreso Real (Con Quiebre)", "2. Extensión R2 (Rend. 30% Constante)", "3. Extensión R1 (Rend. 5% Constante)"),
  Funcion = c("Y_Real", "Y_R2_Ext", "Y_R1_Ext"),
  I_star = c(opt1$maximum, opt2$maximum, opt3$maximum),
  U_star = c(opt1$objective, opt2$objective, opt3$objective),
  # Calcular el valor de y_t en el optimo correspondiente
  Y_star = c(y_t_real(opt1$maximum), y_t_r2_ext(opt2$maximum), y_t_r1_ext(opt3$maximum))
)

cat("\n--- Resultados de los Máximos de Inversión ---\n")
print(optimos %>% select(Etiqueta, I_star, U_star, Y_star))


# =========================================================
# 5. CÁLCULO DE DATOS PARA LA GRÁFICA
# =========================================================

I_range <- seq(0, 50, length.out = 300)

data_ingresos <- tibble(I_tm1 = I_range) %>%
  mutate(
    Y_Real = y_t_real(I_tm1),
    Y_R2_Ext = y_t_r2_ext(I_tm1),
    Y_R1_Ext = y_t_r1_ext(I_tm1)
  ) %>%
  pivot_longer(
    cols = starts_with("Y_"),
    names_to = "Funcion",
    values_to = "Ingreso_y_t"
  ) %>%
  left_join(optimos %>% select(Funcion, Etiqueta), by = "Funcion") # Unir etiquetas


# =========================================================
# 6. GRÁFICA CON GGPLOT2
# =========================================================

ggplot(data_ingresos, aes(x = I_tm1, y = Ingreso_y_t, color = Etiqueta)) +
  
  # 1. Curvas de Ingreso
  geom_line(linewidth = 1.2) +
  
  # 2. Línea vertical para el Kink (I_kink = 40)
  geom_vline(xintercept = I_kink, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = I_kink + 2, y = max(data_ingresos$Ingreso_y_t) * 0.9, 
           label = paste("I =", I_kink, "(Kink)"), color = "black", size = 3.5, angle = 90) +
  
  # 3. Puntos Óptimos I*
  geom_point(data = optimos, aes(x = I_star, y = Y_star, color = Etiqueta), size = 4, shape = 19) +
  
  # 4. Etiquetas de los Óptimos
  geom_text(data = optimos, 
            aes(x = I_star, y = Y_star, 
                label = paste0("I*=", round(I_star, 2), "\nY_t=", round(Y_star, 2))), 
            vjust = -0.5, hjust = 0.5, show.legend = FALSE, size = 3) +
  
  # 5. Etiquetas y Tema
  labs(
    title = "Ingreso Futuro (y_t) y Óptimos de Inversión (I*)",
    subtitle = paste0("r1=", r1, ", r2=", r2, ", H*=", Hstar, ", e_t=", e_t, " | y_{t-1}=", y_tm1),
    x = "Inversión del Padre (I_{t-1})",
    y = "Ingreso del Hijo (y_t)",
    color = "Función"
  ) +
  scale_color_manual(values = c("1. Ingreso Real (Con Quiebre)" = "blue", 
                                "2. Extensión R2 (Rend. 30% Constante)" = "red", 
                                "3. Extensión R1 (Rend. 5% Constante)" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom")
