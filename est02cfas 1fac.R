# Cargar paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(lavaan, EGAnet, dplyr)

# Definir el modelo de un factor para CFA
mod <- 'diglit =~ hab01 + hab02 + hab03 + hab04 + hab05 + hab06 + hab07 + hab08 + hab09 + hab10 + hab11'
#mod <- 'diglit =~ hab01 + hab02 + hab03 + hab05 + hab07 + hab08 + hab09'
#mod <- 'diglit =~ hab01 + hab03 + hab05 + hab07 + hab09'
#mod <- 'diglit =~ hab01 + hab03 + hab05 + hab09'

# Ajustar el modelo CFA con variables ordenadas
fit <- cfa(mod, data = estwdf, ordered = TRUE)

# Extraer medidas de ajuste
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

# Solución estandarizada
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~', c(1:4, 7)]

# Indices de modificación
modindices(fit)[modindices(fit)$mi > 4, ][order(-modindices(fit)[modindices(fit)$mi > 4, ]$mi), ]


# Realizar análisis exploratorio de grafos (EGA)
ega_hab <- EGA(data = estwdf[, c("hab01", "hab02", "hab03", "hab04", "hab05", 
                                 "hab06", "hab07", "hab08", "hab09", "hab10", "hab11")],
               plot.EGA = TRUE)

# Resumen de EGA
summary(ega_hab)





# Vector con nombres descriptivos
vars_hab <- c("hab01", "hab02", "hab03", "hab04", "hab05", 
              "hab06", "hab07", "hab08", "hab09", "hab10", "hab11")

descripciones <- c(
  "crear/editar documentos",
  "enviar archivos por correo",
  "crear planillas con fórmulas",
  "crear presentaciones",
  "crear/editar videos",
  "subir contenido a sitios",
  "buscar información escolar",
  "evaluar confiabilidad información",
  "editar documentos compartidos",
  "coordinar trabajos online",
  "programar"
)

nombres_items <- setNames(descripciones, vars_hab)

# Variables de interés
vars_hab <- c("hab01", "hab02", "hab03", "hab04", "hab05", 
              "hab06", "hab07", "hab08", "hab09", "hab10", "hab11")

# Recalcular EGA sin graficar
ega_hab <- EGA(data = estwdf[, vars_hab], plot.EGA = FALSE)

# Extraer matriz de correlaciones policóricas (la que usa EGA internamente)
cor_mat <- ega_hab$correlation

# Usar qgraph directamente para graficar con etiquetas descriptivas y correlaciones
library(qgraph)

qgraph(cor_mat,
       layout = "spring",
       labels = nombres_items,
       label.scale.equal = TRUE,
       edge.labels = TRUE,
       edge.label.cex = 0.8,
       title = "Red de habilidades digitales (EGA)",
       legend = TRUE,
       legend.cex = 0.6,
       node.width = 1.8,
       groups = ega_hab$wc,
       color = TRUE,
       borders = FALSE)

str(ega_hab, max.level = 2)




# Obtener puntajes factoriales y manejar missing data
factor_scores <- lavPredict(fit, type = "lv", newdata = estwdf)

# Crear un vector del mismo tamaño que estwdf con NAs
estwdf$habfac <- NA

# Identificar los casos completos (sin NA en las variables utilizadas)
complete_cases <- complete.cases(estwdf[, c("hab01", "hab02", "hab03", "hab04", 
                                            "hab05", "hab06", "hab07", "hab08", 
                                            "hab09", "hab10", "hab11")])

# Asignar los puntajes factoriales solo a los casos completos
estwdf$habfac[complete_cases] <- factor_scores[, "diglit"]

# Verificar
summary(estwdf$habfac)
hist(estwdf$habfac)


# Cargar ggplot2
library(ggplot2)

# Crear gráfico con facetas para separar los histogramas por género
ggplot(estwdf[!is.na(estwdf$habfac) & !is.na(estwdf$gen), ], 
       aes(x = habfac, fill = factor(gen, labels = c("Femenino", "Masculino")))) +
  geom_histogram(alpha = 0.8, bins = 15, color = "white") +
  facet_grid(factor(gen, labels = c("Femenino", "Masculino")) ~ ., scales = "free_y") +
  scale_fill_manual(values = c("#00ACC1", "#FF7043")) +
  labs(
    title = "Distribución de Habilidades Digitales por Género",
    x = "Puntuación de Habilidades Digitales",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines")
  )

# Instalar y cargar paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, effsize, coin)

# Comparación estadística entre géneros (Femenino=1, Masculino=2)
gender_stats <- estwdf %>%
  filter(!is.na(habfac) & !is.na(gen)) %>%
  group_by(gen) %>%
  summarise(
    n = n(),
    mean = mean(habfac),
    sd = sd(habfac),
    median = median(habfac),
    Q1 = quantile(habfac, 0.25),
    Q3 = quantile(habfac, 0.75)
  )

gender_stats
# t-test (paramétrico)
t_result <- t.test(habfac ~ gen, data = estwdf)

# Test de Wilcoxon (no paramétrico)
wilcox_result <- wilcox_test(habfac ~ factor(gen), data = estwdf)

# Cálculo de effect size
# Corregir el cálculo de Cohen's d
cohens_d <- cohen.d(estwdf$habfac, factor(estwdf$gen))
cohens_d

# Mostrar resultados
print(paste("Cohen's d:", round(cohens_d$estimate, 3), 
            "- Interpretación:", cohens_d$magnitude))cliff_delta <- cliff.delta(estwdf$habfac, factor(estwdf$gen))

# Mostrar resultados
print(gender_stats)
print(t_result)
print(wilcox_result)
print(paste("Cohen's d:", round(cohens_d$estimate, 3), 
            "- Interpretación:", cohens_d$magnitude))
print(paste("Cliff's Delta:", round(cliff_delta$estimate, 3), 
            "- Interpretación:", cliff_delta$magnitude))





# Calcular Cohen's d manualmente
fem_data <- estwdf$habfac[estwdf$gen == 1 & !is.na(estwdf$habfac)]
masc_data <- estwdf$habfac[estwdf$gen == 2 & !is.na(estwdf$habfac)]

# Calcular medias y desviaciones estándar por grupo
mean_fem <- mean(fem_data, na.rm = TRUE)
mean_masc <- mean(masc_data, na.rm = TRUE)
sd_fem <- sd(fem_data, na.rm = TRUE)
sd_masc <- sd(masc_data, na.rm = TRUE)

# Calcular tamaño de los grupos
n_fem <- length(fem_data)
n_masc <- length(masc_data)

# Calcular SD agrupada
pooled_sd <- sqrt(((n_fem - 1) * sd_fem^2 + (n_masc - 1) * sd_masc^2) / 
                    (n_fem + n_masc - 2))

# Calcular Cohen's d
cohens_d_manual <- (mean_masc - mean_fem) / pooled_sd

# Interpretar magnitud
magnitude <- case_when(
  abs(cohens_d_manual) < 0.2 ~ "negligible",
  abs(cohens_d_manual) < 0.5 ~ "small",
  abs(cohens_d_manual) < 0.8 ~ "medium",
  TRUE ~ "large"
)

# Mostrar resultados
print(paste("Cohen's d (manual):", round(cohens_d_manual, 3),
            "- Interpretación:", magnitude))

ggplot(estwdf[!is.na(estwdf$habfac) & !is.na(estwdf$gen), ], 
       aes(x = factor(gen, labels = c("Femenino", "Masculino")), 
           y = habfac, 
           color = factor(gen, labels = c("Femenino", "Masculino")),
           fill = factor(gen, labels = c("Femenino", "Masculino")))) +
  # Cajas con borde grueso coloreado y relleno vacío
  geom_boxplot(fill = "white", alpha = 0.4, width = 0.5, outlier.shape = NA, linewidth = 1.2) +
  # Puntos aún más pequeños, mismo color que los bordes
  geom_jitter(alpha = 0.15, width = 0.25, height = 0.05, size = 0.6) +
  scale_color_manual(values = c("#00ACC1", "#FF7043")) +
  scale_fill_manual(values = c("#00ACC1", "#FF7043")) +
  labs(
    title = "Distribución de Habilidades Digitales por Género",
    x = "Género",
    y = "Puntuación de Habilidades Digitales"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )


















# Librerías
library(qgraph)
library(EGAnet)

# Variables
vars_hab <- c("hab01", "hab02", "hab03", "hab04", "hab05", 
              "hab06", "hab07", "hab08", "hab09", "hab10", "hab11")

descripciones <- c(
  "crear/editar documentos",
  "enviar archivos por correo",
  "crear planillas con fórmulas",
  "crear presentaciones",
  "crear/editar videos",
  "subir contenido a sitios",
  "buscar información escolar",
  "evaluar confiabilidad información",
  "editar documentos compartidos",
  "coordinar trabajos online",
  "programar"
)

# EGA (si aún no lo hiciste)
ega_hab <- EGA(data = estwdf[, vars_hab], plot.EGA = FALSE)

# Correlaciones
cor_mat <- ega_hab$correlation

# Plot ajustado
qgraph(cor_mat,
       layout = "spring",
       labels = vars_hab,
       label.cex = 1,
       edge.labels = TRUE,
       edge.label.cex = 1,
       title = "Red de habilidades digitales (EGA)",
       legend = FALSE,
       node.width = 1,
       groups = ega_hab$wc,
       color = TRUE,
       borders = TRUE,
       label.scale.equal = TRUE,
       vsize = 1)

# Leyenda textual
legend("bottomleft",
       legend = paste(vars_hab, "=", descripciones),
       cex = 0.7,
       bty = "n",
       x.intersp = 0.4,
       y.intersp = 0.9)
