ggplot(estwdf[!is.na(estwdf$attfac) & !is.na(estwdf$habfac), ],
       aes(x = habfac, y = attfac)) +
  # Añadir puntos con jitter, alta transparencia y pequeños
  geom_jitter(alpha = 0.15, size = 0.5, width = 0.05, height = 0.05, color = "black") +
  # Añadir línea de tendencia
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "gray80", linewidth = 0.8) +
  # Títulos y etiquetas
  labs(
    title = "Relación entre Habilidades Digitales y Actitudes Tecnológicas",
    x = "Habilidades Digitales",
    y = "Actitudes hacia la Tecnología"
  ) +
  # Añadir correlación en el gráfico
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", color = "black", size = 3.5) +
  # Tema minimalista en blanco y negro
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )