# 📦 Cargar paquetes
packages <- c("tidyverse", "psych", "lavaan", "semTools", "knitr")
invisible(lapply(packages, function(p) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)))
lapply(packages, library, character.only = TRUE)

# 🛠 Función principal de análisis
analyze_scale <- function(df, prefix, combo = NULL) {
  items <- if (!is.null(combo)) combo else grep(paste0("^", prefix), colnames(df), value = TRUE)
  df_items <- df %>% select(all_of(items))
  
  # ⚠️ 1. Quitar ítems sin varianza o todo NA
  vars <- sapply(df_items, function(x) var(x, na.rm = TRUE))
  items_valid <- names(vars[!is.na(vars) & vars > 0])
  if (length(items_valid) < 3) return(NULL)  # no se puede estimar
  
  df_valid <- df %>% select(all_of(items_valid))
  n_original <- nrow(df)
  df_valid <- df_valid %>% drop_na()
  n_final <- nrow(df_valid)
  
  tryCatch({
    # 📐 Alpha y Omega
    alpha_val <- psych::alpha(df_valid, check.keys = TRUE)$total$raw_alpha
    omega_val <- psych::omega(df_valid, nfactors = 1, plot = FALSE, check.keys = TRUE)$omega.tot
    
    # 🧱 CFA
    model <- paste0(prefix, " =~ ", paste(items_valid, collapse = " + "))
    fit <- cfa(model, data = df_valid, ordered = items_valid)
    fit_stats <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
    
    list(
      name = prefix,
      alpha = round(alpha_val, 3),
      omega = round(omega_val, 3),
      n_orig = n_original,
      n_used = n_final,
      loss_pct = round(100 * (1 - n_final / n_original), 1),
      fit = tibble(
        scale = prefix,
        chisq = round(fit_stats["chisq"], 2),
        df = fit_stats["df"],
        pval = round(fit_stats["pvalue"], 3),
        cfi = round(fit_stats["cfi"], 3),
        tli = round(fit_stats["tli"], 3),
        rmsea = paste0(round(fit_stats["rmsea"], 3), " (", 
                       round(fit_stats["rmsea.ci.lower"], 3), "-", 
                       round(fit_stats["rmsea.ci.upper"], 3), ")")
      ),
      loadings = standardizedSolution(fit) %>%
        filter(op == "=~") %>%
        select(factor = lhs, item = rhs, loading = est.std) %>%
        arrange(desc(abs(loading))),
      mi = modificationIndices(fit) %>%
        filter(mi > 4) %>%
        arrange(desc(mi))
    )
  }, error = function(e) {
    list(
      name = prefix,
      alpha = NA,
      omega = NA,
      n_orig = n_original,
      n_used = n_final,
      loss_pct = round(100 * (1 - n_final / n_original), 1),
      fit = tibble(
        scale = prefix,
        chisq = NA, df = NA, pval = NA, cfi = NA, tli = NA, rmsea = "ERROR"
      ),
      loadings = tibble(),
      mi = tibble()
    )
  })
}

# 🔎 Escalas finales a analizar
escalas <- c("esc", "pns", "cls", "fac", "eqh", "net", "web", "ese", "asg", 
             "for", "tec", "prg", "cib", "ciu", "hab", "att", "int", "val")

# ✳️ Combinar 'esc' + 'pro'
esc_pro_items <- grep("^esc|^pro", colnames(estwdf), value = TRUE)

# 🚀 Ejecutar análisis
results <- map(escalas, function(prefix) {
  if (prefix == "esc") {
    analyze_scale(estwdf, "esc", combo = esc_pro_items)
  } else {
    analyze_scale(estwdf, prefix)
  }
}) %>% set_names(escalas)

# 📊 Tabla resumen
summary_table <- map_dfr(results, function(res) {
  if (!is.null(res)) res$fit %>%
    mutate(
      alpha = res$alpha,
      omega = res$omega,
      n = res$n_used,
      n_total = res$n_orig,
      loss_pct = res$loss_pct
    )
})

cat("\n\n📋 ===== RESUMEN PSICOMÉTRICO FINAL DE ESCALAS =====\n")
print(knitr::kable(summary_table, format = "simple", align = "lcccccccc"))

# 🔍 Ejemplo: ver cargas y MIs de una escala
cat("\n\n📈 ===== CARGAS DE 'hab' =====\n")
print(results[["hab"]]$loadings)

cat("\n\n⚠️ ===== MOD INDICES DE 'hab' =====\n")
print(results[["hab"]]$mi)
