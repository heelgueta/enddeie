# 📦 Cargar paquetes
packages <- c("tidyverse", "psych", "lavaan", "semTools", "knitr")
invisible(lapply(packages, function(p) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)))
lapply(packages, library, character.only = TRUE)

# ✳️ Combinar 'esc' + 'pro' en una sola lista de ítems
esc_pro_items <- grep("^esc|^pro", colnames(estwdf), value = TRUE)

# 🛠 Función principal de análisis (auto-filtra ítems sin varianza, drop NA)
analyze_scale <- function(df, prefix, combo = NULL) {
  items <- if (!is.null(combo)) combo else grep(paste0("^", prefix), colnames(df), value = TRUE)
  df_items <- df %>% select(all_of(items))
  
  # ⚠️ Filtrar ítems sin varianza o vacíos
  vars <- sapply(df_items, function(x) var(x, na.rm = TRUE))
  items_valid <- names(vars[!is.na(vars) & vars > 0])
  if (length(items_valid) < 3) return(NULL)
  
  df_valid <- df %>% select(all_of(items_valid)) %>% drop_na()
  n_original <- nrow(df)
  n_final <- nrow(df_valid)
  
  tryCatch({
    # Alpha y omega
    alpha_val <- psych::alpha(df_valid, check.keys = TRUE)$total$raw_alpha
    omega_val <- psych::omega(df_valid, nfactors = 1, plot = FALSE, check.keys = TRUE)$omega.tot
    
    # CFA
    model <- paste0(prefix, " =~ ", paste(items_valid, collapse = " + "))
    fit <- lavaan::cfa(model, data = df_valid, ordered = items_valid)
    fit_stats <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", 
                                    "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
    
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
      alpha = NA, omega = NA,
      n_orig = n_original, n_used = n_final,
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





# Cambia este nombre según escala a correr (prefijo)
prefix <- "att"
# Ítems a excluir manualmente para esta escala
exclude_items <- c("")  # 👈 cambia estos por los que quieras sacar

# Ejecutar análisis excluyendo manualmente esos ítems
items_prefix <- grep(paste0("^", prefix), colnames(estwdf), value = TRUE)
items_keep <- setdiff(items_prefix, exclude_items)
result <- analyze_scale(estwdf, prefix, combo = items_keep)


#result <- analyze_scale(estwdf, prefix)

# Para 'esc' usar el combo manual
#result <- if (prefix == "esc") {
#  analyze_scale(estwdf, prefix, combo = esc_pro_items)
#} else {
#  analyze_scale(estwdf, prefix)
#}



# 📋 Mostrar resumen
cat("\n\n📋 ===== RESULTADOS PARA ESCALA:", prefix, " =====\n")
summary <- result$fit %>%
  mutate(alpha = result$alpha, omega = result$omega,
         n = result$n_used, n_total = result$n_orig,
         loss_pct = result$loss_pct)
print(knitr::kable(summary, format = "simple"))

# 📈 Cargas
cat("\n\n📈 ===== CARGAS FACTORIALES =====\n")
print(result$loadings)

# ⚠️ MIs > 4
cat("\n\n⚠️ ===== MOD INDICES > 4 =====\n")
print(result$mi)



table(estwdf$hab11)











# 📦 Ensure required packages are loaded
if (!require("psych")) install.packages("psych")
if (!require("tidyverse")) install.packages("tidyverse")
library(psych)
library(tidyverse)

# 🧪 Select hab items and drop rows with any NA
hab_items <- estwdf %>% 
  select(starts_with("hab")) %>%
  tidyr::drop_na()

# ⚠️ Remove items with zero variance
bad_items <- names(which(sapply(hab_items, function(x) var(x, na.rm = TRUE)) == 0))
if (length(bad_items) > 0) {
  warning("Items with zero variance will be excluded: ", paste(bad_items, collapse = ", "))
  hab_items <- hab_items %>% select(-all_of(bad_items))
}

# ℹ️ Confirm how many items remain
cat("Analyzing", ncol(hab_items), "items and", nrow(hab_items), "valid cases\n")

# ⚙️ Run omega (plot optional)
omega_result <- omega(hab_items, nfactors = 1, plot = TRUE, check.keys = TRUE)

# 📤 Show just omega total
cat("\n\n✅ Omega total:", round(omega_result$omega.tot, 3), "\n")
