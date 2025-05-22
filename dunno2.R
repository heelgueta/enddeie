# Definir el modelo de dos factores correlacionados
mod <- '
  # Primer factor: Habilidades digitales
  diglit =~ hab01 + hab02 + hab03 + hab04 + hab05 + hab06 + hab07 + hab08 + hab09 + hab10 + hab11
  
  # Segundo factor: Actitudes tecnológicas
  tecatt =~ att01 + att02 + att03 + att04 + att05 + att06 + att07
'

# Ajustar el modelo CFA
fit <- cfa(mod, data = estwdf, ordered = TRUE)

# Extraer medidas de ajuste del modelo
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))


standardizedsolution(fit)[standardizedsolution(fit)$op=='~~', c(1:4, 7)]

# Obtener la correlación entre factores
factor_correlation <- parameterEstimates(fit)[
  parameterEstimates(fit)$op == "~~" &
    parameterEstimates(fit)$lhs == "diglit" &
    parameterEstimates(fit)$rhs == "tecatt", 
  c("est", "se", "pvalue", "ci.lower", "ci.upper", "std.all")
]

# Mostrar la correlación entre factores
print(factor_correlation)