# ============================================================
# 05_decisiones_eda.R
# Matriz de hallazgos y decisiones preliminares antes del modelamiento
# ============================================================

library(tidyverse)
library(scales)
library(shiny)

matriz_decisiones_eda <- function(data) {
  
  vars <- variables_perfil_default(data)
  
  map_dfr(vars, function(variable) {
    
    x <- data[[variable]]
    faltantes <- sum(is.na(x))
    pct_faltantes <- faltantes / nrow(data)
    n_codigos <- if (str_detect(variable, "retraso|retrasados")) sum(x >= 98, na.rm = TRUE) else 0
    n_ceros <- sum(x == 0, na.rm = TRUE)
    n_extremos_100 <- sum(x > 100, na.rm = TRUE)
    
    if (variable == "edad") {
      hallazgo <- paste0(
        "No presenta faltantes. Se observan ", sum(x > 100, na.rm = TRUE),
        " clientes mayores de 100 años y ", sum(x >= 90, na.rm = TRUE),
        " clientes de 90 años o más."
      )
      decision <- "Conservar como predictor. Revisar edades mayores a 100 años y documentar el tratamiento antes del modelo."
      uso_modelo <- "Sí, como variable numérica; rangos solo para interpretación."
      prioridad <- "Media"
    
    } else if (variable == "mto_ingreso_mensual") {
      hallazgo <- paste0(
        "Tiene ", percent(pct_faltantes, accuracy = 0.01),
        " de faltantes, ", n_ceros, " ingresos igual a cero y valores altos en la cola superior."
      )
      decision <- "No eliminar filas. Imputar faltantes con una estrategia robusta y crear indicador de ingreso no informado. Evaluar log-transformación o winsorización."
      uso_modelo <- "Sí, con tratamiento de faltantes y escala."
      prioridad <- "Alta"
    
    } else if (variable == "prct_uso_tc") {
      hallazgo <- paste0(
        "Tiene ", percent(pct_faltantes, accuracy = 0.01),
        " de faltantes. La mayoría de valores se concentra entre 0 y 1, pero hay valores extremos mayores a 1 y algunos muy altos."
      )
      decision <- "Confirmar escala. Conservar inicialmente; evaluar winsorización o transformación si los valores extremos dominan el modelo."
      uso_modelo <- "Sí, con revisión de escala y valores extremos."
      prioridad <- "Alta"
    
    } else if (variable == "prct_deuda_vs_ingresos") {
      hallazgo <- paste0(
        "Tiene ", percent(pct_faltantes, accuracy = 0.01),
        " de faltantes y valores extremadamente altos. Puede comportarse como razón no acotada."
      )
      decision <- "Validar escala con el diccionario. Analizar por rangos; considerar transformación o winsorización antes de modelos sensibles a escala."
      uso_modelo <- "Sí, pero no sin tratamiento de extremos."
      prioridad <- "Alta"
    
    } else if (variable == "nro_dependiente") {
      hallazgo <- paste0(
        "Tiene ", percent(pct_faltantes, accuracy = 0.01),
        " de faltantes. Predominan valores pequeños y existen pocos casos altos."
      )
      decision <- "Revisar significado de NA. Si NA significa no reportado, imputar mediana y crear indicador; si significa cero, recodificar con sustento."
      uso_modelo <- "Sí, como conteo o agrupada."
      prioridad <- "Media"
    
    } else if (variable == "nro_prod_financieros_deuda") {
      hallazgo <- paste0(
        "No presenta faltantes. Es un conteo con cola superior; hay ", sum(x > quantile(x, 0.99, na.rm = TRUE), na.rm = TRUE),
        " registros por encima del percentil 99."
      )
      decision <- "Conservar. Puede usarse como conteo; si el modelo es sensible a extremos, evaluar agrupación o winsorización."
      uso_modelo <- "Sí."
      prioridad <- "Media"
    
    } else if (variable == "nro_creditos_hipotecarios") {
      hallazgo <- paste0(
        "No presenta faltantes. La mayoría tiene pocos créditos hipotecarios; existen casos poco frecuentes con valores altos."
      )
      decision <- "Conservar. Usar como conteo o agrupar valores altos para interpretación."
      uso_modelo <- "Sí."
      prioridad <- "Media"
    
    } else if (str_detect(variable, "retraso|retrasados")) {
      hallazgo <- paste0(
        "No presenta faltantes. La mayoría de clientes tiene 0 retrasos. Se observan ", n_codigos,
        " registros con valores 98/99 o superiores que pueden ser códigos especiales."
      )
      decision <- "Variable muy informativa, pero delicada. Validar si corresponde a historial previo y si 98/99 son códigos especiales. No usar en modelo preventivo hasta resolver fuga de información."
      uso_modelo <- "Sí para análisis explicativo; condicionado para modelo preventivo."
      prioridad <- "Muy alta"
    
    } else {
      hallazgo <- paste0("Faltantes: ", percent(pct_faltantes, accuracy = 0.01), ". Requiere revisión general.")
      decision <- "Conservar temporalmente y revisar antes del modelamiento."
      uso_modelo <- "Por definir."
      prioridad <- "Media"
    }
    
    tibble(
      Variable = variable,
      `Nombre de negocio` = nombre_variable(variable),
      Naturaleza = naturaleza_variable(variable),
      `Hallazgo principal` = hallazgo,
      `Decisión preliminar` = decision,
      `Uso sugerido en modelo` = uso_modelo,
      Prioridad = prioridad
    )
  })
}

texto_cierre_eda <- function() {
  tagList(
    h4("Cierre de la etapa exploratoria"),
    p("El objetivo de esta etapa no es limpiar automáticamente la base, sino dejar documentadas las señales que afectan el modelamiento: desbalance del default, faltantes, valores extremos, posibles códigos especiales y variables que podrían producir fuga de información."),
    p("Con esta matriz de decisiones, el siguiente paso será construir la base analítica de modelamiento: imputar faltantes, tratar valores extremos de manera justificada, separar variables explicativas de variables sensibles y definir un modelo preventivo sin fuga de información."),
    p("La decisión más importante antes de modelar es separar dos enfoques: un modelo explicativo, que puede incluir historial de pago si el objetivo es entender asociaciones, y un modelo preventivo, que solo debe usar variables disponibles antes de aprobar o evaluar el crédito.")
  )
}

