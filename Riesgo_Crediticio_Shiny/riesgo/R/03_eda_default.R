# ============================================================
# 03_eda_default.R
# Perfil de clientes y análisis bivariado frente a Default
# ============================================================

library(tidyverse)
library(scales)
library(shiny)

# ============================================================
# Diccionario analítico interno
# ============================================================

metadata_variables <- function() {
  
  tibble::tribble(
    ~variable, ~nombre, ~naturaleza, ~familia, ~rol_analitico,
    "edad", "Edad", "Edad", "Demográfica", "Variable explicativa de perfil. Se analiza por rangos y como variable numérica.",
    "mto_ingreso_mensual", "Ingreso mensual", "Monto", "Capacidad de pago", "Variable financiera con faltantes relevantes. Se analiza por rangos, percentiles y faltantes.",
    "prct_uso_tc", "Uso de tarjeta de crédito", "Razón financiera", "Uso de crédito", "Razón de utilización. Se revisa escala, valores extremos y grupos de uso.",
    "prct_deuda_vs_ingresos", "Deuda financiera versus ingresos", "Razón financiera", "Endeudamiento", "Razón de deuda. Puede presentar valores muy altos y requiere revisar escala.",
    "nro_dependiente", "Número de dependientes", "Conteo", "Carga familiar", "Variable de conteo. Se analiza con frecuencias y grupos pequeños.",
    "nro_prod_financieros_deuda", "Productos financieros con deuda", "Conteo", "Exposición crediticia", "Conteo de productos. Se analiza con rangos porque puede tomar varios valores.",
    "nro_creditos_hipotecarios", "Créditos hipotecarios", "Conteo", "Exposición crediticia", "Conteo de créditos hipotecarios. Se interpreta con frecuencias y rangos.",
    "nro_prestao_retrasados", "Préstamos con retraso mayor a 3 meses", "Conteo de mora", "Historial de pago", "Variable muy sensible. Puede ser predictiva, pero debe validarse por posible fuga de información y códigos 98/99.",
    "nro_retraso_60dias", "Retrasos mayores a 60 días", "Conteo de mora", "Historial de pago", "Variable muy sensible. Debe verificarse si corresponde a historial previo disponible antes de evaluar el préstamo.",
    "nro_retraso_ultm3anios", "Retrasos mayores a 30 días en últimos 3 años", "Conteo de mora", "Historial de pago", "Variable muy sensible. Debe validarse por posible fuga de información y códigos especiales."
  )
}

variables_perfil_default <- function(data) {
  
  data %>%
    select(where(is.numeric)) %>%
    select(-any_of(c("id", "default"))) %>%
    names()
}

nombre_variable <- function(variable) {
  meta <- metadata_variables() %>% filter(.data$variable == !!variable)
  if (nrow(meta) == 0) variable else meta$nombre[1]
}

naturaleza_variable <- function(variable) {
  meta <- metadata_variables() %>% filter(.data$variable == !!variable)
  if (nrow(meta) == 0) "Numérica" else meta$naturaleza[1]
}

familia_variable <- function(variable) {
  meta <- metadata_variables() %>% filter(.data$variable == !!variable)
  if (nrow(meta) == 0) "General" else meta$familia[1]
}

rol_variable <- function(variable) {
  meta <- metadata_variables() %>% filter(.data$variable == !!variable)
  if (nrow(meta) == 0) "Variable numérica usada para explorar diferencias entre clientes con y sin default." else meta$rol_analitico[1]
}

# ============================================================
# Resumen descriptivo por Default
# ============================================================

resumen_variable_default <- function(data, variable) {
  
  data %>%
    transmute(
      `Estado del cliente` = if_else(default == 1, "Default", "No Default"),
      valor = .data[[variable]]
    ) %>%
    group_by(`Estado del cliente`) %>%
    summarise(
      Clientes = n(),
      Faltantes = sum(is.na(valor)),
      Media = mean(valor, na.rm = TRUE),
      Mediana = median(valor, na.rm = TRUE),
      `Desv. estándar` = sd(valor, na.rm = TRUE),
      `P25 / Q1` = quantile(valor, 0.25, na.rm = TRUE),
      `P75 / Q3` = quantile(valor, 0.75, na.rm = TRUE),
      Mínimo = min(valor, na.rm = TRUE),
      Máximo = max(valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

# ============================================================
# Construcción de grupos adecuados por naturaleza de variable
# ============================================================

crear_grupos_variable <- function(data, variable) {
  
  x <- data[[variable]]
  
  if (variable == "edad") {
    grupo <- case_when(
      is.na(x) ~ "Sin dato",
      x <= 30 ~ "Hasta 30 años",
      x <= 40 ~ "31 a 40 años",
      x <= 50 ~ "41 a 50 años",
      x <= 60 ~ "51 a 60 años",
      x > 60 ~ "Más de 60 años"
    )
    niveles <- c("Hasta 30 años", "31 a 40 años", "41 a 50 años", "51 a 60 años", "Más de 60 años", "Sin dato")
    
  } else if (variable == "mto_ingreso_mensual") {
    q <- quantile(x[x > 100], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
    grupo <- case_when(
      is.na(x) ~ "Sin ingreso reportado",
      x == 0 ~ "Ingreso igual a 0",
      x > 0 & x <= 100 ~ "Ingreso muy bajo (1 a 100)",
      x <= q[1] ~ paste0("Ingreso bajo (≤ ", comma(round(q[1], 0)), ")"),
      x <= q[2] ~ paste0("Ingreso medio-bajo (≤ ", comma(round(q[2], 0)), ")"),
      x <= q[3] ~ paste0("Ingreso medio-alto (≤ ", comma(round(q[3], 0)), ")"),
      x > q[3] ~ paste0("Ingreso alto (> ", comma(round(q[3], 0)), ")")
    )
    niveles <- c(
      "Sin ingreso reportado",
      "Ingreso igual a 0",
      "Ingreso muy bajo (1 a 100)",
      paste0("Ingreso bajo (≤ ", comma(round(q[1], 0)), ")"),
      paste0("Ingreso medio-bajo (≤ ", comma(round(q[2], 0)), ")"),
      paste0("Ingreso medio-alto (≤ ", comma(round(q[3], 0)), ")"),
      paste0("Ingreso alto (> ", comma(round(q[3], 0)), ")")
    )
    
  } else if (variable == "prct_uso_tc") {
    grupo <- case_when(
      is.na(x) ~ "Sin dato",
      x == 0 ~ "Sin uso registrado",
      x > 0 & x <= 0.25 ~ "Uso bajo (0% a 25%)",
      x > 0.25 & x <= 0.50 ~ "Uso moderado (25% a 50%)",
      x > 0.50 & x <= 0.75 ~ "Uso alto (50% a 75%)",
      x > 0.75 & x <= 1 ~ "Uso muy alto (75% a 100%)",
      x > 1 & x <= 2 ~ "Sobreuso moderado (>100% a 200%)",
      x > 2 ~ "Uso extremo (>200%)"
    )
    niveles <- c(
      "Sin uso registrado", "Uso bajo (0% a 25%)", "Uso moderado (25% a 50%)",
      "Uso alto (50% a 75%)", "Uso muy alto (75% a 100%)",
      "Sobreuso moderado (>100% a 200%)", "Uso extremo (>200%)", "Sin dato"
    )
    
  } else if (variable == "prct_deuda_vs_ingresos") {
    grupo <- case_when(
      is.na(x) ~ "Sin dato",
      x == 0 ~ "Sin deuda registrada",
      x > 0 & x <= 0.25 ~ "Deuda baja (0 a 0.25)",
      x > 0.25 & x <= 0.50 ~ "Deuda moderada (0.25 a 0.50)",
      x > 0.50 & x <= 1 ~ "Deuda alta (0.50 a 1)",
      x > 1 & x <= 5 ~ "Deuda mayor al ingreso (1 a 5)",
      x > 5 & x <= 100 ~ "Deuda muy elevada (5 a 100)",
      x > 100 ~ "Valor extremo (>100)"
    )
    niveles <- c(
      "Sin deuda registrada", "Deuda baja (0 a 0.25)", "Deuda moderada (0.25 a 0.50)",
      "Deuda alta (0.50 a 1)", "Deuda mayor al ingreso (1 a 5)",
      "Deuda muy elevada (5 a 100)", "Valor extremo (>100)", "Sin dato"
    )
    
  } else if (variable == "nro_dependiente") {
    grupo <- case_when(
      is.na(x) ~ "Sin dato",
      x == 0 ~ "0 dependientes",
      x == 1 ~ "1 dependiente",
      x == 2 ~ "2 dependientes",
      x == 3 ~ "3 dependientes",
      x >= 4 & x <= 6 ~ "4 a 6 dependientes",
      x > 6 ~ "Más de 6 dependientes"
    )
    niveles <- c("0 dependientes", "1 dependiente", "2 dependientes", "3 dependientes", "4 a 6 dependientes", "Más de 6 dependientes", "Sin dato")
    
  } else if (variable == "nro_prod_financieros_deuda") {
    grupo <- case_when(
      is.na(x) ~ "Sin dato",
      x == 0 ~ "0 productos",
      x <= 2 ~ "1 a 2 productos",
      x <= 5 ~ "3 a 5 productos",
      x <= 10 ~ "6 a 10 productos",
      x <= 20 ~ "11 a 20 productos",
      x > 20 ~ "Más de 20 productos"
    )
    niveles <- c("0 productos", "1 a 2 productos", "3 a 5 productos", "6 a 10 productos", "11 a 20 productos", "Más de 20 productos", "Sin dato")
    
  } else if (variable == "nro_creditos_hipotecarios") {
    grupo <- case_when(
      is.na(x) ~ "Sin dato",
      x == 0 ~ "0 créditos",
      x == 1 ~ "1 crédito",
      x == 2 ~ "2 créditos",
      x == 3 ~ "3 créditos",
      x >= 4 & x <= 5 ~ "4 a 5 créditos",
      x > 5 ~ "Más de 5 créditos"
    )
    niveles <- c("0 créditos", "1 crédito", "2 créditos", "3 créditos", "4 a 5 créditos", "Más de 5 créditos", "Sin dato")
    
  } else if (str_detect(variable, "retraso|retrasados")) {
    grupo <- case_when(
      is.na(x) ~ "Sin dato",
      x >= 90 ~ "Posible código especial (≥90)",
      x == 0 ~ "0 retrasos",
      x == 1 ~ "1 retraso",
      x == 2 ~ "2 retrasos",
      x == 3 ~ "3 retrasos",
      x >= 4 & x <= 9 ~ "4 a 9 retrasos",
      x >= 10 ~ "10 o más retrasos"
    )
    niveles <- c("0 retrasos", "1 retraso", "2 retrasos", "3 retrasos", "4 a 9 retrasos", "10 o más retrasos", "Posible código especial (≥90)", "Sin dato")
    
  } else {
    q <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
    grupo <- case_when(
      is.na(x) ~ "Sin dato",
      x <= q[1] ~ "Q1: valores bajos",
      x <= q[2] ~ "Q2: valores medio-bajos",
      x <= q[3] ~ "Q3: valores medio-altos",
      x > q[3] ~ "Q4: valores altos"
    )
    niveles <- c("Q1: valores bajos", "Q2: valores medio-bajos", "Q3: valores medio-altos", "Q4: valores altos", "Sin dato")
  }
  
  data %>%
    mutate(
      valor = .data[[variable]],
      grupo = factor(grupo, levels = niveles)
    )
}

# ============================================================
# Tabla de tasa de default por grupos adecuados
# ============================================================

analisis_complementario_variable <- function(data, variable) {
  
  crear_grupos_variable(data, variable) %>%
    group_by(grupo) %>%
    summarise(
      Clientes = n(),
      `Clientes con default` = sum(default == 1, na.rm = TRUE),
      `Clientes sin default` = sum(default == 0, na.rm = TRUE),
      tasa_default = `Clientes con default` / Clientes,
      .groups = "drop"
    ) %>%
    filter(!is.na(grupo)) %>%
    mutate(
      `Tasa de default` = percent(tasa_default, accuracy = 0.01),
      `% de la base` = percent(Clientes / sum(Clientes), accuracy = 0.01)
    ) %>%
    select(
      Grupo = grupo,
      Clientes,
      `% de la base`,
      `Clientes con default`,
      `Clientes sin default`,
      tasa_default,
      `Tasa de default`
    )
}

# ============================================================
# Frecuencias según naturaleza: exactas para conteos, rangos para continuas
# ============================================================

frecuencia_adecuada_variable <- function(data, variable) {
  
  naturaleza <- naturaleza_variable(variable)
  x <- data[[variable]]
  
  if (naturaleza %in% c("Conteo", "Conteo de mora") || n_distinct(x, na.rm = TRUE) <= 30) {
    
    data %>%
      transmute(valor_original = .data[[variable]], default = default) %>%
      mutate(
        `Valor observado` = case_when(
          is.na(valor_original) ~ "Faltante",
          near(valor_original, round(valor_original)) ~ as.character(as.integer(round(valor_original))),
          TRUE ~ format(round(valor_original, 4), trim = TRUE, scientific = FALSE)
        )
      ) %>%
      group_by(valor_original, `Valor observado`) %>%
      summarise(
        Clientes = n(),
        `Clientes con default` = sum(default == 1, na.rm = TRUE),
        `Clientes sin default` = sum(default == 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(is.na(valor_original), valor_original) %>%
      mutate(
        `% de la base` = percent(Clientes / sum(Clientes), accuracy = 0.01),
        `% acumulado` = percent(cumsum(Clientes) / sum(Clientes), accuracy = 0.01),
        `Tasa de default` = percent(`Clientes con default` / Clientes, accuracy = 0.01)
      ) %>%
      select(`Valor observado`, Clientes, `% de la base`, `% acumulado`, `Clientes con default`, `Clientes sin default`, `Tasa de default`)
    
  } else {
    
    analisis_complementario_variable(data, variable) %>%
      select(
        `Rango / grupo` = Grupo,
        Clientes,
        `% de la base`,
        `Clientes con default`,
        `Clientes sin default`,
        `Tasa de default`
      )
  }
}

# ============================================================
# Comentarios redactados como equipo de análisis
# ============================================================

comentario_perfil_variable <- function(data, variable) {
  
  resumen <- resumen_variable_default(data, variable)
  comp <- analisis_complementario_variable(data, variable)
  nombre <- nombre_variable(variable)
  naturaleza <- naturaleza_variable(variable)
  
  med_def <- resumen %>% filter(`Estado del cliente` == "Default") %>% pull(Mediana)
  med_nodef <- resumen %>% filter(`Estado del cliente` == "No Default") %>% pull(Mediana)
  q1_def <- resumen %>% filter(`Estado del cliente` == "Default") %>% pull(`P25 / Q1`)
  q3_def <- resumen %>% filter(`Estado del cliente` == "Default") %>% pull(`P75 / Q3`)
  q1_nodef <- resumen %>% filter(`Estado del cliente` == "No Default") %>% pull(`P25 / Q1`)
  q3_nodef <- resumen %>% filter(`Estado del cliente` == "No Default") %>% pull(`P75 / Q3`)
  
  grupo_max <- comp %>% filter(!is.na(tasa_default)) %>% arrange(desc(tasa_default)) %>% slice(1)
  grupo_min <- comp %>% filter(!is.na(tasa_default)) %>% arrange(tasa_default) %>% slice(1)
  
  if (variable == "edad") {
    tagList(
      h4("Lectura de la edad"),
      p("Los clientes con default presentan una mediana de edad de ", strong(med_def), 
        " años, mientras que los clientes sin default presentan una mediana de ", strong(med_nodef), " años."),
      p("El 50% central de los clientes con default se ubica entre ", strong(q1_def), " y ", strong(q3_def),
        " años. En los clientes sin default, el 50% central se ubica entre ", strong(q1_nodef), " y ", strong(q3_nodef), " años."),
      p("Esto muestra que el grupo sin default tiende a concentrarse en edades mayores. Aun así, existe superposición: hay edades que aparecen tanto en clientes con default como en clientes sin default. Por eso la edad aporta señal, pero no decide por sí sola el riesgo."),
      p("La tasa de default más alta se observa en ", strong(as.character(grupo_max$Grupo)), " con ", strong(grupo_max$`Tasa de default`),
        ", mientras que la menor se observa en ", strong(as.character(grupo_min$Grupo)), " con ", strong(grupo_min$`Tasa de default`), ".")
    )
    
  } else if (variable == "mto_ingreso_mensual") {
    tagList(
      h4("Lectura del ingreso mensual"),
      p("El ingreso es una variable central para evaluar capacidad de pago, pero debe tratarse con cuidado porque contiene faltantes y valores muy bajos o muy altos."),
      p("La mediana en clientes con default es ", strong(comma(med_def)), ", mientras que en clientes sin default es ", strong(comma(med_nodef)), "."),
      p("Para esta variable no conviene interpretar cada monto exacto; es mejor revisar rangos de ingreso, faltantes, ingresos iguales a cero y percentiles altos."),
      p("El grupo con mayor tasa de default dentro de la clasificación usada es ", strong(as.character(grupo_max$Grupo)), " con ", strong(grupo_max$`Tasa de default`), ".")
    )
    
  } else if (variable %in% c("prct_uso_tc", "prct_deuda_vs_ingresos")) {
    tagList(
      h4(paste("Lectura de", nombre)),
      p("Esta variable se interpreta como una razón financiera. Por eso el análisis debe revisar tanto el nivel típico como la escala de los valores extremos."),
      p("La mediana en clientes con default es ", strong(round(med_def, 4)), ", mientras que en clientes sin default es ", strong(round(med_nodef, 4)), "."),
      p("El equipo no debería recortar ni transformar esta variable sin verificar primero si los valores altos representan situaciones reales de endeudamiento/uso o problemas de registro."),
      p("El grupo con mayor tasa de default es ", strong(as.character(grupo_max$Grupo)), " con ", strong(grupo_max$`Tasa de default`), ".")
    )
    
  } else if (str_detect(variable, "retraso|retrasados")) {
    tagList(
      h4(paste("Lectura de", nombre)),
      p("Esta variable pertenece al historial de pago. En este tipo de variables, la tabla de frecuencias y la tasa de default por número de retrasos son más informativas que un histograma tradicional."),
      p("La mediana puede ser cero en ambos grupos porque la mayoría de clientes no registra retrasos. Aun así, la tasa de default suele aumentar con pocos retrasos acumulados."),
      p("El grupo con mayor tasa de default es ", strong(as.character(grupo_max$Grupo)), " con ", strong(grupo_max$`Tasa de default`), "."),
      p("Antes de usar esta variable en el modelo se debe confirmar que representa historial conocido antes de la evaluación del cliente. Si se genera después del crédito analizado, produciría fuga de información.")
    )
    
  } else if (naturaleza == "Conteo") {
    tagList(
      h4(paste("Lectura de", nombre)),
      p("Esta es una variable de conteo. La lectura más útil no es solo la media, sino la frecuencia de cada valor y la tasa de default por grupos."),
      p("La mediana en clientes con default es ", strong(med_def), ", mientras que en clientes sin default es ", strong(med_nodef), "."),
      p("El grupo con mayor tasa de default es ", strong(as.character(grupo_max$Grupo)), " con ", strong(grupo_max$`Tasa de default`), ".")
    )
    
  } else {
    tagList(
      h4(paste("Lectura de", nombre)),
      p("La variable muestra una diferencia descriptiva entre clientes con y sin default, pero debe interpretarse junto con las demás variables."),
      p("La mediana en clientes con default es ", strong(med_def), ", mientras que en clientes sin default es ", strong(med_nodef), "."),
      p("El grupo con mayor tasa de default es ", strong(as.character(grupo_max$Grupo)), " con ", strong(grupo_max$`Tasa de default`), ".")
    )
  }
}

ficha_variable <- function(variable) {
  
  tibble(
    Campo = c("Variable", "Nombre de negocio", "Naturaleza", "Familia", "Uso analítico"),
    Descripción = c(
      variable,
      nombre_variable(variable),
      naturaleza_variable(variable),
      familia_variable(variable),
      rol_variable(variable)
    )
  )
}

