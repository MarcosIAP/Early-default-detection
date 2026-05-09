# ============================================================
# 02_auditoria_datos.R
# Funciones de auditoría inicial de calidad de datos
# ============================================================

library(tidyverse)
library(scales)

resumen_faltantes <- function(data) {
  
  data %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "Faltantes"
    ) %>%
    mutate(
      `% faltante` = percent(Faltantes / nrow(data), accuracy = 0.01)
    ) %>%
    arrange(desc(Faltantes))
}

resumen_default <- function(data) {
  
  data %>%
    count(default, name = "Clientes") %>%
    mutate(
      Estado = if_else(default == 1, "Default", "No Default"),
      Proporcion = Clientes / sum(Clientes),
      `Proporción` = percent(Proporcion, accuracy = 0.01)
    ) %>%
    select(Estado, Clientes, Proporcion, `Proporción`)
}

resumen_general <- function(data) {
  
  tibble(
    Indicador = c(
      "Número de clientes",
      "Número de variables",
      "Clientes en default",
      "Clientes sin default",
      "Tasa de default"
    ),
    Valor = c(
      comma(nrow(data)),
      comma(ncol(data)),
      comma(sum(data$default == 1, na.rm = TRUE)),
      comma(sum(data$default == 0, na.rm = TRUE)),
      percent(mean(data$default == 1, na.rm = TRUE), accuracy = 0.01)
    )
  )
}

resumen_tipos_variables <- function(data) {
  
  tibble(
    Variable = names(data),
    Clase = map_chr(data, ~ paste(class(.x), collapse = ", ")),
    `Valores únicos` = map_int(data, ~ n_distinct(.x, na.rm = TRUE)),
    Faltantes = map_int(data, ~ sum(is.na(.x))),
    `% faltante` = percent(Faltantes / nrow(data), accuracy = 0.01)
  )
}

texto_auditoria_default <- function(data) {
  
  tasa <- mean(data$default == 1, na.rm = TRUE)
  n_default <- sum(data$default == 1, na.rm = TRUE)
  n_no_default <- sum(data$default == 0, na.rm = TRUE)
  
  tagList(
    h4("Lectura de la variable objetivo"),
    p(
      "La base contiene ", strong(comma(n_default)), " clientes en default y ",
      strong(comma(n_no_default)), " clientes sin default. La tasa observada de default es ",
      strong(percent(tasa, accuracy = 0.01)), "."
    ),
    p(
      "El evento de interés es minoritario. Por ello, cuando se construya el modelo no será suficiente mirar el accuracy: ",
      "se deberá evaluar sensibilidad, especificidad, AUC, matriz de confusión y capacidad de identificar correctamente a los clientes con default."
    )
  )
}

