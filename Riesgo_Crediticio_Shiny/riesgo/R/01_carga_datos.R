# ============================================================
# 01_carga_datos.R
# Carga, estandarización y tipado inicial de la base de créditos
# ============================================================

library(tidyverse)
library(janitor)
library(readxl)
library(here)

buscar_archivo <- function(nombres, carpetas = c(
  here("data", "raw"),
  here("data"),
  here()
)) {
  candidatos <- expand.grid(carpeta = carpetas, nombre = nombres, stringsAsFactors = FALSE) %>%
    mutate(ruta = file.path(carpeta, nombre)) %>%
    pull(ruta)
  
  encontrado <- candidatos[file.exists(candidatos)]
  
  if (length(encontrado) == 0) {
    stop(
      paste0(
        "No se encontró el archivo esperado. Se buscó: ",
        paste(nombres, collapse = ", "),
        " en data/raw, data y la carpeta del proyecto."
      ),
      call. = FALSE
    )
  }
  
  encontrado[1]
}

convertir_numericas_credito <- function(data) {
  
  variables_numericas <- c(
    "id",
    "default",
    "prct_uso_tc",
    "edad",
    "nro_prestao_retrasados",
    "prct_deuda_vs_ingresos",
    "mto_ingreso_mensual",
    "nro_prod_financieros_deuda",
    "nro_retraso_60dias",
    "nro_creditos_hipotecarios",
    "nro_retraso_ultm3anios",
    "nro_dependiente"
  )
  
  data %>%
    mutate(
      across(
        any_of(variables_numericas),
        ~ readr::parse_number(
          as.character(.x),
          locale = readr::locale(decimal_mark = ".", grouping_mark = ","),
          na = c("", "NA", "N/A", "NULL", "NaN")
        )
      ),
      default = as.integer(default),
      id = as.integer(id),
      edad = as.integer(edad)
    )
}

cargar_datos <- function() {
  
  ruta_data <- buscar_archivo(c(
    "2_DS_creditos_SIPREMO.csv",
    "2_DS_creditos_SIPREMO(1).csv"
  ))
  
  ruta_diccionario <- buscar_archivo(c(
    "1_Diccionario credito.xls",
    "1_Diccionario credito(1).xls"
  ))
  
  data_raw <- read_csv(
    ruta_data,
    na = c("", "NA", "N/A", "NULL", "NaN"),
    show_col_types = FALSE
  )
  
  diccionario <- read_excel(ruta_diccionario) %>%
    clean_names() %>%
    filter(!if_all(everything(), ~ is.na(.x))) %>%
    mutate(
      variable_original = nombre_variable,
      variable = nombre_variable %>%
        janitor::make_clean_names(),
      descripcion = descripcion,
      tipo = tipo
    ) %>%
    select(variable, variable_original, descripcion, tipo)
  
  data <- data_raw %>%
    clean_names() %>%
    convertir_numericas_credito()
  
  list(
    data = data,
    diccionario = diccionario,
    ruta_data = ruta_data,
    ruta_diccionario = ruta_diccionario
  )
}

