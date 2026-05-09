# ============================================================
# 04_outliers.R
# Valores atípicos, coherencia de datos y registros a revisar
# ============================================================

library(tidyverse)
library(scales)
library(shiny)

variables_outliers <- function(data) {
  
  data %>%
    select(where(is.numeric)) %>%
    select(-any_of(c("id", "default"))) %>%
    names()
}

# ============================================================
# Límites estadísticos por IQR
# ============================================================

calcular_limites_outlier <- function(data, variable) {
  
  x <- data[[variable]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_x <- IQR(x, na.rm = TRUE)
  
  tibble(
    variable = variable,
    q1 = q1,
    q3 = q3,
    iqr = iqr_x,
    limite_inferior = q1 - 1.5 * iqr_x,
    limite_superior = q3 + 1.5 * iqr_x
  )
}

datos_outlier_variable <- function(data, variable) {
  
  limites <- calcular_limites_outlier(data, variable)
  lim_inf <- limites$limite_inferior[1]
  lim_sup <- limites$limite_superior[1]
  
  data %>%
    rowid_to_column("fila") %>%
    mutate(
      estado_default = if_else(default == 1, "Default", "No Default"),
      variable_analizada = variable,
      valor = .data[[variable]],
      limite_inferior_iqr = lim_inf,
      limite_superior_iqr = lim_sup,
      clasificacion_iqr = case_when(
        is.na(valor) ~ "Faltante",
        valor < lim_inf ~ "Atípico inferior por IQR",
        valor > lim_sup ~ "Atípico superior por IQR",
        TRUE ~ "Dentro del rango IQR"
      )
    )
}

resumen_outliers <- function(data) {
  
  variables <- variables_outliers(data)
  
  map_dfr(variables, function(variable) {
    x <- data[[variable]]
    datos_var <- datos_outlier_variable(data, variable)
    n_validos <- sum(!is.na(x))
    n_out <- sum(str_detect(datos_var$clasificacion_iqr, "Atípico"), na.rm = TRUE)
    
    tibble(
      Variable = variable,
      `Nombre de negocio` = nombre_variable(variable),
      Naturaleza = naturaleza_variable(variable),
      `Registros válidos` = n_validos,
      Faltantes = sum(is.na(x)),
      Mínimo = min(x, na.rm = TRUE),
      `P01` = quantile(x, 0.01, na.rm = TRUE),
      `P05` = quantile(x, 0.05, na.rm = TRUE),
      `P25 / Q1` = quantile(x, 0.25, na.rm = TRUE),
      `Mediana / Q2` = median(x, na.rm = TRUE),
      `P75 / Q3` = quantile(x, 0.75, na.rm = TRUE),
      `P95` = quantile(x, 0.95, na.rm = TRUE),
      `P99` = quantile(x, 0.99, na.rm = TRUE),
      Máximo = max(x, na.rm = TRUE),
      IQR = IQR(x, na.rm = TRUE),
      `Límite inferior IQR` = unique(datos_var$limite_inferior_iqr)[1],
      `Límite superior IQR` = unique(datos_var$limite_superior_iqr)[1],
      `Atípicos por IQR` = n_out,
      `% atípicos` = n_out / n_validos
    )
  }) %>%
    mutate(
      `% atípicos` = percent(`% atípicos`, accuracy = 0.01),
      across(
        c(Mínimo, `P01`, `P05`, `P25 / Q1`, `Mediana / Q2`, `P75 / Q3`, `P95`, `P99`, Máximo, IQR, `Límite inferior IQR`, `Límite superior IQR`),
        ~ round(.x, 2)
      )
    ) %>%
    arrange(desc(`Atípicos por IQR`))
}

resumen_outlier_variable <- function(data, variable) {
  
  x <- data[[variable]]
  datos_var <- datos_outlier_variable(data, variable)
  n_validos <- sum(!is.na(x))
  n_out <- sum(str_detect(datos_var$clasificacion_iqr, "Atípico"), na.rm = TRUE)
  n_inf <- sum(datos_var$clasificacion_iqr == "Atípico inferior por IQR", na.rm = TRUE)
  n_sup <- sum(datos_var$clasificacion_iqr == "Atípico superior por IQR", na.rm = TRUE)
  
  tibble(
    Indicador = c(
      "Registros válidos",
      "Faltantes",
      "Mínimo",
      "Percentil 1",
      "Percentil 5",
      "Percentil 25 / Q1",
      "Mediana / Q2",
      "Percentil 75 / Q3",
      "Percentil 95",
      "Percentil 99",
      "Máximo",
      "IQR = Q3 - Q1",
      "Límite inferior IQR",
      "Límite superior IQR",
      "Atípicos por IQR",
      "Atípicos inferiores",
      "Atípicos superiores",
      "% atípicos por IQR"
    ),
    Valor = c(
      n_validos,
      sum(is.na(x)),
      round(min(x, na.rm = TRUE), 2),
      round(quantile(x, 0.01, na.rm = TRUE), 2),
      round(quantile(x, 0.05, na.rm = TRUE), 2),
      round(quantile(x, 0.25, na.rm = TRUE), 2),
      round(median(x, na.rm = TRUE), 2),
      round(quantile(x, 0.75, na.rm = TRUE), 2),
      round(quantile(x, 0.95, na.rm = TRUE), 2),
      round(quantile(x, 0.99, na.rm = TRUE), 2),
      round(max(x, na.rm = TRUE), 2),
      round(IQR(x, na.rm = TRUE), 2),
      round(unique(datos_var$limite_inferior_iqr)[1], 2),
      round(unique(datos_var$limite_superior_iqr)[1], 2),
      n_out,
      n_inf,
      n_sup,
      percent(n_out / n_validos, accuracy = 0.01)
    )
  )
}

conteo_outliers_variable <- function(data, variable) {
  
  datos_outlier_variable(data, variable) %>%
    count(clasificacion_iqr, name = "Clientes") %>%
    mutate(
      `% de la base` = percent(Clientes / sum(Clientes), accuracy = 0.01),
      clasificacion_iqr = factor(
        clasificacion_iqr,
        levels = c("Dentro del rango IQR", "Atípico inferior por IQR", "Atípico superior por IQR", "Faltante")
      )
    ) %>%
    arrange(clasificacion_iqr) %>%
    rename(`Clasificación IQR` = clasificacion_iqr)
}

# ============================================================
# Coherencia de datos: reglas de negocio / sentido común
# ============================================================

crear_revision <- function(data, punto, condicion, observacion_si_hay, decision_si_hay, observacion_si_no, decision_si_no) {
  
  n_total <- nrow(data)
  n_casos <- sum(condicion, na.rm = TRUE)
  
  tibble(
    `Punto a revisar` = punto,
    Casos = n_casos,
    `% de la base` = percent(n_casos / n_total, accuracy = 0.01),
    `Qué observamos` = if_else(n_casos > 0, observacion_si_hay, observacion_si_no),
    Decisión = if_else(n_casos > 0, decision_si_hay, decision_si_no)
  )
}

coherencia_variable <- function(data, variable) {
  
  x <- data[[variable]]
  p99 <- quantile(x, 0.99, na.rm = TRUE)
  
  if (variable == "edad") {
    bind_rows(
      crear_revision(
        data, "Clientes menores de 18 años", x < 18,
        "Aparecen clientes menores de edad. En un crédito formal esto no sería esperado.",
        "Revisar esos registros antes de modelar. Si no se valida la edad, tratarlos como datos dudosos.",
        "No aparecen clientes menores de edad.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Clientes mayores de 100 años", x > 100,
        "Existen muy pocos clientes con edad mayor a 100 años. No es imposible, pero en crédito bancario son casos poco frecuentes.",
        "Revisar registro por registro. Si no pueden validarse, marcarlos como casos especiales o excluirlos en una prueba de sensibilidad.",
        "No se observan edades mayores a 100 años.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Clientes de 90 años o más", x >= 90,
        "Hay clientes de 90 años o más. No son errores automáticamente, pero pueden representar un segmento con condiciones crediticias distintas.",
        "Conservar inicialmente, documentar el hallazgo y evaluar si influyen demasiado en el modelo.",
        "No se observan clientes de 90 años o más.",
        "No requiere ajuste por este punto."
      )
    )
    
  } else if (variable == "mto_ingreso_mensual") {
    bind_rows(
      crear_revision(
        data, "Ingreso no informado", is.na(x),
        "Hay clientes sin ingreso mensual registrado. La ausencia del dato puede ser informativa en riesgo crediticio.",
        "No eliminar automáticamente. Evaluar imputación con mediana y crear indicador de ingreso faltante.",
        "Todos los clientes tienen ingreso mensual registrado.",
        "No requiere imputación por faltantes."
      ),
      crear_revision(
        data, "Ingreso igual a cero", x == 0,
        "Hay ingresos registrados como cero. Puede ser real, pero también puede representar información no declarada.",
        "Revisar si el cero es un valor real o un código operativo antes de imputar o transformar.",
        "No se observan ingresos iguales a cero.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Ingreso positivo muy bajo", x > 0 & x <= 100,
        "Aparecen ingresos positivos muy bajos. En una variable mensual, estos valores llaman la atención.",
        "Revisar si corresponden a errores, ingresos parciales o codificaciones especiales.",
        "No se observan ingresos positivos muy bajos.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Ingresos por encima del percentil 99", x > p99,
        "Hay montos ubicados en el 1% más alto. Pueden ser reales, pero pueden influir mucho en modelos sensibles a escala.",
        "Conservar inicialmente y evaluar transformación logarítmica, escalamiento o winsorización si afecta el modelo.",
        "No se observan ingresos por encima del percentil 99.",
        "No requiere ajuste por este punto."
      )
    )
    
  } else if (variable %in% c("prct_uso_tc", "prct_deuda_vs_ingresos")) {
    bind_rows(
      crear_revision(
        data, "Valores faltantes", is.na(x),
        "Hay valores faltantes. En razones financieras, la ausencia del dato puede afectar la lectura del riesgo.",
        "Evaluar imputación y, si el porcentaje es relevante, crear indicador de faltante.",
        "No se observan valores faltantes.",
        "No requiere imputación por faltantes."
      ),
      crear_revision(
        data, "Valores negativos", x < 0,
        "Aparecen valores negativos. Para una razón financiera o porcentaje normalmente no tendría sentido.",
        "Revisar con la fuente de datos antes de modelar.",
        "No se observan valores negativos.",
        "No requiere ajuste por valores negativos."
      ),
      crear_revision(
        data, "Valores mayores a 1", x > 1,
        "Hay valores mayores a 1. Puede ser correcto si la variable está como razón, pero no si se esperaba una proporción acotada entre 0 y 1.",
        "Confirmar escala con el diccionario y el negocio antes de recortar.",
        "No hay valores mayores a 1.",
        "La escala parece compatible con proporción 0 a 1, si así lo define el negocio."
      ),
      crear_revision(
        data, "Valores muy altos", x > 100,
        "Hay valores extremadamente altos. Pueden ser casos reales de deuda/uso extremo o problemas de registro.",
        "Revisar registros extremos y decidir si se transforman, se winsorizan o se conservan documentados.",
        "No se observan valores mayores a 100.",
        "No requiere ajuste por este punto."
      )
    )
    
  } else if (str_detect(variable, "retraso|retrasados")) {
    bind_rows(
      crear_revision(
        data, "Retrasos negativos", x < 0,
        "Hay conteos negativos. Eso no tiene interpretación natural para retrasos.",
        "Revisar como error o código especial.",
        "No se observan retrasos negativos.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Valores 98, 99 o superiores", x >= 98,
        "Aparecen valores 98, 99 o superiores. En bases crediticias, estos valores pueden representar códigos especiales y no necesariamente cantidades reales.",
        "Validar obligatoriamente con diccionario o fuente antes de usar esta variable en el modelo.",
        "No se observan valores 98, 99 o superiores.",
        "No hay señal de código especial bajo este criterio."
      ),
      crear_revision(
        data, "Más de 20 retrasos", x > 20 & x < 90,
        "Hay clientes con muchos retrasos. Puede ser señal real de deterioro crediticio, pero debe revisarse la escala.",
        "Revisar consistencia con las otras variables de mora y decidir si conviene agrupar.",
        "No se observan clientes con más de 20 retrasos reales bajo este criterio.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Riesgo de fuga de información", rep(TRUE, nrow(data)),
        "Esta variable describe comportamiento de pago. Puede ser muy predictiva, pero también puede filtrar información posterior al otorgamiento.",
        "Antes de modelar, definir si corresponde a historial previo disponible al momento de evaluar al cliente.",
        "",
        ""
      )
    )
    
  } else if (variable == "nro_dependiente") {
    bind_rows(
      crear_revision(
        data, "Dependientes no informados", is.na(x),
        "Hay clientes sin número de dependientes registrado.",
        "Evaluar si corresponde imputar con mediana, con cero o crear indicador de faltante según el significado del dato ausente.",
        "Todos los clientes tienen número de dependientes registrado.",
        "No requiere imputación por faltantes."
      ),
      crear_revision(
        data, "Dependientes negativos", x < 0,
        "Hay valores negativos en una variable de conteo.",
        "Revisar como posible error de registro.",
        "No se observan dependientes negativos.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Más de 10 dependientes", x > 10,
        "Hay clientes con más de 10 dependientes. No es imposible, pero es poco frecuente.",
        "Revisar registros extremos y conservarlos si son coherentes con la fuente.",
        "No se observan clientes con más de 10 dependientes.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Dependientes con decimales", !is.na(x) & x %% 1 != 0,
        "Hay decimales en una variable que debería ser conteo.",
        "Validar antes de redondear o imputar.",
        "No se observan valores decimales.",
        "No requiere ajuste por este punto."
      )
    )
    
  } else if (str_detect(variable, "nro_")) {
    bind_rows(
      crear_revision(
        data, "Conteos negativos", x < 0,
        "Hay valores negativos en una variable de conteo.",
        "Revisar como posible error o código especial.",
        "No se observan conteos negativos.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Conteos con decimales", !is.na(x) & x %% 1 != 0,
        "Hay decimales en una variable que parece ser conteo.",
        "Validar con el diccionario antes de redondear.",
        "No se observan decimales.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Valores por encima del percentil 99", x > p99,
        "Hay valores en el extremo superior. Pueden ser reales, pero conviene revisar algunos casos.",
        "Decidir si se conservan, agrupan o transforman según su coherencia.",
        "No se observan valores por encima del percentil 99.",
        "No requiere ajuste por este punto."
      )
    )
    
  } else {
    bind_rows(
      crear_revision(
        data, "Valores faltantes", is.na(x),
        "Hay valores faltantes.",
        "Definir estrategia de imputación o exclusión antes de modelar.",
        "No se observan valores faltantes.",
        "No requiere ajuste por este punto."
      ),
      crear_revision(
        data, "Valores por encima del percentil 99", x > p99,
        "Hay valores en el extremo superior.",
        "Revisar si son reales antes de transformar o eliminar.",
        "No se observan valores por encima del percentil 99.",
        "No requiere ajuste por este punto."
      )
    )
  }
}

# ============================================================
# Registros específicos a revisar: IQR + reglas de coherencia
# ============================================================

registros_revision_variable <- function(data, variable) {
  
  base <- datos_outlier_variable(data, variable)
  x <- base$valor
  
  condicion_negocio <- case_when(
    variable == "edad" ~ x < 18 | x > 100,
    variable == "mto_ingreso_mensual" ~ is.na(x) | x == 0 | (x > 0 & x <= 100) | x > quantile(x, 0.99, na.rm = TRUE),
    variable %in% c("prct_uso_tc", "prct_deuda_vs_ingresos") ~ is.na(x) | x < 0 | x > 1 | x > 100,
    str_detect(variable, "retraso|retrasados") ~ x < 0 | x >= 98 | (x > 20 & x < 90),
    variable == "nro_dependiente" ~ is.na(x) | x < 0 | x > 10 | (!is.na(x) & x %% 1 != 0),
    str_detect(variable, "nro_") ~ x < 0 | (!is.na(x) & x %% 1 != 0) | x > quantile(x, 0.99, na.rm = TRUE),
    TRUE ~ is.na(x) | x > quantile(x, 0.99, na.rm = TRUE)
  )
  
  base %>%
    mutate(
      revisar_por_iqr = str_detect(clasificacion_iqr, "Atípico"),
      revisar_por_negocio = condicion_negocio,
      motivo_revision = case_when(
        revisar_por_iqr & revisar_por_negocio ~ "Atípico por IQR y criterio de negocio",
        revisar_por_iqr ~ "Atípico por IQR",
        revisar_por_negocio ~ "Criterio de negocio / coherencia",
        TRUE ~ "Sin revisión"
      )
    ) %>%
    filter(motivo_revision != "Sin revisión") %>%
    select(
      Fila = fila,
      ID = id,
      Default = default,
      `Estado default` = estado_default,
      Variable = variable_analizada,
      Valor = valor,
      `Clasificación IQR` = clasificacion_iqr,
      `Límite inferior IQR` = limite_inferior_iqr,
      `Límite superior IQR` = limite_superior_iqr,
      `Motivo de revisión` = motivo_revision
    ) %>%
    mutate(
      Valor = round(Valor, 4),
      `Límite inferior IQR` = round(`Límite inferior IQR`, 4),
      `Límite superior IQR` = round(`Límite superior IQR`, 4)
    ) %>%
    arrange(desc(Valor))
}

# ============================================================
# Relación entre atípicos y Default
# ============================================================

resumen_outliers_default <- function(data, variable) {
  
  datos_outlier_variable(data, variable) %>%
    mutate(
      Grupo = case_when(
        clasificacion_iqr %in% c("Atípico inferior por IQR", "Atípico superior por IQR") ~ "Atípico por IQR",
        clasificacion_iqr == "Dentro del rango IQR" ~ "Dentro del rango IQR",
        TRUE ~ "Faltante"
      ),
      Grupo = factor(Grupo, levels = c("Dentro del rango IQR", "Atípico por IQR", "Faltante"))
    ) %>%
    group_by(Grupo) %>%
    summarise(
      Clientes = n(),
      `Clientes con default` = sum(default == 1, na.rm = TRUE),
      `Clientes sin default` = sum(default == 0, na.rm = TRUE),
      tasa_default = `Clientes con default` / Clientes,
      .groups = "drop"
    ) %>%
    mutate(`Tasa de default` = percent(tasa_default, accuracy = 0.01))
}

# ============================================================
# Comentario de outliers
# ============================================================

comentario_outlier_variable <- function(data, variable) {
  
  resumen <- resumen_outlier_variable(data, variable)
  nombre <- nombre_variable(variable)
  naturaleza <- naturaleza_variable(variable)
  
  lim_inf <- resumen %>% filter(Indicador == "Límite inferior IQR") %>% pull(Valor)
  lim_sup <- resumen %>% filter(Indicador == "Límite superior IQR") %>% pull(Valor)
  n_out <- resumen %>% filter(Indicador == "Atípicos por IQR") %>% pull(Valor)
  pct_out <- resumen %>% filter(Indicador == "% atípicos por IQR") %>% pull(Valor)
  
  if (str_detect(variable, "retraso|retrasados")) {
    tagList(
      h4(paste("Valores a revisar en", nombre)),
      p("El criterio IQR detecta ", strong(n_out), " registros atípicos (", strong(pct_out), "). En variables de mora esto debe leerse con cuidado: si la mayoría de clientes tiene cero retrasos, el IQR puede marcar como atípico cualquier valor positivo."),
      p("Por eso, para esta variable el equipo debe priorizar la tabla de frecuencias, la identificación de códigos 98/99 y la validación de fuga de información antes de decidir un tratamiento."),
      p("Los límites IQR calculados son ", strong(lim_inf), " y ", strong(lim_sup), ". Estos límites son puntos de alerta estadística, no reglas automáticas de eliminación.")
    )
  } else if (variable == "edad") {
    tagList(
      h4("Valores a revisar en edad"),
      p("El criterio IQR define un límite inferior de ", strong(lim_inf), " y un límite superior de ", strong(lim_sup), ". Esto no significa que existan clientes con esas edades; son fronteras estadísticas para identificar valores poco frecuentes."),
      p("Las edades por encima del límite superior deben revisarse. En particular, edades mayores a 100 años son posibles, pero poco habituales en un contexto crediticio y conviene validarlas antes de modelar."),
      p("No se recomienda eliminar automáticamente estos registros; primero deben documentarse y revisarse por coherencia de negocio.")
    )
  } else if (variable == "mto_ingreso_mensual") {
    tagList(
      h4("Valores a revisar en ingreso mensual"),
      p("Se detectan ", strong(n_out), " valores atípicos por IQR (", strong(pct_out), "). En ingresos es normal encontrar asimetría: muchos clientes tienen ingresos moderados y pocos clientes tienen ingresos muy altos."),
      p("Además de los outliers altos, el equipo debe revisar faltantes, ingresos iguales a cero e ingresos positivos muy bajos. Esos casos pueden representar ausencia de información, errores o situaciones reales."),
      p("La decisión más prudente será conservar la variable, crear indicador de ingreso faltante y evaluar transformación logarítmica o winsorización más adelante.")
    )
  } else if (variable %in% c("prct_uso_tc", "prct_deuda_vs_ingresos")) {
    tagList(
      h4(paste("Valores a revisar en", nombre)),
      p("Esta razón financiera presenta ", strong(n_out), " valores atípicos por IQR (", strong(pct_out), "). Los valores altos pueden ser clientes con uso/deuda extrema, pero también pueden revelar problemas de escala."),
      p("Antes de transformar la variable se debe confirmar si está expresada como proporción, porcentaje o razón no acotada."),
      p("El tratamiento final debe decidirse luego de revisar percentiles, histogramas y coherencia con el negocio.")
    )
  } else if (naturaleza == "Conteo") {
    tagList(
      h4(paste("Valores a revisar en", nombre)),
      p("En variables de conteo, los valores altos pueden ser casos reales aunque poco frecuentes. El IQR ayuda a detectarlos, pero la decisión debe apoyarse también en frecuencias y sentido de negocio."),
      p("Se detectan ", strong(n_out), " registros atípicos por IQR (", strong(pct_out), "). No se recomienda eliminarlos sin revisar si son conteos plausibles." )
    )
  } else {
    tagList(
      h4(paste("Valores a revisar en", nombre)),
      p("El criterio IQR detecta ", strong(n_out), " registros atípicos (", strong(pct_out), "). Estos casos deben revisarse antes de cualquier transformación."),
      p("Un outlier estadístico no es necesariamente un error. La decisión depende de si el valor es coherente con el negocio y con la definición de la variable.")
    )
  }
}

