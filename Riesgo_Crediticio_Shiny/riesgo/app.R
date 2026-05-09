# ============================================================
# app.R
# Aplicación Shiny: Análisis predictivo del incumplimiento crediticio
# ============================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)

p_load(
  shiny,
  bslib,
  tidyverse,
  DT,
  plotly,
  scales,
  bsicons,
  here
)

source(here("R", "01_carga_datos.R"))
source(here("R", "02_auditoria_datos.R"))
source(here("R", "03_eda_default.R"))
source(here("R", "04_outliers.R"))
source(here("R", "05_decisiones_eda.R"))

base <- cargar_datos()

data <- base$data
diccionario <- base$diccionario

variables_perfil <- variables_perfil_default(data)
variables_outlier <- variables_outliers(data)

variable_perfil_default <- if ("edad" %in% variables_perfil) "edad" else variables_perfil[1]
variable_outlier_default <- if ("edad" %in% variables_outlier) "edad" else variables_outlier[1]

formato_tabla <- function(tabla, page_length = 10, filtros = FALSE) {
  datatable(
    tabla,
    rownames = FALSE,
    filter = if (filtros) "top" else "none",
    options = list(
      pageLength = page_length,
      autoWidth = TRUE,
      scrollX = TRUE,
      language = list(
        search = "Buscar:",
        lengthMenu = "Mostrar _MENU_ registros",
        info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
        paginate = list(previous = "Anterior", `next` = "Siguiente")
      )
    )
  )
}

ui <- page_navbar(
  
  title = "Riesgo Crediticio",
  fillable = FALSE,
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  header = tags$head(
    tags$style(
      HTML("
        body {
          background-color: #f6f8fb;
        }

        .card {
          border-radius: 14px;
          border: 1px solid #e5e7eb;
          box-shadow: 0 4px 14px rgba(0,0,0,0.05);
          margin-bottom: 18px;
        }

        .card-header {
          font-weight: 700;
          background-color: #ffffff;
          border-bottom: 1px solid #e5e7eb;
        }

        .navbar {
          box-shadow: 0 3px 12px rgba(0,0,0,0.08);
        }

        .selectize-input {
          border-radius: 10px;
        }

        .descripcion-corta {
          color: #4b5563;
          font-size: 15px;
          line-height: 1.55;
        }

        .interpretacion-texto {
          font-size: 16px;
          line-height: 1.65;
        }

        .nota-equipo {
          background: #eef7f4;
          border-left: 5px solid #18bc9c;
          padding: 14px 16px;
          border-radius: 10px;
          margin-bottom: 15px;
        }
      ")
    )
  ),
  
  # ==========================================================
  # 1. INICIO
  # ==========================================================
  
  nav_panel(
    "Inicio",
    
    layout_columns(
      col_widths = c(12),
      
      card(
        card_header("Proyecto de análisis de crédito"),
        
        h2("Análisis del incumplimiento crediticio"),
        
        div(
          class = "nota-equipo",
          p(
            "Este tablero organiza el análisis : primero se revisa la calidad de la base, luego se analiza cada variable según su naturaleza y finalmente se documentan decisiones preliminares antes de preparar los datos para modelamiento."
          )
        ),
        
        p(
          "La variable objetivo es ", strong("Default"), ", entendida como cliente con incumplimiento severo de pago. El análisis no afirma causalidad; identifica patrones, señales de riesgo y puntos que requieren tratamiento antes de construir un modelo."
        ),
        
        tags$hr(),
        
        p(strong("Base analizada: "), "clientes con información crediticia, financiera y de comportamiento de pago."),
        p(strong("Enfoque: "), "auditoría de datos, EDA, revisión de outliers, coherencia de negocio y decisiones preliminares."),
        p(strong("Herramientas: "), "R, Shiny, estadística aplicada y visualización interactiva.")
      )
    ),
    
    layout_columns(
      col_widths = c(4, 4, 4),
      
      value_box(
        title = "Clientes",
        value = textOutput("n_clientes"),
        showcase = bs_icon("people")
      ),
      
      value_box(
        title = "Variables",
        value = textOutput("n_variables"),
        showcase = bs_icon("table")
      ),
      
      value_box(
        title = "Tasa de default",
        value = textOutput("tasa_default"),
        showcase = bs_icon("exclamation-triangle")
      )
    )
  ),
  
  # ==========================================================
  # 2. AUDITORÍA Y EDA
  # ==========================================================
  
  nav_menu(
    "Auditoría y EDA",
    
    # ----------------------------------------------------------
    # 2.1 CALIDAD DE DATOS
    # ----------------------------------------------------------
    
    nav_panel(
      "2.1 Calidad de datos",
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Distribución de la variable Default"),
          plotlyOutput("grafico_default", height = "420px")
        ),
        
        card(
          card_header("Lectura del equipo"),
          uiOutput("texto_default")
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Valores faltantes por variable"),
          DTOutput("tabla_faltantes")
        ),
        
        card(
          card_header("Tipos y valores únicos"),
          DTOutput("tabla_tipos")
        )
      ),
      
      card(
        card_header("Diccionario de variables"),
        DTOutput("tabla_diccionario")
      )
    ),
    
    # ----------------------------------------------------------
    # 2.2 PERFIL Y RIESGO POR VARIABLE
    # ----------------------------------------------------------
    
    nav_panel(
      "2.2 Perfil y riesgo por variable",
      
      card(
        card_header("Selección de variable"),
        
        layout_columns(
          col_widths = c(4, 8),
          
          div(
            selectInput(
              inputId = "variable_perfil",
              label = "Variable a analizar:",
              choices = variables_perfil,
              selected = variable_perfil_default,
              width = "100%"
            )
          ),
          
          div(
            class = "descripcion-corta",
            p(
              "Cada variable se analiza según su naturaleza. Para edad se usan rangos; para montos y razones se revisan percentiles, histogramas y grupos; para conteos se priorizan frecuencias y tasas de default por valor o por rangos."
            )
          )
        )
      ),
      
      navset_card_tab(
        title = "Análisis de la variable seleccionada",
        
        nav_panel(
          "Lectura",
          layout_columns(
            col_widths = c(5, 7),
            card(
              card_header("Ficha de la variable"),
              DTOutput("tabla_ficha_variable")
            ),
            card(
              card_header("Comentario del equipo"),
              uiOutput("interpretacion_variable")
            )
          )
        ),
        
        nav_panel(
          "Resumen por Default",
          p(
            class = "descripcion-corta",
            "Esta tabla compara la distribución de la variable entre clientes con default y sin default. Los cuartiles permiten ver dónde se concentra el 50% central de cada grupo."
          ),
          DTOutput("tabla_resumen_variable")
        ),
        
        nav_panel(
          "Frecuencias y rangos",
          p(
            class = "descripcion-corta",
            "Para conteos se muestran valores observados. Para variables continuas o con muchos valores distintos se muestran rangos construidos para análisis de negocio."
          ),
          DTOutput("tabla_frecuencia_adecuada")
        ),
        
        nav_panel(
          "Tasa de default",
          p(
            class = "descripcion-corta",
            "Este gráfico muestra la proporción de clientes con default dentro de cada grupo de la variable seleccionada. Es una lectura más útil para riesgo que observar solo medias o medianas."
          ),
          plotlyOutput("grafico_tasa_variable", height = "560px")
        ),
        
        nav_panel(
          "Distribución adecuada",
          p(
            class = "descripcion-corta",
            "El tipo de gráfico cambia según la naturaleza de la variable: histogramas para montos y razones; barras para variables de conteo o grupos discretos. En variables con valores extremos, el eje se limita al percentil 99 solo para facilitar la visualización."
          ),
          plotlyOutput("grafico_distribucion_adecuada", height = "560px")
        ),
        
        nav_panel(
          "Comparación por Default",
          p(
            class = "descripcion-corta",
            "El boxplot ayuda a comparar la ubicación y dispersión de la variable entre clientes con default y sin default. Para variables de conteo con muchos ceros debe leerse como apoyo, no como único diagnóstico."
          ),
          plotlyOutput("boxplot_variable", height = "560px")
        )
      )
    ),
    
    # ----------------------------------------------------------
    # 2.3 VALORES ATÍPICOS Y COHERENCIA
    # ----------------------------------------------------------
    
    nav_panel(
      "2.3 Valores atípicos y coherencia",
      
      card(
        card_header("Selección de variable"),
        
        layout_columns(
          col_widths = c(4, 8),
          
          div(
            selectInput(
              inputId = "variable_outlier",
              label = "Variable a revisar:",
              choices = variables_outlier,
              selected = variable_outlier_default,
              width = "100%"
            )
          ),
          
          div(
            class = "descripcion-corta",
            p(
              "Esta sección separa dos cosas: valores atípicos estadísticos y coherencia de negocio. Un valor puede ser raro sin ser error; por eso la decisión final debe considerar percentiles, frecuencias, sentido de negocio y relación con Default."
            )
          )
        )
      ),
      
      navset_card_tab(
        title = "Revisión de valores a tratar antes del modelo",
        
        nav_panel(
          "Resumen general",
          p(
            class = "descripcion-corta",
            "Resumen de percentiles, límites IQR y cantidad de valores atípicos por variable. Esta tabla sirve para priorizar revisiones, no para eliminar datos automáticamente."
          ),
          DTOutput("tabla_outliers_general")
        ),
        
        nav_panel(
          "Detalle estadístico",
          layout_columns(
            col_widths = c(5, 7),
            card(
              card_header("Resumen de la variable"),
              DTOutput("tabla_outlier_variable")
            ),
            card(
              card_header("Lectura del equipo"),
              uiOutput("interpretacion_outlier")
            )
          )
        ),
        
        nav_panel(
          "Frecuencias y rangos",
          p(
            class = "descripcion-corta",
            "Para conteos y posibles códigos especiales, esta tabla ayuda a ver cuántas veces aparece cada valor. Para variables continuas se muestran grupos de análisis más estables."
          ),
          DTOutput("tabla_frecuencia_outlier")
        ),
        
        nav_panel(
          "Coherencia de datos",
          p(
            class = "descripcion-corta",
            "Esta revisión aplica reglas de sentido común y criterios de negocio: edades poco plausibles, ingresos no informados, porcentajes fuera de escala, conteos negativos o posibles códigos 98/99."
          ),
          DTOutput("tabla_coherencia")
        ),
        
        nav_panel(
          "Registros a revisar",
          p(
            class = "descripcion-corta",
            "Aquí se listan registros que merecen revisión por IQR o por coherencia de negocio. No significa que deban eliminarse; significa que deben documentarse antes de modelar."
          ),
          DTOutput("tabla_registros_revision")
        ),
        
        nav_panel(
          "Atípicos y Default",
          p(
            class = "descripcion-corta",
            "Esta salida compara la tasa de default entre registros dentro del rango IQR, registros atípicos y faltantes. Sirve para saber si los valores extremos también contienen señal de riesgo."
          ),
          layout_columns(
            col_widths = c(5, 7),
            card(
              card_header("Tabla"),
              DTOutput("tabla_outliers_default")
            ),
            card(
              card_header("Gráfico"),
              plotlyOutput("grafico_outliers_default", height = "430px")
            )
          )
        ),
        
        nav_panel(
          "Boxplot",
          p(class = "descripcion-corta", "Boxplot de apoyo para observar valores alejados. En variables con valores extremadamente altos se limita la visualización al percentil 99 para evitar que el gráfico pierda legibilidad."),
          plotlyOutput("boxplot_outlier", height = "560px")
        ),
        
        nav_panel(
          "Histograma / barras",
          p(class = "descripcion-corta", "Distribución de la variable según su naturaleza. Para conteos se muestran barras por grupos; para variables continuas, histograma."),
          plotlyOutput("histograma_outlier", height = "560px")
        )
      )
    ),
    
    # ----------------------------------------------------------
    # 2.4 DECISIONES PRELIMINARES
    # ----------------------------------------------------------
    
    nav_panel(
      "2.4 Decisiones preliminares",
      
      card(
        card_header("Lectura de cierre"),
        uiOutput("texto_decisiones")
      ),
      
      card(
        card_header("Matriz de decisiones antes de preparar datos"),
        DTOutput("tabla_decisiones_eda")
      )
    )
  ),
  
  # ==========================================================
  # 3. VISTA DE DATOS
  # ==========================================================
  
  nav_panel(
    "Vista de datos",
    
    card(
      card_header("Primeras observaciones de la base"),
      DTOutput("tabla_datos")
    )
  )
)

server <- function(input, output, session) {
  
  # ==========================================================
  # Indicadores principales
  # ==========================================================
  
  output$n_clientes <- renderText({
    comma(nrow(data))
  })
  
  output$n_variables <- renderText({
    comma(ncol(data))
  })
  
  output$tasa_default <- renderText({
    percent(mean(data$default == 1, na.rm = TRUE), accuracy = 0.01)
  })
  
  # ==========================================================
  # 2.1 Calidad de datos
  # ==========================================================
  
  output$grafico_default <- renderPlotly({
    
    resumen <- resumen_default(data) %>%
      mutate(Estado = factor(Estado, levels = c("Default", "No Default"))) %>%
      arrange(Estado)
    
    plot_ly(
      data = resumen,
      x = ~Estado,
      y = ~Proporcion,
      type = "bar",
      text = ~`Proporción`,
      textposition = "auto",
      marker = list(
        color = c("#00B2EE", "#C0FF3E"),
        line = list(color = "black", width = 1.5)
      ),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Clientes: %{customdata}<br>",
        "Proporción: %{y:.2%}<br>",
        "<extra></extra>"
      ),
      customdata = ~Clientes
    ) %>%
      layout(
        title = "Proporción de clientes con y sin default",
        xaxis = list(title = ""),
        yaxis = list(title = "Proporción", tickformat = ".0%", range = c(0, 1)),
        showlegend = FALSE
      )
  })
  
  output$texto_default <- renderUI({
    texto_auditoria_default(data)
  })
  
  output$tabla_faltantes <- renderDT({
    formato_tabla(resumen_faltantes(data), page_length = 12)
  })
  
  output$tabla_tipos <- renderDT({
    formato_tabla(resumen_tipos_variables(data), page_length = 12)
  })
  
  output$tabla_diccionario <- renderDT({
    formato_tabla(diccionario, page_length = 12, filtros = TRUE)
  })
  
  output$tabla_datos <- renderDT({
    formato_tabla(head(data, 100), page_length = 10, filtros = TRUE)
  })
  
  # ==========================================================
  # 2.2 Perfil y riesgo por variable
  # ==========================================================
  
  output$tabla_ficha_variable <- renderDT({
    req(input$variable_perfil)
    formato_tabla(ficha_variable(input$variable_perfil), page_length = 5)
  })
  
  output$interpretacion_variable <- renderUI({
    req(input$variable_perfil)
    div(class = "interpretacion-texto", comentario_perfil_variable(data, input$variable_perfil))
  })
  
  output$tabla_resumen_variable <- renderDT({
    req(input$variable_perfil)
    formato_tabla(resumen_variable_default(data, input$variable_perfil), page_length = 5)
  })
  
  output$tabla_frecuencia_adecuada <- renderDT({
    req(input$variable_perfil)
    formato_tabla(frecuencia_adecuada_variable(data, input$variable_perfil), page_length = 15, filtros = TRUE)
  })
  
  output$grafico_tasa_variable <- renderPlotly({
    req(input$variable_perfil)
    
    resumen <- analisis_complementario_variable(data, input$variable_perfil)
    max_y <- max(resumen$tasa_default, na.rm = TRUE)
    
    plot_ly(
      data = resumen,
      x = ~Grupo,
      y = ~tasa_default,
      type = "bar",
      text = ~`Tasa de default`,
      textposition = "outside",
      cliponaxis = FALSE,
      marker = list(color = "#00B2EE", line = list(color = "black", width = 1.2)),
      customdata = ~cbind(Clientes, `Clientes con default`),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Clientes: %{customdata[0]}<br>",
        "Clientes con default: %{customdata[1]}<br>",
        "Tasa de default: %{y:.2%}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = paste("Tasa de default según", nombre_variable(input$variable_perfil)),
        xaxis = list(title = "", tickangle = -20),
        yaxis = list(title = "Tasa de default", tickformat = ".0%", range = c(0, min(1, max_y * 1.35))),
        margin = list(t = 80, b = 120, l = 70, r = 30),
        showlegend = FALSE
      )
  })
  
  output$grafico_distribucion_adecuada <- renderPlotly({
    req(input$variable_perfil)
    
    variable <- input$variable_perfil
    naturaleza <- naturaleza_variable(variable)
    
    if (naturaleza %in% c("Conteo", "Conteo de mora") || variable == "edad") {
      
      resumen <- crear_grupos_variable(data, variable) %>%
        mutate(`Estado default` = if_else(default == 1, "Default", "No Default")) %>%
        count(grupo, `Estado default`, name = "Clientes") %>%
        filter(!is.na(grupo))
      
      plot_ly(
        data = resumen,
        x = ~grupo,
        y = ~Clientes,
        color = ~`Estado default`,
        colors = c("#00B2EE", "#C0FF3E"),
        type = "bar",
        marker = list(line = list(color = "black", width = 0.7)),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Clientes: %{y}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = paste("Distribución por grupos de", nombre_variable(variable)),
          xaxis = list(title = "", tickangle = -20),
          yaxis = list(title = "Número de clientes"),
          barmode = "group",
          margin = list(t = 80, b = 120, l = 70, r = 30)
        )
      
    } else {
      
      p99 <- quantile(data[[variable]], 0.99, na.rm = TRUE)
      datos_plot <- data %>%
        transmute(
          `Estado default` = if_else(default == 1, "Default", "No Default"),
          valor = .data[[variable]],
          valor_grafico = pmin(valor, p99)
        ) %>%
        filter(!is.na(valor_grafico))
      
      plot_ly(
        data = datos_plot,
        x = ~valor_grafico,
        color = ~`Estado default`,
        colors = c("#00B2EE", "#C0FF3E"),
        type = "histogram",
        opacity = 0.75,
        nbinsx = 45,
        marker = list(line = list(color = "black", width = 0.4))
      ) %>%
        layout(
          title = paste("Distribución de", nombre_variable(variable), "(eje limitado a P99 para visualización)"),
          xaxis = list(title = nombre_variable(variable)),
          yaxis = list(title = "Frecuencia"),
          barmode = "overlay",
          margin = list(t = 80, b = 70, l = 70, r = 30)
        )
    }
  })
  
  output$boxplot_variable <- renderPlotly({
    req(input$variable_perfil)
    
    variable <- input$variable_perfil
    p99 <- quantile(data[[variable]], 0.99, na.rm = TRUE)
    
    datos_plot <- data %>%
      transmute(
        `Estado default` = if_else(default == 1, "Default", "No Default"),
        valor = .data[[variable]],
        valor_grafico = if_else(!is.na(valor), pmin(valor, p99), NA_real_)
      )
    
    plot_ly(
      data = datos_plot,
      x = ~`Estado default`,
      y = ~valor_grafico,
      type = "box",
      color = ~`Estado default`,
      colors = c("#00B2EE", "#C0FF3E"),
      boxpoints = "outliers",
      marker = list(line = list(color = "black", width = 1)),
      line = list(color = "black"),
      hovertemplate = paste("<b>%{x}</b><br>", "Valor visual: %{y}<br>", "<extra></extra>")
    ) %>%
      layout(
        title = paste("Comparación de", nombre_variable(variable), "según Default"),
        xaxis = list(title = ""),
        yaxis = list(title = paste(nombre_variable(variable), "(limitado a P99 si corresponde)")),
        showlegend = FALSE,
        margin = list(t = 80, b = 70, l = 70, r = 30)
      )
  })
  
  # ==========================================================
  # 2.3 Valores atípicos y coherencia
  # ==========================================================
  
  output$tabla_outliers_general <- renderDT({
    formato_tabla(resumen_outliers(data), page_length = 12, filtros = TRUE)
  })
  
  output$tabla_outlier_variable <- renderDT({
    req(input$variable_outlier)
    formato_tabla(resumen_outlier_variable(data, input$variable_outlier), page_length = 18)
  })
  
  output$interpretacion_outlier <- renderUI({
    req(input$variable_outlier)
    div(class = "interpretacion-texto", comentario_outlier_variable(data, input$variable_outlier))
  })
  
  output$tabla_frecuencia_outlier <- renderDT({
    req(input$variable_outlier)
    formato_tabla(frecuencia_adecuada_variable(data, input$variable_outlier), page_length = 15, filtros = TRUE)
  })
  
  output$tabla_coherencia <- renderDT({
    req(input$variable_outlier)
    formato_tabla(coherencia_variable(data, input$variable_outlier), page_length = 10)
  })
  
  output$tabla_registros_revision <- renderDT({
    req(input$variable_outlier)
    formato_tabla(registros_revision_variable(data, input$variable_outlier), page_length = 12, filtros = TRUE)
  })
  
  output$tabla_outliers_default <- renderDT({
    req(input$variable_outlier)
    tabla <- resumen_outliers_default(data, input$variable_outlier) %>%
      select(Grupo, Clientes, `Clientes con default`, `Clientes sin default`, `Tasa de default`)
    formato_tabla(tabla, page_length = 5)
  })
  
  output$grafico_outliers_default <- renderPlotly({
    req(input$variable_outlier)
    
    resumen <- resumen_outliers_default(data, input$variable_outlier)
    max_y <- max(resumen$tasa_default, na.rm = TRUE)
    
    plot_ly(
      data = resumen,
      x = ~Grupo,
      y = ~tasa_default,
      type = "bar",
      text = ~`Tasa de default`,
      textposition = "outside",
      marker = list(color = "#00B2EE", line = list(color = "black", width = 1.2)),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Tasa de default: %{y:.2%}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = paste("Tasa de default según clasificación IQR -", nombre_variable(input$variable_outlier)),
        xaxis = list(title = ""),
        yaxis = list(title = "Tasa de default", tickformat = ".0%", range = c(0, min(1, max_y * 1.35))),
        showlegend = FALSE,
        margin = list(t = 80, b = 80, l = 70, r = 30)
      )
  })
  
  output$boxplot_outlier <- renderPlotly({
    req(input$variable_outlier)
    
    variable <- input$variable_outlier
    p99 <- quantile(data[[variable]], 0.99, na.rm = TRUE)
    
    datos_plot <- datos_outlier_variable(data, variable) %>%
      mutate(valor_grafico = if_else(!is.na(valor), pmin(valor, p99), NA_real_))
    
    plot_ly(
      data = datos_plot,
      x = ~estado_default,
      y = ~valor_grafico,
      type = "box",
      color = ~estado_default,
      colors = c("#00B2EE", "#C0FF3E"),
      boxpoints = "outliers",
      marker = list(line = list(color = "black", width = 1)),
      line = list(color = "black"),
      hovertemplate = paste("<b>%{x}</b><br>", "Valor visual: %{y}<br>", "<extra></extra>")
    ) %>%
      layout(
        title = paste("Boxplot de", nombre_variable(variable), "según Default"),
        xaxis = list(title = ""),
        yaxis = list(title = paste(nombre_variable(variable), "(limitado a P99 si corresponde)")),
        showlegend = FALSE,
        margin = list(t = 80, b = 70, l = 70, r = 30)
      )
  })
  
  output$histograma_outlier <- renderPlotly({
    req(input$variable_outlier)
    
    variable <- input$variable_outlier
    naturaleza <- naturaleza_variable(variable)
    
    if (naturaleza %in% c("Conteo", "Conteo de mora") || variable == "edad") {
      
      resumen <- crear_grupos_variable(data, variable) %>%
        count(grupo, name = "Clientes") %>%
        filter(!is.na(grupo))
      
      plot_ly(
        data = resumen,
        x = ~grupo,
        y = ~Clientes,
        type = "bar",
        text = ~Clientes,
        textposition = "outside",
        marker = list(color = "#00B2EE", line = list(color = "black", width = 1.1)),
        hovertemplate = paste("<b>%{x}</b><br>", "Clientes: %{y}<br>", "<extra></extra>")
      ) %>%
        layout(
          title = paste("Distribución por grupos de", nombre_variable(variable)),
          xaxis = list(title = "", tickangle = -20),
          yaxis = list(title = "Clientes"),
          showlegend = FALSE,
          margin = list(t = 80, b = 120, l = 70, r = 30)
        )
      
    } else {
      
      p99 <- quantile(data[[variable]], 0.99, na.rm = TRUE)
      datos_plot <- data %>%
        transmute(valor = .data[[variable]], valor_grafico = pmin(valor, p99)) %>%
        filter(!is.na(valor_grafico))
      
      plot_ly(
        data = datos_plot,
        x = ~valor_grafico,
        type = "histogram",
        nbinsx = 45,
        marker = list(color = "#00B2EE", line = list(color = "black", width = 0.5))
      ) %>%
        layout(
          title = paste("Distribución de", nombre_variable(variable), "(eje limitado a P99 para visualización)"),
          xaxis = list(title = nombre_variable(variable)),
          yaxis = list(title = "Frecuencia"),
          margin = list(t = 80, b = 70, l = 70, r = 30)
        )
    }
  })
  
  # ==========================================================
  # 2.4 Decisiones preliminares
  # ==========================================================
  
  output$texto_decisiones <- renderUI({
    div(class = "interpretacion-texto", texto_cierre_eda())
  })
  
  output$tabla_decisiones_eda <- renderDT({
    formato_tabla(matriz_decisiones_eda(data), page_length = 10, filtros = TRUE)
  })
}

shinyApp(ui, server)
