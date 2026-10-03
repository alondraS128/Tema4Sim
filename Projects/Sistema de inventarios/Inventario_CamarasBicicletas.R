# ============================================================
# Simulación – Sistemas de inventarios (Q, R)
# Cámaras de bicicleta – R / Shiny 
# ============================================================

library(shiny)
library(ggplot2)

# ===== Parámetros del problema =====
DEM_VAL <- c(2, 4, 6, 8, 10)
DEM_P   <- c(0.10, 0.25, 0.30, 0.25, 0.10)
LT_VAL  <- c(2, 3, 4)
LT_P    <- c(0.30, 0.40, 0.30)
COSTO_PEDIDO   <- 150
COSTO_MANTENER <- 0.20
COSTO_FALTANTE <- 40
Q_GRID <- c(40, 60, 80, 100)
R_GRID <- c(15, 20, 25, 30, 35)
ALFA   <- 0.05

# ===== Transformada inversa discreta =====
# Se guarda y restaura la semilla global para no alterar el resto de la app
discreta <- function(n, valores, probs, seed) {
  hay_semilla <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (hay_semilla) old <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (hay_semilla) assign(".Random.seed", old, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  u   <- runif(n)
  cum <- cumsum(probs)
  cum[length(cum)] <- 1                       # evita errores de redondeo
  idx <- findInterval(u, cum) + 1L
  valores[pmin(idx, length(valores))]
}

# Semillas calculadas en aritmética de doble precisión (sin desbordar enteros)
generar <- function(semilla, replica, dias) {
  base <- as.numeric(semilla) * 100000 + replica * 10
  s1 <- as.integer(base %% 2147483647)
  s2 <- as.integer((base + 1) %% 2147483647)
  list(dem = discreta(dias, DEM_VAL, DEM_P,  s1),
       lts = discreta(dias, LT_VAL,  LT_P,   s2))
}

# ===== Modelo (Q, R) =====
simular <- function(Q, R, dem, lts, inv0) {
  dias <- length(dem)
  inv <- as.integer(inv0)
  pendiente <- FALSE
  llegada <- -1L
  lt_act <- 0L
  c_ped <- c_mant <- c_falt <- 0
  perdidas <- n_ped <- suma_inv <- 0
  cierre <- integer(dias + 1)
  resta  <- integer(dias + 1)
  lt_dia <- integer(dias + 1)
  acum   <- numeric(dias + 1)
  cierre[1] <- inv
  ev_pedidos <- ev_llegadas <- ev_faltantes <- integer(0)
  
  for (d in seq_len(dias)) {
    if (pendiente && d == llegada) {
      inv <- inv + as.integer(Q)
      pendiente <- FALSE
      ev_llegadas <- c(ev_llegadas, d)
    }
    x <- as.integer(dem[d])
    vendido <- min(inv, x)
    falt <- x - vendido
    inv <- inv - vendido
    if (falt > 0) {
      perdidas <- perdidas + falt
      c_falt <- c_falt + COSTO_FALTANTE * falt
      ev_faltantes <- c(ev_faltantes, d)
    }
    c_mant <- c_mant + COSTO_MANTENER * inv
    suma_inv <- suma_inv + inv
    if (!pendiente && inv <= R) {
      lt_act <- as.integer(lts[d])
      pendiente <- TRUE
      llegada <- d + lt_act
      n_ped <- n_ped + 1L
      c_ped <- c_ped + COSTO_PEDIDO
      ev_pedidos <- c(ev_pedidos, d)
    }
    cierre[d + 1] <- inv
    resta[d + 1]  <- if (pendiente) as.integer(llegada - d) else 0L
    lt_dia[d + 1] <- if (pendiente) lt_act else 0L
    acum[d + 1]   <- c_ped + c_mant + c_falt
  }
  total_dem <- sum(dem)
  list(
    costo    = c_ped + c_mant + c_falt,
    c_ped    = c_ped, c_mant = c_mant, c_falt = c_falt,
    pedidos  = n_ped, perdidas = perdidas,
    servicio = if (total_dem > 0) 100 * (total_dem - perdidas) / total_dem else 100,
    inv_prom = suma_inv / dias,
    cierre   = cierre, resta = resta, lt_dia = lt_dia, acum = acum,
    ev_pedidos = ev_pedidos, ev_llegadas = ev_llegadas, ev_faltantes = ev_faltantes
  )
}

# ===== Validación de los datos de entrada =====
leer_parametros <- function(input) {
  entero <- function(x, nombre, minimo, maximo = Inf) {
    malo <- is.null(x) || length(x) != 1 || is.na(x) || !is.numeric(x) ||
      x != floor(x) || x < minimo || x > maximo
    if (malo) {
      rango <- if (is.finite(maximo)) sprintf("entre %d y %d", minimo, maximo)
      else sprintf("mayor o igual a %d", minimo)
      stop(sprintf("%s debe ser un entero %s.", nombre, rango), call. = FALSE)
    }
    as.integer(x)
  }
  p <- list(
    q    = entero(input$q,    "Q",              1L),
    r    = entero(input$r,    "R",              0L),
    inv0 = entero(input$inv0, "Inv. inicial",   0L),
    dias = entero(input$dias, "Días",           1L, 500L),
    q2   = entero(input$q2,   "Q₂",             1L),
    r2   = entero(input$r2,   "R₂",             0L),
    reps = entero(input$reps, "Réplicas",       5L, 1000L)
  )
  s <- trimws(if (is.null(input$seed)) "" else input$seed)
  if (nzchar(s)) {
    sv <- suppressWarnings(as.numeric(s))
    if (is.na(sv) || sv != floor(sv) || sv < 0 || sv > 1e9)
      stop("La semilla debe ser un entero entre 0 y 1000000000 (o dejarse vacía).", call. = FALSE)
    p$seed <- as.integer(sv)
  } else {
    p$seed <- sample.int(999999L, 1L)
  }
  p
}

# ===== UI =====
ui <- fluidPage(
  titlePanel("Inventario Cámaras Bicicleta (Q, R) – R/Shiny"),
  fluidRow(
    column(2, numericInput("q",    "Q (lote)",        80,  min = 1)),
    column(2, numericInput("r",    "R (reorden)",     25,  min = 0)),
    column(2, numericInput("inv0", "Inv. inicial",    40,  min = 0)),
    column(2, numericInput("dias", "Días",           365,  min = 1, max = 500)),
    column(2, numericInput("q2",   "Q₂",             100,  min = 1)),
    column(2, numericInput("r2",   "R₂",              30,  min = 0))
  ),
  fluidRow(
    column(2, numericInput("reps", "Réplicas", 30, min = 5)),
    column(2, textInput("seed", "Semilla (vacío = aleatoria)", "")),
    column(8,
           actionButton("iniciar",   "▶ Iniciar año completo", class = "btn-success"),
           actionButton("optimizar", "Optimizar (Q,R) 2D",      class = "btn-primary"),
           actionButton("validar",   "Validar χ² / t / Wilcoxon", class = "btn-info"),
           actionButton("reiniciar", "↻ Reiniciar",             class = "btn-danger")
    )
  ),
  hr(),
  fluidRow(
    column(8,
           h4(textOutput("titulo_graf")),
           plotOutput("grafica", height = "340px")
    ),
    column(4,
           h4("Resultados"),
           verbatimTextOutput("resultados")
    )
  )
)

# ===== Server =====
server <- function(input, output, session) {
  rv <- reactiveValues(
    tr     = NULL,
    R_plot = NULL,
    texto  = "Pulsa «Iniciar año completo».",
    titulo = "Comportamiento del inventario",
    modo   = "vacio",   # vacio | inventario | opt | chi
    mat    = NULL,      # matriz de costos para opt
    mejor  = NULL,
    chi_dem_obs = NULL, chi_dem_esp = NULL,
    chi_lt_obs  = NULL, chi_lt_esp  = NULL
  )
  
  ejecutar <- function(expr) {
    tryCatch(expr, error = function(e) {
      showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 12)
    })
  }
  
  observeEvent(input$reiniciar, {
    rv$tr <- NULL
    rv$R_plot <- NULL
    rv$mat <- NULL
    rv$mejor <- NULL
    rv$chi_dem_obs <- rv$chi_dem_esp <- rv$chi_lt_obs <- rv$chi_lt_esp <- NULL
    rv$modo   <- "vacio"
    rv$texto  <- "Reiniciado."
    rv$titulo <- "Comportamiento del inventario"
    updateNumericInput(session, "q",    value = 80)
    updateNumericInput(session, "r",    value = 25)
    updateNumericInput(session, "inv0", value = 40)
    updateNumericInput(session, "dias", value = 365)
    updateNumericInput(session, "q2",   value = 100)
    updateNumericInput(session, "r2",   value = 30)
    updateNumericInput(session, "reps", value = 30)
    updateTextInput(session, "seed", value = "")
  })
  
  observeEvent(input$iniciar, {
    ejecutar({
      p   <- leer_parametros(input)
      gen <- generar(p$seed, 0, p$dias)
      tr  <- simular(p$q, p$r, gen$dem, gen$lts, p$inv0)
      rv$tr     <- tr
      rv$R_plot <- p$r
      rv$modo   <- "inventario"
      rv$titulo <- sprintf("Inventario diario | Q=%d, R=%d | semilla %d", p$q, p$r, p$seed)
      rv$texto  <- sprintf(
        "══ FIN DEL AÑO ══\n\nPolítica (Q=%d, R=%d)\nSemilla: %d\n\nCosto total: $%.2f\n  Pedidos:   $%.2f\n  Mantener:  $%.2f\n  Faltantes: $%.2f\n\nServicio: %.2f%%\nInv. promedio: %.1f\nPedidos: %d\nPerdidas: %d\n",
        p$q, p$r, p$seed,
        tr$costo, tr$c_ped, tr$c_mant, tr$c_falt,
        tr$servicio, tr$inv_prom, as.integer(tr$pedidos), as.integer(tr$perdidas)
      )
    })
  })
  
  observeEvent(input$optimizar, {
    ejecutar({
      p     <- leer_parametros(input)
      datos <- lapply(0:(p$reps - 1), function(i) generar(p$seed, i, p$dias))
      mat   <- matrix(0, nrow = length(R_GRID), ncol = length(Q_GRID),
                      dimnames = list(R = R_GRID, Q = Q_GRID))
      res   <- list()
      total <- length(Q_GRID) * length(R_GRID)
      withProgress(message = "Optimizando (Q, R)...", value = 0, {
        for (qi in seq_along(Q_GRID)) {
          for (ri in seq_along(R_GRID)) {
            Q <- Q_GRID[qi]; R <- R_GRID[ri]
            rs <- lapply(datos, function(dl) simular(Q, R, dl$dem, dl$lts, p$inv0))
            mc <- mean(sapply(rs, function(z) z$costo))
            ms <- mean(sapply(rs, function(z) z$servicio))
            mat[ri, qi] <- mc
            res[[paste(Q, R, sep = ",")]] <- list(costo = mc, servicio = ms)
            incProgress(1 / total)
          }
        }
      })
      orden <- names(res)[order(sapply(res, function(z) z$costo))]
      mejor <- orden[1]
      parts <- strsplit(mejor, ",")[[1]]
      updateNumericInput(session, "q2", value = as.integer(parts[1]))
      updateNumericInput(session, "r2", value = as.integer(parts[2]))
      
      lin <- c(
        "══ OPTIMIZACIÓN (Q, R) 2D ══",
        sprintf("Semilla %d | %d réplicas de %d días", p$seed, p$reps, p$dias),
        sprintf("%-4s %-4s %12s %10s", "Q", "R", "Costo $", "Servicio%")
      )
      for (k in orden[1:min(10, length(orden))]) {
        r  <- res[[k]]
        pp <- strsplit(k, ",")[[1]]
        marca <- if (k == mejor) " ◄ mejor" else ""
        lin <- c(lin, sprintf("%-4s %-4s %12.2f %10.2f%s", pp[1], pp[2], r$costo, r$servicio, marca))
      }
      lin <- c(lin, sprintf("\nAlternativa cargada: Q₂=%s, R₂=%s", parts[1], parts[2]))
      
      rv$mat    <- mat
      rv$mejor  <- mejor
      rv$modo   <- "opt"
      rv$titulo <- sprintf("Optimización 2D | Mejor: Q=%s, R=%s", parts[1], parts[2])
      rv$texto  <- paste(lin, collapse = "\n")
    })
  })
  
  observeEvent(input$validar, {
    ejecutar({
      p     <- leer_parametros(input)
      datos <- lapply(0:(p$reps - 1), function(i) generar(p$seed, i, p$dias))
      bc <- sapply(datos, function(dl) simular(p$q,  p$r,  dl$dem, dl$lts, p$inv0)$costo)
      ac <- sapply(datos, function(dl) simular(p$q2, p$r2, dl$dem, dl$lts, p$inv0)$costo)
      dm <- mean(bc - ac)
      
      tt <- tryCatch(t.test(bc, ac, paired = TRUE), error = function(e) NULL)
      wt <- tryCatch(suppressWarnings(wilcox.test(bc, ac, paired = TRUE)), error = function(e) NULL)
      
      dem_all <- unlist(lapply(datos, function(dl) dl$dem))
      lt_all  <- unlist(lapply(datos, function(dl) dl$lts))
      tab_dem <- table(factor(dem_all, levels = DEM_VAL))
      tab_lt  <- table(factor(lt_all,  levels = LT_VAL))
      chi_dem <- suppressWarnings(chisq.test(tab_dem, p = DEM_P))
      chi_lt  <- suppressWarnings(chisq.test(tab_lt,  p = LT_P))
      
      rv$chi_dem_obs <- as.numeric(tab_dem)
      rv$chi_dem_esp <- length(dem_all) * DEM_P
      rv$chi_lt_obs  <- as.numeric(tab_lt)
      rv$chi_lt_esp  <- length(lt_all) * LT_P
      
      lin_t <- if (!is.null(tt)) {
        c(sprintf("  t pareada: t=%.4f  p=%.6f", unname(tt$statistic), tt$p.value),
          sprintf("  IC 95%%: [%.2f, %.2f]", tt$conf.int[1], tt$conf.int[2]))
      } else {
        "  t pareada: no calculable (diferencias constantes; ¿son iguales las dos políticas?)"
      }
      lin_w <- if (!is.null(wt)) sprintf("  Wilcoxon: p=%.6f", wt$p.value) else "  Wilcoxon: no calculable"
      concl <- if (!is.null(tt)) {
        if (tt$p.value < ALFA)
          sprintf("  Conclusión: diferencia significativa; cuesta menos la política %s.",
                  if (dm > 0) "alternativa" else "base")
        else "  Conclusión: no hay evidencia de diferencia de costo entre las políticas."
      } else ""
      
      lin <- c(
        "══ VALIDACIÓN ESTADÍSTICA ══",
        sprintf("Semilla %d | %d réplicas | alfa=%.2f\n", p$seed, p$reps, ALFA),
        "A) COMPARACIÓN DE POLÍTICAS",
        sprintf("  Base  (Q=%d, R=%d): $%.2f", p$q,  p$r,  mean(bc)),
        sprintf("  Alter.(Q=%d, R=%d): $%.2f", p$q2, p$r2, mean(ac)),
        sprintf("  Diferencia media: $%.2f", dm),
        lin_t, lin_w, concl,
        "",
        "B) CHI CUADRADA (bondad de ajuste)",
        sprintf("  Demanda: chi²=%.3f  p=%.6f → %s",
                unname(chi_dem$statistic), chi_dem$p.value,
                if (chi_dem$p.value >= ALFA) "NO se rechaza H0" else "SE RECHAZA H0"),
        sprintf("  Tiempo entrega: chi²=%.3f  p=%.6f → %s",
                unname(chi_lt$statistic), chi_lt$p.value,
                if (chi_lt$p.value >= ALFA) "NO se rechaza H0" else "SE RECHAZA H0")
      )
      rv$texto  <- paste(lin, collapse = "\n")
      rv$modo   <- "chi"
      rv$titulo <- "Validación χ² – Observada vs Esperada"
    })
  })
  
  output$resultados  <- renderText(rv$texto)
  output$titulo_graf <- renderText(rv$titulo)
  
  # ---- Gráficas ----
  output$grafica <- renderPlot({
    if (rv$modo == "inventario" && !is.null(rv$tr)) {
      tr   <- rv$tr
      Rpl  <- rv$R_plot
      dias <- length(tr$cierre) - 1
      df   <- data.frame(dia = 0:dias, inv = tr$cierre)
      ggplot(df, aes(x = dia, y = inv)) +
        geom_line(color = "#3b7dd8", linewidth = 0.9) +
        geom_hline(yintercept = Rpl, linetype = "dashed", color = "#d33a2c", linewidth = 0.8) +
        annotate("text", x = dias * 0.92, y = Rpl, label = paste0("R=", Rpl),
                 color = "#d33a2c", vjust = -0.5, size = 3.5) +
        labs(x = "Día", y = "Unidades") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
      
    } else if (rv$modo == "opt" && !is.null(rv$mat)) {
      mat <- rv$mat
      df <- expand.grid(R = as.numeric(rownames(mat)), Q = as.numeric(colnames(mat)))
      df$costo <- as.vector(mat)
      parts <- strsplit(rv$mejor, ",")[[1]]
      ggplot(df, aes(x = factor(Q), y = factor(R), fill = costo)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = sprintf("%.0f", costo)), size = 3.2) +
        scale_fill_gradient(low = "#2e9e4f", high = "#d33a2c", name = "Costo $") +
        geom_point(data = data.frame(Q = parts[1], R = parts[2]),
                   aes(x = Q, y = R), inherit.aes = FALSE,
                   color = "gold", size = 8, shape = 21, fill = "gold", stroke = 1.5) +
        labs(x = "Q (lote)", y = "R (reorden)") +
        theme_minimal(base_size = 12)
      
    } else if (rv$modo == "chi" && !is.null(rv$chi_dem_obs)) {
      df_dem <- data.frame(
        Variable = "Demanda diaria",
        valor = factor(rep(DEM_VAL, 2)),
        tipo = rep(c("Observada", "Esperada"), each = length(DEM_VAL)),
        frecuencia = c(rv$chi_dem_obs, rv$chi_dem_esp)
      )
      
      df_lt <- data.frame(
        Variable = "Tiempo de entrega",
        valor = factor(rep(LT_VAL, 2)),
        tipo = rep(c("Observada", "Esperada"), each = length(LT_VAL)),
        frecuencia = c(rv$chi_lt_obs, rv$chi_lt_esp)
      )
      
      df_chi <- rbind(df_dem, df_lt)
      
      ggplot(df_chi, aes(x = valor, y = frecuencia, fill = tipo)) +
        geom_col(position = "dodge", width = 0.7) +
        facet_wrap(~Variable, scales = "free_x") +
        scale_fill_manual(values = c("Observada" = "#e08a1e", "Esperada" = "#7fb2ea")) +
        labs(x = "Valor / Días", y = "Frecuencia", fill = "Tipo") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")
      
    } else {
      plot.new()
      text(0.5, 0.5, "Pulsa «Iniciar», «Optimizar» o «Validar»", cex = 1.3, col = "grey50")
    }
  })
}

shinyApp(ui, server)

