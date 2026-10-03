
# =============================================================================
# SIMULADOR DE HANGARES - LINEA DE ESPERA MULTISERVIDOR (M/M/c)
# Conversión de simulador_hangares.py a R + Shiny
#
# Incluye:
#   - Simulación de línea de espera M/M/c por eventos discretos.
#   - Análisis económico.
#   - Pruebas paramétricas: Chi-cuadrada y t de Student.
#   - Prueba no paramétrica: Kolmogorov-Smirnov.
#   - Validación mediante IC 95% por medias de lotes.
#   - Exportación de resultados a CSV.
#
# Requiere:
#   install.packages(c("shiny", "ggplot2"))
# =============================================================================

library(shiny)
library(ggplot2)

# -----------------------------------------------------------------------------
# PARÁMETROS DEL MODELO
# -----------------------------------------------------------------------------
LAMBDA <- 1.20
MU <- 0.50
COSTO_HANGAR <- 150.0
COSTO_ESPERA <- 704.0
TIEMPO_MAX <- 10000.0
DT_ANIM <- 0.1
SEMILLA <- 1

# Genera una semilla nueva para cada corrida.
nueva_semilla <- function() {
  as.integer(((as.numeric(Sys.time()) * 1000) + sample.int(1000000, 1)) %% .Machine$integer.max)
}

# -----------------------------------------------------------------------------
# TEORÍA M/M/c
# -----------------------------------------------------------------------------
teoria_mmc <- function(lam, mu, c) {
  a <- lam / mu
  rho <- a / c

  if (rho >= 1) return(NULL)

  suma <- sum(sapply(0:(c - 1), function(n) a^n / factorial(n)))
  p0 <- 1 / (suma + a^c / (factorial(c) * (1 - rho)))
  lq <- p0 * a^c * rho / (factorial(c) * (1 - rho)^2)
  wq <- lq / lam
  w <- wq + 1 / mu

  list(rho = rho, p0 = p0, lq = lq, wq = wq, w = w)
}

# -----------------------------------------------------------------------------
# GENERADOR EXPONENCIAL - TRANSFORMADA INVERSA
# X = -ln(1-U) / tasa
# -----------------------------------------------------------------------------
exponencial <- function(tasa) {
  u <- runif(1, min = 0, max = 1)
  -log(1 - u) / tasa
}

# -----------------------------------------------------------------------------
# CREAR / REINICIAR ESTADO
# -----------------------------------------------------------------------------
crear_estado <- function(c = 3) {
  list(
    num_hangares = c,
    tiempo_sim = 0,
    is_running = FALSE,

    aviones_en_cola = integer(0),
    hangares_ocupados = rep(NA_integer_, 5),
    tiempos_salida_hangar = rep(Inf, 5),

    llegadas_acumuladas = numeric(0),
    servicios_acumulados = numeric(0),
    tiempos_espera_wq = numeric(0),
    tiempos_sistema_w = numeric(0),

    registros = data.frame(
      id = integer(),
      llegada = numeric(),
      entre_llegadas = numeric(),
      hangar = integer(),
      inicio = numeric(),
      servicio = numeric(),
      espera = numeric(),
      salida = numeric(),
      sistema = numeric(),
      stringsAsFactors = FALSE
    ),

    proxima_llegada = NA_real_,
    t_ultima_llegada = 0,
    contador_aviones = 0,

    area_cola = 0,
    area_ocupados = 0,
    t_ultimo_evento = 0,

    hist_t = 0,
    hist_q = 0
  )
}

# -----------------------------------------------------------------------------
# ASEGURAR ARRANQUE
# -----------------------------------------------------------------------------
asegurar_arranque <- function(state) {
  if (is.na(state$proxima_llegada)) {
    state$proxima_llegada <- state$tiempo_sim + exponencial(LAMBDA)
  }
  state
}

# -----------------------------------------------------------------------------
# ACUMULAR AREAS
# -----------------------------------------------------------------------------
acumular_areas <- function(state, t) {
  dt <- t - state$t_ultimo_evento

  if (dt > 0) {
    ocupados <- sum(!is.na(state$hangares_ocupados[seq_len(state$num_hangares)]))
    state$area_cola <- state$area_cola + length(state$aviones_en_cola) * dt
    state$area_ocupados <- state$area_ocupados + ocupados * dt
  }

  state$t_ultimo_evento <- t
  state
}

# -----------------------------------------------------------------------------
# HISTORIAL
# -----------------------------------------------------------------------------
registrar_historial <- function(state, t) {
  state$hist_t <- c(state$hist_t, t)
  state$hist_q <- c(state$hist_q, length(state$aviones_en_cola))
  state
}

# -----------------------------------------------------------------------------
# ASIGNACIÓN DE HANGARES - FIFO
# -----------------------------------------------------------------------------
asignar_hangares <- function(state, t) {

  for (i in seq_len(state$num_hangares)) {

    if (is.na(state$hangares_ocupados[i]) &&
        length(state$aviones_en_cola) > 0) {

      id_avion <- state$aviones_en_cola[1]
      state$aviones_en_cola <- state$aviones_en_cola[-1]

      idx <- which(state$registros$id == id_avion)

      t_servicio <- exponencial(MU)
      espera <- t - state$registros$llegada[idx]

      state$registros$hangar[idx] <- i
      state$registros$inicio[idx] <- t
      state$registros$servicio[idx] <- t_servicio
      state$registros$espera[idx] <- espera

      state$servicios_acumulados <- c(
        state$servicios_acumulados, t_servicio
      )

      state$tiempos_espera_wq <- c(
        state$tiempos_espera_wq, espera
      )

      state$hangares_ocupados[i] <- id_avion
      state$tiempos_salida_hangar[i] <- t + t_servicio
    }
  }

  state
}

# -----------------------------------------------------------------------------
# EVENTO DE LLEGADA
# -----------------------------------------------------------------------------
evento_llegada <- function(state, t) {

  state$contador_aviones <- state$contador_aviones + 1
  id_avion <- state$contador_aviones

  entre_llegadas <- t - state$t_ultima_llegada
  state$t_ultima_llegada <- t

  state$llegadas_acumuladas <- c(
    state$llegadas_acumuladas, entre_llegadas
  )

  nuevo <- data.frame(
    id = id_avion,
    llegada = t,
    entre_llegadas = entre_llegadas,
    hangar = NA_integer_,
    inicio = NA_real_,
    servicio = NA_real_,
    espera = NA_real_,
    salida = NA_real_,
    sistema = NA_real_,
    stringsAsFactors = FALSE
  )

  state$registros <- rbind(state$registros, nuevo)
  state$aviones_en_cola <- c(state$aviones_en_cola, id_avion)

  state$proxima_llegada <- t + exponencial(LAMBDA)

  state <- asignar_hangares(state, t)
  state <- registrar_historial(state, t)

  state
}

# -----------------------------------------------------------------------------
# EVENTO DE SALIDA
# -----------------------------------------------------------------------------
evento_salida <- function(state, t, i) {

  id_avion <- state$hangares_ocupados[i]
  idx <- which(state$registros$id == id_avion)

  state$registros$salida[idx] <- t
  state$registros$sistema[idx] <-
    t - state$registros$llegada[idx]

  state$tiempos_sistema_w <- c(
    state$tiempos_sistema_w,
    state$registros$sistema[idx]
  )

  state$hangares_ocupados[i] <- NA_integer_
  state$tiempos_salida_hangar[i] <- Inf

  state <- asignar_hangares(state, t)
  state <- registrar_historial(state, t)

  state
}

# -----------------------------------------------------------------------------
# MOTOR DE SIMULACIÓN POR EVENTOS DISCRETOS
# -----------------------------------------------------------------------------
procesar_eventos_hasta <- function(state, t_limite) {

  # Motor optimizado: conserva la misma lógica de eventos, pero evita
  # rbind()/c() dentro de cada evento. Esto permite que Fin Rápido avance
  # hasta 10,000 días sin bloquear innecesariamente la interfaz.
  if (t_limite <= state$tiempo_sim) return(state)

  c <- state$num_hangares
  n0 <- state$contador_aviones
  cap <- max(n0 + 1000L,
             n0 + ceiling((t_limite - state$tiempo_sim) * LAMBDA * 1.75) + 100L)

  # Vectores de registros preasignados. El ID del avión coincide con la fila.
  reg_id <- integer(cap)
  reg_llegada <- numeric(cap)
  reg_entre <- numeric(cap)
  reg_hangar <- rep(NA_integer_, cap)
  reg_inicio <- rep(NA_real_, cap)
  reg_servicio <- rep(NA_real_, cap)
  reg_espera <- rep(NA_real_, cap)
  reg_salida <- rep(NA_real_, cap)
  reg_sistema <- rep(NA_real_, cap)

  n_exist <- nrow(state$registros)
  if (n_exist > 0) {
    reg_id[seq_len(n_exist)] <- state$registros$id
    reg_llegada[seq_len(n_exist)] <- state$registros$llegada
    reg_entre[seq_len(n_exist)] <- state$registros$entre_llegadas
    reg_hangar[seq_len(n_exist)] <- state$registros$hangar
    reg_inicio[seq_len(n_exist)] <- state$registros$inicio
    reg_servicio[seq_len(n_exist)] <- state$registros$servicio
    reg_espera[seq_len(n_exist)] <- state$registros$espera
    reg_salida[seq_len(n_exist)] <- state$registros$salida
    reg_sistema[seq_len(n_exist)] <- state$registros$sistema
  }

  # Vectores de observaciones preasignados.
  llegadas <- numeric(max(cap, length(state$llegadas_acumuladas)))
  if (length(state$llegadas_acumuladas) > 0)
    llegadas[seq_along(state$llegadas_acumuladas)] <- state$llegadas_acumuladas
  n_llegadas <- length(state$llegadas_acumuladas)

  servicios <- numeric(max(cap, length(state$servicios_acumulados)))
  if (length(state$servicios_acumulados) > 0)
    servicios[seq_along(state$servicios_acumulados)] <- state$servicios_acumulados
  n_servicios <- length(state$servicios_acumulados)

  esperas <- numeric(max(cap, length(state$tiempos_espera_wq)))
  if (length(state$tiempos_espera_wq) > 0)
    esperas[seq_along(state$tiempos_espera_wq)] <- state$tiempos_espera_wq
  n_esperas <- length(state$tiempos_espera_wq)

  sistemas <- numeric(max(cap, length(state$tiempos_sistema_w)))
  if (length(state$tiempos_sistema_w) > 0)
    sistemas[seq_along(state$tiempos_sistema_w)] <- state$tiempos_sistema_w
  n_sistemas <- length(state$tiempos_sistema_w)

  # Cola FIFO con índice de lectura: evita eliminar el primer elemento.
  qcap <- max(cap, length(state$aviones_en_cola) + 100L)
  cola <- integer(qcap)
  qn <- length(state$aviones_en_cola)
  qhead <- 1L
  if (qn > 0) cola[seq_len(qn)] <- state$aviones_en_cola

  histcap <- max(cap, length(state$hist_t) + 100L)
  hist_t <- numeric(histcap)
  hist_q <- numeric(histcap)
  nhist <- length(state$hist_t)
  if (nhist > 0) {
    hist_t[seq_len(nhist)] <- state$hist_t
    hist_q[seq_len(nhist)] <- state$hist_q
  }

  # Estado de los hangares.
  ocupados <- state$hangares_ocupados
  salidas <- state$tiempos_salida_hangar

  tiempo <- state$tiempo_sim
  proxima_llegada <- state$proxima_llegada
  ultima_llegada <- state$t_ultima_llegada
  area_cola <- state$area_cola
  area_ocupados <- state$area_ocupados
  ultimo_evento <- state$t_ultimo_evento
  contador <- state$contador_aviones

  # Si no existe llegada programada, generar la primera.
  if (is.na(proxima_llegada)) {
    proxima_llegada <- tiempo + exponencial(LAMBDA)
  }

  # Procesamiento de eventos.
  repeat {
    i_sal <- which.min(salidas[seq_len(c)])
    t_sal <- salidas[i_sal]
    t_lleg <- proxima_llegada
    t_evt <- min(t_lleg, t_sal)

    if (is.infinite(t_evt) || t_evt > t_limite) break

    dt <- t_evt - ultimo_evento
    if (dt > 0) {
      n_ocup <- sum(!is.na(ocupados[seq_len(c)]))
      qlen <- if (qn >= qhead) qn - qhead + 1L else 0L
      area_cola <- area_cola + qlen * dt
      area_ocupados <- area_ocupados + n_ocup * dt
    }
    ultimo_evento <- t_evt

    if (t_lleg <= t_sal) {
      # -------------------------- LLEGADA --------------------------
      contador <- contador + 1L
      id <- contador

      if (id > length(reg_id)) {
        extra <- max(1000L, ceiling(length(reg_id) * 0.5))
        reg_id <- c(reg_id, integer(extra))
        reg_llegada <- c(reg_llegada, numeric(extra))
        reg_entre <- c(reg_entre, numeric(extra))
        reg_hangar <- c(reg_hangar, rep(NA_integer_, extra))
        reg_inicio <- c(reg_inicio, rep(NA_real_, extra))
        reg_servicio <- c(reg_servicio, rep(NA_real_, extra))
        reg_espera <- c(reg_espera, rep(NA_real_, extra))
        reg_salida <- c(reg_salida, rep(NA_real_, extra))
        reg_sistema <- c(reg_sistema, rep(NA_real_, extra))
        llegadas <- c(llegadas, numeric(extra))
        servicios <- c(servicios, numeric(extra))
        esperas <- c(esperas, numeric(extra))
        sistemas <- c(sistemas, numeric(extra))
        cola <- c(cola, integer(extra))
        hist_t <- c(hist_t, numeric(extra))
        hist_q <- c(hist_q, numeric(extra))
      }

      entre <- t_evt - ultima_llegada
      ultima_llegada <- t_evt
      n_llegadas <- n_llegadas + 1L
      llegadas[n_llegadas] <- entre

      reg_id[id] <- id
      reg_llegada[id] <- t_evt
      reg_entre[id] <- entre

      qn <- qn + 1L
      if (qn > length(cola)) cola <- c(cola, integer(max(1000L, length(cola) %/% 2L)))
      cola[qn] <- id

      proxima_llegada <- t_evt + exponencial(LAMBDA)

    } else {
      # --------------------------- SALIDA --------------------------
      id <- ocupados[i_sal]
      if (!is.na(id) && id > 0) {
        reg_salida[id] <- t_evt
        sistema <- t_evt - reg_llegada[id]
        reg_sistema[id] <- sistema
        n_sistemas <- n_sistemas + 1L
        if (n_sistemas > length(sistemas)) sistemas <- c(sistemas, numeric(max(1000L, length(sistemas) %/% 2L)))
        sistemas[n_sistemas] <- sistema
      }

      ocupados[i_sal] <- NA_integer_
      salidas[i_sal] <- Inf
    }

    # Asignación FIFO a hangares libres. Se ejecuta después de llegada o salida.
    for (i in seq_len(c)) {
      if (is.na(ocupados[i]) && qn >= qhead) {
        id <- cola[qhead]
        qhead <- qhead + 1L

        servicio <- exponencial(MU)
        espera <- t_evt - reg_llegada[id]

        reg_hangar[id] <- i
        reg_inicio[id] <- t_evt
        reg_servicio[id] <- servicio
        reg_espera[id] <- espera

        n_servicios <- n_servicios + 1L
        if (n_servicios > length(servicios)) servicios <- c(servicios, numeric(max(1000L, length(servicios) %/% 2L)))
        servicios[n_servicios] <- servicio

        n_esperas <- n_esperas + 1L
        if (n_esperas > length(esperas)) esperas <- c(esperas, numeric(max(1000L, length(esperas) %/% 2L)))
        esperas[n_esperas] <- espera

        ocupados[i] <- id
        salidas[i] <- t_evt + servicio
      }
    }

    nhist <- nhist + 1L
    if (nhist > length(hist_t)) {
      extra <- max(1000L, length(hist_t) %/% 2L)
      hist_t <- c(hist_t, numeric(extra))
      hist_q <- c(hist_q, numeric(extra))
    }
    hist_t[nhist] <- t_evt
    hist_q[nhist] <- if (qn >= qhead) qn - qhead + 1L else 0L
  }

  # Área desde el último evento hasta el horizonte.
  dt <- t_limite - ultimo_evento
  if (dt > 0) {
    n_ocup <- sum(!is.na(ocupados[seq_len(c)]))
    qlen <- if (qn >= qhead) qn - qhead + 1L else 0L
    area_cola <- area_cola + qlen * dt
    area_ocupados <- area_ocupados + n_ocup * dt
  }

  # Reconstruir la cola pendiente y los registros para el resto de la app.
  cola_final <- if (qn >= qhead) cola[qhead:qn] else integer(0)
  n_reg <- contador
  registros <- data.frame(
    id = reg_id[seq_len(n_reg)],
    llegada = reg_llegada[seq_len(n_reg)],
    entre_llegadas = reg_entre[seq_len(n_reg)],
    hangar = reg_hangar[seq_len(n_reg)],
    inicio = reg_inicio[seq_len(n_reg)],
    servicio = reg_servicio[seq_len(n_reg)],
    espera = reg_espera[seq_len(n_reg)],
    salida = reg_salida[seq_len(n_reg)],
    sistema = reg_sistema[seq_len(n_reg)],
    stringsAsFactors = FALSE
  )

  state$tiempo_sim <- t_limite
  state$aviones_en_cola <- cola_final
  state$hangares_ocupados <- ocupados
  state$tiempos_salida_hangar <- salidas
  state$llegadas_acumuladas <- llegadas[seq_len(n_llegadas)]
  state$servicios_acumulados <- servicios[seq_len(n_servicios)]
  state$tiempos_espera_wq <- esperas[seq_len(n_esperas)]
  state$tiempos_sistema_w <- sistemas[seq_len(n_sistemas)]
  state$registros <- registros
  state$proxima_llegada <- proxima_llegada
  state$t_ultima_llegada <- ultima_llegada
  state$contador_aviones <- contador
  state$area_cola <- area_cola
  state$area_ocupados <- area_ocupados
  state$t_ultimo_evento <- t_limite
  state$hist_t <- hist_t[seq_len(nhist)]
  state$hist_q <- hist_q[seq_len(nhist)]

  state
}

# -----------------------------------------------------------------------------
# MÉTRICAS
# -----------------------------------------------------------------------------
calcular_metricas <- function(state) {

  t <- state$tiempo_sim
  c <- state$num_hangares

  wq <- if (length(state$tiempos_espera_wq) > 0)
    mean(state$tiempos_espera_wq) else 0

  w <- if (length(state$tiempos_sistema_w) > 0)
    mean(state$tiempos_sistema_w) else 0

  lq <- if (t > 0) state$area_cola / t else 0
  rho <- if (t > 0) state$area_ocupados / (c * t) else 0

  list(wq = wq, w = w, lq = lq, rho = rho)
}

# -----------------------------------------------------------------------------
# MEDIAS POR LOTES
# -----------------------------------------------------------------------------
medias_por_lotes <- function(state, k = 20, calentamiento = 0.10) {

  datos <- as.numeric(state$tiempos_espera_wq)

  if (length(datos) == 0) return(NULL)

  inicio <- floor(length(datos) * calentamiento) + 1
  if (inicio > length(datos)) return(NULL)

  datos <- datos[inicio:length(datos)]
  n <- length(datos)

  if (n < 100) return(NULL)

  m <- floor(n / k)
  datos <- datos[seq_len(m * k)]

  matrix(datos, nrow = k, ncol = m, byrow = TRUE) |>
    rowMeans()
}

# -----------------------------------------------------------------------------
# INTERVALO DE CONFIANZA 95% PARA Wq
# -----------------------------------------------------------------------------
intervalo_confianza_wq <- function(state) {

  medias <- medias_por_lotes(state)

  if (is.null(medias)) return(NULL)

  k <- length(medias)
  media <- mean(medias)
  se <- sd(medias) / sqrt(k)
  t_crit <- qt(0.975, df = k - 1)

  c(
    max(0, media - t_crit * se),
    media + t_crit * se
  )
}

# -----------------------------------------------------------------------------
# CHI-CUADRADA DE EXPONENCIAL
# -----------------------------------------------------------------------------
chi2_exponencial <- function(datos, tasa, k = 10) {

  datos <- as.numeric(datos)
  n <- length(datos)

  F <- 1 - exp(-tasa * datos)
  clases <- pmin(floor(F * k), k - 1)

  observadas <- tabulate(clases + 1, nbins = k)
  esperadas <- rep(n / k, k)

  # Estadístico chi-cuadrada
  chi2 <- sum((observadas - esperadas)^2 / esperadas)
  gl <- k - 1
  crit <- qchisq(0.95, df = gl)
  p <- pchisq(chi2, df = gl, lower.tail = FALSE)

  list(
    chi2 = chi2,
    crit = crit,
    p = p,
    gl = gl
  )
}

# -----------------------------------------------------------------------------
# REPORTE DE VALIDACIÓN
# -----------------------------------------------------------------------------
generar_reporte_validacion <- function(state) {

  c <- state$num_hangares
  decisiones <- logical(0)
  L <- character(0)

  L <- c(
    L,
    "==================================================",
    "  REPORTE DE VALIDACIÓN DEL SIMULADOR ",
    "==================================================",
    sprintf("Nivel de significancia α = 0.05 | c = %d hangares", c),
    sprintf("Tiempo simulado: %.1f días\n", state$tiempo_sim)
  )

  bloque <- function(titulo, datos, tasa) {

    nonlocal <- list()
    n <- length(datos)

    out <- c(
      titulo,
      "--------------------------------------------------"
    )

    if (n < 50) {
      out <- c(
        out,
        sprintf("   Datos insuficientes (n = %d). Ejecuta la simulación", n),
        "   (Iniciar o Fin Rápido) para tener al menos 50 datos.\n"
      )
      return(list(texto = out, decisiones = logical(0)))
    }

    ch <- chi2_exponencial(datos, tasa)

    ks <- ks.test(
      datos,
      "pexp",
      rate = tasa
    )

    crit_ks <- 1.36 / sqrt(n)

    ok_chi <- ch$chi2 <= ch$crit
    ok_ks <- as.numeric(ks$statistic) <= crit_ks

    out <- c(
      out,
      sprintf("   n = %d", n),
      sprintf(
        "   [Paramétrica] Chi-Cuadrada (10 clases, gl=%d):",
        ch$gl
      ),
      sprintf(
        "      chi2=%.3f (crit=%.3f, p=%.3f) -> %s",
        ch$chi2,
        ch$crit,
        ch$p,
        ifelse(ok_chi, "NO SE RECHAZA H0", "SE RECHAZA H0")
      ),
      "   [No Paramét.] Kolmogorov-Smirnov:",
      sprintf(
        "      D_max=%.3f (crit=%.3f, p=%.3f) -> %s\n",
        as.numeric(ks$statistic),
        crit_ks,
        ks$p.value,
        ifelse(ok_ks, "NO SE RECHAZA H0", "SE RECHAZA H0")
      )
    )

    list(
      texto = out,
      decisiones = c(ok_chi, ok_ks)
    )
  }

  b1 <- bloque(
    sprintf(
      "1. TIEMPOS ENTRE LLEGADAS (Exponencial, λ = %.2f)",
      LAMBDA
    ),
    state$llegadas_acumuladas,
    LAMBDA
  )

  L <- c(L, b1$texto)
  decisiones <- c(decisiones, b1$decisiones)

  b2 <- bloque(
    sprintf(
      "2. TIEMPOS DE SERVICIO (Exponencial, μ = %.2f)",
      MU
    ),
    state$servicios_acumulados,
    MU
  )

  L <- c(L, b2$texto)
  decisiones <- c(decisiones, b2$decisiones)

  L <- c(
    L,
    "3. MEDIA DE LA ESPERA EN COLA Wq vs TEORÍA M/M/c",
    "--------------------------------------------------"
  )

  th <- teoria_mmc(LAMBDA, MU, c)

  if (is.null(th)) {

    L <- c(
      L,
      sprintf(
        "   ρ = %.2f ≥ 1: no existe estado estable, por lo tanto",
        LAMBDA / (c * MU)
      ),
      "   no hay Wq teórico contra el cual comparar.\n"
    )

  } else {

    medias <- medias_por_lotes(state)

    if (is.null(medias)) {

      L <- c(
        L,
        sprintf(
          "   Datos insuficientes (n = %d); se requieren ≥ 100.\n",
          length(state$tiempos_espera_wq)
        )
      )

    } else {

      t_test <- t.test(
        medias,
        mu = th$wq
      )

      ok_t <- t_test$p.value >= 0.05
      decisiones <- c(decisiones, ok_t)

      ic <- intervalo_confianza_wq(state)

      L <- c(
        L,
        sprintf(
          "   Wq simulado = %.3f | Wq teórico = %.3f",
          mean(state$tiempos_espera_wq),
          th$wq
        ),
        "   [Paramétrica] t de Student (20 medias por lotes, sin 10% inicial),",
        sprintf(
          "      H0: media de Wq = %.3f",
          th$wq
        ),
        sprintf(
          "      t=%.3f, p=%.3f -> %s",
          as.numeric(t_test$statistic),
          t_test$p.value,
          ifelse(ok_t, "NO SE RECHAZA H0", "SE RECHAZA H0")
        ),
        sprintf(
          "   IC 95%% Wq: [%.3f, %.3f] -> teórico %s del intervalo\n",
          ic[1],
          ic[2],
          ifelse(
            ic[1] <= th$wq && th$wq <= ic[2],
            "DENTRO",
            "FUERA"
          )
        )
      )
    }
  }

  L <- c(
    L,
    "CONCLUSIÓN",
    "--------------------------------------------------"
  )

  if (length(decisiones) == 0) {

    L <- c(
      L,
      "   Sin datos suficientes para concluir."
    )

  } else if (all(decisiones)) {

    L <- c(
      L,
      "   No se rechaza H0 en ninguna prueba: el simulador reproduce",
      "   el modelo M/M/c y sus generadores son válidos."
    )

  } else {

    L <- c(
      L,
      "   Al menos una prueba rechaza H0. Con α = 0.05 cada prueba rechaza",
      "   por puro azar ~1 de cada 20 veces. Reinicia y vuelve a correr",
      "   para confirmar."
    )
  }

  paste(L, collapse = "\n")
}


# =============================================================================
# INTERFAZ SHINY - VERSIÓN FUNCIONAL Y SIMPLIFICADA
# =============================================================================
#
# Se conserva TODO el motor de simulación anterior.
# Únicamente se reorganiza la interfaz y el control reactivo para que:
#   - Iniciar/Pausar/Reiniciar funcionen de forma independiente.
#   - Fin Rápido ejecute toda la corrida sin depender de la animación.
#   - Las gráficas estén siempre visibles.
#   - La validación se pueda consultar sin abrir ventanas modales.
#   - El estado de la simulación sea claramente visible.
# =============================================================================

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      body {
        background: #f5f7fa;
        color: #222;
        font-family: Arial, sans-serif;
      }

      .container-fluid {
        padding: 20px 28px;
      }

      .titulo {
        margin: 0 0 18px 0;
        font-weight: 700;
        color: #1f2937;
      }

      .panel-control {
        background: white;
        border: 1px solid #d9dee7;
        border-radius: 10px;
        padding: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,.06);
      }

      .panel {
        background: white;
        border: 1px solid #d9dee7;
        border-radius: 10px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,.05);
      }

      .panel h3, .panel h4 {
        margin-top: 0;
        color: #1f2937;
      }

      .btn-control {
        width: 100%;
        margin-bottom: 8px;
        font-weight: 600;
      }

      .estado {
        padding: 10px 12px;
        border-radius: 7px;
        background: #eef2ff;
        border: 1px solid #c7d2fe;
        margin: 12px 0;
        font-weight: 600;
      }

      .dato {
        background: #f8fafc;
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        padding: 12px;
        margin-bottom: 10px;
      }

      .dato .valor {
        font-size: 22px;
        font-weight: 700;
      }

      .tabla-metricas {
        width: 100%;
        border-collapse: collapse;
      }

      .tabla-metricas th,
      .tabla-metricas td {
        border: 1px solid #e5e7eb;
        padding: 9px;
        text-align: center;
      }

      .tabla-metricas th {
        background: #f1f5f9;
        font-weight: 700;
      }

      .reporte {
        background: #111827;
        color: #f9fafb;
        border-radius: 8px;
        padding: 15px;
        white-space: pre-wrap;
        font-family: Consolas, monospace;
        font-size: 13px;
        line-height: 1.45;
        max-height: 650px;
        overflow-y: auto;
      }

      .nota {
        color: #6b7280;
        font-size: 12px;
      }

      .shiny-download-link {
        display: block;
        width: 100%;
        text-align: center;
        margin-top: 8px;
        padding: 9px 12px;
        border-radius: 5px;
        background: #6c757d;
        color: white !important;
        text-decoration: none;
        font-weight: 600;
      }
    "))
  ),

  div(
    class = "container-fluid",

    h1(
      "SIMULADOR DE HANGARES (M/M/c)",
      class = "titulo"
    ),

    fluidRow(

      # -----------------------------------------------------------------------
      # COLUMNA DE CONTROLES
      # -----------------------------------------------------------------------
      column(
        width = 3,

        div(
          class = "panel-control",

          h3("Controles"),

          actionButton(
            "iniciar",
            "▶  Iniciar",
            class = "btn btn-success btn-control"
          ),

          actionButton(
            "pausar",
            "⏸  Pausar",
            class = "btn btn-warning btn-control"
          ),

          actionButton(
            "reiniciar",
            "↻  Reiniciar",
            class = "btn btn-secondary btn-control"
          ),

          actionButton(
            "fin_rapido",
            "⚡  Fin Rápido",
            class = "btn btn-info btn-control"
          ),

          div(
            class = "estado",
            strong("Estado: "),
            textOutput("estado", inline = TRUE)
          ),

          sliderInput(
            "hangares",
            "Número de hangares:",
            min = 1,
            max = 3,
            value = 3,
            step = 1
          ),

          sliderInput(
            "velocidad",
            "Velocidad de animación:",
            min = 1,
            max = 10,
            value = 5,
            step = 1
          ),

          hr(),

          h4("Parámetros"),

          tags$div(
            class = "dato",
            tags$div("Llegadas λ"),
            tags$div(
              class = "valor",
              sprintf("%.2f", LAMBDA)
            )
          ),

          tags$div(
            class = "dato",
            tags$div("Servicio μ"),
            tags$div(
              class = "valor",
              sprintf("%.2f", MU)
            )
          ),

          tags$div(
            class = "dato",
            tags$div("Horizonte"),
            tags$div(
              class = "valor",
              sprintf("%.0f días", TIEMPO_MAX)
            )
          ),

          hr(),

          actionButton(
            "validacion",
            "📋  Actualizar Validación",
            class = "btn btn-warning btn-control"
          ),

          downloadButton(
            "exportar",
            "💾  Exportar resultados CSV",
            class = "shiny-download-link"
          ),

          br(),

        )
      ),

      # -----------------------------------------------------------------------
      # COLUMNA PRINCIPAL
      # -----------------------------------------------------------------------
      column(
        width = 9,

        # MONITOR
        div(
          class = "panel",

          h3("Monitor de la simulación"),

          uiOutput("tarjetas"),

          h4("Simulado vs teoría"),

          tableOutput("tabla_metricas"),

          uiOutput("mensaje_estabilidad")
        ),

        # ESCENARIO
        div(
          class = "panel",

          h3("Escenario de hangares"),

          plotOutput(
            "escenario",
            height = "430px"
          )
        ),

        # GRÁFICAS
        fluidRow(

          column(
            width = 6,

            div(
              class = "panel",

              h3("Aviones en cola"),

              plotOutput(
                "grafica_cola",
                height = "330px"
              )
            )
          ),

          column(
            width = 6,

            div(
              class = "panel",

              h3("Tiempo de espera Wq"),

              plotOutput(
                "grafica_espera",
                height = "330px"
              )
            )
          )
        ),

        # VALIDACIÓN
        div(
          class = "panel",

          h3("Validación estadística"),

          p(
            class = "nota",
            "Chi-cuadrada y K-S para llegadas/servicios; t de Student e IC 95% para Wq."
          ),

          div(
            class = "reporte",
            verbatimTextOutput(
              "reporte_validacion",
              placeholder = TRUE
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVIDOR
# =============================================================================
server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # ESTADO ÚNICO DE LA SIMULACIÓN
  # ---------------------------------------------------------------------------
  # Cada nueva corrida usa una semilla distinta para que las 5 ejecuciones
  # solicitadas sean independientes y no produzcan exactamente los mismos datos.
  contador_corridas <- reactiveVal(0L)

  estado <- reactiveVal({
    # Cada inicio de la aplicación recibe una semilla nueva.
    # Así una nueva sesión tampoco repite automáticamente la corrida anterior.
    set.seed(nueva_semilla())
    s <- crear_estado(3)
    asegurar_arranque(s)
  })

  # Timer independiente del reactiveVal.
  # Se usa únicamente para avanzar la animación.
  reloj <- reactiveTimer(150)

  # ---------------------------------------------------------------------------
  # FUNCIÓN PARA REINICIAR DE FORMA DETERMINISTA
  # ---------------------------------------------------------------------------
  reiniciar_simulacion <- function(c) {

    # Cada reinicio recibe una semilla nueva para que cada ejecución sea
    # estadísticamente independiente de la anterior.
    n <- contador_corridas() + 1L
    contador_corridas(n)
    set.seed(nueva_semilla())

    s <- crear_estado(c)
    s <- asegurar_arranque(s)

    estado(s)
  }

  # ---------------------------------------------------------------------------
  # INICIAR
  # ---------------------------------------------------------------------------
  observeEvent(input$iniciar, {

    s <- isolate(estado())

    if (s$tiempo_sim >= TIEMPO_MAX) {
      showNotification(
        "La simulación terminó. Presiona Reiniciar para comenzar otra corrida.",
        type = "warning",
        duration = 4
      )
      return()
    }

    s$is_running <- TRUE
    estado(s)

    showNotification(
      "Simulación iniciada.",
      type = "message",
      duration = 2
    )
  })

  # ---------------------------------------------------------------------------
  # PAUSAR
  # ---------------------------------------------------------------------------
  observeEvent(input$pausar, {

    s <- isolate(estado())
    s$is_running <- FALSE
    estado(s)

    showNotification(
      "Simulación pausada.",
      type = "message",
      duration = 2
    )
  })

  # ---------------------------------------------------------------------------
  # REINICIAR
  # ---------------------------------------------------------------------------
  observeEvent(input$reiniciar, {

    reiniciar_simulacion(input$hangares)

    showNotification(
      "Simulación reiniciada.",
      type = "message",
      duration = 2
    )
  })

  # ---------------------------------------------------------------------------
  # CAMBIAR NÚMERO DE HANGARES
  # ---------------------------------------------------------------------------
  observeEvent(input$hangares, {

    reiniciar_simulacion(input$hangares)

    showNotification(
      sprintf(
        "Nuevo escenario: %d hangar(es). La simulación se reinició.",
        input$hangares
      ),
      type = "message",
      duration = 2
    )
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # FIN RÁPIDO
  # ---------------------------------------------------------------------------
  observeEvent(input$fin_rapido, {

    s <- isolate(estado())

    # El botón Fin Rápido NO depende del reloj de animación.
    # Ejecuta directamente todos los eventos restantes hasta 10,000 días.
    if (s$tiempo_sim < TIEMPO_MAX) {

      # Garantizar que exista una próxima llegada válida.
      if (is.na(s$proxima_llegada) || s$proxima_llegada <= s$tiempo_sim) {
        s <- asegurar_arranque(s)
      }

      s$is_running <- FALSE

      # Avance directo al horizonte completo.
      s <- procesar_eventos_hasta(s, TIEMPO_MAX)

      # Forzar explícitamente el estado final por seguridad.
      s$tiempo_sim <- TIEMPO_MAX
      s$is_running <- FALSE
    }

    estado(s)

    showNotification(
      sprintf(
        "Corrida completa finalizada: %.1f días y %d aviones atendidos.",
        s$tiempo_sim,
        length(s$tiempos_sistema_w)
      ),
      type = "message",
      duration = 4
    )
  })

  # ---------------------------------------------------------------------------
  # ANIMACIÓN
  # ---------------------------------------------------------------------------
  observe({

    reloj()

    # IMPORTANTE:
    # isolate evita que cambios en estado() vuelvan a disparar este observer
    # de manera recursiva.
    s <- isolate(estado())

    if (!isTRUE(s$is_running)) {
      return()
    }

    # La velocidad modifica solamente el tamaño del paso visual.
    # El motor de eventos sigue siendo el mismo.
    incremento <- 0.25 * input$velocidad

    t_nuevo <- min(
      s$tiempo_sim + incremento,
      TIEMPO_MAX
    )

    s <- procesar_eventos_hasta(
      s,
      t_nuevo
    )

    if (s$tiempo_sim >= TIEMPO_MAX) {
      s$is_running <- FALSE

      showNotification(
        "La simulación alcanzó los 10,000 días.",
        type = "message",
        duration = 4
      )
    }

    estado(s)
  })

  # ---------------------------------------------------------------------------
  # ESTADO
  # ---------------------------------------------------------------------------
  output$estado <- renderText({

    s <- estado()

    if (s$tiempo_sim >= TIEMPO_MAX) {
      return("FINALIZADA")
    }

    if (isTRUE(s$is_running)) {
      return("EN EJECUCIÓN")
    }

    if (s$tiempo_sim > 0) {
      return("PAUSADA")
    }

    "LISTA"
  })

  # ---------------------------------------------------------------------------
  # TARJETAS DE RESUMEN
  # ---------------------------------------------------------------------------
  output$tarjetas <- renderUI({

    s <- estado()

    ocupados <- sum(
      !is.na(
        s$hangares_ocupados[
          seq_len(s$num_hangares)
        ]
      )
    )

    tagList(

      fluidRow(

        column(
          width = 3,
          div(
            class = "dato",
            tags$div("Tiempo simulado"),
            tags$div(
              class = "valor",
              sprintf("%.1f días", s$tiempo_sim)
            )
          )
        ),

        column(
          width = 3,
          div(
            class = "dato",
            tags$div("Aviones atendidos"),
            tags$div(
              class = "valor",
              length(s$tiempos_sistema_w)
            )
          )
        ),

        column(
          width = 3,
          div(
            class = "dato",
            tags$div("Aviones en cola"),
            tags$div(
              class = "valor",
              length(s$aviones_en_cola)
            )
          )
        ),

        column(
          width = 3,
          div(
            class = "dato",
            tags$div("Hangares ocupados"),
            tags$div(
              class = "valor",
              sprintf(
                "%d / %d",
                ocupados,
                s$num_hangares
              )
            )
          )
        )
      )
    )
  })

  # ---------------------------------------------------------------------------
  # TABLA DE MÉTRICAS
  # ---------------------------------------------------------------------------
  output$tabla_metricas <- renderTable({

    s <- estado()
    m <- calcular_metricas(s)

    th <- teoria_mmc(
      LAMBDA,
      MU,
      s$num_hangares
    )

    if (is.null(th)) {

      data.frame(
        Métrica = c(
          "Wq - Espera en cola",
          "W - Tiempo en sistema",
          "Lq - Longitud de cola",
          "ρ - Utilización"
        ),
        Simulado = round(
          c(m$wq, m$w, m$lq, m$rho),
          4
        ),
        Teoría = rep(
          NA_real_,
          4
        )
      )

    } else {

      data.frame(
        Métrica = c(
          "Wq - Espera en cola",
          "W - Tiempo en sistema",
          "Lq - Longitud de cola",
          "ρ - Utilización"
        ),
        Simulado = round(
          c(m$wq, m$w, m$lq, m$rho),
          4
        ),
        Teoría = round(
          c(th$wq, th$w, th$lq, th$rho),
          4
        )
      )
    }

  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE,
  spacing = "m"
  )

  # ---------------------------------------------------------------------------
  # MENSAJE DE ESTABILIDAD + COSTOS
  # ---------------------------------------------------------------------------
  output$mensaje_estabilidad <- renderUI({

    s <- estado()
    m <- calcular_metricas(s)
    th <- teoria_mmc(
      LAMBDA,
      MU,
      s$num_hangares
    )

    costo_hangares <- s$num_hangares * COSTO_HANGAR
    costo_espera <- COSTO_ESPERA * m$lq
    costo_total <- costo_hangares + costo_espera

    if (is.null(th)) {

      div(
        style = "background:#fff3cd; border:1px solid #ffe69c; padding:14px; border-radius:8px; margin-top:15px;",
        strong("Advertencia: "),
        sprintf(
          "ρ = %.3f ≥ 1. El sistema no está en estado estable y no existe un Wq teórico finito.",
          LAMBDA / (s$num_hangares * MU)
        ),
        tags$br(),
        sprintf(
          "Costo simulado actual: $%.2f por día.",
          costo_total
        )
      )

    } else {

      ic <- intervalo_confianza_wq(s)

      ic_texto <- if (is.null(ic)) {
        "Se requieren al menos 100 tiempos de espera para calcular el IC 95%."
      } else {
        dentro <- ic[1] <= th$wq && th$wq <= ic[2]

        sprintf(
          "IC 95%% de Wq = [%.4f, %.4f]. Wq teórico %.4f: %s.",
          ic[1],
          ic[2],
          th$wq,
          ifelse(
            dentro,
            "dentro del intervalo",
            "fuera del intervalo"
          )
        )
      }

      div(
        style = "background:#eefbf3; border:1px solid #b7ebc6; padding:14px; border-radius:8px; margin-top:15px;",

        strong("Análisis económico"),

        tags$br(),

        sprintf(
          "Hangares: $%.2f/día | Espera: $%.2f/día | Total simulado: $%.2f/día.",
          costo_hangares,
          costo_espera,
          costo_total
        ),

        tags$br(),

        sprintf(
          "Costo teórico: $%.2f/día.",
          costo_hangares + COSTO_ESPERA * th$lq
        ),

        tags$br(),

        ic_texto
      )
    }
  })

  # ---------------------------------------------------------------------------
  # ESCENARIO
  # ---------------------------------------------------------------------------
  output$escenario <- renderPlot({

    s <- estado()
    c <- s$num_hangares

    datos_hangares <- data.frame(
      hangar = seq_len(c),
      y = seq_len(c),
      ocupado = !is.na(
        s$hangares_ocupados[
          seq_len(c)
        ]
      )
    )

    ggplot() +

      annotate(
        "rect",
        xmin = 0,
        xmax = 10,
        ymin = 0,
        ymax = 10,
        fill = "#f3f4f6",
        color = "#d1d5db"
      ) +

      geom_rect(
        data = datos_hangares,
        aes(
          xmin = 5,
          xmax = 9,
          ymin = 10 - y,
          ymax = 10 - y + 0.8,
          fill = factor(ocupado)
        ),
        color = "#6b7280"
      ) +

      geom_text(
        data = datos_hangares,
        aes(
          x = 7,
          y = 10 - y + 0.4,
          label = paste0(
            "Hangar ",
            hangar,
            ifelse(
              ocupado,
              " - OCUPADO",
              " - LIBRE"
            )
          )
        ),
        size = 4
      ) +

      annotate(
        "rect",
        xmin = 0.5,
        xmax = 4,
        ymin = 4,
        ymax = 6,
        fill = "#e5e7eb",
        color = "#9ca3af"
      ) +

      annotate(
        "text",
        x = 2.25,
        y = 5,
        label = paste(
          "Cola de entrada:",
          length(s$aviones_en_cola)
        ),
        size = 4
      ) +

      scale_fill_manual(
        values = c(
          "FALSE" = "#d1fae5",
          "TRUE" = "#fecaca"
        ),
        labels = c(
          "FALSE" = "Libre",
          "TRUE" = "Ocupado"
        ),
        name = "Estado"
      ) +

      coord_fixed() +

      xlim(0, 10) +
      ylim(0, 10) +

      labs(
        title = sprintf(
          "Escenario | Tiempo = %.1f días",
          s$tiempo_sim
        )
      ) +

      theme_minimal(base_size = 13) +

      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        legend.position = "bottom",
        panel.grid = element_blank()
      )
  })

  # ---------------------------------------------------------------------------
  # GRÁFICA DE COLA
  # ---------------------------------------------------------------------------
  output$grafica_cola <- renderPlot({

    s <- estado()

    datos <- data.frame(
      tiempo = s$hist_t,
      cola = s$hist_q
    )

    p <- ggplot(
      datos,
      aes(
        x = tiempo,
        y = cola
      )
    ) +

      geom_step(
        linewidth = 0.8
      ) +

      labs(
        title = "Número de aviones en cola",
        x = "Tiempo (días)",
        y = "Aviones"
      ) +

      theme_minimal(base_size = 12)

    th <- teoria_mmc(
      LAMBDA,
      MU,
      s$num_hangares
    )

    if (!is.null(th)) {

      p <- p +
        geom_hline(
          yintercept = th$lq,
          linetype = "dashed",
          linewidth = 0.8
        )
    }

    p
  })

  # ---------------------------------------------------------------------------
  # GRÁFICA DE ESPERA
  # ---------------------------------------------------------------------------
  output$grafica_espera <- renderPlot({

    s <- estado()
    th <- teoria_mmc(
      LAMBDA,
      MU,
      s$num_hangares
    )

    esperas <- s$tiempos_espera_wq

    if (length(esperas) == 0) {

      plot.new()

      text(
        0.5,
        0.5,
        "Aún no hay aviones atendidos.",
        cex = 1.2
      )

      return()
    }

    datos <- data.frame(
      avion = seq_along(esperas),
      espera = esperas
    )

    datos$promedio <- cumsum(
      datos$espera
    ) / datos$avion

    p <- ggplot(
      datos,
      aes(x = avion)
    ) +

      geom_point(
        aes(y = espera),
        size = 1.5
      ) +

      geom_line(
        aes(y = promedio),
        linewidth = 0.8
      ) +

      labs(
        title = "Tiempo de espera Wq",
        x = "Avión atendido",
        y = "Días"
      ) +

      theme_minimal(base_size = 12)

    if (!is.null(th)) {

      p <- p +
        geom_hline(
          yintercept = th$wq,
          linetype = "dashed",
          linewidth = 0.8
        )
    }

    p
  })

  # ---------------------------------------------------------------------------
  # VALIDACIÓN
  # ---------------------------------------------------------------------------
  output$reporte_validacion <- renderText({

    # input$validacion se utiliza como disparador.
    # Si nunca se presionó el botón, igual mostramos el reporte actual.
    generar_reporte_validacion(
      estado()
    )
  })

  observeEvent(input$validacion, {

    showNotification(
      "Validación actualizada.",
      type = "message",
      duration = 2
    )
  })

  # ---------------------------------------------------------------------------
  # EXPORTACIÓN
  # ---------------------------------------------------------------------------
  output$exportar <- downloadHandler(

    filename = function() {
      paste0(
        "resultados_hangares_",
        format(
          Sys.time(),
          "%Y%m%d_%H%M%S"
        ),
        ".csv"
      )
    },

    content = function(file) {

      s <- estado()

      if (nrow(s$registros) == 0) {

        write.csv(
          data.frame(
            Mensaje = "No hay datos para exportar."
          ),
          file,
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )

        return()
      }

      registros <- s$registros

      names(registros) <- c(
        "Cliente",
        "Hora_Llegada",
        "Tiempo_Entre_Llegadas",
        "Hangar",
        "Hora_Inicio_Servicio",
        "Tiempo_Servicio",
        "Tiempo_Espera_Wq",
        "Hora_Salida",
        "Tiempo_Sistema_W"
      )

      registros[] <- lapply(
        registros,
        function(x) {
          if (is.numeric(x)) {
            round(x, 4)
          } else {
            x
          }
        }
      )

      write.csv(
        registros,
        file,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )

      m <- calcular_metricas(s)
      th <- teoria_mmc(
        LAMBDA,
        MU,
        s$num_hangares
      )

      resumen <- data.frame(
        Metrica = c(
          "Hangares",
          "Lambda",
          "Mu",
          "Tiempo simulado",
          "Wq simulado",
          "W simulado",
          "Lq simulado",
          "Rho simulado",
          "Wq teorico",
          "W teorico",
          "Lq teorico",
          "Rho teorico",
          "Costo total simulado",
          "Costo total teorico"
        ),
        Valor = c(
          s$num_hangares,
          LAMBDA,
          MU,
          s$tiempo_sim,
          m$wq,
          m$w,
          m$lq,
          m$rho,
          ifelse(is.null(th), NA, th$wq),
          ifelse(is.null(th), NA, th$w),
          ifelse(is.null(th), NA, th$lq),
          ifelse(is.null(th), NA, th$rho),
          s$num_hangares * COSTO_HANGAR +
            COSTO_ESPERA * m$lq,
          ifelse(
            is.null(th),
            NA,
            s$num_hangares * COSTO_HANGAR +
              COSTO_ESPERA * th$lq
          )
        )
      )

      write.table(
        resumen,
        file,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE,
        quote = TRUE,
        append = TRUE,
        fileEncoding = "UTF-8"
      )

      ic <- intervalo_confianza_wq(s)

      if (!is.null(ic)) {

        ic_df <- data.frame(
          Metrica = "IC 95% Wq",
          Valor = sprintf(
            "[%.4f, %.4f]",
            ic[1],
            ic[2]
          )
        )

        write.table(
          ic_df,
          file,
          sep = ",",
          row.names = FALSE,
          col.names = TRUE,
          quote = TRUE,
          append = TRUE,
          fileEncoding = "UTF-8"
        )
      }
    }
  )
}

# =============================================================================
# EJECUCIÓN
# =============================================================================
shinyApp(
  ui = ui,
  server = server
)
