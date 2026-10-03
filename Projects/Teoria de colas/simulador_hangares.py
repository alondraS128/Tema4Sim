# -*- coding: utf-8 -*-
"""
LÍNEA DE ESPERA- MULTISERVIDOr hangares (M/M/c)  
Incluye:
  - 4.3.1 Simulación de líneas de espera (Hangares M/M/c) por eventos discretos.
  - 4.3.1 Análisis económico (modelo de costos).
  - 4.4.1 Pruebas paramétricas (Chi-cuadrada y t de Student).
  - 4.4.2 Pruebas no paramétricas (Kolmogorov-Smirnov).
  - Validación con intervalo de confianza del 95 % (medias por lotes).
  - Exportación de resultados a CSV.

Componentes de temas anteriores:
  - Tema 2: números pseudoaleatorios U(0,1) con semilla fija (reproducible).
  - Tema 3: variables aleatorias exponenciales por el método de la
            transformada inversa:  X = -ln(1 - U) / tasa.
"""
import math
import csv
import tkinter as tk
from tkinter import messagebox, filedialog
import numpy as np
import scipy.stats as stats
from matplotlib.figure import Figure
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# ----------------------------------------------------------------------------
# PARÁMETROS DEL MODELO (cámbialos aquí si el problema pide otros valores)
# ----------------------------------------------------------------------------
LAMBDA = 1.20          # tasa de llegadas (aviones/día)
MU = 0.50              # tasa de servicio por hangar (aviones/día)
COSTO_HANGAR = 150.0   # costo por hangar por día
COSTO_ESPERA = 704.0   # costo por avión en cola por día de espera
TIEMPO_MAX = 10000.0   # horizonte de simulación (días); usa ⚡ Fin Rápido para llegar rápido
DT_ANIM = 0.1          # avance del reloj en cada cuadro de la animación (días)

def teoria_mmc(lam, mu, c):
    """Fórmulas teóricas del modelo M/M/c. Devuelve None si ρ >= 1 (sin estado estable)."""
    a = lam / mu
    rho = a / c
    if rho >= 1:
        return None
    suma = sum(a ** n / math.factorial(n) for n in range(c))
    p0 = 1.0 / (suma + a ** c / (math.factorial(c) * (1 - rho)))
    lq = p0 * a ** c * rho / (math.factorial(c) * (1 - rho) ** 2)
    wq = lq / lam
    w = wq + 1.0 / mu
    return {"rho": rho, "p0": p0, "lq": lq, "wq": wq, "w": w}


class SimuladorHangaresApp:
    def __init__(self, root):
        self.root = root
        self.root.title("SIMULADOR DE HANGARES (M/M/c)")
        self.root.geometry("1100x700")
        self.root.configure(bg="#F4F6F9")

        self.num_hangares = 3
        self.factor_velocidad = 1  # Multiplicador de velocidad (1x, 2x, etc.)
        self._after_id = None

        # Generador pseudoaleatorio 
        self.rng = np.random.default_rng()

        self._reset_estado()
        self.setup_ui()
        self.actualizar_monitor()

    # ------------------------------------------------------------------
    # ESTADO DE LA SIMULACIÓN
    # ------------------------------------------------------------------
    def _reset_estado(self):
        if self._after_id is not None:
            try:
                self.root.after_cancel(self._after_id)
            except Exception:
                pass
            self._after_id = None

        self.is_running = False
        self.tiempo_sim = 0.0

        # Objetos de simulación gráfica (aviones)
        self.aviones_en_cola = []              # ids de aviones esperando (FIFO)
        self.hangares_ocupados = [None] * 5    # id del avión en cada hangar
        self.tiempos_salida_hangar = [math.inf] * 5

        # Datos estocásticos generados/medidos en la corrida
        self.llegadas_acumuladas = []          # tiempos entre llegadas
        self.servicios_acumulados = []         # tiempos de servicio
        self.tiempos_espera_wq = []            # espera en cola (medida)
        self.tiempos_sistema_w = []            # tiempo total en el sistema (medido)
        self.registros = []                    # una fila por avión (para el CSV)

        # Próximos eventos
        self.proxima_llegada = None
        self.t_ultima_llegada = 0.0
        self.contador_aviones = 0

        # Integrales en el tiempo (para Lq y utilización medidas)
        self.area_cola = 0.0
        self.area_ocupados = 0.0
        self.t_ultimo_evento = 0.0

        # Historial para las gráficas
        self.hist_t = [0.0]
        self.hist_q = [0]

    # ------------------------------------------------------------------
    # INTERFAZ
    # ------------------------------------------------------------------
    def setup_ui(self):
        # Panel principal
        self.main_frame = tk.Frame(self.root, bg="#F4F6F9")
        self.main_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)

        # Lienzo visual (Izquierda)
        self.canvas_frame = tk.Frame(self.main_frame, bg="#E2E8F0", bd=2, relief=tk.GROOVE)
        self.canvas_frame.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=(0, 10))

        self.canvas = tk.Canvas(self.canvas_frame, bg="#E2E8F0", highlightthickness=0)
        self.canvas.pack(fill=tk.BOTH, expand=True)

        # Monitor de Métricas (Derecha)
        self.monitor_frame = tk.Frame(self.main_frame, bg="white", width=340, bd=1, relief=tk.SOLID)
        self.monitor_frame.pack(side=tk.RIGHT, fill=tk.Y)
        self.setup_monitor()

        # Barra Inferior de Controles
        self.control_frame = tk.Frame(self.root, bg="#1E293B", height=60, padx=10, pady=8)
        self.control_frame.pack(side=tk.BOTTOM, fill=tk.X)

        self.setup_botones_inferiores()
        self.dibujar_escenario()

    def dibujar_escenario(self):
        self.canvas.delete("all")

        # Fondo: terreno del aeródromo
        self.canvas.create_rectangle(0, 0, 2000, 2000, fill="#D9E2EC", outline="")
        # Plataforma de concreto frente a los hangares
        self.canvas.create_rectangle(225, 40, 420, 420, fill="#C3CCD6", outline="#9AA5B1")

        # Pista / Área de cola de entrada
        self.canvas.create_rectangle(30, 200, 210, 270, fill="#CBD5E1", outline="#94A3B8", width=2)
        self.canvas.create_text(120, 235, text="Cola de Entrada", fill="#475569", font=("Segoe UI", 10, "bold"))
        # Calle de rodaje (línea punteada) hacia los hangares
        self.canvas.create_line(210, 235, 270, 235, fill="#F8FAFC", width=3, dash=(8, 6))

        # Dibujar Hangares
        for i in range(self.num_hangares):
            y = 70 + i * 110
            ocupado = self.hangares_ocupados[i] is not None
            self.dibujar_hangar(i, y, ocupado)

            # Dibujar avión si el hangar está ocupado
            if ocupado:
                self.dibujar_avion(300, y + 40, self.hangares_ocupados[i], color="#EF4444")

        # Dibujar aviones en la cola visual
        for idx, id_avion in enumerate(self.aviones_en_cola[:6]):
            x = 180 - (idx * 25)
            y = 225
            self.dibujar_avion(x, y, id_avion, color="#F59E0B")
        if len(self.aviones_en_cola) > 6:
            self.canvas.create_text(120, 285, text=f"+{len(self.aviones_en_cola) - 6} más en cola",
                                    fill="#475569", font=("Segoe UI", 9, "bold"))

        self.lbl_tiempo = self.canvas.create_text(
            320, 530, text=f"Tiempo = {self.tiempo_sim:.1f} / {TIEMPO_MAX:.0f} días",
            font=("Segoe UI", 12, "bold"), fill="#0F172A")

    def dibujar_hangar(self, i, y, ocupado):
        """Dibuja un hangar realista: techo curvo de lámina, paredes, puerta y luz de estado."""
        cx, rx, ry = 320, 58, 40          # centro y radios del techo abovedado
        x0, x1 = cx - rx, cx + rx          # paredes laterales
        y_base = y + 76                    # piso del hangar
        y_arco = y + 30                    # donde inicia la curva del techo

        # Sombra en el suelo
        self.canvas.create_rectangle(x0 + 6, y_base, x1 + 8, y_base + 6, fill="#94A3B8", outline="")

        # Paredes
        self.canvas.create_rectangle(x0, y_arco, x1, y_base, fill="#94A3B8", outline="#334155", width=2)

        # Techo abovedado (semi-elipse)
        pts = []
        for ang in range(0, 181, 10):
            t = math.radians(ang)
            pts.extend([cx + rx * math.cos(t), y_arco - ry * math.sin(t)])
        self.canvas.create_polygon(pts, fill="#64748B", outline="#334155", width=2)

        # Nervaduras de la lámina del techo (líneas verticales)
        for ang in range(20, 180, 20):
            t = math.radians(ang)
            xr = cx + rx * math.cos(t)
            yr = y_arco - ry * math.sin(t)
            self.canvas.create_line(xr, yr, xr, y_arco, fill="#475569")

        # Lámina corrugada en las paredes laterales
        for xl in range(x0 + 4, x0 + 26, 5):
            self.canvas.create_line(xl, y_arco + 2, xl, y_base - 2, fill="#7B8798")
        for xl in range(x1 - 24, x1 - 2, 5):
            self.canvas.create_line(xl, y_arco + 2, xl, y_base - 2, fill="#7B8798")

        # Cartel con el nombre del hangar
        self.canvas.create_rectangle(cx - 30, y + 6, cx + 30, y + 22, fill="#0F172A", outline="#F8FAFC")
        self.canvas.create_text(cx, y + 14, text=f"Hangar {i+1}", fill="white", font=("Segoe UI", 8, "bold"))

        # Marco de la puerta
        dx0, dx1, dy0 = cx - 36, cx + 36, y + 34
        self.canvas.create_rectangle(dx0 - 3, dy0 - 3, dx1 + 3, y_base, fill="#334155", outline="#1E293B")

        if ocupado:
            # Puerta abierta: se ve el interior oscuro
            self.canvas.create_rectangle(dx0, dy0, dx1, y_base, fill="#1E293B", outline="")
            # Puertas plegadas a los lados
            self.canvas.create_rectangle(dx0, dy0, dx0 + 6, y_base, fill="#64748B", outline="#334155")
            self.canvas.create_rectangle(dx1 - 6, dy0, dx1, y_base, fill="#64748B", outline="#334155")
        else:
            # Puerta cerrada con paneles horizontales
            self.canvas.create_rectangle(dx0, dy0, dx1, y_base, fill="#CBD5E1", outline="#475569")
            for yy in range(dy0 + 6, y_base, 6):
                self.canvas.create_line(dx0, yy, dx1, yy, fill="#94A3B8")
            self.canvas.create_line(cx, dy0, cx, y_base, fill="#64748B", width=2)

        # Luz de estado (roja = ocupado, verde = libre)
        color_luz = "#EF4444" if ocupado else "#22C55E"
        self.canvas.create_rectangle(x1 + 8, y + 36, x1 + 14, y + 60, fill="#334155", outline="")
        self.canvas.create_oval(x1 + 5, y + 34, x1 + 17, y + 46, fill=color_luz, outline="#1E293B")

        # Franja amarilla de seguridad frente a la puerta
        for xs in range(dx0, dx1, 12):
            self.canvas.create_rectangle(xs, y_base + 2, xs + 6, y_base + 5, fill="#FACC15", outline="")

    def dibujar_avion(self, x, y, id_avion, color="#3B82F6"):
        """Dibuja la representación gráfica de un avión en el Canvas."""
        self.canvas.create_rectangle(x, y, x+35, y+20, fill=color, outline="#1E293B", tags="avion")
        self.canvas.create_polygon(x+10, y, x+18, y-8, x+22, y, fill=color, outline="#1E293B", tags="avion")
        self.canvas.create_polygon(x+10, y+20, x+18, y+28, x+22, y+20, fill=color, outline="#1E293B", tags="avion")
        self.canvas.create_text(x+17, y+10, text=f"A{id_avion}", fill="white", font=("Segoe UI", 7, "bold"), tags="avion")

    def setup_monitor(self):
        lbl_title = tk.Label(self.monitor_frame, text="Monitor 4.3 & 4.4", font=("Segoe UI", 12, "bold"), bg="white", fg="#0F172A")
        lbl_title.pack(pady=(10, 5))

        self.txt_monitor = tk.Text(self.monitor_frame, font=("Consolas", 9), bg="white", bd=0, highlightthickness=0, width=42)
        self.txt_monitor.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)

        btn_graficas = tk.Button(self.monitor_frame, text="📈 Ver Gráficas Real-Time", command=self.abrir_graficas_rt,
                                 bg="#2563EB", fg="white", font=("Segoe UI", 9, "bold"), bd=0, pady=6, cursor="hand2")
        btn_graficas.pack(fill=tk.X, padx=10, pady=4)

        btn_exportar = tk.Button(self.monitor_frame, text="💾 Exportar Resultados (CSV)", command=self.exportar_csv,
                                 bg="#059669", fg="white", font=("Segoe UI", 9, "bold"), bd=0, pady=6, cursor="hand2")
        btn_exportar.pack(fill=tk.X, padx=10, pady=(0, 10))

    def setup_botones_inferiores(self):
        style_start = {"bg": "#10B981", "fg": "white", "activebackground": "#059669"}
        style_pause = {"bg": "#F59E0B", "fg": "white", "activebackground": "#D97706"}
        style_reset = {"bg": "#64748B", "fg": "white", "activebackground": "#475569"}
        style_fast =  {"bg": "#0EA5E9", "fg": "white", "activebackground": "#0284C7"}
        style_val =   {"bg": "#D97706", "fg": "white", "activebackground": "#B45309"}

        btn_font = ("Segoe UI", 9, "bold")

        tk.Button(self.control_frame, text="▶ Iniciar", font=btn_font, command=self.iniciar_sim, **style_start, bd=0, padx=10, pady=5, cursor="hand2").pack(side=tk.LEFT, padx=3)
        tk.Button(self.control_frame, text="⏸ Pausar", font=btn_font, command=self.pausar_sim, **style_pause, bd=0, padx=10, pady=5, cursor="hand2").pack(side=tk.LEFT, padx=3)
        tk.Button(self.control_frame, text="🔄 Reiniciar", font=btn_font, command=self.reiniciar_sim, **style_reset, bd=0, padx=10, pady=5, cursor="hand2").pack(side=tk.LEFT, padx=3)
        tk.Button(self.control_frame, text="⚡ Fin Rápido", font=btn_font, command=self.fin_rapido, **style_fast, bd=0, padx=10, pady=5, cursor="hand2").pack(side=tk.LEFT, padx=3)
        tk.Button(self.control_frame, text="📑 Validación 4.4 (Chi², t & K-S)", font=btn_font, command=self.abrir_validacion, **style_val, bd=0, padx=10, pady=5, cursor="hand2").pack(side=tk.LEFT, padx=3)

        # Control de Velocidad con botones + y -
        tk.Label(self.control_frame, text="Vel:", bg="#1E293B", fg="white", font=btn_font).pack(side=tk.LEFT, padx=(10, 2))

        tk.Button(self.control_frame, text="-", font=btn_font, command=self.disminuir_velocidad,
                  bg="#475569", fg="white", bd=0, width=2, cursor="hand2").pack(side=tk.LEFT, padx=1)

        self.lbl_vel = tk.Label(self.control_frame, text="1x", bg="#1E293B", fg="white", font=btn_font, width=4)
        self.lbl_vel.pack(side=tk.LEFT, padx=1)

        tk.Button(self.control_frame, text="+", font=btn_font, command=self.aumentar_velocidad,
                  bg="#475569", fg="white", bd=0, width=2, cursor="hand2").pack(side=tk.LEFT, padx=1)

        # Control de Hangares (Slider)
        tk.Label(self.control_frame, text="Hangares:", bg="#1E293B", fg="white", font=btn_font).pack(side=tk.LEFT, padx=(10, 2))

        self.lbl_num_hangares = tk.Label(self.control_frame, text="3", bg="#1E293B", fg="white", font=btn_font)
        self.lbl_num_hangares.pack(side=tk.LEFT, padx=(2, 2))

        self.scale_hangares = tk.Scale(self.control_frame, from_=1, to=3, orient=tk.HORIZONTAL, bg="#1E293B", fg="white",
                                       highlightthickness=0, showvalue=0, length=60, command=self.cambiar_hangares)
        self.scale_hangares.set(3)
        self.scale_hangares.pack(side=tk.LEFT, padx=2)

    def aumentar_velocidad(self):
        if self.factor_velocidad < 10:
            self.factor_velocidad += 1
            self.lbl_vel.config(text=f"{self.factor_velocidad}x")

    def disminuir_velocidad(self):
        if self.factor_velocidad > 1:
            self.factor_velocidad -= 1
            self.lbl_vel.config(text=f"{self.factor_velocidad}x")

    def cambiar_hangares(self, val):
        nuevo = int(val)
        if nuevo == self.num_hangares:
            return
        # Cambiar c cambia el sistema: se reinicia la corrida para no mezclar datos
        self.num_hangares = nuevo
        self.lbl_num_hangares.config(text=str(nuevo))
        self._reset_estado()
        self.dibujar_escenario()
        self.actualizar_monitor()

    def obtener_delay_ms(self):
        """Calcula el retraso del ciclo en ms según el factor de velocidad."""
        return max(10, int(100 / self.factor_velocidad))

    # ------------------------------------------------------------------
    # GENERACIÓN DE VARIABLES ALEATORIAS (Temas 2 y 3)
    # ------------------------------------------------------------------
    def exponencial(self, tasa):
        """Método de la transformada inversa: X = -ln(1 - U) / tasa, con U ~ U(0,1)."""
        u = self.rng.random()
        return -math.log(1.0 - u) / tasa

    # ------------------------------------------------------------------
    # MOTOR DE SIMULACIÓN POR EVENTOS DISCRETOS
    # ------------------------------------------------------------------
    def _asegurar_arranque(self):
        if self.proxima_llegada is None:
            self.proxima_llegada = self.tiempo_sim + self.exponencial(LAMBDA)

    def _acumular_areas(self, t):
        """Integra en el tiempo el nº de aviones en cola y de hangares ocupados."""
        dt = t - self.t_ultimo_evento
        if dt > 0:
            ocupados = sum(1 for h in self.hangares_ocupados[:self.num_hangares] if h is not None)
            self.area_cola += len(self.aviones_en_cola) * dt
            self.area_ocupados += ocupados * dt
        self.t_ultimo_evento = t

    def _registrar_historial(self, t):
        self.hist_t.append(t)
        self.hist_q.append(len(self.aviones_en_cola))

    def _evento_llegada(self, t):
        self.contador_aviones += 1
        id_avion = self.contador_aviones
        entre_llegadas = t - self.t_ultima_llegada
        self.t_ultima_llegada = t
        self.llegadas_acumuladas.append(entre_llegadas)

        self.registros.append({
            "id": id_avion, "llegada": t, "entre_llegadas": entre_llegadas,
            "hangar": None, "inicio": None, "servicio": None,
            "espera": None, "salida": None, "sistema": None,
        })
        self.aviones_en_cola.append(id_avion)

        # Programar la siguiente llegada
        self.proxima_llegada = t + self.exponencial(LAMBDA)

        self._asignar_hangares(t)
        self._registrar_historial(t)

    def _asignar_hangares(self, t):
        """Cada hangar libre toma al primer avión de la cola (FIFO)."""
        for i in range(self.num_hangares):
            if self.hangares_ocupados[i] is None and self.aviones_en_cola:
                id_avion = self.aviones_en_cola.pop(0)
                reg = self.registros[id_avion - 1]

                t_servicio = self.exponencial(MU)
                espera = t - reg["llegada"]          # espera REAL en cola

                reg["hangar"] = i + 1
                reg["inicio"] = t
                reg["servicio"] = t_servicio
                reg["espera"] = espera

                self.servicios_acumulados.append(t_servicio)
                self.tiempos_espera_wq.append(espera)

                self.hangares_ocupados[i] = id_avion
                self.tiempos_salida_hangar[i] = t + t_servicio

    def _evento_salida(self, t, i):
        id_avion = self.hangares_ocupados[i]
        reg = self.registros[id_avion - 1]
        reg["salida"] = t
        reg["sistema"] = t - reg["llegada"]
        self.tiempos_sistema_w.append(reg["sistema"])

        self.hangares_ocupados[i] = None
        self.tiempos_salida_hangar[i] = math.inf

        self._asignar_hangares(t)
        self._registrar_historial(t)

    def procesar_eventos_hasta(self, t_limite):
        """Procesa en orden cronológico todas las llegadas y salidas hasta t_limite."""
        while True:
            i_sal = min(range(self.num_hangares), key=lambda k: self.tiempos_salida_hangar[k])
            t_sal = self.tiempos_salida_hangar[i_sal]
            t_lleg = self.proxima_llegada
            t_evt = min(t_lleg, t_sal)
            if t_evt > t_limite:
                break
            self._acumular_areas(t_evt)
            if t_lleg <= t_sal:
                self._evento_llegada(t_evt)
            else:
                self._evento_salida(t_evt, i_sal)
        self._acumular_areas(t_limite)
        self.tiempo_sim = t_limite

    # ------------------------------------------------------------------
    # CONTROLES
    # ------------------------------------------------------------------
    def iniciar_sim(self):
        if self.is_running:
            return
        if self.tiempo_sim >= TIEMPO_MAX:
            messagebox.showinfo("Simulación finalizada",
                                f"La simulación ya llegó al horizonte de {TIEMPO_MAX:.0f} días.\n"
                                "Presiona Reiniciar para una nueva corrida.")
            return
        self._asegurar_arranque()
        self.is_running = True
        self.ciclo_animacion()

    def pausar_sim(self):
        self.is_running = False
        if self._after_id is not None:
            self.root.after_cancel(self._after_id)
            self._after_id = None
        self.actualizar_monitor()

    def ciclo_animacion(self):
        if not self.is_running:
            return

        t_nuevo = min(self.tiempo_sim + DT_ANIM, TIEMPO_MAX)
        self.procesar_eventos_hasta(t_nuevo)

        self.dibujar_escenario()
        self.actualizar_monitor()

        if self.tiempo_sim >= TIEMPO_MAX:
            self.is_running = False
            self._after_id = None
            return

        self._after_id = self.root.after(self.obtener_delay_ms(), self.ciclo_animacion)

    def reiniciar_sim(self):
        self._reset_estado()
        self.dibujar_escenario()
        self.actualizar_monitor()

    def fin_rapido(self):
        """Simula (sin animación) hasta el horizonte, con la misma lógica de eventos."""
        self.is_running = False
        if self._after_id is not None:
            self.root.after_cancel(self._after_id)
            self._after_id = None
        self._asegurar_arranque()
        self.procesar_eventos_hasta(TIEMPO_MAX)
        self.dibujar_escenario()
        self.actualizar_monitor()

    # ------------------------------------------------------------------
    # MÉTRICAS
    # ------------------------------------------------------------------
    def calcular_metricas(self):
        t = self.tiempo_sim
        c = self.num_hangares
        wq = float(np.mean(self.tiempos_espera_wq)) if self.tiempos_espera_wq else 0.0
        w = float(np.mean(self.tiempos_sistema_w)) if self.tiempos_sistema_w else 0.0
        lq = self.area_cola / t if t > 0 else 0.0
        rho = self.area_ocupados / (c * t) if t > 0 else 0.0
        return {"wq": wq, "w": w, "lq": lq, "rho": rho}

    def _medias_por_lotes(self, k=20, calentamiento=0.10):
        """Medias por lotes de Wq. Las esperas consecutivas están correlacionadas,
        por eso se agrupan en k lotes; además se descarta el 10 % inicial (arranque
        con el sistema vacío) para reducir el sesgo del periodo transitorio."""
        datos = np.array(self.tiempos_espera_wq, dtype=float)
        datos = datos[int(len(datos) * calentamiento):]
        n = len(datos)
        if n < 100:
            return None
        m = n // k
        return datos[:m * k].reshape(k, m).mean(axis=1)

    def intervalo_confianza_wq(self):
        medias = self._medias_por_lotes()
        if medias is None:
            return None
        k = len(medias)
        media = float(medias.mean())
        se = float(medias.std(ddof=1) / math.sqrt(k))
        t_crit = float(stats.t.ppf(0.975, k - 1))
        return max(0.0, media - t_crit * se), media + t_crit * se

    def actualizar_monitor(self):
        m = self.calcular_metricas()
        c = self.num_hangares
        th = teoria_mmc(LAMBDA, MU, c)
        rho_teo = LAMBDA / (c * MU)
        ocupados = sum(1 for h in self.hangares_ocupados[:c] if h is not None)

        if self.tiempo_sim >= TIEMPO_MAX:
            estado = "Finalizada"
        elif self.is_running:
            estado = "En ejecución"
        elif self.tiempo_sim > 0:
            estado = "Pausada"
        else:
            estado = "Listo"

        def f(v):
            return f"{v:>8.3f}" if v is not None else "     n/a"

        th_wq = th["wq"] if th else None
        th_w = th["w"] if th else None
        th_lq = th["lq"] if th else None
        th_rho = th["rho"] if th else None

        costo_serv = c * COSTO_HANGAR
        costo_esp_sim = COSTO_ESPERA * m["lq"]
        costo_tot_sim = costo_serv + costo_esp_sim
        if th:
            costo_tot_teo = f"$ {costo_serv + COSTO_ESPERA * th['lq']:.2f} /día"
        else:
            costo_tot_teo = "n/a (ρ ≥ 1)"

        aviso = ""
        if th is None:
            aviso = (f"\n⚠ ρ = {rho_teo:.2f} ≥ 1: no hay estado\n"
                     "  estable; la cola crece sin límite\n"
                     "  y no existen valores teóricos.\n")

        ic = self.intervalo_confianza_wq()
        if ic is None:
            ic_txt = "requiere ≥ 100 aviones atendidos"
            dentro = ""
        else:
            ic_txt = f"[{ic[0]:.3f}, {ic[1]:.3f}]"
            if th:
                dentro = f"Wq teórico dentro del IC: {'Sí' if ic[0] <= th['wq'] <= ic[1] else 'No'}\n"
            else:
                dentro = ""

        texto = f"""  SIMULADO vs TEORÍA (M/M/c), c = {c}
------------------------------------
Tiempo:            {self.tiempo_sim:.1f} / {TIEMPO_MAX:.0f} días
Estado:            {estado}
Llegadas/Salidas:  {len(self.llegadas_acumuladas)} / {len(self.tiempos_sistema_w)}
Hangares Ocupados: {ocupados} de {c}
Aviones en cola:   {len(self.aviones_en_cola)}

Métrica       Simul.    Teoría
Wq (espera) {f(m['wq'])} {f(th_wq)}
W  (sistema){f(m['w'])} {f(th_w)}
Lq (cola)   {f(m['lq'])} {f(th_lq)}
ρ  (uso)    {f(m['rho'])} {f(th_rho)}
{aviso}
  ANÁLISIS ECONÓMICO
------------------------------------
Costo Hangares:    $ {costo_serv:.2f} /día
Costo Espera:      $ {costo_esp_sim:.2f} /día
Costo Total Simul: $ {costo_tot_sim:.2f} /día
Costo Total Teor:  {costo_tot_teo}

  VALIDACIÓN 4.4 (IC 95% para Wq)
------------------------------------
IC 95% Wq: {ic_txt}
{dentro}"""
        self.txt_monitor.delete("1.0", tk.END)
        self.txt_monitor.insert(tk.END, texto)

    # ------------------------------------------------------------------
    # GRÁFICAS (datos reales de la corrida, se refrescan solas)
    # ------------------------------------------------------------------
    def abrir_graficas_rt(self):
        win = tk.Toplevel(self.root)
        win.title("Gráficas en Tiempo Real")
        win.geometry("780x400")

        fig = Figure(figsize=(7.8, 3.8), dpi=100)
        ax1 = fig.add_subplot(121)
        ax2 = fig.add_subplot(122)
        canvas = FigureCanvasTkAgg(fig, master=win)
        canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

        def refrescar():
            if not win.winfo_exists():
                return
            th = teoria_mmc(LAMBDA, MU, self.num_hangares)

            ax1.clear()
            ax1.set_title("Aviones en cola (Lq)", fontsize=10, fontweight="bold")
            ax1.set_xlabel("Tiempo (días)")
            ax1.set_ylabel("Aviones en cola")
            ax1.grid(True, linestyle="--", alpha=0.6)
            if len(self.hist_t) > 1:
                ax1.step(self.hist_t, self.hist_q, where="post", color="#2563EB", lw=1.2, label="Simulado")
            if th:
                ax1.axhline(th["lq"], color="#DC2626", ls="--", lw=1.2, label="Lq teórico")
            if ax1.get_legend_handles_labels()[0]:
                ax1.legend(fontsize=7, loc="upper left")

            ax2.clear()
            ax2.set_title("Espera en cola Wq (días)", fontsize=10, fontweight="bold")
            ax2.set_xlabel("Avión atendido (nº)")
            ax2.set_ylabel("Días en espera")
            ax2.grid(True, linestyle="--", alpha=0.6)
            esperas = self.tiempos_espera_wq
            if esperas:
                x = np.arange(1, len(esperas) + 1)
                ax2.plot(x, esperas, ".", color="#93C5FD", ms=3, label="Wq por avión")
                ax2.plot(x, np.cumsum(esperas) / x, color="#059669", lw=1.5, label="Wq promedio")
            if th:
                ax2.axhline(th["wq"], color="#DC2626", ls="--", lw=1.2, label="Wq teórico")
            if ax2.get_legend_handles_labels()[0]:
                ax2.legend(fontsize=7, loc="upper left")

            fig.tight_layout(pad=2.0)
            canvas.draw_idle()
            win.after(500, refrescar)

        refrescar()

    # ------------------------------------------------------------------
    # VALIDACIÓN ESTADÍSTICA
    # ------------------------------------------------------------------
    @staticmethod
    def _chi2_exponencial(datos, tasa, k=10):
        """Chi-cuadrada de bondad de ajuste a la exponencial con k clases equiprobables."""
        datos = np.asarray(datos, dtype=float)
        n = len(datos)
        F = 1.0 - np.exp(-tasa * datos)                       # F(x) de la exponencial
        clases = np.minimum((F * k).astype(int), k - 1)       # clase equiprobable
        observadas = np.bincount(clases, minlength=k)
        esperadas = np.full(k, n / k)
        chi2, p = stats.chisquare(observadas, f_exp=esperadas)
        crit = float(stats.chi2.ppf(0.95, k - 1))
        return float(chi2), crit, float(p), k - 1

    def generar_reporte_validacion(self):
        c = self.num_hangares
        decisiones = []
        L = []
        L.append("==================================================")
        L.append("  REPORTE DE VALIDACIÓN DEL SIMULADOR ")
        L.append("==================================================")
        L.append(f"Nivel de significancia α = 0.05 | c = {c} hangares")
        L.append(f"Tiempo simulado: {self.tiempo_sim:.1f} días\n")

        def bloque(titulo, datos, tasa):
            n = len(datos)
            L.append(titulo)
            L.append("--------------------------------------------------")
            if n < 50:
                L.append(f"   Datos insuficientes (n = {n}). Ejecuta la simulación")
                L.append("   (Iniciar o Fin Rápido) para tener al menos 50 datos.\n")
                return
            chi2, crit, p, gl = self._chi2_exponencial(datos, tasa)
            d, p_ks = stats.kstest(datos, 'expon', args=(0, 1 / tasa))
            crit_ks = 1.36 / math.sqrt(n)
            ok_chi, ok_ks = chi2 <= crit, d <= crit_ks
            decisiones.extend([ok_chi, ok_ks])
            L.append(f"   n = {n}")
            L.append(f"   [Paramétrica] Chi-Cuadrada (10 clases, gl={gl}):")
            L.append(f"      chi2={chi2:.3f} (crit={crit:.3f}, p={p:.3f}) -> "
                     f"{'NO SE RECHAZA H0' if ok_chi else 'SE RECHAZA H0'}")
            L.append(f"   [No Paramét.] Kolmogorov-Smirnov:")
            L.append(f"      D_max={d:.3f} (crit={crit_ks:.3f}, p={p_ks:.3f}) -> "
                     f"{'NO SE RECHAZA H0' if ok_ks else 'SE RECHAZA H0'}\n")

        bloque(f"1. TIEMPOS ENTRE LLEGADAS (Exponencial, λ = {LAMBDA:.2f})",
               self.llegadas_acumuladas, LAMBDA)
        bloque(f"2. TIEMPOS DE SERVICIO (Exponencial, μ = {MU:.2f})",
               self.servicios_acumulados, MU)

        L.append("3. MEDIA DE LA ESPERA EN COLA Wq vs TEORÍA M/M/c")
        L.append("--------------------------------------------------")
        th = teoria_mmc(LAMBDA, MU, c)
        if th is None:
            L.append(f"   ρ = {LAMBDA / (c * MU):.2f} ≥ 1: no existe estado estable, por lo tanto")
            L.append("   no hay Wq teórico contra el cual comparar (usa c = 3).\n")
        else:
            medias = self._medias_por_lotes()
            if medias is None:
                L.append(f"   Datos insuficientes (n = {len(self.tiempos_espera_wq)}); se requieren ≥ 100.\n")
            else:
                t_stat, p_t = stats.ttest_1samp(medias, th["wq"])
                ok_t = p_t >= 0.05
                decisiones.append(ok_t)
                ic = self.intervalo_confianza_wq()
                L.append(f"   Wq simulado = {np.mean(self.tiempos_espera_wq):.3f} | Wq teórico = {th['wq']:.3f}")
                L.append("   [ Paramétrica] t de Student (20 medias por lotes, sin 10% inicial),")
                L.append(f"      H0: media de Wq = {th['wq']:.3f}")
                L.append(f"      t={t_stat:.3f}, p={p_t:.3f} -> {'NO SE RECHAZA H0' if ok_t else 'SE RECHAZA H0'}")
                L.append(f"   IC 95% Wq: [{ic[0]:.3f}, {ic[1]:.3f}] -> teórico "
                         f"{'DENTRO' if ic[0] <= th['wq'] <= ic[1] else 'FUERA'} del intervalo\n")

        L.append("CONCLUSIÓN")
        L.append("--------------------------------------------------")
        if not decisiones:
            L.append("   Sin datos suficientes para concluir.")
        elif all(decisiones):
            L.append("   No se rechaza H0 en ninguna prueba: el simulador reproduce")
            L.append("   el modelo M/M/c y sus generadores son válidos.")
        else:
            L.append("   Al menos una prueba rechaza H0. Con α = 0.05 cada prueba rechaza")
            L.append("   por puro azar ~1 de cada 20 veces (con 5 pruebas, ~1 de cada 4-5")
            L.append("   corridas muestra algún rechazo). Reinicia y vuelve a correr para confirmar.")
        return "\n".join(L)

    def abrir_validacion(self):
        win = tk.Toplevel(self.root)
        win.title("Validación Estadística")
        win.geometry("640x560")
        win.configure(bg="white")

        txt = tk.Text(win, font=("Consolas", 9), bg="white", bd=0, padx=15, pady=15)
        txt.pack(fill=tk.BOTH, expand=True)
        txt.insert(tk.END, self.generar_reporte_validacion())

    # ------------------------------------------------------------------
    # EXPORTACIÓN A CSV
    # ------------------------------------------------------------------
    def exportar_csv(self):
        if not self.registros:
            messagebox.showwarning("Sin datos",
                                   "Aún no hay datos para exportar.\n"
                                   "Ejecuta la simulación (Iniciar o Fin Rápido) primero.")
            return

        filename = filedialog.asksaveasfilename(
            parent=self.root,
            title="Guardar resultados de la simulación",
            defaultextension=".csv",
            initialfile="resultados_simulacion_hangares.csv",
            filetypes=[("Archivo CSV", "*.csv"), ("Todos los archivos", "*.*")]
        )
        if not filename:
            return  # El usuario canceló

        def r4(x):
            return "" if x is None else round(x, 4)

        try:
            # utf-8-sig: Excel abre correctamente los acentos
            with open(filename, mode="w", newline="", encoding="utf-8-sig") as file:
                writer = csv.writer(file)
                writer.writerow(["Cliente", "Hora_Llegada", "Tiempo_Entre_Llegada", "Hangar",
                                 "Hora_Inicio_Servicio", "Tiempo_Servicio", "Tiempo_Espera_Wq",
                                 "Hora_Salida", "Tiempo_Sistema_W"])
                for r in self.registros:
                    writer.writerow([r["id"], r4(r["llegada"]), r4(r["entre_llegadas"]),
                                     "" if r["hangar"] is None else r["hangar"],
                                     r4(r["inicio"]), r4(r["servicio"]), r4(r["espera"]),
                                     r4(r["salida"]), r4(r["sistema"])])

                # Resumen de la corrida
                m = self.calcular_metricas()
                th = teoria_mmc(LAMBDA, MU, self.num_hangares)
                c = self.num_hangares
                writer.writerow([])
                writer.writerow(["RESUMEN DE LA CORRIDA"])
                writer.writerow(["Hangares (c)", c])
                writer.writerow(["Lambda", LAMBDA])
                writer.writerow(["Mu", MU])
                writer.writerow(["Tiempo simulado (dias)", round(self.tiempo_sim, 2)])
                writer.writerow(["Semilla", SEMILLA])
                writer.writerow([])
                writer.writerow(["Metrica", "Simulado", "Teorico"])
                writer.writerow(["Wq", round(m["wq"], 4), round(th["wq"], 4) if th else "n/a"])
                writer.writerow(["W", round(m["w"], 4), round(th["w"], 4) if th else "n/a"])
                writer.writerow(["Lq", round(m["lq"], 4), round(th["lq"], 4) if th else "n/a"])
                writer.writerow(["Utilizacion", round(m["rho"], 4), round(th["rho"], 4) if th else "n/a"])
                costo_serv = c * COSTO_HANGAR
                writer.writerow(["Costo total por dia",
                                 round(costo_serv + COSTO_ESPERA * m["lq"], 2),
                                 round(costo_serv + COSTO_ESPERA * th["lq"], 2) if th else "n/a"])
                ic = self.intervalo_confianza_wq()
                if ic:
                    writer.writerow(["IC 95% Wq", f"[{ic[0]:.4f}, {ic[1]:.4f}]"])

            messagebox.showinfo("Éxito", f"Se han exportado {len(self.registros)} registros correctamente a:\n'{filename}'")
        except PermissionError:
            messagebox.showerror("Error", "No se pudo guardar el archivo. Si está abierto en Excel, ciérralo "
                                          "o elige otro nombre/carpeta.")
        except Exception as e:
            messagebox.showerror("Error", f"No se pudo exportar el archivo CSV:\n{e}")


if __name__ == "__main__":
    root = tk.Tk()
    app = SimuladorHangaresApp(root)
    root.mainloop()