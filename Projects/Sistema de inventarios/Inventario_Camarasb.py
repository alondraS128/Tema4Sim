# -*- coding: utf-8 -*-
"""
Simulación | Inventarios (Q, R) - Cámaras de bicicleta
"""
import math
import tkinter as tk
from tkinter import ttk, messagebox

try:
    import numpy as np
    from scipy import stats
    import matplotlib
    matplotlib.use("TkAgg")
    from matplotlib import cm
    from matplotlib.figure import Figure
    from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
except ImportError as exc:
    raise SystemExit(
        "Faltan librerías (%s).\nInstala con: python -m pip install numpy scipy matplotlib" % exc
    )

# ==================== PARÁMETROS ====================
DEM_VAL = np.array([2, 4, 6, 8, 10])
DEM_P = np.array([0.10, 0.25, 0.30, 0.25, 0.10])
LT_VAL = np.array([2, 3, 4])
LT_P = np.array([0.30, 0.40, 0.30])
COSTO_PEDIDO = 150.0
COSTO_MANTENER = 0.20
COSTO_FALTANTE = 40.0
MAX_DIAS = 500
ALFA = 0.05
Q_GRID = [40, 60, 80, 100]
R_GRID = [15, 20, 25, 30, 35]

AZUL = "#3b7dd8"
AZUL_FILL = "#4a9fd8"
ROJO = "#d33a2c"
NARANJA = "#e39a4a"
VERDE = "#2e9e4f"
ORO = "#f2c200"


# ==================== GENERACIÓN ALEATORIA ====================
def discreta(rng, valores, probs, n):
    u = rng.random(n)
    idx = np.searchsorted(np.cumsum(probs), u, side="right")
    return valores[np.minimum(idx, len(valores) - 1)]


def generar(semilla, replica, dias):
    rd = np.random.default_rng([semilla, replica, 0])
    rl = np.random.default_rng([semilla, replica, 1])
    return discreta(rd, DEM_VAL, DEM_P, dias), discreta(rl, LT_VAL, LT_P, dias)


# ==================== MODELO (Q, R) ====================
def simular(Q, R, dem, lts, inv0):
    dias = len(dem)
    inv, pendiente, llegada, lt_act = int(inv0), False, -1, 0
    c_ped = c_mant = c_falt = 0.0
    perdidas = n_ped = suma_inv = 0
    cierre = np.zeros(dias + 1, dtype=int)
    resta = np.zeros(dias + 1, dtype=int)
    lt_dia = np.zeros(dias + 1, dtype=int)
    acum = np.zeros(dias + 1)
    cierre[0] = inv
    ev_pedidos, ev_llegadas, ev_faltantes = [], [], []
    for d in range(1, dias + 1):
        if pendiente and d == llegada:
            inv += Q
            pendiente = False
            ev_llegadas.append(d)
        x = int(dem[d - 1])
        vendido = min(inv, x)
        falt = x - vendido
        inv -= vendido
        if falt > 0:
            perdidas += falt
            c_falt += COSTO_FALTANTE * falt
            ev_faltantes.append(d)
        c_mant += COSTO_MANTENER * inv
        suma_inv += inv
        if (not pendiente) and inv <= R:
            lt_act = int(lts[d - 1])
            pendiente, llegada = True, d + lt_act
            n_ped += 1
            c_ped += COSTO_PEDIDO
            ev_pedidos.append(d)
        cierre[d] = inv
        resta[d] = llegada - d if pendiente else 0
        lt_dia[d] = lt_act if pendiente else 0
        acum[d] = c_ped + c_mant + c_falt
    total_dem = int(np.sum(dem))
    return {
        "costo": c_ped + c_mant + c_falt,
        "c_ped": c_ped, "c_mant": c_mant, "c_falt": c_falt,
        "pedidos": n_ped, "perdidas": perdidas, "demanda": total_dem,
        "servicio": 100.0 * (total_dem - perdidas) / total_dem if total_dem else 100.0,
        "inv_prom": suma_inv / dias,
        "cierre": cierre, "resta": resta, "lt_dia": lt_dia, "acum": acum,
        "ev_pedidos": ev_pedidos, "ev_llegadas": ev_llegadas, "ev_faltantes": ev_faltantes,
    }


def fmt_p(v):
    if not np.isfinite(v):
        return "no calculable"
    return "< 0.000001" if v < 1e-6 else "%.6f" % v


def chi_cuadrada(datos, valores, probs):
    n = len(datos)
    obs = np.array([int(np.sum(datos == v)) for v in valores])
    esp = n * np.asarray(probs)
    chi = float(np.sum((obs - esp) ** 2 / esp))
    gl = len(valores) - 1
    crit = float(stats.chi2.ppf(1 - ALFA, gl))
    return {
        "n": n, "obs": obs, "esp": esp, "chi": chi, "gl": gl, "crit": crit,
        "p": float(stats.chi2.sf(chi, gl)), "acepta": chi <= crit,
        "ok_esp": bool(esp.min() >= 5),
    }


# ==================== INTERFAZ (todo en una sola pantalla) ====================
class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Simulación | Inventarios (Q, R) - Cámaras de bicicleta")
        self.geometry("1150x780")
        self.minsize(1000, 680)
        self.configure(bg="#f0f2f5")

        self.v = {}
        self.tr = None
        self.p = None
        self.anim_id = None
        self.acum_paso = 0.0
        self.d = 0
        self.pausado = False
        self.velocidad = 30

        self._parametros()
        self._cuerpo()
        self._barra_botones()

    def _parametros(self):
        f = ttk.LabelFrame(self, text="Parámetros de la simulación")
        f.pack(fill="x", padx=8, pady=6)

        def campo(texto, clave, defecto, fila, col, ancho=7):
            ttk.Label(f, text=texto).grid(row=fila, column=col, sticky="w", padx=3, pady=2)
            self.v[clave] = tk.StringVar(value=str(defecto))
            ttk.Entry(f, textvariable=self.v[clave], width=ancho).grid(
                row=fila, column=col + 1, padx=3, pady=2
            )

        campo("Q (cámaras por pedido):", "q", 80, 0, 0)
        campo("R (punto de reorden):", "r", 25, 0, 2)
        campo("Inventario inicial:", "inv0", 40, 0, 4)
        campo("Días (máx. 500):", "dias", 365, 0, 6)
        campo("Q₂ (alternativa):", "q2", 100, 1, 0)
        campo("R₂ (alternativa):", "r2", 30, 1, 2)
        campo("Réplicas:", "reps", 30, 1, 4)
        campo("Semilla (vacía = aleatoria):", "seed", "", 1, 6, 10)

    def _cuerpo(self):
        cont = tk.Frame(self, bg="#f0f2f5")
        cont.pack(fill="both", expand=True, padx=8, pady=2)

        izq = tk.Frame(cont, bg="#f0f2f5")
        izq.pack(side="left", fill="both", expand=True)

        # Bodega
        zona_bod = tk.Frame(izq, bg="white", height=210)
        zona_bod.pack(fill="x", pady=(0, 4))
        zona_bod.pack_propagate(False)
        self.c1 = tk.Canvas(zona_bod, bg="white", highlightthickness=1, highlightbackground="#ccc")
        self.c1.pack(fill="both", expand=True)

        # Gráfica
        zona_graf = tk.Frame(izq, bg="white")
        zona_graf.pack(fill="both", expand=True)
        self.fig = Figure(figsize=(8, 3.0), dpi=100, facecolor="white")
        self.ax = self.fig.add_subplot(111)
        self.ax.set_title("Comportamiento del Inventario Diario (Día vs Unidades)", fontsize=10)
        self.ax.set_xlabel("Día", fontsize=8)
        self.ax.set_ylabel("Unidades", fontsize=8)
        self.ax.grid(True, alpha=0.3)
        self.fig.tight_layout()
        self.cv = FigureCanvasTkAgg(self.fig, master=zona_graf)
        self.cv.get_tk_widget().pack(fill="both", expand=True)

        # Panel resultados
        der = tk.LabelFrame(cont, text="  Resultados  ", font=("Arial", 10, "bold"),
                            bg="white", fg="#333", padx=6, pady=4)
        der.pack(side="right", fill="y", padx=(6, 0))
        self.txt = tk.Text(der, width=40, height=32, font=("Consolas", 9),
                           wrap="word", bg="#fafafa", relief="flat")
        self.txt.pack(fill="both", expand=True)
        self.txt.insert("1.0",
            "Pulsa «Iniciar» para comenzar la simulación.\n\n"
            "• Iniciar / Reanudar: arranca o continúa\n"
            "• Pausar: detiene la animación\n"
            "• Reiniciar: limpia todo\n"
            "• Optimizar (Q,R) 2D: busca la mejor política\n"
            "• Validar χ²: pruebas estadísticas\n"
        )

        self.after(150, self._dibujar_bodega_vacia)

    def _barra_botones(self):
        barra = tk.Frame(self, bg="#d5dbe3", height=48)
        barra.pack(side="bottom", fill="x")
        barra.pack_propagate(False)

        f = tk.Frame(barra, bg="#d5dbe3")
        f.pack(side="left", padx=10, pady=8)

        estilo = {"font": ("Arial", 9, "bold"), "padx": 14, "pady": 5, "cursor": "hand2", "bd": 0}
        tk.Button(f, text="▶ Iniciar / Reanudar", bg=VERDE, fg="white",
                  command=self.iniciar_reanudar, **estilo).pack(side="left", padx=3)
        tk.Button(f, text="⏸ Pausar", bg=NARANJA, fg="white",
                  command=self.pausar, **estilo).pack(side="left", padx=3)
        tk.Button(f, text="↻ Reiniciar", bg=ROJO, fg="white",
                  command=self.reiniciar, **estilo).pack(side="left", padx=3)
        tk.Button(f, text="Optimizar (Q,R) 2D", bg=AZUL, fg="white",
                  command=self.optimizar, **estilo).pack(side="left", padx=3)
        tk.Button(f, text="Validar χ² / t / Wilcoxon", bg="#6b4c9a", fg="white",
                  command=self.validar, **estilo).pack(side="left", padx=3)

        f_vel = tk.Frame(barra, bg="#d5dbe3")
        f_vel.pack(side="right", padx=12)
        tk.Label(f_vel, text="Velocidad:", bg="#d5dbe3", font=("Arial", 9)).pack(side="left", padx=(0, 4))
        for txt, val in (("Lento", 10), ("Normal", 30), ("Rápido", 80)):
            tk.Button(f_vel, text=txt, bg="#5a7a9a", fg="white", font=("Arial", 8, "bold"),
                      padx=8, pady=3, cursor="hand2", bd=0,
                      command=lambda v=val: self.set_vel(v)).pack(side="left", padx=2)

    def set_vel(self, v):
        self.velocidad = v

    def _dibujar_bodega_vacia(self):
        cv = self.c1
        cv.delete("all")
        w = max(cv.winfo_width(), 700)
        h = max(cv.winfo_height(), 190)
        cv.create_text(w // 2, 18, text="BODEGA (INVENTARIO)", font=("Arial", 12, "bold"), fill="#333")
        bw, bh = 280, 110
        bx = (w - bw) // 2
        by = 40
        cv.create_rectangle(bx, by, bx + bw, by + bh, outline="#888", fill="#e8e8e8", width=2)
        cv.create_text(bx + bw // 2, by + bh // 2, text="—", font=("Arial", 22, "bold"), fill="#999")
        cv.create_line(20, h - 26, w - 20, h - 26, fill="#c0392b", width=3)
        cv.create_text(80, h - 42, text="Sin pedido pendiente", font=("Arial", 9), fill="#888")

    def leer(self):
        try:
            p = {k: int(self.v[k].get()) for k in ("q", "r", "inv0", "dias", "q2", "r2", "reps")}
            s = self.v["seed"].get().strip()
            p["seed"] = int(s) if s else int(np.random.SeedSequence().entropy % (2 ** 31))
            if not 1 <= p["dias"] <= MAX_DIAS:
                raise ValueError("Días entre 1 y %d." % MAX_DIAS)
            if p["q"] <= 0:
                raise ValueError("Q debe ser positivo.")
            if min(p["r"], p["inv0"]) < 0:
                raise ValueError("R e inventario inicial ≥ 0.")
            if p["reps"] < 5:
                p["reps"] = 30
            return p
        except ValueError as e:
            messagebox.showerror("Parámetros inválidos", str(e))
            return None

    def iniciar_reanudar(self):
        if self.tr is None or (not self.pausado and self.p is not None and self.d >= self.p["dias"]):
            p = self.leer()
            if p is None:
                return
            if self.anim_id:
                self.after_cancel(self.anim_id)
            dem, lts = generar(p["seed"], 0, p["dias"])
            self.p = p
            self.tr = simular(p["q"], p["r"], dem, lts, p["inv0"])
            self.d = 0
            self.acum_paso = 0.0
            self.pausado = False
            self._paso()
        else:
            self.pausado = False
            if self.anim_id is None and self.p is not None and self.d < self.p["dias"]:
                self._paso()

    def pausar(self):
        self.pausado = True
        if self.anim_id:
            self.after_cancel(self.anim_id)
            self.anim_id = None

    def reiniciar(self):
        if self.anim_id:
            self.after_cancel(self.anim_id)
            self.anim_id = None
        self.tr = None
        self.p = None
        self.d = 0
        self.pausado = False
        self._dibujar_bodega_vacia()
        self.ax.clear()
        self.ax.set_title("Comportamiento del Inventario Diario (Día vs Unidades)", fontsize=10)
        self.ax.set_xlabel("Día", fontsize=8)
        self.ax.set_ylabel("Unidades", fontsize=8)
        self.ax.grid(True, alpha=0.3)
        self.cv.draw()
        self.txt.delete("1.0", "end")
        self.txt.insert("1.0", "Reiniciado. Pulsa «Iniciar» para comenzar.")

    def _paso(self):
        if self.pausado or self.tr is None:
            self.anim_id = None
            return
        self.acum_paso += self.velocidad * 0.04
        n = int(self.acum_paso)
        self.acum_paso -= n
        avance = max(n, 1) if self.d == 0 else max(n, 0)
        self.d = min(self.p["dias"], self.d + max(avance, 1 if self.d == 0 else 0))
        self.dibujar()
        if self.d < self.p["dias"]:
            self.anim_id = self.after(40, self._paso)
        else:
            self.anim_id = None
            tr = self.tr
            self.txt.delete("1.0", "end")
            self.txt.insert("1.0",
                "══ FIN DEL AÑO ══\n\n"
                "Política (Q=%d, R=%d)\n"
                "Semilla: %d\n\n"
                "Costo total:  $%.2f\n"
                "  Pedidos:    $%.2f\n"
                "  Mantener:   $%.2f\n"
                "  Faltantes:  $%.2f\n\n"
                "Servicio:     %.2f%%\n"
                "Inv. promedio: %.1f\n"
                "Pedidos hechos: %d\n"
                "Unidades perdidas: %d\n"
                % (self.p["q"], self.p["r"], self.p["seed"],
                   tr["costo"], tr["c_ped"], tr["c_mant"], tr["c_falt"],
                   tr["servicio"], tr["inv_prom"], tr["pedidos"], tr["perdidas"])
            )

    def dibujar(self):
        if self.tr is None or self.p is None:
            return
        cv, tr, p = self.c1, self.tr, self.p
        cv.delete("all")
        self.update_idletasks()
        w = max(cv.winfo_width(), 700)
        h = max(cv.winfo_height(), 190)
        dias, Q, R = p["dias"], p["q"], p["r"]
        inv = int(tr["cierre"][self.d])
        cap = max(20, int(math.ceil(max(p["inv0"], Q + R) * 1.15 / 20.0) * 20))

        cv.create_text(w // 2, 16, text="BODEGA (INVENTARIO)", font=("Arial", 11, "bold"), fill="#333")

        bw, bh = 300, 115
        bx = (w - bw) // 2
        by = 32
        cv.create_rectangle(bx, by, bx + bw, by + bh, outline="#666", fill="#e8e8e8", width=2)
        frac = min(inv / float(cap), 1.0)
        fill_h = int(bh * frac)
        if fill_h > 0:
            color = AZUL_FILL if inv > R else NARANJA
            cv.create_rectangle(bx + 2, by + bh - fill_h, bx + bw - 2, by + bh - 1,
                                outline="", fill=color)
        cv.create_text(bx + bw // 2, by + bh // 2, text="%d uds" % inv,
                       font=("Arial", 20, "bold"), fill="#222")
        if 0 < R < cap:
            y_r = by + bh - int(bh * R / float(cap))
            cv.create_line(bx - 10, y_r, bx + bw + 10, y_r, fill=ROJO, dash=(4, 3), width=1)
            cv.create_text(bx - 14, y_r, text="R", fill=ROJO, anchor="e", font=("Arial", 8, "bold"))

        line_y = h - 24
        cv.create_line(20, line_y, w - 20, line_y, fill="#c0392b", width=3)
        resta = int(tr["resta"][self.d])
        if resta > 0:
            ltt = max(1, int(tr["lt_dia"][self.d]))
            progreso = (ltt - resta) / float(ltt)
            x = 40 + progreso * (bx - 60)
            cv.create_rectangle(x, line_y - 26, x + 46, line_y - 5, fill=AZUL, outline="#222")
            cv.create_rectangle(x + 46, line_y - 20, x + 58, line_y - 5, fill=ROJO, outline="#222")
            cv.create_oval(x + 5, line_y - 9, x + 16, line_y + 2, fill="#222")
            cv.create_oval(x + 36, line_y - 9, x + 47, line_y + 2, fill="#222")
            cv.create_text(x + 28, line_y - 38, text="Llega en: %dd" % resta,
                           font=("Arial", 9, "bold"), fill=ROJO)
        else:
            cv.create_text(90, line_y - 38, text="Sin pedido pendiente", font=("Arial", 9), fill="#888")

        self.ax.clear()
        dias_x = np.arange(self.d + 1)
        self.ax.plot(dias_x, tr["cierre"][: self.d + 1], color=AZUL, lw=1.4)
        self.ax.axhline(R, color=ROJO, ls="--", lw=1.2, label="Reorden (R=%d)" % R)
        self.ax.set_xlim(0, max(dias, 1))
        ymax = max(cap, int(tr["cierre"][: self.d + 1].max()) + 5) if self.d > 0 else cap
        self.ax.set_ylim(0, ymax)
        self.ax.set_title("Comportamiento del Inventario Diario (Día vs Unidades)", fontsize=10)
        self.ax.set_xlabel("Día", fontsize=8)
        self.ax.set_ylabel("Unidades", fontsize=8)
        self.ax.legend(loc="upper right", fontsize=8)
        self.ax.grid(True, alpha=0.3)
        self.fig.tight_layout()
        self.cv.draw()

        n_ped = sum(1 for e in tr["ev_pedidos"] if e <= self.d)
        n_fal = sum(1 for e in tr["ev_faltantes"] if e <= self.d)
        self.txt.delete("1.0", "end")
        self.txt.insert("1.0",
            "Día %d / %d\n"
            "Política (Q=%d, R=%d)\n"
            "Semilla: %d\n\n"
            "Inventario: %d uds\n"
            "Pedidos hechos: %d\n"
            "Días con faltante: %d\n"
            "Costo acum.: $%.2f\n"
            % (self.d, dias, Q, R, p["seed"], inv, n_ped, n_fal, tr["acum"][self.d])
        )
        if resta > 0:
            self.txt.insert("end", "\nPedido en camino:\nllega en %d día(s)" % resta)

    def optimizar(self):
        p = self.leer()
        if p is None:
            return
        self.config(cursor="watch")
        self.update_idletasks()
        datos = [generar(p["seed"], i, p["dias"]) for i in range(p["reps"])]
        res = {}
        for Q in Q_GRID:
            for R in R_GRID:
                rs = [simular(Q, R, d, l, p["inv0"]) for d, l in datos]
                res[(Q, R)] = {
                    "costo": np.mean([r["costo"] for r in rs]),
                    "servicio": np.mean([r["servicio"] for r in rs]),
                    "inv_prom": np.mean([r["inv_prom"] for r in rs]),
                }
        self.config(cursor="")
        orden = sorted(res, key=lambda k: res[k]["costo"])
        mejor = orden[0]

        self.ax.clear()
        mat = np.zeros((len(R_GRID), len(Q_GRID)))
        for i, Q in enumerate(Q_GRID):
            for j, R in enumerate(R_GRID):
                mat[j, i] = res[(Q, R)]["costo"]
        im = self.ax.imshow(mat, cmap="RdYlGn_r", aspect="auto", origin="lower")
        self.ax.set_xticks(range(len(Q_GRID)))
        self.ax.set_xticklabels(Q_GRID)
        self.ax.set_yticks(range(len(R_GRID)))
        self.ax.set_yticklabels(R_GRID)
        self.ax.set_xlabel("Q (lote)")
        self.ax.set_ylabel("R (reorden)")
        self.ax.set_title("Optimización – Costo anual  |  Mejor: Q=%d, R=%d" % (mejor[0], mejor[1]))
        self.fig.colorbar(im, ax=self.ax, fraction=0.046, pad=0.04)
        qi = Q_GRID.index(mejor[0])
        ri = R_GRID.index(mejor[1])
        self.ax.plot(qi, ri, "o", color=ORO, ms=14, markeredgecolor="black", markeredgewidth=2)
        self.fig.tight_layout()
        self.cv.draw()

        lin = ["══ OPTIMIZACIÓN (Q, R) 2D ══\n",
               "Semilla %d | %d réplicas de %d días\n" % (p["seed"], p["reps"], p["dias"]),
               "%-4s %-4s %12s %10s %10s" % ("Q", "R", "Costo $", "Servicio%", "Inv.prom")]
        for k in orden[:10]:
            r = res[k]
            marca = " ◄ mejor" if k == mejor else ""
            lin.append("%-4d %-4d %12.2f %10.2f %10.1f%s" %
                       (k[0], k[1], r["costo"], r["servicio"], r["inv_prom"], marca))
        self.v["q2"].set(str(mejor[0]))
        self.v["r2"].set(str(mejor[1]))
        lin.append("\nAlternativa cargada: Q₂=%d, R₂=%d" % (mejor[0], mejor[1]))
        self.txt.delete("1.0", "end")
        self.txt.insert("1.0", "\n".join(lin))

    def validar(self):
        p = self.leer()
        if p is None:
            return
        datos = [generar(p["seed"], i, p["dias"]) for i in range(p["reps"])]
        base = [simular(p["q"], p["r"], d, l, p["inv0"]) for d, l in datos]
        alt = [simular(p["q2"], p["r2"], d, l, p["inv0"]) for d, l in datos]
        bc = np.array([r["costo"] for r in base])
        ac = np.array([r["costo"] for r in alt])
        dif = bc - ac
        n = len(dif)
        dm = float(np.mean(dif))
        ds = float(np.std(dif, ddof=1))
        se = ds / math.sqrt(n) if n else 0

        L = ["══ VALIDACIÓN ESTADÍSTICA ══\n",
             "Semilla %d | %d réplicas | alfa=%.2f\n" % (p["seed"], n, ALFA),
             "A) COMPARACIÓN DE POLÍTICAS",
             "  Base  (Q=%d, R=%d): $%.2f" % (p["q"], p["r"], bc.mean()),
             "  Alter.(Q=%d, R=%d): $%.2f" % (p["q2"], p["r2"], ac.mean()),
             "  Diferencia media: $%.2f\n" % dm]
        if np.ptp(dif) > 0:
            t, t_p = stats.ttest_rel(bc, ac)
            tc = stats.t.ppf(1 - ALFA / 2, n - 1)
            L.append("  t pareada: t=%.4f  p=%s" % (t, fmt_p(t_p)))
            L.append("  IC 95%%: [$%.2f, $%.2f]" % (dm - tc * se, dm + tc * se))
            try:
                ws, w_p = stats.wilcoxon(bc, ac)
                L.append("  Wilcoxon: est=%.1f  p=%s" % (ws, fmt_p(w_p)))
            except ValueError:
                L.append("  Wilcoxon: no calculable.")
        L.append("")
        dem_all = np.concatenate([d for d, _ in datos])
        lt_all = np.concatenate([l for _, l in datos])
        cd = chi_cuadrada(dem_all, DEM_VAL, DEM_P)
        cl = chi_cuadrada(lt_all, LT_VAL, LT_P)
        L.append("B) CHI CUADRADA (bondad de ajuste)")
        for nombre, c in (("Demanda diaria", cd), ("Tiempo de entrega", cl)):
            L.append("  %s:" % nombre)
            L.append("    chi²=%.3f  crítica=%.3f  p=%s" % (c["chi"], c["crit"], fmt_p(c["p"])))
            L.append("    → %s" % ("NO se rechaza H0" if c["acepta"] else "SE RECHAZA H0"))

        self.txt.delete("1.0", "end")
        self.txt.insert("1.0", "\n".join(L))

        self.ax.clear()
        self.fig.clear()
        for k, (c, vals, tit) in enumerate(
            ((cd, DEM_VAL, "Demanda diaria"), (cl, LT_VAL, "Tiempo de entrega"))
        ):
            ax = self.fig.add_subplot(1, 2, k + 1)
            x = np.arange(len(vals))
            ax.bar(x - 0.2, c["obs"], 0.4, label="Observada", color="#e08a1e", edgecolor="#333")
            ax.bar(x + 0.2, c["esp"], 0.4, label="Esperada", color="#7fb2ea", edgecolor="#333")
            ax.set_xticks(x)
            ax.set_xticklabels(vals)
            ax.set_title("%s  chi²=%.2f" % (tit, c["chi"]), fontsize=9)
            ax.legend(fontsize=7)
        self.fig.tight_layout()
        self.cv.draw()
        self.ax = self.fig.axes[0] if self.fig.axes else self.fig.add_subplot(111)


if __name__ == "__main__":
    App().mainloop()