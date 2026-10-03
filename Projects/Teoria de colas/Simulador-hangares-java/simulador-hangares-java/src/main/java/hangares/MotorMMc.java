package hangares;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Random;

/**
 * Motor de la simulación de líneas de espera M/M/c (hangares).
 *
 * Incluye:
 *  - 4.3.1 Simulación por eventos discretos, métricas medidas y análisis económico.
 *  - 4.4.1 Pruebas paramétricas (Chi-cuadrada y t de Student).
 *  - 4.4.2 Pruebas no paramétricas (Kolmogorov-Smirnov).
 *  - Intervalo de confianza del 95 % (medias por lotes).
 *  - Exportación a CSV.
 *
 * Componentes de temas anteriores:
 *  - Tema 2: números pseudoaleatorios U(0,1) con semilla fija (java.util.Random).
 *  - Tema 3: variables exponenciales por transformada inversa X = -ln(1-U)/tasa.
 */
public class MotorMMc {

    // ------------------------------------------------------------------
    // PARÁMETROS DEL MODELO (cámbialos aquí si el problema pide otros valores)
    // ------------------------------------------------------------------
    public static final double LAMBDA = 1.20;         // llegadas por día
    public static final double MU = 0.50;             // servicios por día y por hangar
    public static final double COSTO_HANGAR = 150.0;  // costo por hangar por día
    public static final double COSTO_ESPERA = 704.0;  // costo por avión en cola por día
    public static final double TIEMPO_MAX = 10000.0;  // horizonte de simulación (días)
    public static final long SEMILLA = 4L;            // semilla del generador

    // ------------------------------------------------------------------
    // TEORÍA M/M/c
    // ------------------------------------------------------------------
    public static final class Teoria {
        public final double rho, p0, lq, wq, w;

        Teoria(double rho, double p0, double lq, double wq, double w) {
            this.rho = rho;
            this.p0 = p0;
            this.lq = lq;
            this.wq = wq;
            this.w = w;
        }
    }

    /** Fórmulas teóricas M/M/c. Devuelve null si rho >= 1 (sin estado estable). */
    public static Teoria teoria(double lam, double mu, int c) {
        double a = lam / mu;
        double rho = a / c;
        if (rho >= 1) return null;
        double suma = 0, fact = 1;                 // al final del ciclo fact = (c-1)!
        for (int n = 0; n < c; n++) {
            if (n > 0) fact *= n;
            suma += Math.pow(a, n) / fact;
        }
        double factC = fact * c;                   // c!
        double p0 = 1.0 / (suma + Math.pow(a, c) / (factC * (1 - rho)));
        double lq = p0 * Math.pow(a, c) * rho / (factC * Math.pow(1 - rho, 2));
        double wq = lq / lam;
        return new Teoria(rho, p0, lq, wq, wq + 1.0 / mu);
    }

    // ------------------------------------------------------------------
    // REGISTRO POR AVIÓN (una fila del CSV)
    // ------------------------------------------------------------------
    public static final class Registro {
        public final int id;
        public final double llegada, entreLlegadas;
        public int hangar = 0;                     // 0 = aún no entra a un hangar
        public double inicio = Double.NaN, servicio = Double.NaN, espera = Double.NaN;
        public double salida = Double.NaN, sistema = Double.NaN;

        Registro(int id, double llegada, double entreLlegadas) {
            this.id = id;
            this.llegada = llegada;
            this.entreLlegadas = entreLlegadas;
        }
    }

    // ------------------------------------------------------------------
    // ESTADO DE LA SIMULACIÓN
    // ------------------------------------------------------------------
    public int numHangares;
    public boolean corriendo;
    public double tiempoSim;
    private final Random rng;

    public final ArrayDeque<Integer> cola = new ArrayDeque<>();        // ids esperando (FIFO)
    public final int[] hangaresOcupados = new int[5];                   // id del avión (0 = libre)
    public final double[] tiemposSalida = new double[5];
    public final List<Registro> registros = new ArrayList<>();

    public final List<Double> llegadas = new ArrayList<>();             // tiempos entre llegadas
    public final List<Double> servicios = new ArrayList<>();            // tiempos de servicio
    public final List<Double> esperas = new ArrayList<>();              // espera en cola (medida)
    public final List<Double> sistemas = new ArrayList<>();            // tiempo en el sistema (medido)

    public final List<Double> histT = new ArrayList<>();                // historial para gráficas
    public final List<Double> histQ = new ArrayList<>();

    public double proximaLlegada = Double.NaN;
    private double tUltimaLlegada, tUltimoEvento, areaCola, areaOcupados;
    private int contadorAviones;

    public MotorMMc(int numHangares, long semilla) {
        this.numHangares = numHangares;
        this.rng = new Random(semilla);
        reiniciar();
    }

    /** Limpia la corrida (no vuelve a fijar la semilla: cada corrida es independiente). */
    public void reiniciar() {
        corriendo = false;
        tiempoSim = 0.0;
        cola.clear();
        for (int i = 0; i < 5; i++) {
            hangaresOcupados[i] = 0;
            tiemposSalida[i] = Double.POSITIVE_INFINITY;
        }
        registros.clear();
        llegadas.clear();
        servicios.clear();
        esperas.clear();
        sistemas.clear();
        histT.clear();
        histQ.clear();
        histT.add(0.0);
        histQ.add(0.0);
        proximaLlegada = Double.NaN;
        tUltimaLlegada = 0.0;
        tUltimoEvento = 0.0;
        areaCola = 0.0;
        areaOcupados = 0.0;
        contadorAviones = 0;
    }

    // ------------------------------------------------------------------
    // GENERACIÓN DE VARIABLES ALEATORIAS (Temas 2 y 3)
    // ------------------------------------------------------------------
    /** Transformada inversa: X = -ln(1 - U) / tasa, con U ~ U(0,1). */
    private double exponencial(double tasa) {
        double u = rng.nextDouble();
        return -Math.log(1.0 - u) / tasa;
    }

    // ------------------------------------------------------------------
    // MOTOR DE EVENTOS DISCRETOS
    // ------------------------------------------------------------------
    public void asegurarArranque() {
        if (Double.isNaN(proximaLlegada)) {
            proximaLlegada = tiempoSim + exponencial(LAMBDA);
        }
    }

    /** Integra en el tiempo el nº de aviones en cola y de hangares ocupados. */
    private void acumularAreas(double t) {
        double dt = t - tUltimoEvento;
        if (dt > 0) {
            int ocupados = 0;
            for (int i = 0; i < numHangares; i++) if (hangaresOcupados[i] > 0) ocupados++;
            areaCola += cola.size() * dt;
            areaOcupados += ocupados * dt;
        }
        tUltimoEvento = t;
    }

    private void registrarHistorial(double t) {
        histT.add(t);
        histQ.add((double) cola.size());
    }

    private void eventoLlegada(double t) {
        contadorAviones++;
        int id = contadorAviones;
        double entre = t - tUltimaLlegada;
        tUltimaLlegada = t;
        llegadas.add(entre);
        registros.add(new Registro(id, t, entre));
        cola.addLast(id);

        proximaLlegada = t + exponencial(LAMBDA);     // programar la siguiente llegada

        asignarHangares(t);
        registrarHistorial(t);
    }

    /** Cada hangar libre toma al primer avión de la cola (FIFO). */
    private void asignarHangares(double t) {
        for (int i = 0; i < numHangares; i++) {
            if (hangaresOcupados[i] == 0 && !cola.isEmpty()) {
                int id = cola.pollFirst();
                Registro reg = registros.get(id - 1);

                double tServicio = exponencial(MU);
                double espera = t - reg.llegada;      // espera REAL en cola

                reg.hangar = i + 1;
                reg.inicio = t;
                reg.servicio = tServicio;
                reg.espera = espera;

                servicios.add(tServicio);
                esperas.add(espera);

                hangaresOcupados[i] = id;
                tiemposSalida[i] = t + tServicio;
            }
        }
    }

    private void eventoSalida(double t, int i) {
        int id = hangaresOcupados[i];
        Registro reg = registros.get(id - 1);
        reg.salida = t;
        reg.sistema = t - reg.llegada;
        sistemas.add(reg.sistema);

        hangaresOcupados[i] = 0;
        tiemposSalida[i] = Double.POSITIVE_INFINITY;

        asignarHangares(t);
        registrarHistorial(t);
    }

    /** Procesa en orden cronológico todas las llegadas y salidas hasta tLimite. */
    public void procesarHasta(double tLimite) {
        while (true) {
            int iSal = 0;
            double tSal = Double.POSITIVE_INFINITY;
            for (int k = 0; k < numHangares; k++) {
                if (tiemposSalida[k] < tSal) {
                    tSal = tiemposSalida[k];
                    iSal = k;
                }
            }
            double tLleg = proximaLlegada;
            double tEvt = Math.min(tLleg, tSal);
            if (tEvt > tLimite) break;
            acumularAreas(tEvt);
            if (tLleg <= tSal) eventoLlegada(tEvt);
            else eventoSalida(tEvt, iSal);
        }
        acumularAreas(tLimite);
        tiempoSim = tLimite;
    }

    // ------------------------------------------------------------------
    // MÉTRICAS
    // ------------------------------------------------------------------
    private static double[] arreglo(List<Double> lista) {
        double[] a = new double[lista.size()];
        for (int i = 0; i < a.length; i++) a[i] = lista.get(i);
        return a;
    }

    /** Devuelve {Wq, W, Lq, rho} medidos en la corrida. */
    public double[] metricas() {
        double wq = esperas.isEmpty() ? 0.0 : Estadistica.media(arreglo(esperas));
        double w = sistemas.isEmpty() ? 0.0 : Estadistica.media(arreglo(sistemas));
        double lq = tiempoSim > 0 ? areaCola / tiempoSim : 0.0;
        double rho = tiempoSim > 0 ? areaOcupados / (numHangares * tiempoSim) : 0.0;
        return new double[]{wq, w, lq, rho};
    }

    /**
     * Medias por lotes de Wq. Las esperas consecutivas están correlacionadas, por eso
     * se agrupan en k lotes; además se descarta el 10 % inicial (arranque con el
     * sistema vacío) para reducir el sesgo del periodo transitorio.
     */
    public double[] mediasPorLotes() {
        int k = 20;
        double[] todos = arreglo(esperas);
        int ini = (int) Math.floor(todos.length * 0.10);
        int n = todos.length - ini;
        if (n < 100) return null;
        int m = n / k;
        double[] medias = new double[k];
        for (int j = 0; j < k; j++) {
            double s = 0;
            for (int q = 0; q < m; q++) s += todos[ini + j * m + q];
            medias[j] = s / m;
        }
        return medias;
    }

    /** IC 95 % de Wq {inferior, superior} o null si hay pocos datos. */
    public double[] intervaloConfianzaWq() {
        double[] medias = mediasPorLotes();
        if (medias == null) return null;
        int k = medias.length;
        double media = Estadistica.media(medias);
        double se = Estadistica.desviacion(medias) / Math.sqrt(k);
        double tCrit = Estadistica.tInv(0.975, k - 1);
        return new double[]{Math.max(0.0, media - tCrit * se), media + tCrit * se};
    }

    // ------------------------------------------------------------------
    // TEXTO DEL MONITOR
    // ------------------------------------------------------------------
    private static String fm(Double v) {
        return v == null ? "     n/a" : String.format(Locale.US, "%8.3f", v);
    }

    private static String f(double v, int d) {
        return String.format(Locale.US, "%." + d + "f", v);
    }

    public String textoMonitor() {
        double[] m = metricas();
        int c = numHangares;
        Teoria th = teoria(LAMBDA, MU, c);
        double rhoTeo = LAMBDA / (c * MU);
        int ocupados = 0;
        for (int i = 0; i < c; i++) if (hangaresOcupados[i] > 0) ocupados++;

        String estado = tiempoSim >= TIEMPO_MAX ? "Finalizada"
                : corriendo ? "En ejecución"
                : tiempoSim > 0 ? "Pausada" : "Listo";

        double costoServ = c * COSTO_HANGAR;
        double costoEsp = COSTO_ESPERA * m[2];
        double costoSim = costoServ + costoEsp;
        String costoTeo = th != null
                ? "$ " + f(costoServ + COSTO_ESPERA * th.lq, 2) + " /día"
                : "n/a (ρ ≥ 1)";

        StringBuilder sb = new StringBuilder();
        sb.append("  SIMULADO vs TEORÍA (M/M/c), c = ").append(c).append("\n");
        sb.append("------------------------------------\n");
        sb.append("Tiempo:            ").append(f(tiempoSim, 1)).append(" / ")
                .append(f(TIEMPO_MAX, 0)).append(" días\n");
        sb.append("Estado:            ").append(estado).append("\n");
        sb.append("Llegadas/Salidas:  ").append(llegadas.size()).append(" / ").append(sistemas.size()).append("\n");
        sb.append("Hangares Ocupados: ").append(ocupados).append(" de ").append(c).append("\n");
        sb.append("Aviones en cola:   ").append(cola.size()).append("\n\n");

        sb.append("Métrica       Simul.    Teoría\n");
        sb.append("Wq (espera) ").append(fm(m[0])).append(" ").append(fm(th == null ? null : th.wq)).append("\n");
        sb.append("W  (sistema)").append(fm(m[1])).append(" ").append(fm(th == null ? null : th.w)).append("\n");
        sb.append("Lq (cola)   ").append(fm(m[2])).append(" ").append(fm(th == null ? null : th.lq)).append("\n");
        sb.append("ρ  (uso)    ").append(fm(m[3])).append(" ").append(fm(th == null ? null : th.rho)).append("\n");
        if (th == null) {
            sb.append("⚠ ρ = ").append(f(rhoTeo, 2)).append(" ≥ 1: no hay estado\n");
            sb.append("  estable; la cola crece sin límite\n");
            sb.append("  y no existen valores teóricos.\n");
        }

        sb.append("\n  ANÁLISIS ECONÓMICO \n");
        sb.append("------------------------------------\n");
        sb.append("Costo Hangares:    $ ").append(f(costoServ, 2)).append(" /día\n");
        sb.append("Costo Espera:      $ ").append(f(costoEsp, 2)).append(" /día\n");
        sb.append("Costo Total Simul: $ ").append(f(costoSim, 2)).append(" /día\n");
        sb.append("Costo Total Teor:  ").append(costoTeo).append("\n");

        sb.append("\n  VALIDACIÓN  (IC 95% para Wq)\n");
        sb.append("------------------------------------\n");
        double[] ic = intervaloConfianzaWq();
        if (ic == null) {
            sb.append("IC 95% Wq: requiere ≥ 100 aviones atendidos\n");
        } else {
            sb.append("IC 95% Wq: [").append(f(ic[0], 3)).append(", ").append(f(ic[1], 3)).append("]\n");
            if (th != null) {
                sb.append("Wq teórico dentro del IC: ")
                        .append(ic[0] <= th.wq && th.wq <= ic[1] ? "Sí" : "No").append("\n");
            }
        }
        return sb.toString();
    }

    // ------------------------------------------------------------------
    // VALIDACIÓN ESTADÍSTICA (SUBTEMAS 4.4.1 y 4.4.2)
    // ------------------------------------------------------------------
    /** Chi-cuadrada de bondad de ajuste a la exponencial con k clases equiprobables. */
    private static double[] chi2Exponencial(double[] x, double tasa, int k) {
        int n = x.length;
        int[] obs = new int[k];
        for (double v : x) {
            double F = 1.0 - Math.exp(-tasa * v);
            int clase = Math.min((int) Math.floor(F * k), k - 1);
            obs[clase]++;
        }
        double esp = (double) n / k;
        double chi2 = 0;
        for (int o : obs) chi2 += (o - esp) * (o - esp) / esp;
        return new double[]{chi2, Estadistica.chi2Inv(0.95, k - 1), Estadistica.chi2Sf(chi2, k - 1), k - 1};
    }

    private void bloque(StringBuilder sb, List<Boolean> dec, String titulo, List<Double> lista, double tasa) {
        double[] datos = arreglo(lista);
        int n = datos.length;
        sb.append(titulo).append("\n");
        sb.append("--------------------------------------------------\n");
        if (n < 50) {
            sb.append("   Datos insuficientes (n = ").append(n).append("). Ejecuta la simulación\n");
            sb.append("   (Iniciar o Fin Rápido) para tener al menos 50 datos.\n\n");
            return;
        }
        double[] ch = chi2Exponencial(datos, tasa, 10);
        double d = Estadistica.ksD(datos, tasa);
        double pKs = Estadistica.ksPValor(d, n);
        double critKs = Estadistica.ksCritico(n);
        boolean okChi = ch[0] <= ch[1];
        boolean okKs = d <= critKs;
        dec.add(okChi);
        dec.add(okKs);

        sb.append("   n = ").append(n).append("\n");
        sb.append("   [4.4.1 Paramétrica] Chi-Cuadrada (10 clases, gl=").append((int) ch[3]).append("):\n");
        sb.append("      chi2=").append(f(ch[0], 3)).append(" (crit=").append(f(ch[1], 3))
                .append(", p=").append(f(ch[2], 3)).append(") -> ")
                .append(okChi ? "NO SE RECHAZA H0" : "SE RECHAZA H0").append("\n");
        sb.append("   [No Paramét.] Kolmogorov-Smirnov:\n");
        sb.append("      D_max=").append(f(d, 3)).append(" (crit=").append(f(critKs, 3))
                .append(", p=").append(f(pKs, 3)).append(") -> ")
                .append(okKs ? "NO SE RECHAZA H0" : "SE RECHAZA H0").append("\n\n");
    }

    public String generarReporteValidacion() {
        int c = numHangares;
        List<Boolean> dec = new ArrayList<>();
        StringBuilder sb = new StringBuilder();
        sb.append("==================================================\n");
        sb.append("  REPORTE DE VALIDACIÓN DEL SIMULACION\n");
        sb.append("==================================================\n");
        sb.append("Nivel de significancia α = 0.05 | c = ").append(c).append(" hangares\n");
        sb.append("Tiempo simulado: ").append(f(tiempoSim, 1)).append(" días\n\n");

        bloque(sb, dec, "1. TIEMPOS ENTRE LLEGADAS (Exponencial, λ = " + f(LAMBDA, 2) + ")", llegadas, LAMBDA);
        bloque(sb, dec, "2. TIEMPOS DE SERVICIO (Exponencial, μ = " + f(MU, 2) + ")", servicios, MU);

        sb.append("3. MEDIA DE LA ESPERA EN COLA Wq vs TEORÍA M/M/c\n");
        sb.append("--------------------------------------------------\n");
        Teoria th = teoria(LAMBDA, MU, c);
        if (th == null) {
            sb.append("   ρ = ").append(f(LAMBDA / (c * MU), 2)).append(" ≥ 1: no existe estado estable, por lo tanto\n");
            sb.append("   no hay Wq teórico contra el cual comparar (usa c = 3).\n\n");
        } else {
            double[] medias = mediasPorLotes();
            if (medias == null) {
                sb.append("   Datos insuficientes (n = ").append(esperas.size()).append("); se requieren ≥ 100.\n\n");
            } else {
                int k = medias.length;
                double media = Estadistica.media(medias);
                double se = Estadistica.desviacion(medias) / Math.sqrt(k);
                double tStat = (media - th.wq) / se;
                double pT = Estadistica.tPValorDosColas(tStat, k - 1);
                boolean okT = pT >= 0.05;
                dec.add(okT);
                double[] ic = intervaloConfianzaWq();
                sb.append("   Wq simulado = ").append(f(Estadistica.media(arreglo(esperas)), 3))
                        .append(" | Wq teórico = ").append(f(th.wq, 3)).append("\n");
                sb.append("   [4.4.1 Paramétrica] t de Student (20 medias por lotes, sin 10% inicial),\n");
                sb.append("      H0: media de Wq = ").append(f(th.wq, 3)).append("\n");
                sb.append("      t=").append(f(tStat, 3)).append(", p=").append(f(pT, 3)).append(" -> ")
                        .append(okT ? "NO SE RECHAZA H0" : "SE RECHAZA H0").append("\n");
                sb.append("   IC 95% Wq: [").append(f(ic[0], 3)).append(", ").append(f(ic[1], 3)).append("] -> teórico ")
                        .append(ic[0] <= th.wq && th.wq <= ic[1] ? "DENTRO" : "FUERA").append(" del intervalo\n\n");
            }
        }

        sb.append("CONCLUSIÓN\n");
        sb.append("--------------------------------------------------\n");
        boolean todas = true;
        for (boolean b : dec) todas &= b;
        if (dec.isEmpty()) {
            sb.append("   Sin datos suficientes para concluir.\n");
        } else if (todas) {
            sb.append("   No se rechaza H0 en ninguna prueba: el simulador reproduce\n");
            sb.append("   el modelo M/M/c y sus generadores son válidos.\n");
        } else {
            sb.append("   Al menos una prueba rechaza H0. Con α = 0.05 cada prueba rechaza\n");
            sb.append("   por puro azar ~1 de cada 20 veces (con 5 pruebas, ~1 de cada 4-5\n");
            sb.append("   corridas muestra algún rechazo). Reinicia y vuelve a correr para confirmar.\n");
        }
        return sb.toString();
    }

    // ------------------------------------------------------------------
    // EXPORTACIÓN A CSV
    // ------------------------------------------------------------------
    private static String r4(double v) {
        return Double.isNaN(v) ? "" : String.format(Locale.US, "%.4f", v);
    }

    private static String rr(Double v) {
        return v == null ? "n/a" : String.format(Locale.US, "%.4f", v);
    }

    public void exportarCsv(File archivo) throws IOException {
        try (Writer w = new OutputStreamWriter(new FileOutputStream(archivo), StandardCharsets.UTF_8)) {
            w.write('\uFEFF');   // BOM: Excel abre correctamente los acentos
            w.write("Cliente,Hora_Llegada,Tiempo_Entre_Llegada,Hangar,Hora_Inicio_Servicio,"
                    + "Tiempo_Servicio,Tiempo_Espera_Wq,Hora_Salida,Tiempo_Sistema_W\n");
            for (Registro r : registros) {
                w.write(r.id + "," + r4(r.llegada) + "," + r4(r.entreLlegadas) + ","
                        + (r.hangar == 0 ? "" : String.valueOf(r.hangar)) + ","
                        + r4(r.inicio) + "," + r4(r.servicio) + "," + r4(r.espera) + ","
                        + r4(r.salida) + "," + r4(r.sistema) + "\n");
            }

            // Resumen de la corrida (simulado vs teórico)
            double[] m = metricas();
            Teoria th = teoria(LAMBDA, MU, numHangares);
            double costoServ = numHangares * COSTO_HANGAR;
            w.write("\nRESUMEN DE LA CORRIDA\n");
            w.write("Hangares (c)," + numHangares + "\n");
            w.write("Lambda," + LAMBDA + "\n");
            w.write("Mu," + MU + "\n");
            w.write("Tiempo simulado (dias)," + f(tiempoSim, 2) + "\n");
            w.write("Semilla," + SEMILLA + "\n\n");
            w.write("Metrica,Simulado,Teorico\n");
            w.write("Wq," + rr(m[0]) + "," + rr(th == null ? null : th.wq) + "\n");
            w.write("W," + rr(m[1]) + "," + rr(th == null ? null : th.w) + "\n");
            w.write("Lq," + rr(m[2]) + "," + rr(th == null ? null : th.lq) + "\n");
            w.write("Utilizacion," + rr(m[3]) + "," + rr(th == null ? null : th.rho) + "\n");
            w.write("Costo total por dia," + f(costoServ + COSTO_ESPERA * m[2], 2) + ","
                    + (th == null ? "n/a" : f(costoServ + COSTO_ESPERA * th.lq, 2)) + "\n");
            double[] ic = intervaloConfianzaWq();
            if (ic != null) {
                w.write("IC 95% Wq,[" + f(ic[0], 4) + " ; " + f(ic[1], 4) + "]\n");
            }
        }
    }
}
