package inventario;

import javafx.animation.AnimationTimer;
import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.stage.Stage;

import java.util.*;

public class InventariosApp extends Application {

    static final int[] DEM_VAL = {2, 4, 6, 8, 10};
    static final double[] DEM_P = {0.10, 0.25, 0.30, 0.25, 0.10};
    static final int[] LT_VAL = {2, 3, 4};
    static final double[] LT_P = {0.30, 0.40, 0.30};
    static final double COSTO_PEDIDO = 150, COSTO_MANTENER = 0.20, COSTO_FALTANTE = 40;
    static final int[] Q_GRID = {40, 60, 80, 100};
    static final int[] R_GRID = {15, 20, 25, 30, 35};
    static final double ALFA = 0.05;

    private final TextField tfQ = new TextField("80");
    private final TextField tfR = new TextField("25");
    private final TextField tfInv0 = new TextField("40");
    private final TextField tfDias = new TextField("365");
    private final TextField tfQ2 = new TextField("100");
    private final TextField tfR2 = new TextField("30");
    private final TextField tfReps = new TextField("30");
    private final TextField tfSeed = new TextField("");
    private final TextArea resultados = new TextArea();
    private final Canvas canvas = new Canvas(700, 150);
    private final Canvas canvasGraf = new Canvas(960, 180);
    private final Label lblEstado = new Label("Listo. Pulsa Iniciar.");

    private SimResult tr;
    private Params p;
    private int dia = 0;
    private int velocidad = 30;
    private boolean pausado = false;
    private long lastNs = 0;
    private double acumPaso = 0;
    private AnimationTimer timer;

    private double[][] matrizCostos;
    private String mejorKey;
    private double[] chiObsDem, chiEspDem, chiObsLt, chiEspLt;
    private String modoGraf = "vacio";

    static class Params {
        int q, r, inv0, dias, q2, r2, reps, seed;
    }

    static class SimResult {
        double costo, cPed, cMant, cFalt, servicio, invProm;
        int pedidos, perdidas;
        int[] cierre, resta, ltDia;
        double[] acum;
        List<Integer> evPedidos = new ArrayList<>();
        List<Integer> evLlegadas = new ArrayList<>();
        List<Integer> evFaltantes = new ArrayList<>();
    }

    static int[] discreta(Random rng, int[] valores, double[] probs, int n) {
        int[] out = new int[n];
        double[] cum = new double[probs.length];
        cum[0] = probs[0];
        for (int i = 1; i < probs.length; i++) cum[i] = cum[i - 1] + probs[i];
        for (int i = 0; i < n; i++) {
            double u = rng.nextDouble();
            int idx = 0;
            while (idx < cum.length - 1 && u > cum[idx]) idx++;
            out[i] = valores[idx];
        }
        return out;
    }

    static int[][] generar(int seed, int replica, int dias) {
        Random rd = new Random(Objects.hash(seed, replica, 0));
        Random rl = new Random(Objects.hash(seed, replica, 1));
        return new int[][]{
                discreta(rd, DEM_VAL, DEM_P, dias),
                discreta(rl, LT_VAL, LT_P, dias)
        };
    }

    static SimResult simular(int Q, int R, int[] dem, int[] lts, int inv0) {
        int dias = dem.length;
        int inv = inv0;
        boolean pendiente = false;
        int llegada = -1, ltAct = 0;
        double cPed = 0, cMant = 0, cFalt = 0;
        int perdidas = 0, nPed = 0, sumaInv = 0;
        SimResult r = new SimResult();
        r.cierre = new int[dias + 1];
        r.resta = new int[dias + 1];
        r.ltDia = new int[dias + 1];
        r.acum = new double[dias + 1];
        r.cierre[0] = inv;
        for (int d = 1; d <= dias; d++) {
            if (pendiente && d == llegada) {
                inv += Q;
                pendiente = false;
                r.evLlegadas.add(d);
            }
            int x = dem[d - 1];
            int vendido = Math.min(inv, x);
            int falt = x - vendido;
            inv -= vendido;
            if (falt > 0) {
                perdidas += falt;
                cFalt += COSTO_FALTANTE * falt;
                r.evFaltantes.add(d);
            }
            cMant += COSTO_MANTENER * inv;
            sumaInv += inv;
            if (!pendiente && inv <= R) {
                ltAct = lts[d - 1];
                pendiente = true;
                llegada = d + ltAct;
                nPed++;
                cPed += COSTO_PEDIDO;
                r.evPedidos.add(d);
            }
            r.cierre[d] = inv;
            r.resta[d] = pendiente ? llegada - d : 0;
            r.ltDia[d] = pendiente ? ltAct : 0;
            r.acum[d] = cPed + cMant + cFalt;
        }
        int totalDem = Arrays.stream(dem).sum();
        r.costo = cPed + cMant + cFalt;
        r.cPed = cPed;
        r.cMant = cMant;
        r.cFalt = cFalt;
        r.pedidos = nPed;
        r.perdidas = perdidas;
        r.servicio = totalDem > 0 ? 100.0 * (totalDem - perdidas) / totalDem : 100;
        r.invProm = (double) sumaInv / dias;
        return r;
    }

    @Override
    public void start(Stage stage) {
        GridPane params = new GridPane();
        params.setHgap(6);
        params.setVgap(3);
        params.setPadding(new Insets(6));
        params.addRow(0, new Label("Q:"), tfQ, new Label("R:"), tfR,
                new Label("Inv. inicial:"), tfInv0, new Label("Días:"), tfDias);
        params.addRow(1, new Label("Q₂:"), tfQ2, new Label("R₂:"), tfR2,
                new Label("Réplicas:"), tfReps, new Label("Semilla:"), tfSeed);
        for (TextField tf : List.of(tfQ, tfR, tfInv0, tfDias, tfQ2, tfR2, tfReps, tfSeed))
            tf.setPrefWidth(65);

        BorderPane bodegaBox = new BorderPane(canvas);
        bodegaBox.setStyle("-fx-background-color: white; -fx-border-color: #ccc;");
        bodegaBox.setPrefHeight(150);
        HBox.setHgrow(bodegaBox, Priority.ALWAYS);

        resultados.setEditable(false);
        resultados.setPrefWidth(280);
        resultados.setWrapText(true);
        resultados.setText("Pulsa «Iniciar» para comenzar.\n");

        HBox centro = new HBox(6, bodegaBox, resultados);

        BorderPane grafBox = new BorderPane(canvasGraf);
        grafBox.setStyle("-fx-background-color: white; -fx-border-color: #ccc;");
        grafBox.setPrefHeight(180);
        VBox.setVgrow(grafBox, Priority.ALWAYS);

        Button btnIniciar = btn("▶ Iniciar", "#2e9e4f");
        Button btnPausar = btn("⏸ Pausar", "#e39a4a");
        Button btnReiniciar = btn("↻ Reiniciar", "#d33a2c");
        Button btnOpt = btn("Optimizar (Q,R) 2D", "#3b7dd8");
        Button btnVal = btn("Validar χ² / t / Wilcoxon", "#6b4c9a");

        btnIniciar.setOnAction(e -> iniciarReanudar());
        btnPausar.setOnAction(e -> pausado = true);
        btnReiniciar.setOnAction(e -> reiniciar());
        btnOpt.setOnAction(e -> optimizar());
        btnVal.setOnAction(e -> validar());

        Button bLento = new Button("Lento");
        Button bNormal = new Button("Normal");
        Button bRapido = new Button("Rápido");
        bLento.setOnAction(e -> velocidad = 10);
        bNormal.setOnAction(e -> velocidad = 30);
        bRapido.setOnAction(e -> velocidad = 80);

        HBox barra = new HBox(6, btnIniciar, btnPausar, btnReiniciar, btnOpt, btnVal,
                new Label("Velocidad:"), bLento, bNormal, bRapido);
        barra.setAlignment(Pos.CENTER_LEFT);
        barra.setPadding(new Insets(6));
        barra.setStyle("-fx-background-color: #d5dbe3;");

        VBox root = new VBox(4, params, centro, lblEstado, grafBox, barra);
        root.setPadding(new Insets(4));
        root.setStyle("-fx-background-color: #f0f2f5;");

        dibujarBodegaVacia();
        dibujarGrafVacio();

        grafBox.widthProperty().addListener((o, a, b) -> {
            canvasGraf.setWidth(b.doubleValue() - 4);
            redibujarGraf();
        });
        grafBox.heightProperty().addListener((o, a, b) -> {
            canvasGraf.setHeight(b.doubleValue() - 4);
            redibujarGraf();
        });

        stage.setTitle("InventarioCamarasBicicleta | Tema 4.3.2 Inventarios (Q, R)");
        stage.setScene(new Scene(root, 1000, 620));
        stage.show();
    }

    private static Button btn(String texto, String color) {
        Button b = new Button(texto);
        b.setStyle("-fx-background-color: " + color + "; -fx-text-fill: white; -fx-font-weight: bold; -fx-font-size: 11px;");
        return b;
    }

    private Params leer() {
        try {
            Params p = new Params();
            p.q = Integer.parseInt(tfQ.getText().trim());
            p.r = Integer.parseInt(tfR.getText().trim());
            p.inv0 = Integer.parseInt(tfInv0.getText().trim());
            p.dias = Integer.parseInt(tfDias.getText().trim());
            p.q2 = Integer.parseInt(tfQ2.getText().trim());
            p.r2 = Integer.parseInt(tfR2.getText().trim());
            p.reps = Math.max(5, Integer.parseInt(tfReps.getText().trim()));
            String s = tfSeed.getText().trim();
            p.seed = s.isEmpty() ? new Random().nextInt(1_000_000_000) : Integer.parseInt(s);
            if (p.dias < 1 || p.dias > 500) throw new IllegalArgumentException("Días entre 1 y 500");
            if (p.q <= 0) throw new IllegalArgumentException("Q debe ser > 0");
            return p;
        } catch (Exception ex) {
            new Alert(Alert.AlertType.ERROR, "Parámetros inválidos: " + ex.getMessage()).showAndWait();
            return null;
        }
    }

    private void iniciarReanudar() {
        if (tr == null || (!pausado && p != null && dia >= p.dias)) {
            p = leer();
            if (p == null) return;
            int[][] gen = generar(p.seed, 0, p.dias);
            tr = simular(p.q, p.r, gen[0], gen[1], p.inv0);
            dia = 0;
            acumPaso = 0;
            pausado = false;
            lastNs = 0;
            modoGraf = "inventario";
            if (timer != null) timer.stop();
            timer = new AnimationTimer() {
                @Override
                public void handle(long now) {
                    if (pausado || tr == null) return;
                    if (lastNs == 0) { lastNs = now; return; }
                    double dt = (now - lastNs) / 1e9;
                    lastNs = now;
                    acumPaso += velocidad * dt;
                    int n = (int) acumPaso;
                    acumPaso -= n;
                    int avance = Math.max(n, dia == 0 ? 1 : 0);
                    dia = Math.min(p.dias, dia + avance);
                    dibujar();
                    redibujarGraf();
                    if (dia >= p.dias) {
                        stop();
                        resultados.setText(String.format(
                                "══ FIN DEL AÑO ══%n%nPolítica (Q=%d, R=%d)%nSemilla: %d%n%n" +
                                        "Costo total: $%.2f%n  Pedidos: $%.2f%n  Mantener: $%.2f%n  Faltantes: $%.2f%n%n" +
                                        "Servicio: %.2f%%%nInv. promedio: %.1f%nPedidos: %d%nPerdidas: %d%n",
                                p.q, p.r, p.seed, tr.costo, tr.cPed, tr.cMant, tr.cFalt,
                                tr.servicio, tr.invProm, tr.pedidos, tr.perdidas));
                    }
                }
            };
            timer.start();
        } else {
            pausado = false;
            lastNs = 0;
        }
    }

    private void reiniciar() {
        if (timer != null) timer.stop();
        tr = null; p = null; dia = 0; pausado = false;
        modoGraf = "vacio"; matrizCostos = null; chiObsDem = null;
        dibujarBodegaVacia();
        dibujarGrafVacio();
        resultados.setText("Reiniciado. Pulsa «Iniciar».");
        lblEstado.setText("Reiniciado.");
    }

    private void dibujarBodegaVacia() {
        GraphicsContext g = canvas.getGraphicsContext2D();
        double w = canvas.getWidth(), h = canvas.getHeight();
        g.setFill(Color.WHITE); g.fillRect(0, 0, w, h);
        g.setFill(Color.web("#333"));
        g.setFont(Font.font("Arial", FontWeight.BOLD, 12));
        g.fillText("BODEGA (INVENTARIO)", w / 2 - 70, 16);
        double bw = 240, bh = 90, bx = (w - bw) / 2, by = 28;
        g.setStroke(Color.GRAY); g.setLineWidth(2);
        g.setFill(Color.web("#e8e8e8")); g.fillRect(bx, by, bw, bh);
        g.strokeRect(bx, by, bw, bh);
        g.setFill(Color.web("#999"));
        g.setFont(Font.font("Arial", FontWeight.BOLD, 18));
        g.fillText("—", bx + bw / 2 - 6, by + bh / 2 + 6);
        g.setStroke(Color.web("#c0392b")); g.setLineWidth(2);
        g.strokeLine(20, h - 18, w - 20, h - 18);
    }

    private void dibujar() {
        if (tr == null || p == null) return;
        GraphicsContext g = canvas.getGraphicsContext2D();
        double w = canvas.getWidth(), h = canvas.getHeight();
        g.setFill(Color.WHITE); g.fillRect(0, 0, w, h);
        int inv = tr.cierre[dia];
        int cap = Math.max(20, (int) (Math.ceil(Math.max(p.inv0, p.q + p.r) * 1.15 / 20) * 20));
        g.setFill(Color.web("#333"));
        g.setFont(Font.font("Arial", FontWeight.BOLD, 12));
        g.fillText("BODEGA (INVENTARIO)", w / 2 - 70, 14);
        double bw = 260, bh = 95, bx = (w - bw) / 2, by = 22;
        g.setFill(Color.web("#e8e8e8")); g.fillRect(bx, by, bw, bh);
        g.setStroke(Color.web("#666")); g.setLineWidth(2); g.strokeRect(bx, by, bw, bh);
        double frac = Math.min(inv / (double) cap, 1.0);
        double fillH = bh * frac;
        if (fillH > 0) {
            g.setFill(inv > p.r ? Color.web("#4a9fd8") : Color.web("#e39a4a"));
            g.fillRect(bx + 2, by + bh - fillH, bw - 4, fillH);
        }
        g.setFill(Color.web("#222"));
        g.setFont(Font.font("Arial", FontWeight.BOLD, 16));
        g.fillText(inv + " uds", bx + bw / 2 - 28, by + bh / 2 + 6);
        if (p.r > 0 && p.r < cap) {
            double yR = by + bh - bh * p.r / (double) cap;
            g.setStroke(Color.web("#d33a2c")); g.setLineDashes(4, 3); g.setLineWidth(1);
            g.strokeLine(bx - 8, yR, bx + bw + 8, yR); g.setLineDashes();
            g.setFill(Color.web("#d33a2c"));
            g.setFont(Font.font("Arial", FontWeight.BOLD, 9));
            g.fillText("R", bx - 18, yR + 3);
        }
        double lineY = h - 16;
        g.setStroke(Color.web("#c0392b")); g.setLineWidth(2);
        g.strokeLine(20, lineY, w - 20, lineY);
        int resta = tr.resta[dia];
        if (resta > 0) {
            int ltt = Math.max(1, tr.ltDia[dia]);
            double prog = (ltt - resta) / (double) ltt;
            double x = 30 + prog * (bx - 50);
            g.setFill(Color.web("#3b7dd8")); g.fillRect(x, lineY - 22, 40, 18);
            g.setFill(Color.web("#c0392b")); g.fillRect(x + 40, lineY - 16, 10, 12);
            g.setFill(Color.web("#222"));
            g.fillOval(x + 4, lineY - 8, 9, 9);
            g.fillOval(x + 30, lineY - 8, 9, 9);
            g.setFill(Color.web("#c0392b"));
            g.setFont(Font.font("Arial", FontWeight.BOLD, 10));
            g.fillText("Llega en: " + resta + "d", x, lineY - 26);
        } else {
            g.setFill(Color.web("#888")); g.setFont(Font.font("Arial", 9));
            g.fillText("Sin pedido pendiente", 50, lineY - 24);
        }
        long nPed = tr.evPedidos.stream().filter(e -> e <= dia).count();
        long nFal = tr.evFaltantes.stream().filter(e -> e <= dia).count();
        lblEstado.setText(String.format("Día %d / %d  |  Inventario: %d uds  |  Pedidos: %d  |  Faltantes: %d",
                dia, p.dias, inv, nPed, nFal));
        resultados.setText(String.format(
                "Día %d / %d%nPolítica (Q=%d, R=%d)%nSemilla: %d%n%nInventario: %d uds%n" +
                        "Pedidos: %d%nFaltantes: %d%nCosto acum.: $%.2f%n%s",
                dia, p.dias, p.q, p.r, p.seed, inv, nPed, nFal, tr.acum[dia],
                resta > 0 ? "%nPedido en camino: " + resta + " d" : ""));
    }

    private void dibujarGrafVacio() {
        GraphicsContext g = canvasGraf.getGraphicsContext2D();
        double w = Math.max(canvasGraf.getWidth(), 700);
        double h = Math.max(canvasGraf.getHeight(), 160);
        g.setFill(Color.WHITE); g.fillRect(0, 0, w, h);
        g.setFill(Color.web("#999")); g.setFont(Font.font("Arial", 12));
        g.fillText("Aquí: inventario  |  optimización (Q,R)  |  χ² validación", 30, h / 2);
    }

    private void redibujarGraf() {
        switch (modoGraf) {
            case "inventario" -> dibujarGrafInventario();
            case "opt" -> dibujarGrafOptimizacion();
            case "chi" -> dibujarGrafChi();
            default -> dibujarGrafVacio();
        }
    }

    private void dibujarGrafInventario() {
        if (tr == null || p == null) { dibujarGrafVacio(); return; }
        GraphicsContext g = canvasGraf.getGraphicsContext2D();
        double w = Math.max(canvasGraf.getWidth(), 700);
        double h = Math.max(canvasGraf.getHeight(), 160);
        g.setFill(Color.WHITE); g.fillRect(0, 0, w, h);
        double mL = 45, mR = 15, mT = 24, mB = 30;
        double plotW = w - mL - mR, plotH = h - mT - mB;
        int n = dia, maxInv = 1;
        for (int i = 0; i <= n; i++) maxInv = Math.max(maxInv, tr.cierre[i]);
        maxInv = Math.max(maxInv, p.r + 5);
        g.setStroke(Color.web("#333")); g.setLineWidth(1);
        g.strokeLine(mL, mT, mL, mT + plotH);
        g.strokeLine(mL, mT + plotH, mL + plotW, mT + plotH);
        g.setFill(Color.web("#333"));
        g.setFont(Font.font("Arial", FontWeight.BOLD, 11));
        g.fillText("Inventario diario (Día vs Unidades)", mL, 16);
        double yR = mT + plotH - (p.r / (double) maxInv) * plotH;
        g.setStroke(Color.web("#d33a2c")); g.setLineDashes(4, 3);
        g.strokeLine(mL, yR, mL + plotW, yR); g.setLineDashes();
        g.setFill(Color.web("#d33a2c")); g.setFont(Font.font("Arial", 8));
        g.fillText("R=" + p.r, mL + plotW - 35, yR - 3);
        g.setStroke(Color.web("#3b7dd8")); g.setLineWidth(1.4);
        g.beginPath();
        for (int i = 0; i <= n; i++) {
            double x = mL + (i / (double) Math.max(p.dias, 1)) * plotW;
            double y = mT + plotH - (tr.cierre[i] / (double) maxInv) * plotH;
            if (i == 0) g.moveTo(x, y); else g.lineTo(x, y);
        }
        g.stroke();
        g.setFill(Color.web("#555")); g.setFont(Font.font("Arial", 8));
        g.fillText("0", mL - 10, mT + plotH + 3);
        g.fillText(String.valueOf(maxInv), mL - 24, mT + 6);
        g.fillText(String.valueOf(p.dias), mL + plotW - 18, mT + plotH + 12);
    }

    private void dibujarGrafOptimizacion() {
        if (matrizCostos == null) { dibujarGrafVacio(); return; }
        GraphicsContext g = canvasGraf.getGraphicsContext2D();
        double w = Math.max(canvasGraf.getWidth(), 700);
        double h = Math.max(canvasGraf.getHeight(), 160);
        g.setFill(Color.WHITE); g.fillRect(0, 0, w, h);
        g.setFill(Color.web("#333"));
        g.setFont(Font.font("Arial", FontWeight.BOLD, 11));
        g.fillText("Optimización 2D – Costo anual (Q, R)  |  ● = mejor", 15, 14);
        int nQ = Q_GRID.length, nR = R_GRID.length;
        double mL = 50, mT = 28, mR = 50, mB = 35;
        double cellW = (w - mL - mR) / nQ, cellH = (h - mT - mB) / nR;
        double cMin = Double.MAX_VALUE, cMax = Double.MIN_VALUE;
        for (int j = 0; j < nR; j++)
            for (int i = 0; i < nQ; i++) {
                cMin = Math.min(cMin, matrizCostos[j][i]);
                cMax = Math.max(cMax, matrizCostos[j][i]);
            }
        int bestQi = 0, bestRi = 0;
        if (mejorKey != null) {
            String[] parts = mejorKey.split(",");
            int bQ = Integer.parseInt(parts[0]), bR = Integer.parseInt(parts[1]);
            for (int i = 0; i < nQ; i++) if (Q_GRID[i] == bQ) bestQi = i;
            for (int j = 0; j < nR; j++) if (R_GRID[j] == bR) bestRi = j;
        }
        for (int j = 0; j < nR; j++) {
            for (int i = 0; i < nQ; i++) {
                double t = (cMax > cMin) ? (matrizCostos[j][i] - cMin) / (cMax - cMin) : 0.5;
                Color col = Color.color(t, 1 - t * 0.7, 0.15);
                double x = mL + i * cellW, y = mT + (nR - 1 - j) * cellH;
                g.setFill(col); g.fillRect(x, y, cellW - 2, cellH - 2);
                g.setFill(Color.web("#222")); g.setFont(Font.font("Arial", 8));
                g.fillText(String.format("%.0f", matrizCostos[j][i]), x + 6, y + cellH / 2 + 3);
            }
        }
        double bx = mL + bestQi * cellW + cellW / 2;
        double by = mT + (nR - 1 - bestRi) * cellH + cellH / 2;
        g.setFill(Color.GOLD); g.setStroke(Color.BLACK); g.setLineWidth(2);
        g.fillOval(bx - 8, by - 8, 16, 16); g.strokeOval(bx - 8, by - 8, 16, 16);
        g.setFill(Color.web("#333")); g.setFont(Font.font("Arial", 9));
        for (int i = 0; i < nQ; i++)
            g.fillText("Q=" + Q_GRID[i], mL + i * cellW + 6, mT + nR * cellH + 12);
        for (int j = 0; j < nR; j++)
            g.fillText("R=" + R_GRID[j], 6, mT + (nR - 1 - j) * cellH + cellH / 2 + 3);
    }

    private void dibujarGrafChi() {
        if (chiObsDem == null) { dibujarGrafVacio(); return; }
        GraphicsContext g = canvasGraf.getGraphicsContext2D();
        double w = Math.max(canvasGraf.getWidth(), 700);
        double h = Math.max(canvasGraf.getHeight(), 160);
        g.setFill(Color.WHITE); g.fillRect(0, 0, w, h);
        g.setFill(Color.web("#333"));
        g.setFont(Font.font("Arial", FontWeight.BOLD, 11));
        g.fillText("Validación χ² – Observada vs Esperada", 15, 14);
        dibujarBarrasChi(g, 25, 28, w / 2 - 40, h - 45, DEM_VAL, chiObsDem, chiEspDem, "Demanda");
        dibujarBarrasChi(g, w / 2 + 15, 28, w / 2 - 40, h - 45, LT_VAL, chiObsLt, chiEspLt, "Tiempo entrega");
    }

    private void dibujarBarrasChi(GraphicsContext g, double x0, double y0, double pw, double ph,
                                  int[] vals, double[] obs, double[] esp, String titulo) {
        g.setFill(Color.web("#333"));
        g.setFont(Font.font("Arial", FontWeight.BOLD, 10));
        g.fillText(titulo, x0, y0 - 2);
        double maxV = 1;
        for (int i = 0; i < vals.length; i++) {
            maxV = Math.max(maxV, obs[i]);
            maxV = Math.max(maxV, esp[i]);
        }
        double groupW = pw / vals.length, barW = groupW * 0.35;
        for (int i = 0; i < vals.length; i++) {
            double x = x0 + i * groupW + groupW * 0.15;
            double hObs = (obs[i] / maxV) * (ph - 20);
            double hEsp = (esp[i] / maxV) * (ph - 20);
            g.setFill(Color.web("#e08a1e")); g.fillRect(x, y0 + ph - 16 - hObs, barW, hObs);
            g.setFill(Color.web("#7fb2ea")); g.fillRect(x + barW + 2, y0 + ph - 16 - hEsp, barW, hEsp);
            g.setFill(Color.web("#333")); g.setFont(Font.font("Arial", 8));
            g.fillText(String.valueOf(vals[i]), x + barW / 2, y0 + ph - 2);
        }
        g.setFill(Color.web("#e08a1e")); g.fillRect(x0 + pw - 100, y0, 10, 10);
        g.setFill(Color.web("#333")); g.setFont(Font.font("Arial", 8));
        g.fillText("Observada", x0 + pw - 87, y0 + 8);
        g.setFill(Color.web("#7fb2ea")); g.fillRect(x0 + pw - 100, y0 + 13, 10, 10);
        g.setFill(Color.web("#333")); g.fillText("Esperada", x0 + pw - 87, y0 + 21);
    }

    private void optimizar() {
        Params pp = leer();
        if (pp == null) return;
        List<int[][]> datos = new ArrayList<>();
        for (int i = 0; i < pp.reps; i++) datos.add(generar(pp.seed, i, pp.dias));
        matrizCostos = new double[R_GRID.length][Q_GRID.length];
        Map<String, double[]> res = new LinkedHashMap<>();
        mejorKey = null;
        double mejorCosto = Double.MAX_VALUE;
        for (int qi = 0; qi < Q_GRID.length; qi++) {
            for (int ri = 0; ri < R_GRID.length; ri++) {
                int Q = Q_GRID[qi], R = R_GRID[ri];
                double sumC = 0, sumS = 0;
                for (int[][] dl : datos) {
                    SimResult r = simular(Q, R, dl[0], dl[1], pp.inv0);
                    sumC += r.costo; sumS += r.servicio;
                }
                double mc = sumC / pp.reps, ms = sumS / pp.reps;
                matrizCostos[ri][qi] = mc;
                String key = Q + "," + R;
                res.put(key, new double[]{mc, ms});
                if (mc < mejorCosto) { mejorCosto = mc; mejorKey = key; }
            }
        }
        List<String> orden = new ArrayList<>(res.keySet());
        orden.sort(Comparator.comparingDouble(k -> res.get(k)[0]));
        StringBuilder sb = new StringBuilder("══ OPTIMIZACIÓN (Q, R) 2D ══\n");
        sb.append(String.format("Semilla %d | %d réplicas%n%n", pp.seed, pp.reps));
        sb.append(String.format("%-4s %-4s %12s %10s%n", "Q", "R", "Costo $", "Servicio%"));
        for (int i = 0; i < Math.min(10, orden.size()); i++) {
            String k = orden.get(i);
            String[] parts = k.split(",");
            double[] v = res.get(k);
            sb.append(String.format("%-4s %-4s %12.2f %10.2f%s%n",
                    parts[0], parts[1], v[0], v[1], k.equals(mejorKey) ? " ◄ mejor" : ""));
        }
        String[] best = mejorKey.split(",");
        tfQ2.setText(best[0]); tfR2.setText(best[1]);
        sb.append(String.format("%nQ₂=%s, R₂=%s cargados%n", best[0], best[1]));
        resultados.setText(sb.toString());
        modoGraf = "opt";
        redibujarGraf();
    }

    private void validar() {
        Params pp = leer();
        if (pp == null) return;
        List<int[][]> datos = new ArrayList<>();
        for (int i = 0; i < pp.reps; i++) datos.add(generar(pp.seed, i, pp.dias));
        double[] bc = new double[pp.reps], ac = new double[pp.reps];
        for (int i = 0; i < pp.reps; i++) {
            bc[i] = simular(pp.q, pp.r, datos.get(i)[0], datos.get(i)[1], pp.inv0).costo;
            ac[i] = simular(pp.q2, pp.r2, datos.get(i)[0], datos.get(i)[1], pp.inv0).costo;
        }
        double[] dif = new double[pp.reps];
        for (int i = 0; i < pp.reps; i++) dif[i] = bc[i] - ac[i];
        double dm = mean(dif), ds = sd(dif), se = ds / Math.sqrt(pp.reps);
        double tStat = (ds > 0) ? dm / se : 0;
        double tP = 2 * (1 - normalCdf(Math.abs(tStat)));
        int pos = 0, neg = 0;
        for (double d : dif) { if (d > 0) pos++; else if (d < 0) neg++; }
        double wP = (pos + neg > 0) ? 2 * (1 - normalCdf(Math.abs(pos - neg) / Math.sqrt(pos + neg))) : 1.0;
        List<Integer> demAll = new ArrayList<>(), ltAll = new ArrayList<>();
        for (int[][] dl : datos) {
            for (int v : dl[0]) demAll.add(v);
            for (int v : dl[1]) ltAll.add(v);
        }
        chiObsDem = new double[DEM_VAL.length]; chiEspDem = new double[DEM_VAL.length];
        for (int i = 0; i < DEM_VAL.length; i++) {
            final int v = DEM_VAL[i];
            chiObsDem[i] = demAll.stream().filter(x -> x == v).count();
            chiEspDem[i] = demAll.size() * DEM_P[i];
        }
        chiObsLt = new double[LT_VAL.length]; chiEspLt = new double[LT_VAL.length];
        for (int i = 0; i < LT_VAL.length; i++) {
            final int v = LT_VAL[i];
            chiObsLt[i] = ltAll.stream().filter(x -> x == v).count();
            chiEspLt[i] = ltAll.size() * LT_P[i];
        }
        StringBuilder sb = new StringBuilder("══ VALIDACIÓN ESTADÍSTICA ══\n");
        sb.append(String.format("Semilla %d | %d réplicas | alfa=%.2f%n%n", pp.seed, pp.reps, ALFA));
        sb.append("A) COMPARACIÓN DE POLÍTICAS\n");
        sb.append(String.format("  Base  (Q=%d, R=%d): $%.2f%n", pp.q, pp.r, mean(bc)));
        sb.append(String.format("  Alter.(Q=%d, R=%d): $%.2f%n", pp.q2, pp.r2, mean(ac)));
        sb.append(String.format("  Diferencia media: $%.2f%n", dm));
        sb.append(String.format("  t pareada: t=%.4f  p≈%.6f%n", tStat, tP));
        sb.append(String.format("  IC 95%%: [$%.2f, $%.2f]%n", dm - 1.96 * se, dm + 1.96 * se));
        sb.append(String.format("  Wilcoxon: p≈%.6f%n%n", wP));
        sb.append("B) CHI CUADRADA\n");
        sb.append("  Demanda: ").append(chiTexto(demAll, DEM_VAL, DEM_P)).append("\n");
        sb.append("  Tiempo entrega: ").append(chiTexto(ltAll, LT_VAL, LT_P)).append("\n");
        resultados.setText(sb.toString());
        modoGraf = "chi";
        redibujarGraf();
    }

    private String chiTexto(List<Integer> datos, int[] valores, double[] probs) {
        int n = datos.size();
        double chi = 0;
        int gl = valores.length - 1;
        for (int i = 0; i < valores.length; i++) {
            final int v = valores[i];
            double obs = datos.stream().filter(x -> x == v).count();
            double esp = n * probs[i];
            if (esp > 0) chi += (obs - esp) * (obs - esp) / esp;
        }
        double crit = (gl == 2) ? 5.991 : (gl == 4) ? 9.488 : 7.815;
        return String.format("chi²=%.3f  crítica=%.3f → %s", chi, crit, chi <= crit ? "NO se rechaza H0" : "SE RECHAZA H0");
    }

    private static double mean(double[] a) {
        double s = 0; for (double v : a) s += v; return s / a.length;
    }
    private static double sd(double[] a) {
        double m = mean(a), s = 0; for (double v : a) s += (v - m) * (v - m);
        return Math.sqrt(s / (a.length - 1));
    }
    private static double normalCdf(double x) { return 0.5 * (1 + erf(x / Math.sqrt(2))); }
    private static double erf(double x) {
        double t = 1.0 / (1.0 + 0.3275911 * Math.abs(x));
        double[] a = {0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429};
        double y = 1 - (((((a[4] * t + a[3]) * t) + a[2]) * t + a[1]) * t + a[0]) * t * Math.exp(-x * x);
        return x >= 0 ? y : -y;
    }

    public static void main(String[] args) { launch(args); }
}