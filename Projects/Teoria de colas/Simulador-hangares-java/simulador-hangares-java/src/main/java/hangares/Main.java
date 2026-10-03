package hangares;

import javafx.animation.AnimationTimer;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.geometry.VPos;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.chart.LineChart;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.control.TextArea;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.scene.text.TextAlignment;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import javafx.util.Duration;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Interfaz gráfica (JavaFX) del simulador de hangares M/M/c.
 * Toda la lógica de simulación y estadística está en MotorMMc y Estadistica.
 */
public class Main extends Application {

    private static final double DT_ANIM = 0.1;          // días simulados por cuadro a 1x
    private static final double ANCHO = 760, ALTO = 580;

    private final MotorMMc motor = new MotorMMc(3, MotorMMc.SEMILLA);

    private Stage stagePrincipal;
    private GraphicsContext gc;
    private TextArea txtMonitor;
    private Label lblVel;
    private Label lblNumHangares;
    private Slider sliderHangares;
    private int factorVelocidad = 1;
    private AnimationTimer timer;
    private long ultimoTick = 0;

    // ------------------------------------------------------------------
    // ARRANQUE Y CONSTRUCCIÓN DE LA INTERFAZ
    // ------------------------------------------------------------------
    @Override
    public void start(Stage stage) {
        stagePrincipal = stage;

        // Lienzo visual (izquierda)
        Canvas canvas = new Canvas(ANCHO, ALTO);
        gc = canvas.getGraphicsContext2D();
        Pane panelCanvas = new Pane(canvas);
        panelCanvas.setStyle("-fx-background-color: #E2E8F0; -fx-border-color: #94A3B8; -fx-border-width: 2;");

        // Monitor de métricas (derecha)
        Label titulo = new Label("Monitor");
        titulo.setStyle("-fx-font-size: 15px; -fx-font-weight: bold; -fx-text-fill: #0F172A;");

        txtMonitor = new TextArea();
        txtMonitor.setEditable(false);
        txtMonitor.setWrapText(false);
        txtMonitor.setStyle("-fx-font-family: 'Consolas', 'Monospaced'; -fx-font-size: 12px;");
        VBox.setVgrow(txtMonitor, Priority.ALWAYS);

        Button btnGraficas = boton("Ver Gráficas Real-Time", "#2563EB", this::abrirGraficas);
        Button btnExportar = boton("Exportar Resultados (CSV)", "#059669", this::exportarCsv);
        btnGraficas.setMaxWidth(Double.MAX_VALUE);
        btnExportar.setMaxWidth(Double.MAX_VALUE);

        VBox monitorBox = new VBox(8, titulo, txtMonitor, btnGraficas, btnExportar);
        monitorBox.setPadding(new Insets(10));
        monitorBox.setPrefWidth(350);
        monitorBox.setMinWidth(350);
        monitorBox.setStyle("-fx-background-color: white; -fx-border-color: #CBD5E1;");

        // Barra inferior de controles
        HBox barra = new HBox(8);
        barra.setPadding(new Insets(8, 10, 8, 10));
        barra.setAlignment(Pos.CENTER_LEFT);
        barra.setStyle("-fx-background-color: #1E293B;");

        lblVel = etiqueta("1x");
        lblVel.setMinWidth(34);
        lblVel.setAlignment(Pos.CENTER);
        lblNumHangares = etiqueta("3");

        sliderHangares = new Slider(1, 3, 3);
        sliderHangares.setMajorTickUnit(1);
        sliderHangares.setMinorTickCount(0);
        sliderHangares.setSnapToTicks(true);
        sliderHangares.setPrefWidth(90);
        sliderHangares.valueProperty().addListener(
                (obs, viejo, nuevo) -> cambiarHangares((int) Math.round(nuevo.doubleValue())));

        barra.getChildren().addAll(
                boton("Iniciar", "#10B981", this::iniciarSim),
                boton("Pausar", "#F59E0B", this::pausarSim),
                boton("Reiniciar", "#64748B", this::reiniciarSim),
                boton("Fin Rápido", "#0EA5E9", this::finRapido),
                boton("Validación 4.4 (Chi², t & K-S)", "#D97706", this::abrirValidacion),
                etiqueta("   Vel:"),
                boton("-", "#475569", this::disminuirVelocidad),
                lblVel,
                boton("+", "#475569", this::aumentarVelocidad),
                etiqueta("   Hangares:"),
                lblNumHangares,
                sliderHangares);

        BorderPane root = new BorderPane();
        root.setPadding(new Insets(10));
        root.setStyle("-fx-background-color: #F4F6F9;");
        root.setCenter(panelCanvas);
        root.setRight(monitorBox);
        root.setBottom(barra);
        BorderPane.setMargin(monitorBox, new Insets(0, 0, 0, 10));
        BorderPane.setMargin(barra, new Insets(10, 0, 0, 0));

        // Ciclo de animación: un paso cada 100 ms (a 10x avanza 10 veces más días por paso)
        timer = new AnimationTimer() {
            @Override
            public void handle(long now) {
                if (!motor.corriendo) {
                    stop();
                    return;
                }
                if (now - ultimoTick < 100_000_000L) return;
                ultimoTick = now;
                paso();
            }
        };

        stage.setTitle("SIMULADOR DE HANGARES (M/M/c)");
        stage.setScene(new Scene(root, 1180, 740));
        stage.show();

        dibujarEscenario();
        actualizarMonitor();
    }

    private Button boton(String texto, String colorHex, Runnable accion) {
        Button b = new Button(texto);
        b.setStyle("-fx-background-color: " + colorHex + "; -fx-text-fill: white; -fx-font-weight: bold;"
                + " -fx-cursor: hand; -fx-background-radius: 3;");
        b.setOnAction(e -> accion.run());
        return b;
    }

    private Label etiqueta(String texto) {
        Label l = new Label(texto);
        l.setStyle("-fx-text-fill: white; -fx-font-weight: bold;");
        return l;
    }

    // ------------------------------------------------------------------
    // CONTROLES
    // ------------------------------------------------------------------
    private void iniciarSim() {
        if (motor.corriendo) return;
        if (motor.tiempoSim >= MotorMMc.TIEMPO_MAX) {
            alerta(Alert.AlertType.INFORMATION, "Simulación finalizada",
                    "La simulación ya llegó al horizonte de " + (int) MotorMMc.TIEMPO_MAX
                            + " días.\nPresiona Reiniciar para una nueva corrida.");
            return;
        }
        motor.asegurarArranque();
        motor.corriendo = true;
        ultimoTick = 0;
        timer.start();
    }

    private void pausarSim() {
        motor.corriendo = false;
        timer.stop();
        actualizarMonitor();
    }

    private void reiniciarSim() {
        timer.stop();
        motor.reiniciar();
        dibujarEscenario();
        actualizarMonitor();
    }

    /** Simula (sin animación) hasta el horizonte, con la misma lógica de eventos. */
    private void finRapido() {
        motor.corriendo = false;
        timer.stop();
        motor.asegurarArranque();
        motor.procesarHasta(MotorMMc.TIEMPO_MAX);
        dibujarEscenario();
        actualizarMonitor();
    }

    private void paso() {
        double dt = DT_ANIM * factorVelocidad;
        motor.procesarHasta(Math.min(motor.tiempoSim + dt, MotorMMc.TIEMPO_MAX));
        if (motor.tiempoSim >= MotorMMc.TIEMPO_MAX) {
            motor.corriendo = false;
            timer.stop();
        }
        dibujarEscenario();
        actualizarMonitor();
    }

    private void aumentarVelocidad() {
        if (factorVelocidad < 10) {
            factorVelocidad++;
            lblVel.setText(factorVelocidad + "x");
        }
    }

    private void disminuirVelocidad() {
        if (factorVelocidad > 1) {
            factorVelocidad--;
            lblVel.setText(factorVelocidad + "x");
        }
    }

    /** Cambiar c cambia el sistema: se reinicia la corrida para no mezclar datos. */
    private void cambiarHangares(int nuevo) {
        if (nuevo == motor.numHangares) return;
        timer.stop();
        motor.numHangares = nuevo;
        lblNumHangares.setText(String.valueOf(nuevo));
        motor.reiniciar();
        dibujarEscenario();
        actualizarMonitor();
    }

    private void actualizarMonitor() {
        txtMonitor.setText(motor.textoMonitor());
    }

    private void alerta(Alert.AlertType tipo, String titulo, String mensaje) {
        Alert a = new Alert(tipo);
        a.setTitle(titulo);
        a.setHeaderText(null);
        a.setContentText(mensaje);
        a.showAndWait();
    }

    // ------------------------------------------------------------------
    // DIBUJO (mismas coordenadas que las versiones de Python y R)
    // ------------------------------------------------------------------
    private void rect(double x0, double y0, double x1, double y1, String relleno, String borde, double grosor) {
        if (relleno != null) {
            gc.setFill(Color.web(relleno));
            gc.fillRect(x0, y0, x1 - x0, y1 - y0);
        }
        if (borde != null) {
            gc.setStroke(Color.web(borde));
            gc.setLineWidth(grosor);
            gc.strokeRect(x0, y0, x1 - x0, y1 - y0);
        }
    }

    private void linea(double x0, double y0, double x1, double y1, String color, double grosor) {
        gc.setStroke(Color.web(color));
        gc.setLineWidth(grosor);
        gc.strokeLine(x0, y0, x1, y1);
    }

    private void poligono(double[] xs, double[] ys, String relleno, String borde, double grosor) {
        if (relleno != null) {
            gc.setFill(Color.web(relleno));
            gc.fillPolygon(xs, ys, xs.length);
        }
        if (borde != null) {
            gc.setStroke(Color.web(borde));
            gc.setLineWidth(grosor);
            gc.strokePolygon(xs, ys, xs.length);
        }
    }

    private void ovalo(double x0, double y0, double x1, double y1, String relleno, String borde) {
        gc.setFill(Color.web(relleno));
        gc.fillOval(x0, y0, x1 - x0, y1 - y0);
        gc.setStroke(Color.web(borde));
        gc.setLineWidth(1);
        gc.strokeOval(x0, y0, x1 - x0, y1 - y0);
    }

    private void texto(double x, double y, String s, String color, double tamano, boolean negrita) {
        gc.setTextAlign(TextAlignment.CENTER);
        gc.setTextBaseline(VPos.CENTER);
        gc.setFont(Font.font("Segoe UI", negrita ? FontWeight.BOLD : FontWeight.NORMAL, tamano));
        gc.setFill(Color.web(color));
        gc.fillText(s, x, y);
    }

    private void dibujarEscenario() {
        // Fondo: terreno del aeródromo
        rect(0, 0, ANCHO, ALTO, "#D9E2EC", null, 0);
        // Plataforma de concreto frente a los hangares
        rect(225, 40, 420, 420, "#C3CCD6", "#9AA5B1", 1);

        // Pista / área de cola de entrada
        rect(30, 200, 210, 270, "#CBD5E1", "#94A3B8", 2);
        texto(120, 235, "Cola de Entrada", "#475569", 13, true);
        // Calle de rodaje (línea punteada) hacia los hangares
        gc.setLineDashes(8, 6);
        linea(210, 235, 270, 235, "#F8FAFC", 3);
        gc.setLineDashes((double[]) null);

        // Hangares (y avión dentro si está ocupado)
        for (int i = 0; i < motor.numHangares; i++) {
            double y = 70 + i * 110;
            int idAvion = motor.hangaresOcupados[i];
            boolean ocupado = idAvion > 0;
            dibujarHangar(i, y, ocupado);
            if (ocupado) dibujarAvion(300, y + 40, idAvion, "#EF4444");
        }

        // Aviones en la cola visual (los primeros 6)
        int idx = 0;
        for (Integer id : motor.cola) {
            if (idx >= 6) break;
            dibujarAvion(180 - idx * 25, 225, id, "#F59E0B");
            idx++;
        }
        if (motor.cola.size() > 6) {
            texto(120, 285, "+" + (motor.cola.size() - 6) + " más en cola", "#475569", 12, true);
        }

        texto(320, 530, String.format("Tiempo = %.1f / %.0f días", motor.tiempoSim, MotorMMc.TIEMPO_MAX),
                "#0F172A", 16, true);
    }

    /** Hangar realista: techo curvo de lámina, paredes, puerta y luz de estado. */
    private void dibujarHangar(int i, double y, boolean ocupado) {
        double cx = 320, rx = 58, ry = 40;
        double x0 = cx - rx, x1 = cx + rx;
        double yBase = y + 76;              // piso del hangar
        double yArco = y + 30;              // donde inicia la curva del techo

        // Sombra en el suelo
        rect(x0 + 6, yBase, x1 + 8, yBase + 6, "#94A3B8", null, 0);

        // Paredes
        rect(x0, yArco, x1, yBase, "#94A3B8", "#334155", 2);

        // Techo abovedado (semi-elipse)
        double[] px = new double[19];
        double[] py = new double[19];
        for (int k = 0; k < 19; k++) {
            double t = Math.toRadians(k * 10);
            px[k] = cx + rx * Math.cos(t);
            py[k] = yArco - ry * Math.sin(t);
        }
        poligono(px, py, "#64748B", "#334155", 2);

        // Nervaduras de la lámina del techo (líneas verticales)
        for (int ang = 20; ang < 180; ang += 20) {
            double t = Math.toRadians(ang);
            double xr = cx + rx * Math.cos(t);
            double yr = yArco - ry * Math.sin(t);
            linea(xr, yr, xr, yArco, "#475569", 1);
        }

        // Lámina corrugada en las paredes laterales
        for (double xl = x0 + 4; xl < x0 + 26; xl += 5) linea(xl, yArco + 2, xl, yBase - 2, "#7B8798", 1);
        for (double xl = x1 - 24; xl < x1 - 2; xl += 5) linea(xl, yArco + 2, xl, yBase - 2, "#7B8798", 1);

        // Cartel con el nombre del hangar
        rect(cx - 30, y + 6, cx + 30, y + 22, "#0F172A", "#F8FAFC", 1);
        texto(cx, y + 14, "Hangar " + (i + 1), "#FFFFFF", 11, true);

        // Marco de la puerta
        double dx0 = cx - 36, dx1 = cx + 36, dy0 = y + 34;
        rect(dx0 - 3, dy0 - 3, dx1 + 3, yBase, "#334155", "#1E293B", 1);

        if (ocupado) {
            // Puerta abierta: se ve el interior oscuro y las hojas plegadas a los lados
            rect(dx0, dy0, dx1, yBase, "#1E293B", null, 0);
            rect(dx0, dy0, dx0 + 6, yBase, "#64748B", "#334155", 1);
            rect(dx1 - 6, dy0, dx1, yBase, "#64748B", "#334155", 1);
        } else {
            // Puerta cerrada con paneles horizontales
            rect(dx0, dy0, dx1, yBase, "#CBD5E1", "#475569", 1);
            for (double yy = dy0 + 6; yy < yBase; yy += 6) linea(dx0, yy, dx1, yy, "#94A3B8", 1);
            linea(cx, dy0, cx, yBase, "#64748B", 2);
        }

        // Luz de estado (roja = ocupado, verde = libre)
        String colorLuz = ocupado ? "#EF4444" : "#22C55E";
        rect(x1 + 8, y + 36, x1 + 14, y + 60, "#334155", null, 0);
        ovalo(x1 + 5, y + 34, x1 + 17, y + 46, colorLuz, "#1E293B");

        // Franja amarilla de seguridad frente a la puerta
        for (double xs = dx0; xs < dx1; xs += 12) rect(xs, yBase + 2, xs + 6, yBase + 5, "#FACC15", null, 0);
    }

    private void dibujarAvion(double x, double y, int idAvion, String color) {
        rect(x, y, x + 35, y + 20, color, "#1E293B", 1);
        poligono(new double[]{x + 10, x + 18, x + 22}, new double[]{y, y - 8, y}, color, "#1E293B", 1);
        poligono(new double[]{x + 10, x + 18, x + 22}, new double[]{y + 20, y + 28, y + 20}, color, "#1E293B", 1);
        texto(x + 17, y + 10, "A" + idAvion, "#FFFFFF", 9.5, true);
    }

    // ------------------------------------------------------------------
    // GRÁFICAS EN TIEMPO REAL (datos reales de la corrida, cada 500 ms)
    // ------------------------------------------------------------------
    private void abrirGraficas() {
        NumberAxis x1 = new NumberAxis();
        NumberAxis y1 = new NumberAxis();
        x1.setLabel("Tiempo (días)");
        x1.setForceZeroInRange(false);
        y1.setLabel("Aviones en cola");
        LineChart<Number, Number> ch1 = new LineChart<>(x1, y1);
        ch1.setTitle("Aviones en cola (Lq)");
        ch1.setCreateSymbols(false);
        ch1.setAnimated(false);

        NumberAxis x2 = new NumberAxis();
        NumberAxis y2 = new NumberAxis();
        x2.setLabel("Avión atendido (nº)");
        x2.setForceZeroInRange(false);
        y2.setLabel("Días en espera");
        LineChart<Number, Number> ch2 = new LineChart<>(x2, y2);
        ch2.setTitle("Espera en cola Wq (días)");
        ch2.setCreateSymbols(false);
        ch2.setAnimated(false);

        HBox contenedor = new HBox(ch1, ch2);
        HBox.setHgrow(ch1, Priority.ALWAYS);
        HBox.setHgrow(ch2, Priority.ALWAYS);

        Stage st = new Stage();
        st.setTitle("Gráficas en Tiempo Real");
        st.setScene(new Scene(contenedor, 960, 430));

        Timeline tl = new Timeline(new KeyFrame(Duration.millis(500), e -> refrescarGraficas(ch1, ch2)));
        tl.setCycleCount(Timeline.INDEFINITE);
        tl.play();
        st.setOnHidden(e -> tl.stop());

        refrescarGraficas(ch1, ch2);
        st.show();
    }

    private void refrescarGraficas(LineChart<Number, Number> ch1, LineChart<Number, Number> ch2) {
        MotorMMc.Teoria th = MotorMMc.teoria(MotorMMc.LAMBDA, MotorMMc.MU, motor.numHangares);

        // --- Aviones en cola (se toman máx. ~500 puntos para que la gráfica sea fluida)
        XYChart.Series<Number, Number> sCola = new XYChart.Series<>();
        sCola.setName("Simulado");
        int n = motor.histT.size();
        int paso = Math.max(1, n / 500);
        for (int i = 0; i < n; i += paso) {
            sCola.getData().add(new XYChart.Data<Number, Number>(motor.histT.get(i), motor.histQ.get(i)));
        }
        List<XYChart.Series<Number, Number>> series1 = new ArrayList<>();
        series1.add(sCola);
        if (th != null && n > 1) {
            XYChart.Series<Number, Number> sTeo = new XYChart.Series<>();
            sTeo.setName("Lq teórico");
            sTeo.getData().add(new XYChart.Data<Number, Number>(motor.histT.get(0), th.lq));
            sTeo.getData().add(new XYChart.Data<Number, Number>(motor.histT.get(n - 1), th.lq));
            series1.add(sTeo);
        }
        ch1.getData().setAll(series1);

        // --- Espera Wq por avión y promedio acumulado
        XYChart.Series<Number, Number> sPorAvion = new XYChart.Series<>();
        sPorAvion.setName("Wq por avión");
        XYChart.Series<Number, Number> sProm = new XYChart.Series<>();
        sProm.setName("Wq promedio");
        int m = motor.esperas.size();
        int paso2 = Math.max(1, m / 500);
        double acumulado = 0;
        for (int i = 0; i < m; i++) {
            acumulado += motor.esperas.get(i);
            if (i % paso2 == 0) {
                sPorAvion.getData().add(new XYChart.Data<Number, Number>(i + 1, motor.esperas.get(i)));
                sProm.getData().add(new XYChart.Data<Number, Number>(i + 1, acumulado / (i + 1)));
            }
        }
        List<XYChart.Series<Number, Number>> series2 = new ArrayList<>();
        series2.add(sPorAvion);
        series2.add(sProm);
        if (th != null && m > 1) {
            XYChart.Series<Number, Number> sTeo2 = new XYChart.Series<>();
            sTeo2.setName("Wq teórico");
            sTeo2.getData().add(new XYChart.Data<Number, Number>(1, th.wq));
            sTeo2.getData().add(new XYChart.Data<Number, Number>(m, th.wq));
            series2.add(sTeo2);
        }
        ch2.getData().setAll(series2);
    }

    // ------------------------------------------------------------------
    // VALIDACIÓN ESTADÍSTICA (4.4.1 y 4.4.2)
    // ------------------------------------------------------------------
    private void abrirValidacion() {
        TextArea ta = new TextArea(motor.generarReporteValidacion());
        ta.setEditable(false);
        ta.setStyle("-fx-font-family: 'Consolas', 'Monospaced'; -fx-font-size: 12px;");
        Stage st = new Stage();
        st.setTitle("Validación Estadística ");
        st.setScene(new Scene(new BorderPane(ta), 700, 620));
        st.show();
    }

    // ------------------------------------------------------------------
    // EXPORTACIÓN A CSV
    // ------------------------------------------------------------------
    private void exportarCsv() {
        if (motor.registros.isEmpty()) {
            alerta(Alert.AlertType.WARNING, "Sin datos",
                    "Aún no hay datos para exportar.\nEjecuta la simulación (Iniciar o Fin Rápido) primero.");
            return;
        }
        FileChooser fc = new FileChooser();
        fc.setTitle("Guardar resultados de la simulación");
        fc.setInitialFileName("resultados_simulacion_hangares.csv");
        fc.getExtensionFilters().add(new FileChooser.ExtensionFilter("Archivo CSV", "*.csv"));
        File archivo = fc.showSaveDialog(stagePrincipal);
        if (archivo == null) return;   // el usuario canceló

        try {
            motor.exportarCsv(archivo);
            alerta(Alert.AlertType.INFORMATION, "Éxito",
                    "Se han exportado " + motor.registros.size() + " registros correctamente a:\n"
                            + archivo.getAbsolutePath());
        } catch (IOException ex) {
            alerta(Alert.AlertType.ERROR, "Error",
                    "No se pudo guardar el archivo. Si está abierto en Excel, ciérralo o elige otro nombre.\n"
                            + ex.getMessage());
        }
    }

    public static void main(String[] args) {
        launch(args);
    }
}
