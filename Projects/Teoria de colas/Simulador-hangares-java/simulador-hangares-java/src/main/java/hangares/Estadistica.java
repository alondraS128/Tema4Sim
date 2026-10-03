package hangares;

import java.util.Arrays;

/**
 * Funciones estadísticas propias (sin librerías externas):
 * Chi-cuadrada, t de Student y Kolmogorov-Smirnov.
 */
public final class Estadistica {
    private Estadistica() { }

    private static final double FPMIN = 1e-300;
    private static final double EPS = 1e-14;
    private static final double[] COF = {
            76.18009172947146, -86.50532032941677, 24.01409824083091,
            -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5};

    /** ln(Gamma(x)) */
    public static double gammln(double xx) {
        double y = xx;
        double tmp = xx + 5.5;
        tmp -= (xx + 0.5) * Math.log(tmp);
        double ser = 1.000000000190015;
        for (double c : COF) {
            y += 1;
            ser += c / y;
        }
        return -tmp + Math.log(2.5066282746310005 * ser / xx);
    }

    private static double gser(double a, double x) {
        double ap = a, sum = 1.0 / a, del = sum;
        for (int n = 0; n < 2000; n++) {
            ap += 1;
            del *= x / ap;
            sum += del;
            if (Math.abs(del) < Math.abs(sum) * EPS) break;
        }
        return sum * Math.exp(-x + a * Math.log(x) - gammln(a));
    }

    private static double gcf(double a, double x) {
        double b = x + 1.0 - a;
        double c = 1.0 / FPMIN;
        double d = 1.0 / b;
        double h = d;
        for (int i = 1; i <= 2000; i++) {
            double an = -i * (i - a);
            b += 2.0;
            d = an * d + b;
            if (Math.abs(d) < FPMIN) d = FPMIN;
            c = b + an / c;
            if (Math.abs(c) < FPMIN) c = FPMIN;
            d = 1.0 / d;
            double del = d * c;
            h *= del;
            if (Math.abs(del - 1.0) < EPS) break;
        }
        return Math.exp(-x + a * Math.log(x) - gammln(a)) * h;
    }

    /** P(X <= x) para Chi-cuadrada con k grados de libertad. */
    public static double chi2Cdf(double x, double k) {
        if (x <= 0) return 0.0;
        double a = k / 2.0, xx = x / 2.0;
        return xx < a + 1.0 ? gser(a, xx) : 1.0 - gcf(a, xx);
    }

    /** P(X > x) para Chi-cuadrada (valor p). */
    public static double chi2Sf(double x, double k) {
        if (x <= 0) return 1.0;
        double a = k / 2.0, xx = x / 2.0;
        return xx < a + 1.0 ? 1.0 - gser(a, xx) : gcf(a, xx);
    }

    /** Cuantil de la Chi-cuadrada (valor crítico) por bisección. */
    public static double chi2Inv(double p, double k) {
        double lo = 0, hi = k + 12 * Math.sqrt(2 * k) + 60;
        for (int i = 0; i < 200; i++) {
            double mid = 0.5 * (lo + hi);
            if (chi2Cdf(mid, k) < p) lo = mid; else hi = mid;
        }
        return 0.5 * (lo + hi);
    }

    private static double betacf(double a, double b, double x) {
        double qab = a + b, qap = a + 1.0, qam = a - 1.0;
        double c = 1.0, d = 1.0 - qab * x / qap;
        if (Math.abs(d) < FPMIN) d = FPMIN;
        d = 1.0 / d;
        double h = d;
        for (int m = 1; m <= 500; m++) {
            int m2 = 2 * m;
            double aa = m * (b - m) * x / ((qam + m2) * (a + m2));
            d = 1.0 + aa * d;
            if (Math.abs(d) < FPMIN) d = FPMIN;
            c = 1.0 + aa / c;
            if (Math.abs(c) < FPMIN) c = FPMIN;
            d = 1.0 / d;
            h *= d * c;
            aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
            d = 1.0 + aa * d;
            if (Math.abs(d) < FPMIN) d = FPMIN;
            c = 1.0 + aa / c;
            if (Math.abs(c) < FPMIN) c = FPMIN;
            d = 1.0 / d;
            double del = d * c;
            h *= del;
            if (Math.abs(del - 1.0) < EPS) break;
        }
        return h;
    }

    /** Función beta incompleta regularizada I_x(a, b). */
    private static double betai(double a, double b, double x) {
        if (x <= 0) return 0.0;
        if (x >= 1) return 1.0;
        double bt = Math.exp(gammln(a + b) - gammln(a) - gammln(b)
                + a * Math.log(x) + b * Math.log(1.0 - x));
        if (x < (a + 1.0) / (a + b + 2.0)) return bt * betacf(a, b, x) / a;
        return 1.0 - bt * betacf(b, a, 1.0 - x) / b;
    }

    /** P(T <= t) para t de Student con df grados de libertad. */
    public static double tCdf(double t, double df) {
        double ib = betai(df / 2.0, 0.5, df / (df + t * t));
        return t >= 0 ? 1.0 - 0.5 * ib : 0.5 * ib;
    }

    /** Valor p de dos colas para un estadístico t. */
    public static double tPValorDosColas(double t, double df) {
        return betai(df / 2.0, 0.5, df / (df + t * t));
    }

    /** Cuantil de la t de Student por bisección. */
    public static double tInv(double p, double df) {
        double lo = -1000, hi = 1000;
        for (int i = 0; i < 200; i++) {
            double mid = 0.5 * (lo + hi);
            if (tCdf(mid, df) < p) lo = mid; else hi = mid;
        }
        return 0.5 * (lo + hi);
    }

    /** Estadístico D de Kolmogorov-Smirnov contra la exponencial de tasa dada. */
    public static double ksD(double[] datos, double tasa) {
        double[] x = datos.clone();
        Arrays.sort(x);
        int n = x.length;
        double d = 0;
        for (int i = 0; i < n; i++) {
            double f = 1.0 - Math.exp(-tasa * x[i]);
            d = Math.max(d, Math.max((i + 1.0) / n - f, f - (double) i / n));
        }
        return d;
    }

    /** Valor p asintótico de K-S (corrección de Stephens). */
    public static double ksPValor(double d, int n) {
        double sn = Math.sqrt(n);
        double lam = (sn + 0.12 + 0.11 / sn) * d;
        double a2 = -2.0 * lam * lam;
        double fac = 2.0, sum = 0.0, termbf = 0.0;
        for (int j = 1; j <= 100; j++) {
            double term = fac * Math.exp(a2 * j * j);
            sum += term;
            if (Math.abs(term) <= 1e-3 * termbf || Math.abs(term) <= 1e-8 * sum) {
                return Math.min(1.0, Math.max(0.0, sum));
            }
            fac = -fac;
            termbf = Math.abs(term);
        }
        return 1.0;
    }

    /** Valor crítico aproximado de K-S para alfa = 0.05. */
    public static double ksCritico(int n) {
        return 1.36 / Math.sqrt(n);
    }

    public static double media(double[] x) {
        double s = 0;
        for (double v : x) s += v;
        return s / x.length;
    }

    public static double desviacion(double[] x) {
        double m = media(x), s = 0;
        for (double v : x) s += (v - m) * (v - m);
        return Math.sqrt(s / (x.length - 1));
    }
}
