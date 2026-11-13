# Demonstration du package taylorosc
# Auteur : KUEKAM ZEUFACK JEAMS NAEL
#
# Avant la demo (une seule fois) :
# devtools::install_github("blacmis/taylorosc")
library(taylorosc)
# --- 1. Taylor : approximation et erreur -------------------------------
plot_taylor("sin(x)")
taylor_error("sin(x)", x = c(0.1, 0.5, 1), a = 0, n = 3)
plot_taylor_error("sin(x)", n = 3, h_max = 1)
# --- 2. Etude de cas 1 : batiment KIUT de Tachkent (donnees mesurees) ----
bat_kiut <- kiut_building("Y")
bat_kiut$f0       # frequence propre mesuree (Hz)
bat_kiut$xi       # amortissement deduit de l'amplification mesuree
kiut_scenarios()   # amplitude limite du modele lineaire selon k3/k
# --- 3. Etude de cas 2 : batiment UBC de Vancouver (donnees reelles CSV) ---
bat_ubc <- ubc_building()
bat_ubc$f0        # frequence propre identifiee sur vibration libre (Hz)
bat_ubc$xi        # amortissement identifie par decroissance libre
# Visualisation du spectre des microtremblements et de la vibration libre
est_spec <- estimate_frequency(ubc_signal("1_Y")$v, fs = 200, fmin = 0.3, fmax = 5)
plot_spectrum(est_spec)
libre <- ubc_signal("3_Y_free")
amort <- estimate_damping(libre$t, libre$v, f0 = bat_ubc$f0, t_start = 20, t_end = 45, min_amplitude = 0.012)
plot_free_decay(libre$t, libre$v, amort)
# --- 4. Simulation : modele lineaire contre non lineaire ---------------
kiut_compare(ratio = 10)
kiut_compare(ratio = 100) + ggplot2::coord_cartesian(xlim = c(50, 60))
# --- 5. Animation : le polynome s'ameliore avec l'ordre ----------------
animate_taylor("sin(x)", n_max = 8, wait_key = TRUE)
