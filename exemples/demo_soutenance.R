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

# --- 2. Etude de cas : batiment KIUT de Tachkent (donnees mesurees) ----
bat <- kiut_building("Y")
bat$f0       # frequence propre mesuree (Hz)
bat$xi       # amortissement deduit de l'amplification mesuree

kiut_scenarios()   # amplitude limite du modele lineaire selon k3/k

# --- 3. Simulation : modele lineaire contre non lineaire ---------------
kiut_compare(ratio = 10)
kiut_compare(ratio = 100) + ggplot2::coord_cartesian(xlim = c(50, 60))

# --- 4. Animation : le polynome s'ameliore avec l'ordre ----------------
animate_taylor("sin(x)", n_max = 8, wait_key = TRUE)
