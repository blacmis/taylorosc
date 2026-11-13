# taylorosc

Formules de Taylor appliquées aux oscillations des bâtiments et à l'analyse vibratoire : un package R.

Projet du cours *Introduction à R*, École Nationale Supérieure Polytechnique de Douala (ENSPD), niveau 3, filière SDIA (Science des Données et Intelligence Artificielle).

Auteur : KUEKAM ZEUFACK JEAMS NAEL

---

## Objectif du projet

Un bâtiment soumis au vent ou à un séisme oscille. Sa force de rappel non linéaire peut être approchée par un polynôme de Taylor (développement d'ordre 3, fonction impaire : $g(x) \approx k\,x + k_3\,x^3$).

Le package taylorosc fournit une boîte à outils complète pour l'ingénieur structure :
1. Mathématiques : Construction de polynômes de Taylor, calcul des coefficients et majoration rigoureuse de l'erreur (reste de Lagrange).
2. Dynamique des structures : Simulation pas à pas du mouvement par la méthode des différences centrées, calcul de la fréquence propre et de l'amplitude limite garantissant la validité du modèle linéaire.
3. Identification expérimentale : Extraction automatique de la fréquence propre $f_0$ (par analyse spectrale et lissage du périodogramme) et du taux d'amortissement $\xi$ (par régression sur la décroissance des amplitudes en vibration libre).
4. Études de cas réels : Exploitation de mesures expérimentales réelles issues de deux bâtiments :
   - Bâtiment de l'UBC (Vancouver, Canada) : Immeuble hybride bois-béton de 18 étages (53 m), à partir de données de capteurs réels (microtremblements et vibrations libres).
   - Bâtiment de Tachkent (Ouzbekistan) : Immeuble en béton armé de 6 étages.

---

## Les bases mathématiques

Formule de Taylor d'ordre $n$ en $a$ :

$$f(x) = \sum_{k=0}^{n} \frac{f^{(k)}(a)}{k!}\,(x-a)^k + R_n(x)$$

Reste de Lagrange (borne de l'erreur) :

$$\vert{}R_n(x)\vert{} \le \frac{M}{(n+1)!}\,\vert{}x-a\vert{}^{n+1} \qquad\text{avec}\qquad M = \max \left\vert{}f^{(n+1)}\right\vert{}$$

Équation du mouvement (système à un degré de liberté) :

$$m\,\ddot x + c\,\dot x + g(x) = F(t)$$

---

## Fonctions principales du package

| Fonction | Rôle |
|---|---|
| taylor_coef() | Coefficients du polynôme de Taylor |
| taylor_poly() | Évaluation du polynôme de Taylor |
| taylor_error() | Erreur réelle et majorant de Lagrange |
| restoring_force() | Force de rappel non linéaire $kx + k_3x^3$ |
| restoring_force_taylor() | Extrait $k$ et $k_3$ d'une force de rappel quelconque |
| natural_frequency() | Pulsation, fréquence et période propres |
| linear_limit() | Amplitude limite du modèle linéaire |
| estimate_frequency() | Estimation de la fréquence propre par spectre (périodogramme) |
| estimate_damping() | Estimation du taux d'amortissement $\xi$ par vibration libre |
| ubc_signal() | Chargement des signaux bruts de l'UBC (microtremblements et vibration libre) |
| ubc_building() | Paramètres et identification complète sur le bâtiment UBC |
| kiut_building() | Paramètres et analyse du bâtiment KIUT de Tachkent |
| case_scenarios() / kiut_scenarios() | Tableaux de validité du modèle linéaire selon $k_3/k$ |
| case_compare() / kiut_compare() | Simulation comparative linéaire vs non linéaire sur un bâtiment |
| simulate_oscillator() | Simulation temporelle (différences centrées) |
| plot_taylor() / plot_taylor_error() | Graphiques des polynômes de Taylor et de l'erreur de Lagrange |
| plot_spectrum() / plot_free_decay() | Visualisation du spectre et de la décroissance libre |
| compare_models() / animate_taylor() | Comparaison graphique des modèles et animation pas à pas |

---

## Installation

install.packages("devtools")
devtools::install_github("blacmis/taylorosc")
---

## Exemple d'utilisation avec les données réelles (UBC)

`r
library(taylorosc)

# 1. Charger et analyser les données réelles du bâtiment UBC
bat <- ubc_building()
bat$f0  # Fréquence propre mesurée (Hz)
bat$xi  # Taux d'amortissement estimé

# 2. Analyser les scénarios de validité du modèle linéaire
case_scenarios(bat)

# 3. Tracer le spectre et identifier le pic de fréquence
est <- estimate_frequency(ubc_signal("1_Y")$v, fs = 200, fmin = 0.3, fmax = 5)
plot_spectrum(est)

# 4. Tracer la décroissance libre et l'amortissement
libre <- ubc_signal("3_Y_free")
amort <- estimate_damping(libre$t, libre$v, f0 = bat$f0, t_start = 20, t_end = 45, min_amplitude = 0.012)
plot_free_decay(libre$t, libre$v, amort)

---

## Tests unitaires

Le package intègre une batterie complète de 60 tests automatiques (`testthat`) validant l'ensemble des calculs mathématiques, numériques et physiques :

r
devtools::test()
`

---

## Limites du modèle

1. Modèle à un seul degré de liberté (SDOF) : Le bâtiment est réduit à une masse unique équivalente sur un ressort (mode fondamental dominant). Les modes supérieurs de vibration et les déplacements relatifs inter-étages ne sont pas modélisés.
2. Non-linéarité restreinte : La force de rappel est limitée au développement de Taylor d'ordre 3 ($k\,x + k_3\,x^3$), ce qui suppose un comportement symétrique (identique dans les deux sens d'oscillation).
3. Estimation numérique du majorant : Le majorant de l'erreur de Lagrange est évalué numériquement sur une grille discrète de 1000 points, ce qui dépend de la finesse de cette grille.
4. Sensibilité au bruit expérimental : L'identification du taux d'amortissement $\xi$ par détection des pics en vibration libre dépend du choix de la fenêtre temporelle (`t_start`, `t_end`), du seuil de bruit (`min_amplitude`) et du lissage appliqué.

---

## Licence

MIT.
