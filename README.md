# taylorosc

**Formules de Taylor appliquées aux oscillations des bâtiments** : un package R.

Projet du cours *Introduction à R*, École Nationale Supérieure Polytechnique de Douala (ENSPD), niveau 3, filière SDIA (Science des Données et Intelligence Artificielle).

**Auteur :** KUEKAM ZEUFACK JEAMS NAEL

---

## Objectif

Un bâtiment soumis au vent ou à un séisme oscille. Sa force de rappel (la force qui le ramène vers sa position d'équilibre) est une courbe compliquée. Les formules de Taylor permettent de la remplacer par un polynôme simple, de **mesurer l'erreur commise**, et de **simuler** le mouvement.

Le package `taylorosc` met ces outils à la disposition d'un ingénieur :

1. construire des polynômes de Taylor et borner leur erreur ;
2. modéliser la force de rappel d'un bâtiment ;
3. calculer sa fréquence propre ;
4. savoir jusqu'à quelle amplitude le modèle linéaire reste valable ;
5. simuler son déplacement et comparer les modèles linéaire et non linéaire.

## Les idées mathématiques

**Formule de Taylor d'ordre $n$ en $a$ :**

$$
f(x) = \sum_{k=0}^{n} \frac{f^{(k)}(a)}{k!}\,(x-a)^k + R_n(x)
$$

**Reste de Lagrange (borne de l'erreur) :**

$$
|R_n(x)| \le \frac{M}{(n+1)!}\,|x-a|^{n+1}
\qquad\text{avec}\qquad
M = \max \left|f^{(n+1)}\right|
$$

**Force de rappel d'un bâtiment (Taylor d'ordre 3, fonction impaire) :**

$$
g(x) \approx k\,x + k_3\,x^3
$$

**Équation du mouvement :**

$$
m\,\ddot x + c\,\dot x + g(x) = F(t)
$$

Elle est résolue pas à pas par la méthode des différences centrées, qui provient elle aussi des développements de Taylor.

## Fonctions du package

| Fonction | Rôle |
|---|---|
| `taylor_coef()` | coefficients du polynôme de Taylor |
| `taylor_poly()` | valeur du polynôme de Taylor |
| `taylor_error()` | erreur réelle et majorant de Lagrange |
| `restoring_force()` | force de rappel $kx + k_3x^3$ |
| `restoring_force_taylor()` | extrait $k$ et $k_3$ d'une force de rappel quelconque |
| `natural_frequency()` | pulsation, fréquence et période propres |
| `linear_limit()` | amplitude limite du modèle linéaire |
| `simulate_oscillator()` | simulation du déplacement du bâtiment |
| `plot_taylor()` | courbes de Taylor de plusieurs ordres |
| `plot_taylor_error()` | erreur contre majorant de Lagrange |
| `compare_models()` | modèle linéaire contre non linéaire |
| `animate_taylor()` | animation : le polynôme s'améliore avec l'ordre |

## Installation

```r
install.packages("devtools")
devtools::install_github("blacmis/taylorosc")
```

## Exemple rapide

```r
library(taylorosc)

# Polynôme de Taylor de sin(x) en 0, ordre 5
taylor_poly("sin(x)", x = 0.5, a = 0, n = 5)

# Erreur réelle et majorant de Lagrange
taylor_error("sin(x)", x = c(0.1, 0.5, 1), a = 0, n = 3)

# Fréquence propre d'un bâtiment (k en N/m, m en kg)
natural_frequency(k = 8e7, m = 2e6)

# Amplitude limite du modèle linéaire (tolérance de 5 %)
linear_limit(k = 8e7, k3 = -1.6e8, tol = 0.05)

# Simulation sous une force sinusoïdale à la résonance
res <- simulate_oscillator(m = 2e6, k = 8e7, xi = 0.02, F0 = 4e5,
                           Omega = sqrt(8e7 / 2e6), t_end = 100)

# Graphiques
plot_taylor("sin(x)")
compare_models(m = 2e6, k = 8e7, k3 = -1.6e8, F0 = 4e5,
               Omega = sqrt(8e7 / 2e6), t_end = 60)
```

## Tests

Le package contient des tests automatiques (`testthat`), lancés avec :

```r
devtools::test()
```

Ils comparent les résultats des fonctions à des valeurs connues à la main : par exemple l'amplitude à la résonance $x_{\max} = F_0 / (2\,\xi\,k)$.

## Limites

- Le bâtiment est modélisé par **un seul degré de liberté** (une masse sur un ressort non linéaire).
- Le majorant de Lagrange est estimé numériquement sur une grille de points.
- Les valeurs des exemples ci-dessus sont **illustratives**.

## Licence

MIT.
