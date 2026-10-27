Package: taylorosc
Title: Formules de Taylor appliquees aux oscillations des batiments
Version: 0.1.0
Authors@R: 
    person("Jeams Naël", "KUEKAM ZEUFACK", , "kuekamzeufack@gmail.com", role = c("aut", "cre"))
Description: Outils pour construire des polynomes de Taylor, majorer leur
    erreur (reste de Lagrange) et les appliquer a l'etude des vibrations d'un
    batiment : linearisation de la force de rappel, frequence propre,
    amplitude limite du modele lineaire et simulation numerique.
License: MIT + file LICENSE
Encoding: UTF-8
Language: fr
Roxygen: list(markdown = TRUE)
Depends: R (>= 4.1.0)
Imports: 
    stats
Suggests: 
    knitr,
    rmarkdown,
    testthat (>= 3.0.0)
VignetteBuilder: knitr
Config/testthat/edition: 3
URL: https://github.com/blacmis/taylorosc
BugReports: https://github.com/blacmis/taylorosc/issues
