.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Si c'est votre premiere utilisation du package, il est recommande de lire les vignettes, ou de lancer la commande insee_sirene_aide()")
}
utils::globalVariables(c("nafniv5", "cjniv3",
												 "Code", "sirentreff",
												 "code", "indic", "categ",
												 "souscat"))
