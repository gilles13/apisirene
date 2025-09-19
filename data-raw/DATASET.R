## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

# coller le code utilisé pour générer le jeu de données
urlfile <- "https://www.insee.fr/fr/statistiques/fichier/2120875/naf2008_5_niveaux.xls"
tmpfile <- tempfile()
system2(command = "wget", args = c("-O", tmpfile, urlfile))
naf5n <- readxl::read_xls(tmpfile)
