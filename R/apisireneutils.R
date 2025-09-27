globalVariables(c("Libelle", "val", "nicSiegeUniteLegale"))

# Des utilitaires pour utiliser l'api sirene

#' @title findmynaflib
#' @description Retourne le libelle d'un code naf 5 caracteres.
#' @param nafc une chaine de caracteres
#' @return Une chaine de caracteres.
#' @examples
#' \dontrun{
#' findmynaflib("01.21Z")
#' }
#' @export
findmynaflib <- function(nafc) {
	if(!exists("nafniv5")) stop("Vous n'avez pas la table nafniv5")
	res <- 
		nafniv5 |> 
		dplyr::filter(Code == nafc) |> 
		dplyr::pull()
	return(res)
}

#' @title trymynaflib
#' @description Retourne le code parmi un libelle saisi par l'utilisateur.
#' @return Une chaine de caracteres ou rien si pas d'echos
#' @examples
#' \dontrun{
#' trymynaflib()
#' }
#' @export
trymynaflib <- function() {
	system2("clear")
	message("Nous allons tenter de trouver le code naf ensemble !")
	message("Dites moi ce que vous cherchez comme activite ?")
	var1 = readline(prompt = "Entrer une activite : ")
	if(length(var1) == 0) stop("Il faut saisir qque chose ^^'")
	res <- 
		nafniv5 |> 
		dplyr::slice(grep(pattern = var1, x = Libelle, ignore.case = TRUE))
	if(nrow(res) == 0) {
		message("Desole, je n'ai rien trouve ... :(")
		message("Re-essayer avec un expression reguliere ?")
	} else return(res)
}

#' @title findmycj
#' @description Retourne le libelle d'un code CJ de niveau 3.
#' @param cj une chaine de caracteres
#' @return Une chaine de caracteres.
#' @examples
#' \dontrun{
#' findmycj("5599")
#' }
#' @export
findmycj <- function(cj) {
	if(!exists("cjniv3")) stop("Vous n'avez pas la table cjniv3")
	res <- 
		cjniv3 |> 
		dplyr::filter(Code == cj) |> 
		dplyr::pull()
	return(res)
}

#' @title trymycjlib
#' @description Retourne le code parmi un libelle de categorie juridique saisi par l'utilisateur.
#' @return Un tibble ou rien si pas d'echo
#' @examples 
#' \dontrun{
#' trymycjlib()
#' }
#' @export
trymycjlib <- function() {
	system2("clear")
	message("Nous allons tenter de trouver le code CJ ensemble !")
	message("Dites moi ce que vous cherchez comme categorie juridique ?")
	var1 = readline(prompt = "Entrer une CJ : ")
	if(length(var1) == 0) stop("Il faut saisir qque chose ^^'")
	res <- 
		cjniv3 |> 
		dplyr::slice(grep(pattern = var1, x = Libelle, ignore.case = TRUE))
	if(nrow(res) == 0) {
		message("Desole, je n'ai rien trouve ... :(")
		message("Re-essayer avec un expression reguliere ?")
	} else return(res)
}

#' @title findmytreff
#' @description Retourne le libelle d'un code tranche d'effectif
#' @param tr une chaine de caracteres
#' @return Un tibble ou rien si pas d'echo
#' @examples
#' \dontrun{
#' findmytreff("42")
#' }
#' @export
findmytreff <- function(tr) {
	if(!exists("sirentreff")) stop("Vous n'avez pas la table des tranches d'effectif")
  if(length(tr) == 0 || is.na(tr)) {
    return("0 salarie")
  } else {
    res <- 
      sirentreff |> 
      dplyr::filter(code == tr) |> 
      dplyr::pull()
    return(res)  
  }
}

#' @title get_api_key
#' @description Retourne la valeur de la variable d'environnement INSEESIRENE_APIKEY
#' @details Pour l'instant, la valeur de la variable d'environnement est fixee a INSEESIRENE_APIKEY.
#' @param keyname le nom de la variable d'environnement contenant votre clef de l'API Sirene.
#' @return Une chaine de caracteres.
#' @examples
#' \dontrun{
#' get_api_key("INSEESIRENE_APIKEY")
#' }
#' @export
get_api_key <- function(keyname = "") {
  key <- Sys.getenv(keyname)
  if(identical(key, "")) {
    stop(paste0("No ", keyname, " API key found, check your environment vars"))
  }
  return(key)
}

#' @title insee_utils_sirene_varenv_check
#' @description Retourne la valeur de la variable d'environnement INSEESIRENE_APIKEY
#' @param var le nom de la variable d'environnement dont il faut verifier l'existence.
#' @return un message d'avertissement si la variable n'est pas trouvee, sinon rien.
#' @examples
#' \dontrun{
#' insee_utils_sirene_varenv_check("INSEESIRENE_APIKEY")
#' }
#' @export
insee_utils_sirene_varenv_check <- function(var = "") {
  sysenvlist <- names(Sys.getenv())
  if(!any(sysenvlist == var)) {
    message("Vous ne semblez pas avoir la variable d'environnement ",
            var,
            "\nVous devriez peut etre consulter",
            "\nhttps://www.sirene.fr/static-resources/documentation/Insee_API_publique_modalites_connexion.pdf"
      )
  }
}

#' @title insee_utils_sirene_aide
#' @description Retourne un message d'aide
#' @return un message d'information.
#' @examples
#' \dontrun{
#' insee_utils_sirene_aide()
#' }
#' @export
insee_utils_sirene_aide <- function() {
    message(
      "Si vous souhaitez utiliser l'API Sirene,",
      "\nvous devez prealablement creer un compte",
      "\nsur le portail des API de l'Insee :",
      "\nhttps://portail-api.insee.fr/",
      "\n",
      "\nToute la documentation est disponible a l'adresse suivante :",
      "\nhttps://www.sirene.fr/static-resources/documentation/Insee_API_publique_modalites_connexion.pdf"
    )
}

#' @title insee_utils_remove_url_curseurx
#' @description Retourne une url sans le fragment curseur=*
#' @param url l'url a modifier
#' @return une chaine de caractere (l'url) sans le curseur
#' @examples
#' \dontrun{
#' insee_utils_remove_url_cruseurx("https://mapetiteurl?curseur=*")
#' }
#' @export
insee_utils_remove_url_curseurx <- function(url) {
  m <- gregexpr("&curseur=\\*", url)
  frag <- unlist(regmatches(url, m))
  res <- gsub(pattern = frag, 
              replacement = "",
              url,
              fixed = TRUE)
  return(res)
}

#' @title insee_utils_howmany_echos
#' @description Retourne le nombre d'echos de la requete
#' @param url l'url a interroger
#' @return une chaine de caractere : le nombre d'echos
#' @examples
#' \dontrun{
#' insee_utils_howmany_echos("https://api.insee.fr/api-sirene/3.11/siret?q=codeCommuneEtablissement%3A13*%20AND%20activitePrincipaleUniteLegale%3A10.71*&nombre=1000&curseur=*")
#' }
#' @export
insee_utils_howmany_echos <- function(url) {
	res <- 
		url |> 
		httr2::request() |> 
		httr2::req_method("HEAD") |>
		httr2::req_headers("Accept" = 'application/json') |>  
		httr2::req_headers(
				"X-INSEE-Api-Key-Integration" = get_api_key("INSEESIRENE_APIKEY")) |> 
		httr2::req_perform() |> 
		httr2::resp_headers("total") |> 
		as.numeric()
	return(res)
}

#' @title insee_utils_find_nb_echos_demande
#' @description Retourne le nombre d'echos demande dans la requete
#' @param url l'url a interroger
#' @return une chaine de caractere : le nombre d'echos demande par la requete
#' @examples
#' \dontrun{
#' insee_utils_find_nb_echos_demande("https://api.insee.fr/api-sirene/3.11/siret?q=codeCommuneEtablissement%3A13*%20AND%20activitePrincipaleUniteLegale%3A10.71*&nombre=1000&curseur=*")
#' }
#' @export
insee_utils_find_nb_echos_demande <- function(url) {
  m <- gregexpr("&nombre=\\d+", url)
  res <- unlist(regmatches(url, m))
  res <- as.numeric(gsub(pattern = "[^0-9.]", replacement = "", res))
  return(res)
}

#' @title insee_utils_siret_sel
#' @description Retourne les informations du data.frame parse
#' @param df le data.frame utilise
#' @param myindic l'indicateur (famille d'information)
#' @param mycateg l'indicateur recherche
#' @return une chaine de caractere : la valeur de l'indicateur
#' @examples
#' \dontrun{
#' insee_utils_siret_sel(mydf, mycateg = "activitePrincipaleUniteLegale")
#' }
#' @export
insee_utils_siret_sel <- function(df, myindic = "uniteLegale", mycateg = NA) {
	if(is.na(mycateg)) {
		res <- 
			df |> 
			dplyr::filter(indic == myindic) |> 
			dplyr::select(val) |> 
			dplyr::pull()
	} else {
		res <- 
			df |> 
			dplyr::filter(indic == myindic, categ == mycateg) |> 
			dplyr::select(val) |> 
			dplyr::pull()
	}
	return(res)
}

#' @title insee_utils_find_nb_periodes
#' @description Retourne le nombre de periodes disponibles pour un etablissement
#' @param df le data.frame utilise
#' @return une chaine de caractere : le nombre de periodes d'un etablissement
#' @examples
#' \dontrun{
#' insee_utils_find_nb_periodes(mydf)
#' }
#' @export
insee_utils_find_nb_periodes <- function(df) {
	res <- 
		df |> 
		dplyr::filter(indic == "nombrePeriodesEtablissement") |> 
		dplyr::select(val) |> 
		dplyr::pull()
	return(res)
}

#' @title insee_utils_concat_multi
#' @description Retourne la chaine concatenee d'une requete multicriteres
#' @param url l'url a interroger
#' @param multi la chaine de caracteres des parametres de la requete multicriteres
#' @param nb booleen si vrai ajoute a l'url le nb max d'echos autorises par l'api (1000)
#' @return une chaine de char
#' @examples
#' \dontrun{
#' insee_utils_concat_multi("https://api.insee.fr/api-sirene/3.11/siret?q=", multi = "")
#' }
#' @export
insee_utils_concat_multi <- function(url = NULL, multi = "", nb = TRUE) {
# 	if(is.null(url)) stop("Il faut une url")
	url = "https://api.insee.fr/api-sirene/3.11/siret?q="
	if(nb) {
		multi <- gsub(":", "%3A", multi)
		multi <- gsub(" ", "%20", multi)
		urltot <- paste0(url, multi, "&nombre=1000")
	} else {
		multi <- gsub(":", "%3A", multi)
		multi <- gsub(" ", "%20", multi)
		urltot <- paste0(url, multi)
	}
	return(urltot)
}

#' @title acceder au repertoire du template pour tdb siret_
#' @description Retourne le chemin du template Rmarkdown pour dashboard siret
#' @param pack le nom du package
#' @param tempname le nom du template
#' @return une chaine de char
#' @examples
#' \dontrun{
#' insee_utils_template_path(pack = "apisirene")
#' }
#' @export
insee_utils_template_path <- function(pack = "apisirene", tempname = "siren") {
# 	r_libs <- Sys.getenv("R_HOME")
# 	r_path <- paste0(r_libs, "/library/apisirene/rmarkdown/templates/",
	r_libs <- Sys.getenv("R_LIBS_USER")
	r_path <- paste0(r_libs, "/apisirene/rmarkdown/templates/",
									 tempname,
									 "/skeleton/skeleton.Rmd")
	return(file.path(r_path))
}

#' @title generer un rapport html d'un etablissement
#' @description Affiche une page html synthetisant de l'information de base sur un etablissement
#' @param siren une chaine de caractere pour l'etablissement a consulter
#' @return un fichier html, affiche dans le browser
#' @examples
#' \dontrun{
#' insee_utils_make_report(siret = "40188740100040")
#' }
#' @export
insee_utils_make_report <- function(siren) {
	apisitempfile <- tempfile(fileext = ".html")
	rmarkdown::render(
			input = insee_utils_template_path(),
			params = list(mysiren = siren),
			envir = new.env(),
			output_file = apisitempfile,
			quiet = TRUE
	)
	message("Le fichier genere : ", apisitempfile)
	utils::browseURL(apisitempfile)
}

#' @title insee_utils_siret_multi_make
#' @description Retourne l'url formatee d'une requete multicriteres
#' @param multi la chaine de caractere des criteres de la requete
#' @return une chaine de caractere
#' @examples
#' \dontrun{
#' insee_utils_siret_multi_make("DEPCOM:132* & NAF:10.71C")
#' }
#' @export
insee_utils_siret_multi_make <- function(multi = "") {
	urlbase <- "https://api.insee.fr/api-sirene/3.11/siret?q="
	multi <- gsub("CJ", "categorieJuridiqueUniteLegale", multi)
	multi <- gsub("DEPCOM", "codeCommuneEtablissement", multi)
	multi <- gsub("NAF", "activitePrincipaleUniteLegale", multi)
	multi <- gsub("ETATUL", "etatAdministratifUniteLegale", multi)
	multi <- gsub(":", "%3A", multi)
	multi <- gsub(" ", "%20", multi)
	multi <- gsub("\\&", "AND", multi)
	multi <- gsub("\\|", "OR", multi)
	urltot <- paste0(urlbase, multi, "&nombre=1000&curseur=*")
	return(urltot)
}

#' @title insee_utils_resp_code
#' @description Renvoie le code retour de la requete
#' @param url l'url a interroger
#' @param mode le mode d'interrogation : simple (S) ou etendu (E)
#' @return une chaine de caractere : le code retour de la requete
#' @examples
#' \dontrun{
#' insee_utils_resp_code("https://api.insee.fr/api-sirene/3.11/siret?q=codeCommuneEtablissement%3A13*%20AND%20activitePrincipaleUniteLegale%3A10.71*&nombre=1000&curseur=*")
#' }
#' @export
insee_utils_resp_code <- function(url, mode = "S") {
	response <- 
		url |> 
		httr2::request() |> 
		httr2::req_method("HEAD") |>
		httr2::req_headers("Accept" = 'application/json') |>  
		httr2::req_headers(
				"X-INSEE-Api-Key-Integration" = get_api_key("INSEESIRENE_APIKEY")) |> 
		httr2::req_perform()
	res1 <- response |> httr2::resp_status()
	res2 <- response |> httr2::resp_status_desc()
	resAll <- list(status = res1, description = res2)
	if(mode == "S") {
		return(res1)
	} else {
		return(resAll)
	}
}

#' @title insee_utils_get_nic
#' @description Renvoie le nic du siege a partir d'une requete sur siren
#' @param x la requete sur siren a interroger
#' @return une chaine de caractere : le code nic du siege de l'unite legale
#' @examples
#' \dontrun{
#' insee_utils_get_nic(x = marequetesiren)
#' }
#' @export
insee_utils_get_nic <- function(x) {
	res <- 
		x |> 
		purrr::pluck("unitesLegales") |> 
		purrr::pluck("periodesUniteLegale") |> 
		data.frame() |> 
		dplyr::slice(1) |> 
		dplyr::select(nicSiegeUniteLegale) |> 
		dplyr::pull()
	return(res)
}

#' @title insee_utils_avis_sit
#' @description Renvoie un avis de situation a partir d'un siret
#' @param siret le siret a interroger
#' @param url if FALSE (default) return file, else return link to file
#' @return un avis de situation sirene
#' @examples
#' \dontrun{
#' insee_utils_avis_sit(siret = "12345678900001")
#' }
#' @export
insee_utils_avis_sit <- function(siret, url = FALSE) {
	siret <- gsub(" ", "", siret)
	pdfpath <- tempfile(fileext = ".pdf")
	urlBase <- "https://api-avis-situation-sirene.insee.fr/identification/pdf/"
	urlTot <- paste0(urlBase, siret)
	if(url) {
		return(urlTot)
	} else {
		pdfraw <- 
			urlTot |> 
			httr2::request() |> 
			httr2::req_headers("Accept" = "application/pdf") |>  
			httr2::req_perform() |> 
			httr2::resp_body_raw() 
		writeBin(pdfraw, pdfpath)
		system2("xdg-open", pdfpath)
		message(paste0("Le fichier pdf : ", pdfpath))
		return(urlTot)
	}
}
