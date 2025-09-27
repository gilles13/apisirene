#  Les fonctions de type get

#' @title insee_get_query
#' @description Retourne le resultat de la requete
#' @param url l'url a interroger
#' @param simp if TRUE (default), simplifyVector
#' @return un data.frame des donnees renvoyees par l'API Sirene
#' @examples
#' \dontrun{
#' insee_get_query("https://api.insee.fr/api-sirene/3.11/siret/40188740100040")
#' }
#' @export
insee_get_query <- function(url, simp = TRUE) {
	message("L'url est : ", url)
	res <- 
		url |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>  
		httr2::req_headers(
				"X-INSEE-Api-Key-Integration" = get_api_key("INSEESIRENE_APIKEY")) |> 
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = simp)
	return(res)
}

#' @title insee_get_siret
#' @description Retourne le resultat parse de la requete sur numero siret
#' @param siret le code siret a interroger
#' @param simp if TRUE (default), simplifyVector
#' @return un data.frame des donnees renvoyees par l'API Sirene
#' @examples
#' \dontrun{
#' insee_get_siret(siret = "40188740100040")
#' }
#' @export
insee_get_siret <- function(siret = NULL, simp = TRUE) {
  siret <- gsub(" ", "", siret)
  if(is.null(siret)) stop("Il faut passer un siret")
	urlbase <- "https://api.insee.fr/api-sirene/3.11/siret/"
	urltot <- paste0(urlbase, siret)
	res <- insee_get_query(urltot, simp = simp) |> 
		purrr::pluck("etablissement")
	return(res)
}

#' @title insee_get_siret_all
#' @description Retourne le resultat (non parse) de la requete sur numero siret
#' @param siret le code siret a interroger
#' @return une liste non parsee retournee par l'API
#' @examples
#' \dontrun{
#' insee_get_siret_all("40188740100040")
#' }
#' @export
insee_get_siret_all <- function(siret = NULL) {
	siret <- gsub(" ", "", siret)
  if(is.null(siret)) stop("Il faut passer un siret")
	urlbase <- "https://api.insee.fr/api-sirene/3.11/siret/"
	urltot <- paste0(urlbase, siret)
	res <- insee_get_query(urltot)
	return(res)
}

#' @title insee_get_siret_multi
#' @description Retourne le resultat de la requete multicriteres sur numeros siret
#' @param url l'url multicriteres a adresser a l'API
#' @param simp if TRUE (default) simplifyVector
#' @return un data.frame parse du resultat de la requete multicriteres
#' @examples
#' \dontrun{
#' insee_get_siret_multi("https://api.insee.fr/api-sirene/3.11/siret?q=codeCommuneEtablissement%3A13*%20AND%20activitePrincipaleUniteLegale%3A10.71*&nombre=1000&curseur=*")
#' }
#' @export
insee_get_siret_multi <- function(url, simp = TRUE) {
  nbecho <- insee_utils_find_nb_echos_demande(url)
  nbechodemande <- 
    ifelse(length(nbecho) == 0,
           20,
           nbecho)
  nbechoresultat <- insee_utils_howmany_echos(url)
	system2("clear")
	message("Votre requete concerne ", nbechoresultat, " entreprises ou etablissements")
  nbssreqafaire <- ceiling(nbechoresultat / nbechodemande)
  urlsanscurs <- insee_utils_remove_url_curseurx(url)
  maliste <- vector(mode = "list", length = 0)
	message(" Debut de traitement: ", Sys.time())
  for(i in seq_len(nbssreqafaire)) {
    x <- 
      url |> 
      httr2::request() |> 
      httr2::req_headers("Accept" = 'application/json') |>  
      httr2::req_headers(
        "X-INSEE-Api-Key-Integration" = get_api_key("INSEESIRENE_APIKEY")) |> 
      httr2::req_perform() |> 
      httr2::resp_body_json(simplifyVector = simp)
    maliste <- append(maliste, list(x |> insee_parse_siret_multi()))
    curseur <- x |> purrr::pluck("header", "curseur")
    curssuivant <- x |> purrr::pluck("header", "curseurSuivant")
# 		message("Le curseur suivant est :", curssuivant)
		message(Sys.time())
    url <- paste0(urlsanscurs, "&curseur=", curssuivant)
  }
	message("Fin de traitement : ", Sys.time())
  res <- do.call("rbind", maliste)
  return(res)
}

#' @title insee_get_siren
#' @description Retourne le resultat de la requete sur un numero siren
#' @param siren le numero siren a requeter
#' @param champs limiter le champ de la reponse siren. Pas encore actif TODO
#' @return un data.frame parse du resultat de la requete sur numero siren
#' @examples
#' \dontrun{
#' insee_get_siren("monnumerosiren")
#' }
#' @export
insee_get_siren <- function(siren = NULL, champs = NULL) {
	if(is.null(siren)) stop("Il faut passer un siren")
	siren <- gsub(" ", "", siren)
	urlbase <- "https://api.insee.fr/api-sirene/3.11/siren?q=siren%3A"
	if(is.null(champs)) {
		urltot <- paste0(urlbase, siren)
	} else {
		urltot <- paste0(urlbase, siren, "&champs=", champs)
	}
	res <- insee_get_query(urltot)
	return(res)
}

#' @title insee_get_liens_succession
#' @description Retourne le resultat de la requete liens de succession
#' @param type_lien le type de lien recherche : predecesseur (P) ou successeur (S)
#' @param etab le numero siret de l'etablissement dont on cherche le lien de succession
#' @return un data.frame des donnees renvoyees par l'API Sirene
#' @examples
#' \dontrun{
#' insee_get_liens_succession(type_lien = "S", etab = "123 456 789 00012")
#' }
#' @export
insee_get_liens_succession <- function(type_lien = "S", etab = NULL) {
	if(is.null(etab)) stop("Il faut saisir un numero d'etablissement")
	if(!type_lien %in% c("S", "P")) stop("Le type de lien doit etre 'S' ou 'P'")
	etab <- gsub(" ", "", etab)
	urlbase <- ifelse(type_lien == "S",
										"https://api.insee.fr/api-sirene/3.11/siret/liensSuccession?q=siretEtablissementPredecesseur:",
										"https://api.insee.fr/api-sirene/3.11/siret/liensSuccession?q=siretEtablissementSuccesseur:")
	urltot <- paste0(urlbase, etab)
	tryCatch({
	res <- 
		urltot |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>  
		httr2::req_headers(
				"X-INSEE-Api-Key-Integration" = get_api_key("INSEESIRENE_APIKEY")) |> 
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = FALSE)
	return(res)
}, error = function(e) {
  message("Pas de lien trouve")
})
}

