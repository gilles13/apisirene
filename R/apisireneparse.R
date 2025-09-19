# insee api sirene parse utils

#' @title insee_parse_siret_resp
#' @description Parse le résultat d'une requête sur UN numero siret
#' @param x une réponse siret sur un etablissement
#' @return un data.frame parse d'une requête mono etab siret
#' @examples
#' insee_parse_siret_resp(objetnonparsedunerequetesiretmonoetab)
#' @export
insee_parse_siret_resp <- function(x) {
  res <- x |> 
    purrr::pluck("etablissement") |> 
    purrr::list_flatten() |>
    purrr::list_flatten() |>
    unlist() |> 
    data.frame() |> 
    dplyr::rename(val=1) |> 
    tibble::rownames_to_column("indic") |> 
    tidyr::separate(
      col = indic,
      into = c("indic", "categ", "souscat"),
      sep = "_",
      fill = "right"
    )
  if(all(is.na(res$souscat))) {
    res <- 
      res |> 
      dplyr::select(-souscat)
  }
  return(res)
}

#' @title insee_parse_siret_resps
#' @description Parse le résultat d'une requête sur un ensemble de siret
#' @param x une réponse siret sur plusieurs etablissements
#' @return un data.frame parse d'une requête multi etab siret
#' @examples
#' insee_parse_siret_resps(objetnonparsedunerequetesiretmultietab)
# #' @export
insee_parse_siret_resps <- function(x) {
  res <- x |> 
    purrr::pluck("etablissements") |> 
    purrr::list_flatten() |>
    purrr::list_flatten() |>
    unlist() |> 
    data.frame() |> 
    dplyr::rename(val=1) |> 
    tibble::rownames_to_column("indic") |> 
    tidyr::separate(
      col = indic,
      into = c("indic", "categ", "souscat"),
      sep = "_",
      fill = "right"
    )
  if(all(is.na(res$souscat))) {
    res <- 
      res |> 
      dplyr::select(-souscat)
  }
  return(res)
}

#' @title insee_parse_siren_resp
#' @description Parse le résultat d'une requête siren mono UL
#' @param x une réponse siren mono unite legale
#' @return un data.frame parse d'une requête siren mono unite legale
#' @examples
#' insee_parse_siren_resp(objetnonparsedunerequetesiren)
#' @export
insee_parse_siren_resp <- function(x) {
  res <- x |> 
    purrr::pluck("unitesLegales") |> 
    purrr::list_flatten() |>
    purrr::list_flatten() |>
    unlist() |> 
    data.frame() |> 
    dplyr::rename(val=1) |> 
    tibble::rownames_to_column("indic") |> 
    tidyr::separate(
      col = indic,
      into = c("indic", "categ"),
      sep = "_",
      fill = "right"
    ) |> 
    tidyr::separate(
      col = categ,
      into = c("period", "souscat"),
      sep = "\\.",
      fill = "left"
    )
  if(all(is.na(res$souscat))) {
    res <- 
      res |> 
      dplyr::select(-souscat)
  }
  return(res)
}

#' @title insee_parse_siret_multi
#' @description Parse le résultat d'une requête multicriteres de siret
#' @param x une réponse siret multicriteres
#' @return un data.frame parse d'une requête multicriteres sur des etab siret
#' @examples
#' insee_parse_siret_multi(objetnonparsedunerequetesiretmulticriteres)
#' @export
insee_parse_siret_multi <- function(x) {
	res <- 
		x |> 
		purrr::pluck("etablissements") |> 
		purrr::map_dfr(function(x) {
									 tibble::tibble(
        gSiren = x |> purrr::pluck("siren", .default = NA),
        gNic = x |> purrr::pluck("nic", .default = NA),
        gSiret = x |> purrr::pluck("siret", .default = NA),
        gStatutDiffEtb = x |>
					purrr::pluck("statuDiffusionEtbalissement", .default = NA),
        gDateCreaEtb = x |> 
					purrr::pluck("dateCreationEtablissement",
											 .default = NA),
        gTreffEtb = x |> 
					purrr::pluck("trancheEffectifsEtablissement",
											 .default = NA),
        gAnneeEffEtb = x |> 
					purrr::pluck("anneeEffectifsEtablissement",
											 .default = NA),
        gApeRmEtb = x |> 
					purrr::pluck("activitePrincipaleRegistreMetiersEtablissement",
											 .default = NA),
        gDateDernTraitEtb = x |> 
					purrr::pluck("dateDernierTraitementEtablissement",
											 .default = NA),
        gEtbSiege = x |> 
					purrr::pluck("etablissementSiege",
											 .default = NA),
        gNbPeriodEtb = x |> 
					purrr::pluck("nombrePeriodesEtablissement",
											 .default = NA),
        ulEtatAdm = x |> 
					purrr::pluck("uniteLegale",
											 "etatAdministratifUniteLegale",
											 .default = NA),
        ulDenom = x |> 
					purrr::pluck("uniteLegale",
											 "denominationUniteLegale",
											 .default = NA),
        ulSigle = x |> 
					purrr::pluck("uniteLegale",
											 "sigleUniteLegale",
											 .default = NA),
        ulDenomUsuelle = x |> 
					purrr::pluck("uniteLegale",
											 "denominationUsuelle1UniteLegale",
											 .default = NA),
        ulCodeAPE = x |> 
					purrr::pluck("uniteLegale",
											 "activitePrincipaleUniteLegale",
											 .default = NA),
        ulCJ = x |> 
					purrr::pluck("uniteLegale",
											 "categorieJuridiqueUniteLegale",
											 .default = NA),
        aEtbDEPCOM = x |> 
					purrr::pluck("adresseEtablissement",
											 "codeCommuneEtablissement",
											 .default = NA),
        aEtbNumVoie = x |> 
					purrr::pluck("adresseEtablissement",
											 "numeroVoieEtablissement",
											 .default = NA),
        aEtbTypeVoie = x |> 
					purrr::pluck("adresseEtablissement",
											 "typeVoieEtablissement",
											 .default = NA),
        aEtbLibVoie = x |> 
					purrr::pluck("adresseEtablissement",
											 "libelleVoieEtablissement",
											 .default = NA),
        aEtbCodePostal = x |> 
					purrr::pluck("adresseEtablissement",
											 "codePostalEtablissement",
											 .default = NA),
        aEtbLibCom = x |> 
					purrr::pluck("adresseEtablissement", 
											 "libelleCommuneEtablissement",
											 .default = NA),
        aEtbCoordX = x |> 
					purrr::pluck("adresseEtablissement", 
											 "coordonneeLambertAbscisseEtablissement",
											 .default = NA),
        aEtbCoordY = x |> 
					purrr::pluck("adresseEtablissement",
											 "coordonneeLambertOrdonneeEtablissement",
											 .default = NA),
        pDateFin = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 1,
											 .default = NA),
        pDateDeb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 2,
											 .default = NA),
        pEtatAdmEtb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 3,
											 .default = NA),
        pChgtEtatAdmEtb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 4,
											 .default = NA),
        pEns1Etb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 5,
											 .default = NA),
        pEns2Etb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 6,
											 .default = NA),
        pEns3Etb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 7,
											 .default = NA),
        pChgtEnsgEtb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 8,
											 .default = NA),
        pDenoUsEtb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 9,
											 .default = NA),
        pChgtDenoUsEtb = x |> 
					purrr::pluck("periodesEtablissement",
											 1, 10,
											 .default = NA)
      )
    })
		if(exists("nafniv5")) {
			 res <- res |>
				 dplyr::left_join(y = nafniv5, by = c("ulCodeAPE" = "Code")) |> 
				 dplyr::rename(libAPE = Libellé)
		}
		if(exists("cjniv3")) {
			 res <- res |>
				 dplyr::left_join(y = cjniv3, by = c("ulCJ" = "Code")) |> 
				 dplyr::rename(libCJ = Libellé)
		}
		return(res)
}
