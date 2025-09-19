## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library("apisirene")

airbus <- "383 474 814" |> 
	insee_get_siren() |> 
	purrr::pluck("unitesLegales") |> 
	knitr::kable()

## -----------------------------------------------------------------------------
"58204194300405" |> 
	insee_get_siret_all() |> 
	insee_parse_siret_resp() |> 
	knitr::kable()

## ----dashboard, eval=FALSE----------------------------------------------------
# "40188740100040" |>
# 	insee_utils_make_report()

