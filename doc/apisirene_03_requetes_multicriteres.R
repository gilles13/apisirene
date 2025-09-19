## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library("apisirene")

insee_utils_siret_multi_make("DEPCOM:13201 & NAF:10.71C") |> 
	insee_get_siret_multi()

## -----------------------------------------------------------------------------
insee_utils_siret_multi_make("(DEPCOM:13201 | DEPCOM:13205 | DEPCOM:13208) & (NAF:10.71B | NAF:10.71C | NAF:10.13B) & (CJ:1000 | CJ:5499)") |> 
	insee_get_siret_multi() |> 
	head(n = 15)

## -----------------------------------------------------------------------------
insee_utils_siret_multi_make("NAF:10.71C") |> 
	insee_utils_howmany_echos()

