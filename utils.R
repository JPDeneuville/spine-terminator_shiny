# R/utils.R

get_cabinet_from_kine <- function(kine_id) {
  if (grepl("_APS$", kine_id)) return("APS")
  if (grepl("_MSV$", kine_id)) return("MSV")
  return("GENERIC")
}

get_logo_for_cabinet <- function(cabinet) {
  if (cabinet == "APS") return("logo_aps.jpg")
  if (cabinet == "MSV") return("logo_mspv.png")
  return("logo.png")
}

get_bg_color_for_cabinet <- function(cabinet) {
  if (cabinet == "APS") return("#FFD7A6")
  if (cabinet == "MSV") return("#F1F3F0")
  return("#F0F7FE")
}
