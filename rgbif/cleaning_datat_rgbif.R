library(rgbif)

# GET SOME DATA
  # get taxon key for Helianthus annuus
  key <- name_suggest(q  ="Helianthus annuus", rank = "species")$key[1]
  # pass to occ_search()
  res <- occ_search(taxonKey = key, limit = 100)
  