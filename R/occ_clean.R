#' Clean Occurrence Records from GBIF
#'
#' @param data The file path to species occurrence records downloaded from GBIF
#' @param coord_uncertainty The maximum uncertainty around the location of an occurrence.
#' @param occ_type The types of occurrence records to be included.
#' @param crs_chosen The chosen projection for the occurrences. This parameter is useful if mapping occurrences.
#' @param occ_issue A list of species occurrence issues that should not be included in the final data.
#'
#' @return An sf object with clean GBIF data.
#' @export
#'
#' @examples
#'
occ_clean <- function(data,
                      coord_uncertainty = 1000,
                      occ_type = c('PhysicalObject',
                                   'specimen',
                                   'PRESERVED_SPECIMEN',
                                   'PreservedSpecimen',
                                   'Occurrence',
                                   'Collection',
                                   'Physical Object',
                                   'Objeto fÃsico'),
                      crs_chosen = 'albersNA',
                      occ_issue = c('COORDINATE_ROUNDED',
                                    'RECORDED_DATE_INVALID',
                                    'COORDINATE_UNCERTAINTY_METERS_INVALID',
                                    'PRESUMED_NEGATED_LONGITUDE')){
  species <<- data %>%
    dplyr::mutate(gbifID = as.character(gbifID)) %>%
    dplyr::filter(type %in% occ_type) %>%
    dplyr::mutate_if(is.character, list(~na_if(.,""))) %>% #This removes empty coordinates stored as a blankspace "".
    dplyr::filter(!is.na(decimalLatitude) &
                    !is.na(decimalLongitude))

  speciesSf <<- sf::st_as_sf(x = species,
                             coords = c(x = 'decimalLongitude',
                                        y = 'decimalLatitude'),
                             crs = 4326,
                             remove = FALSE)

  species_crs <<- sf::st_transform(speciesSf,
                                   enmSdm::getCRS(crs_chosen))

  # buffer_distance <<- units::as_units(80, "km")
  # range_buffer <<- rangeMap %>%
  #   st_buffer(dist = buffer_distance)

  #Create a buffer 3x the distance of the rangemap buffer for automatic exclusion.
  # exclusion_distance <<- units::as_units(240, "km")
  # exclusion_buffer <<- rangeMap %>%
  #   st_buffer(dist = exclusion_distance)

  # speciesSf_filtered <<- species_alb %>%
  #   dplyr::mutate(within_range = lengths(sf::st_within(x = species_alb,
  #                                           y = rangeMap)),
  #          within_buffer = lengths(sf::st_within(x = species_alb,
  #                                            y = range_buffer)),
  #          within_exclusion = lengths(sf::st_within(x = species_alb,
  #                                               y = exclusion_buffer)),
  #          species_name = species_name) %>%
  # filter(year >= 1970) %>% #I have a feeling this should not be here as my climate extraction takes into consideration when the occurrence was collected.
  species_filter <<- species_crs %>%
    dplyr::filter(coordinateUncertaintyInMeters <= 1000 | is.na(coordinateUncertaintyInMeters)) %>%
    dplyr::filter(!(coordinateUncertaintyInMeters == "NA")) %>%
    dplyr::filter(!(grepl(paste(occ_issue, collapse = '|'), issue)))

  return(species_filter)
}
