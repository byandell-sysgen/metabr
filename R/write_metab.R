#' Write Metabolite File
#'
#' @param object data frame
#' @param filename name of saved file
#' @param targeted targeted if `TRUE`
#' @param ... ignored
#'
#' @return invisible
#' @export
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider unite
#' @importFrom utils write.csv
write_metab <- function(object, filename, targeted = TRUE, ...) {
  object <- dplyr::select(object, label:parent, sample, value)
  object <- tidyr::pivot_wider(object, id_cols = label:parent,
                               names_from = "sample", values_from = "value")
  invisible(utils::write.csv(object, filename, row.names = FALSE))
}
# As used earlier. Note that new version creates output that matches
# format of raw data, while below is different.
old_write_metab <- function(object, filename, targeted = TRUE, ...) {
  if(targeted) {
    object <- dplyr::select(object,
      sample, run, rundate, Batch, Plate, mouse_id, time, rep, compound, value)
  } else {
    object <- dplyr::select(object,
      sample, run, rundate, Batch, Plate, mouse_id, time, rep, compound, medRt, value) |>
      dplyr::mutate(medRt = round(medRt, 3)) |>
      tidyr::unite(compound, compound, medRt)
  }
  object <- tidyr::pivot_wider(object, id_cols = sample:rep,
    names_from = "compound", values_from = "value")
  names(object)[1:8] <- 
    c("Original Sample Name", "Run Order", "Run Date", "Batch", "Plate",
      "Strains", "Timepoints", "Replicates")
  
  invisible(utils::write.csv(object, filename, row.names = FALSE))
}