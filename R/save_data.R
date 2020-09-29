#' Save data from PCreg objects
#'
#' @param data list containing objects to sabe
#' @param save.out how to save the object: comma- delimited, text, or R data. Must be one of "csv", "table", or "R".
#' @param dir directory
#'
#' @return
#' @export
#'
#' @examples
save_data <- function(data, out.fmt = out.fmt, out.dir = out.dir) {
  if(!out.fmt %in% c("csv", "table", "R"))
    stop("save.out must be one of: csv, table, or R")
  if(!dir.exists(out.dir)){
    dir.create(out.dir)
  }
  for(i in 1:length(data)) {
    object <- data[[i]]
    name <- names(data)[i]
    switch(out.fmt,
           csv = write.csv(object, file = paste0(dir, name, ".csv"), quote = FALSE, row.names = FALSE),
           table = write.table(object, file = paste0(dir, name, ".txt")),
           R = save(object, file = paste0(dir, name, ".rd")))
  }
}
