#' Save data from PCreg objects
#'
#' @param out_fmt
#' @param out_dir
#' @param data list containing objects to sabe
#'
#' @return
#' @export
#'
#' @examples
save_data <- function(data, out_fmt = out_fmt, out_dir = out_dir) {
  if(!out_fmt %in% c("csv", "table", "R"))
    stop("save.out must be one of: csv, table, or R")
  if(!dir.exists(out_dir)){
    dir.create(out_dir)
  }
  for(i in 1:length(data)) {
    object <- data[[i]]
    name <- names(data)[i]
    switch(out_fmt,
           csv = write.csv(object, file = paste0(out_dir, name, ".csv"), quote = FALSE, row.names = FALSE),
           table = write.table(object, file = paste0(out_dir, name, ".txt")),
           R = save(object, file = paste0(out_dir, name, ".rd")))
  }
}
