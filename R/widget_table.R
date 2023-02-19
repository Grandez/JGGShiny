guiTable = function (id) {
  reactable::reactableOutput(id)
}
updTable = function (data, ...) {
   obj = data
   args = list(...)
   if (length(args) > 0) obj = .prepareTable(data, ...)

   if (!("reactable" %in% class(data))) obj = reactable::reactable( data, width="100%", pagination = FALSE)
   reactable::renderReactable({obj})
}
makeGroupedTable = function(data, group=NULL, columns=NULL, method=NULL, ...) {
    if (is.null(group))    stop("Missing grouped columns")
    if (is.null(columns))  stop("Missing columns to group")
    if (is.null(method))   stop("Missing agrregation method")

    cols = lapply(columns, function(x) colDef(aggregate = method))
    names(cols) = columns
    makeTable(data, colDefs=cols, groupBy = group, ...)
}

makeTable = function (data, ...) {
   jscode = function(idTable) {
      data = paste("{ row: rowInfo.index + 1, colName: colInfo.id")
      data = paste(data, ",detail: JSON.stringify(rowInfo.row)}")
      evt = paste0("Shiny.setInputValue('", idTable, "',", data, ",{ priority: 'event' });")

      js_code = "function(rowInfo, colInfo) {"
      js_code = paste(js_code, evt, sep="\n")
      js_code = paste(js_code, "}", sep="\n")
      htmlwidgets::JS(js_code)
   }
   cols    = NULL
   onClick = NULL
   groupBy = NULL
   mtheme = reactable::reactableTheme(cellPadding = "0px 0px")

   args = list(...)

   if (!is.null(args$colDefs)) cols    = args$colDefs
   if (!is.null(args$groupBy)) groupBy = args$groupBy

   if (!is.null(args$hide)) {
       hideCols = lapply(args$hide, function(col) colDef(show = FALSE))
       names(hideCols) = args$hide
       cols = jgg_list_combine(cols, hideCols)
   }
   if (!is.null(args$colNames)) {
       cnames = lapply(args$colNames, function(nm) colDef(name = nm))
       names(cnames) = names(args$colNames)
       cols = jgg_list_combine(cols, cnames)
   }

   if (!is.null(args$click)) onClick = jscode(args$click)
   reactable::reactable( data, width="100%", pagination = FALSE, onClick=onClick
                        ,columns = cols
                        ,groupBy = groupBy
                       )
}
