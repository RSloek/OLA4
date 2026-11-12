Fejl i (function (..., row.names = NULL, check.rows = FALSE, check.names =
TRUE, : argumenter antyder forskelligt antal rækker: 1, 0, 2
Traceback:
6: stop(gettextf("arguments imply differing number of rows: %s", 
       paste(unique(nrows), collapse = ", ")), domain = NA)
5: (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE, 
       fix.empty.names = TRUE, stringsAsFactors = FALSE) 
   {
       data.row.names <- if (check.rows && is.null(row.names)) 
           function(current, new, i) {
               if (is.character(current)) 
                   new <- as.character(new)
               if (is.character(new)) 
                   current <- as.character(current)
               if (anyDuplicated(new)) 
                   return(current)
               if (is.null(current)) 
                   return(new)
               if (all(current == new) || all(current == "")) 
                   return(new)
               stop(gettextf("mismatch of row names in arguments of 'data.frame', item %d", 
                   i), domain = NA)
           }
       else function(current, new, i) {
           current %||% if (anyDuplicated(new)) {
     ...
4: do.call(data.frame, c(x, alis))
3: as.data.frame.list(rawJSONlist)
2: as.data.frame(rawJSONlist) at util_Rasmus.R#76
1: tracksMakeDF(raw, tabs)

