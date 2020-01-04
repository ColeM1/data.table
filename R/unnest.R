unnest = function(x, cols = NULL, is.cartesian = TRUE){
  
  n = length(x)
  
  x_lists = vapply(x, is.list, TRUE)
  which_lists = which(x_lists)
  
  if(!is.null(cols)){
    if (is.character(cols)) {
      cols = match(cols, names(x))
      }
    unnest_cols = which_lists[na.omit(match(cols, which_lists))]
    if (length(unnest_cols) == 0L) stop('no columns were selected with a list')
    which_lists = unnest_cols
  }
  
  tot_lists = length(which_lists)
  non_lists = setdiff(seq_len(n), which_lists)
  
  if (tot_lists == 0L){
    warning('There were no lists to unnest')
    return(x)
  } else if (tot_lists == 1L){
    is.cartesian = FALSE #things are simplified if there is one column
  }
  
  #could add cartesian check - if all lengths the same, user would likely like non-cartesian unnest
  #if (is.cartesian){
    # row_lens = x[, vapply(.SD, lengths, seq_len(.N)), .SDcols = which_lists]
    # is.cartesian = !all(apply(row_lens, 1, function(x) length(unique(x)) == 1L))
  # }
  
  res = vector('list', length = n)
  names(res) = copy(names(x))
  
  if (!is.cartesian){
    row_lens = lengths(x[[which_lists[1]]]) 
    
    res[non_lists] = x[rep(seq_len(nrow(x)), row_lens), ..non_lists]
    res[which_lists] = x[, lapply(.SD, function(col) unlist(col, use.names = FALSE)), .SDcols = which_lists]
    
  } else {
    row_lens = x[, vapply(.SD, lengths, seq_len(.N)), .SDcols = which_lists]
  
    if (tot_lists == 2L) { #if tot_lists == 1L, we used !is.cartesian above
      row_reps = row_lens[, 1L] * row_lens[, 2L]
    } else { #cumprod is used to help repeat indices
      row_cum_reps = row_lens
      for (i in seq_len(ncol(row_lens))[-1L]) {
        row_cum_reps[, i] = row_cum_reps[, i - 1L] * row_cum_reps[, i] 
      }
      row_reps = row_cum_reps[, tot_lists]
    }
    
    res[non_lists] = x[rep(seq_len(nrow(x)), row_reps), ..non_lists]
    
    # the first list will always unnest this way
    res[[which_lists[1L]]] = rep(unlist(x[[which_lists[1L]]], use.names = FALSE), rep(row_reps / row_lens[, 1L], row_lens[, 1L]))
    
    # the nth list will always unnest this way
    tmp = x[[which_lists[tot_lists]]]
    res[[which_lists[tot_lists]]] = unlist(tmp[rep(seq_along(row_reps), fcoalesce(row_reps / row_lens[, tot_lists], 0))], use.names = FALSE)
    
    # the lists in between 1L and n have tortured logic
    for (i in seq_len(tot_lists)[-c(1L, tot_lists)]) {
      tmp = x[[which_lists[i]]] 
      
      ##TODO make this more performant - should be in C / C++ to create the indices faster
      ind = unlist(Map(rep, Map(function(x, y) rep(x, each = y), Map(`+`, c(0, cumsum(row_lens[-nrow(row_lens), i])), lapply(row_lens[, i], seq_len)), row_cum_reps[, tot_lists] / row_cum_reps[, i]), row_cum_reps[, i - 1L]), use.names = FALSE)
      res[[which_lists[i]]] = unlist(tmp)[ind]
    }
  }
  as.data.table(res)
}

