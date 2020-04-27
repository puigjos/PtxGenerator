

gen_calendar_data <- function(dt_dependencias, id_var, 
                              start_date = Sys.Date()){
  if(!all(c("Dependencias", "Dias") %in% names(dt_dependencias) )){
    stop('Colnames error Dependencias Dias')
  }
  if(!id_var %in% names(dt_dependencias)){
    stop('id_var not are in columns')
  }
  dtx = dt_dependencias
  dtx$end = NA
  if(start_date %in% names(dt_dependencias)){
    dtx$start = dtx[[start_date]]
    dtx$start = as.Date(dtx$start)
  }else{
    dtx$start = NA
    dtx$start[dtx$Dependencias == '-'] = as.character(start_date)
    dtx$start = as.Date(dtx$start)
  }
  
  dtx$end[dtx$Dependencias == '-'] = as.character(bizdays::offset(dtx$start[dtx$Dependencias == '-'], 
                                                     n = dtx$Dias[dtx$Dependencias == '-'], 
                                                     'myCalendar')) 
  dtx$end = as.Date(dtx$end)
  iter = 1
  while(any(is.na(dtx$end))){
    num = sum(is.na(dtx$end))
    print(iter)
    end_iniciatives = dtx[[id_var]][!is.na(dtx$end)]
    
    ini_with_end = dtx[[id_var]][dtx$Dependencias %in% end_iniciatives]
    ini_with_end = ini_with_end[!ini_with_end %in% end_iniciatives]
    
    can_do_ini = c()
    
    for(i in ini_with_end){
      ini_dep = dtx$Dependencias[dtx[[id_var]] == i]
      if(all(ini_dep %in% end_iniciatives)){
        can_do_ini = c(can_do_ini, i)
        max_end = bizdays::offset(max(dtx$end[dtx[[id_var]] %in% ini_dep], 
                                      na.rm = T), n = 1, 'myCalendar')
        dtx$start[dtx[[id_var]] == i] = max_end
        dtx$end[dtx[[id_var]] == i] = bizdays::offset(max_end, 
                                                      n = dtx$Dias[dtx[[id_var]] == i] - 1, 
                                                      'myCalendar')
      }else{
        next
      }
    }
    iter = iter + 1
    
    if(num == sum(is.na(dtx$end))){
      warning('Something does not work')
      break
    }
  }
  dtx = dtx[, c(id_var, 'Dias', 'Dependencias', 'start', 'end')]
  return(dtx)
}
