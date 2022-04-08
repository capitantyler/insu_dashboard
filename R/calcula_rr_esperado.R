# resultado esperado

calcula_rr_esperado <- function(
  emision,
  siniestralidad
){
  out <- tryCatch ({
    
    # posición del reporte
    
    sh <- "tablero"
    xy <- c(colExcel2num('AE'),1)
    cols <- (xy[1]):(xy[1] + length(per))
    colsdata <- (xy[1] + 1):(xy[1] + length(per))
    colwidths <- c(15, rep(12, length(per)))
    
    # datos render variables
    render.data <- list()
    
    #siniestros: cuadro de siniestros ----
    
    render.data$siniestros <- 
      siniestralidad[["rpt4"]] %>%
      group_by(`AÑO`) %>% 
      summarise(
        N = sum(N, na.rm = TRUE),
        GR_66 = sum(GR_66, na.rm = TRUE),
        JU = sum(JU, na.rm = TRUE),
        JN = sum(JN, na.rm = TRUE),
        PORINC_66s = sum(PORINC_66s, na.rm = TRUE),
        EDADs = sum(EDADs, na.rm = TRUE),
        SALARIOs = sum(SALARIOs, na.rm = TRUE)
      ) %>% 
      mutate(
        PORINC = if_else(GR_66 == 0, 0, PORINC_66s / GR_66),
        SALARIO = if_else(N == 0, 0, SALARIOs / N),
        EDAD = if_else(N == 0, 0, EDADs / N)
      )
    
    
    #emision: cuadro de emision ----
    
    render.data$emision <-
      emision[["rpt2"]] %>%
      group_by(`AÑO`) %>% 
      summarise_at(
        .vars = vars(TRABAJADORESMES_r, MASA_r, TRABAJADORESMESsinDOM),
        .funs = list(~sum(., na.rm = TRUE))
      ) %>% 
      mutate( 
        MESES = if_else(`AÑO` == !!per.ultimo, !!mesMax %% 100, 12)
      )
    
    #frecuencias: cuadro de frecuencias ----
    
    render.data$frecuencias <- left_join(
      render.data$emision %>% select(`AÑO`, TRABAJADORESMES_r),
      render.data$siniestros %>% select(`AÑO`, N, JU, JN),
      by = c("AÑO")
    ) 
    
    #siniestralidad: cuadro de siniestralidad ----
    
    render.data$siniestralidad <- siniestralidad[["rpt2"]] %>%
      group_by(`AÑO`) %>% 
      summarise_at(
        .vars = vars(
          ILT_LIQ, ILT_RVA, ILT_IBNR,
          ESP_LIQ, ESP_RVA, ESP_IBNR,
          IND_LIQ, IND_RVA, IND_IBNR, IND_ULT,
          JUI_LIQ, JUI_RVA, JUI_IBNR
        ),
        .funs = list(~sum(., na.rm = TRUE))
      ) 
    
    #rr_esp1: cuadro de variables principales ----
    render.data$rr_esp1 <- render.data$emision %>%
      left_join(
        render.data$siniestros,
        by = "AÑO"
      ) %>% 
      mutate(
        TRAB_PROM = TRABAJADORESMES_r / MESES
      ) %>% 
      select(
        `AÑO`, MESES, TRAB_PROM, PORINC, SALARIO, EDAD
      ) 
    
    #rr_esp2: cuadro de índices de indicencia de siniestros ----
    render.data$rr_esp2 <- render.data$frecuencia %>% 
      mutate(
        FRSA = if_else(TRABAJADORESMES_r == 0, 0, N / TRABAJADORESMES_r * 12),
        FRSA_FINAL = FRSA + if_else(`AÑO` == !!per.ultimo, 0.0007, 0), # corregir 
        FRSA_IBNR = FRSA_FINAL - FRSA
      ) %>% 
      select(
        `AÑO`, FRSA, FRSA_IBNR, FRSA_FINAL
      ) 
    
    #rr_esp3: cuadro de índices de indicencia de juicios ----    
    render.data$rr_esp3 <- render.data$frecuencia %>% 
      mutate( # variables de indicencia judicial
        FRJA = if_else(TRABAJADORESMES_r == 0, 0, JU / TRABAJADORESMES_r * 12),
        FRJN = if_else(TRABAJADORESMES_r == 0, 0, JN / TRABAJADORESMES_r * 12),
        FRJA_FINAL = frja_final(FRJA, FRJN),
        FRJA_IBNR = FRJA_FINAL - FRJA
      ) %>% 
      select(
        `AÑO`, FRJA, FRJN, FRJA_IBNR, FRJA_FINAL
      ) 
    
    #rr_esp4: cuadro prestaciones en especie ----
    render.data$rr_esp4 <- 
      render.data$siniestralidad %>% 
      select(
        `AÑO`, ESP_LIQ, ESP_RVA, ESP_IBNR
      ) %>% 
      inner_join(
        render.data$rr_esp1 %>% select(`AÑO`, MESES, TRAB_PROM, SALARIO), 
        by = "AÑO"
      ) %>% 
      inner_join(
        render.data$rr_esp2 %>% select(`AÑO`, FRSA_FINAL), 
        by = "AÑO"
      ) %>% 
      mutate(
        ESP_ULT = ESP_LIQ + ESP_RVA + ESP_IBNR,
        ESP_ULT = if_else(
          `AÑO` == !!per.ultimo,
          lag(ESP_ULT, order_by = `AÑO`) *
            (TRAB_PROM * FRSA_FINAL * SALARIO) / 
            (lag(TRAB_PROM) * lag(FRSA_FINAL) * lag(SALARIO)) ,
          ESP_ULT
        ),
        ESP_IBNR = ESP_ULT - ESP_LIQ - ESP_RVA
      ) %>% 
      select(
        `AÑO`, ESP_LIQ, ESP_RVA, ESP_IBNR, ESP_ULT
      ) 
    
    #rr_esp5: cuadro prestaciones ILT ----
    render.data$rr_esp5 <- 
      render.data$siniestralidad %>% 
      select(
        `AÑO`, ILT_LIQ, ILT_RVA, ILT_IBNR
      ) %>% 
      inner_join(
        render.data$rr_esp1 %>% select(`AÑO`, MESES, TRAB_PROM, SALARIO), 
        by = "AÑO"
      ) %>% 
      inner_join(
        render.data$rr_esp2 %>% select(`AÑO`, FRSA_FINAL), 
        by = "AÑO"
      ) %>% 
      mutate(
        ILT_ULT = ILT_LIQ + ILT_RVA + ILT_IBNR,
        ILT_ULT = if_else(
          `AÑO` == !!per.ultimo,
          lag(ILT_ULT, order_by = `AÑO`) *
            (TRAB_PROM * FRSA_FINAL * SALARIO) / 
            (lag(TRAB_PROM) * lag(FRSA_FINAL) * lag(SALARIO)) ,
          ILT_ULT
        ),
        ILT_IBNR = ILT_ULT - ILT_LIQ - ILT_RVA
      ) %>% 
      select(
        `AÑO`, ILT_LIQ, ILT_RVA, ILT_IBNR, ILT_ULT
      ) 
    
    
    #rr_esp6: cuadro prestaciones Incapacidades ----
    render.data$rr_esp6 <- 
      render.data$siniestralidad %>% 
      select(
        `AÑO`, IND_LIQ, IND_RVA, IND_ULT
      ) %>% 
      inner_join(
        render.data$siniestros %>% select(`AÑO`, N, GR_66, PORINC, PORINC_66s), 
        by = "AÑO"
      ) %>% 
      inner_join(
        render.data$rr_esp1 %>% select(`AÑO`, MESES), 
        by = "AÑO"
      ) %>% 
      inner_join(
        render.data$rr_esp2 %>% select(`AÑO`, FRSA_FINAL), 
        by = "AÑO"
      ) %>% 
      inner_join(
        render.data$rr_esp3 %>% select(`AÑO`, FRJA_FINAL), 
        by = "AÑO"
      ) %>% 
      mutate(
        # corregir valor medio del punto. contiene fallecimientos y IT
        PORINC = if_else(
          `AÑO` == !!per.ultimo & PORINC == 0,
          lag(PORINC),
          PORINC
        ),
        VALOR_PORINC = if_else(
          `AÑO` == !!per.ultimo,
          if_else(
            PORINC_66s + lag(PORINC_66s) == 0, 0, 
            (IND_ULT + lag(IND_ULT)) / (PORINC_66s + lag(PORINC_66s))
          ),
          if_else(PORINC_66s == 0, 0, IND_ULT / PORINC_66s)
        ),
        FREC_GR = if_else(
          `AÑO` == !!per.ultimo,
          if_else(
            N + lag(N) == 0, 0, 
            ((FRSA_FINAL - FRJA_FINAL) * GR_66 +
               (lag(FRSA_FINAL) - lag(FRJA_FINAL)) * lag(GR_66)) / 
              (N + lag(N))
          ),
          if_else(N == 0, 0, (FRSA_FINAL - FRJA_FINAL) * GR_66 / N)
        ),
        IND_ULT = if_else(
          `AÑO` == !!per.ultimo,
          N * 12 / MESES * FREC_GR * PORINC * VALOR_PORINC,
          IND_ULT
        ),
        IND_IBNR = IND_ULT - IND_LIQ - IND_RVA
      ) %>% 
      select(
        `AÑO`, IND_LIQ, IND_RVA, IND_IBNR, IND_ULT
      ) 
    
    #rr_esp7 cuadro juicios ----
    render.data$rr_esp7 <- 
      render.data$siniestralidad %>% 
      select(
        `AÑO`, JUI_LIQ, JUI_RVA
      ) %>% 
      inner_join(
        render.data$rr_esp1 %>% select(`AÑO`, TRAB_PROM, SALARIO, EDAD, PORINC), 
        by = "AÑO"
      ) %>% 
      inner_join(
        render.data$rr_esp3 %>% select(`AÑO`, FRJA_IBNR), 
        by = "AÑO"
      ) %>% 
      mutate(
        CM_JUICIO = calcula_CM_juicio_f(
          EDAD = EDAD,
          PORINC = PORINC,
          VIB = SALARIO
        ),
        JUI_IBNR = TRAB_PROM * FRJA_IBNR * CM_JUICIO,
        JUI_ULT = JUI_LIQ + JUI_RVA + JUI_IBNR
      ) %>% 
      select(
        `AÑO`, JUI_LIQ, JUI_RVA, JUI_IBNR, JUI_ULT, CM_JUICIO 
      )
    
    
    # traslado medida CM_JUICIO a rr_esp1
    render.data$rr_esp1 <- 
      cbind(render.data$rr_esp1, CM_JUICIO = render.data$rr_esp7$CM_JUICIO)
    render.data$rr_esp7 <-
      select(render.data$rr_esp7, -CM_JUICIO)
    
    # filtro elementos parte del reporte
    rr_elements = which(startsWith(names(render.data), prefix = "rr_esp"))
    
    # modifico caracter+isticas comunes en un loop
    for (i in rr_elements) {
      render.data[[i]] = render.data[[i]] %>%
        complete(`AÑO` = per) %>%
        gather(key = "metrica", value = "valor", -`AÑO`) %>%
        mutate(# orden de las metricas
          metrica = factor(metrica, levels = unique(metrica))) %>%
        spread(key = `AÑO`, value = "valor") 
    }
    
    #armo resumen
    render.data$rr_esp8 <- cbind(
      metrica = c("TOT_LIQ", "TOT_RVA", "TOT_IBNR", "TOT_ULT"),
      render.data$rr_esp4 %>% select(-metrica) + 
        render.data$rr_esp5 %>% select(-metrica) +
        render.data$rr_esp6 %>% select(-metrica) +
        render.data$rr_esp7 %>% select(-metrica) 
    )
    
    # render excel tablero rr_esperado ----
    
    # título principal
    writeData(wb, sh, "RR Esperado", xy = xy, colNames = F)
    writeData(wb, sh, t(per), xy = xy + c(1, 0), colNames = F)
    addStyle(wb, sh, stl$halignCenter, xy[2], colsdata, stack = TRUE)
    addStyle(wb, sh, stl$titulo_tabla_flotante_1, xy[2], colsdata, stack = TRUE)
    addStyle(wb, sh, stl$borde_medio, xy[2], cols, stack = TRUE)
    
    # variables
    f <- 1L
    writeData(wb, sh, "Variables", xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$fuentenegrita, xy[2] + f, 1, stack = TRUE)
    f <- f + 1L
    writeData(wb, sh, render.data$rr_esp1, xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$dec0,
             xy[2] + f - 1L + filanro_metrica(render.data$rr_esp1, "TRAB_PROM"), colsdata,  
             stack = TRUE, gridExpand = TRUE)
    invisible(lapply(
      sapply(list("PORINC", "EDAD"), 
             filanro_metrica, data = render.data$rr_esp1
      ) + xy[2] + f - 1L, 
      addStyle, style = stl$dec2,
      wb = wb, sh = sh, cols = colsdata, stack = TRUE, gridExpand = TRUE
    ))
    invisible(lapply(
      sapply(list("SALARIO", "CM_JUICIO"), 
             filanro_metrica, data = render.data$rr_esp1
      ) + xy[2] + f - 1L, 
      addStyle, style = stl$peso0,
      wb = wb, sh = sh, cols = colsdata, stack = TRUE, gridExpand = TRUE
    ))  
    f <- f + nrow(render.data$rr_esp1)
    # frecuencias
    writeData(wb, sh, "Indice de incidencia", xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$fuentenegrita, xy[2] + f, 1, stack = TRUE)
    f <- f + 1L
    writeData(wb, sh, render.data$rr_esp2, xy = xy + c(0, f), colNames = F)
    invisible(lapply(
      sapply(list("FRSA", "FRSA_IBNR", "FRSA_FINAL"), 
             filanro_metrica, data = render.data$rr_esp2
      ) + xy[2] + f - 1L, 
      addStyle, style = stl$porc2,
      wb = wb, sh = sh, cols = colsdata, stack = TRUE, gridExpand = TRUE
    ))
    f <- f + nrow(render.data$rr_esp2)
    # frecuencias de juicios
    writeData(wb, sh, "Indice de judicialidad", xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$fuentenegrita, xy[2] + f, 1, stack = TRUE)
    f <- f + 1L
    writeData(wb, sh, render.data$rr_esp3, xy = xy + c(0, f), colNames = F)
    invisible(lapply(
      sapply(list("FRJA", "FRJN", "FRJA_IBNR", "FRJA_FINAL"), 
             filanro_metrica, data = render.data$rr_esp3
      ) + xy[2] + f - 1L, 
      addStyle, style = stl$porc2,
      wb = wb, sh = sh, cols = colsdata, stack = TRUE, gridExpand = TRUE
    ))  
    f <- f + nrow(render.data$rr_esp3)
    # siniestralidades
    f <- f + 1L # línea en blanco
    writeData(wb, sh, t(per), xy = xy + c(1, f), colNames = F)
    invisible(sapply(
      c(stl$borde_medio, stl$halignCenter),
      addStyle,
      cols = colsdata, rows = xy[2] + f,
      wb = wb, sh = sh, stack = TRUE, gridExpand = TRUE
    ))
    f <- f + 1L
    # prestaciones en especie
    writeData(wb, sh, "Prestaciones en Especie", xy = xy + c(1, f), colNames = F)
    openxlsx::mergeCells(wb, sh, cols = colsdata, rows = xy[2] + f)
    invisible(sapply(
      c(stl$fuentenegrita, stl$subrayado, 
        stl$borde_doble, stl$halignCenter),
      addStyle,
      cols = colsdata, rows = xy[2] + f,
      wb = wb, sh = sh, stack = TRUE, gridExpand = TRUE
    ))
    f <- f + 1
    writeData(wb, sh, render.data$rr_esp4, xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$colorpeso0,  
             seq.int(xy[2] + f, length.out = nrow(render.data$rr_esp4)), colsdata,
             stack = TRUE, gridExpand = TRUE)
    f <- f + nrow(render.data$rr_esp4)
    # prestaciones por ILT
    writeData(wb, sh, "Prestaciones por ILT", xy = xy + c(1, f), colNames = F)
    openxlsx::mergeCells(wb, sh, cols = colsdata, rows = xy[2] + f)
    invisible(sapply(
      c(stl$fuentenegrita, stl$subrayado, 
        stl$borde_doble, stl$halignCenter),
      addStyle,
      cols = colsdata, rows = xy[2] + f,
      wb = wb, sh = sh, stack = TRUE, gridExpand = TRUE
    ))
    f <- f + 1
    writeData(wb, sh, render.data$rr_esp5, xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$colorpeso0, 
             seq.int(xy[2] + f, length.out = nrow(render.data$rr_esp5)), colsdata, 
             stack = TRUE, gridExpand = TRUE)
    f <- f + nrow(render.data$rr_esp5)
    # prestaciones por Incapacidades
    writeData(wb, sh, "Prestaciones por Incapacidades", xy = xy + c(1, f), colNames = F)
    openxlsx::mergeCells(wb, sh, cols = colsdata, rows = xy[2] + f)
    invisible(sapply(
      c(stl$fuentenegrita, stl$subrayado, 
        stl$borde_doble, stl$halignCenter),
      addStyle,
      cols = colsdata, rows = xy[2] + f,
      wb = wb, sh = sh, stack = TRUE, gridExpand = TRUE
    ))
    f <- f + 1
    writeData(wb, sh, render.data$rr_esp6, xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$colorpeso0,  
             seq.int(xy[2] + f, length.out = nrow(render.data$rr_esp6)), colsdata,
             stack = TRUE, gridExpand = TRUE)
    f <- f + nrow(render.data$rr_esp6)  
    # Judicialidad
    writeData(wb, sh, "Judicialidad", xy = xy + c(1, f), colNames = F)
    openxlsx::mergeCells(wb, sh, cols = colsdata, rows = xy[2] + f)
    invisible(sapply(
      c(stl$fuentenegrita, stl$subrayado, 
        stl$borde_doble, stl$halignCenter),
      addStyle,
      cols = colsdata, rows = xy[2] + f,
      wb = wb, sh = sh, stack = TRUE, gridExpand = TRUE
    ))
    f <- f + 1
    writeData(wb, sh, render.data$rr_esp7, xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$colorpeso0,
             seq.int(xy[2] + f, length.out = nrow(render.data$rr_esp7)), colsdata, 
             stack = TRUE, gridExpand = TRUE)
    f <- f + nrow(render.data$rr_esp7)   
    # Resumen
    writeData(wb, sh, "Resumen", xy = xy + c(1, f), colNames = F)
    openxlsx::mergeCells(wb, sh, cols = colsdata, rows = xy[2] + f)
    invisible(sapply(
      c(stl$fuentenegrita, stl$subrayado, 
        stl$borde_doble, stl$halignCenter),
      addStyle,
      cols = colsdata, rows = xy[2] + f,
      wb = wb, sh = sh, stack = TRUE, gridExpand = TRUE
    ))
    f <- f + 1
    writeData(wb, sh, render.data$rr_esp8, xy = xy + c(0, f), colNames = F)
    addStyle(wb, sh, stl$colorpeso0,
             seq.int(xy[2] + f, length.out = nrow(render.data$rr_esp8)),  colsdata, 
             stack = TRUE, gridExpand = TRUE)
    f <- f + nrow(render.data$rr_esp8)     
    
    # fuente general
    
    addStyle(wb, sh, stl$fuente.calibri8, cols = cols, 
             rows = 1:f, stack = TRUE, gridExpand = TRUE)
    
    setColWidths(wb, sh, cols, colwidths)
    
    # corrijo área de impresión
    if(tipo.reporte == "tarifa") ExcelprintArea(
      wb = wb, sheet = sh, 
      rows = 1:(filas.tot + 1),
      cols = 1:cols[length(cols)]
    )
    
    # render PDF actividades ----
    
    if (armar.PDF) {
      # bla bla
    }
  }
  , error = function (x) {
    message ("Ocurrió un error en calcula_rr_esperado.R: ")
    message (x)
    # retorno un mensaje en caso de error
    return (NA)
  })
  return (out)
  
}

