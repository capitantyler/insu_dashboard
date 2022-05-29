#' EMISION CON MOVIMIENTOS
#'
#' Retorna un listado de reportes de emisión. Altas, Bajas, Movimientos,
#' Aumentos, Rebajas y cambios de régimen (MPymes).
#' Incorporación Rectificativas. Separación de trabajadores domésticos.
#' Calcula cobranzas y comisiones adicionales.
#'
#' @param reportes
#' @param emiorig
#' @param rectificativa
#' @param domesticas
#' @param comisiones
#' @param contratos
#' @param filtroContratos
#' @param factorContrato
#' @param mesMin
#' @param mesMax
#' @param modoRectificativa
#' @param cierreDeMes  integer, nro de mes en que se hace el rolling sum.
#'                     para ejercicio económico, usar 6 (junio)
#' @param EliminarMesesCon0Tr
#'
#' @details
#' Reportes:
#' /itemize{
#' /item rpt1 detalle de emisión por mes
#' /item rpt2 emisión anual básico
#' /item rpt4 abm premio mensual
#' /item rpt5 abm premio anual
#' }
#'
#'
#' @note
#' Inicio: 09/02/2018
#' Si hay problemas de memoriam aumentarla con utils::memory.limit(32000)
#'
#' @return data.frame
#' /itemize{
#' /item rpt1
#' /item rpt2
#' /item rpt3
#' /item rpt4
#' /item rpt5
#' /item rpt6
#' }
#'
#' @export
#'
#' @author Esteban Cervetto (/email{ecervetto@@asociart.com.ar},
#' /email{estebancster@@gmail.com})
#' @examples
#'
#' @notes  TODO: factorContrato no tiene definido cómo devolver los reportes si
#' es distinto de "CONTRATO"
#'

# library(profvis)
# profvis(prof_output = "emision.Movimientos.RProf", split = "h", {


EmisionConMovimientos <- function(
  reportes = NULL,
  emiorig,
  rectificativa = NULL,
  domesticas = NULL,
  comisiones = NULL,
  cobranza = NULL,
  contratos,
  sucursal,
  filtroContratos = NULL,
  factorContrato = "CONTRATO",
  mesMin = 201207,
  mesMax = 209901,
  modoRectificativa = NULL,
  cierreDeMes = 12L,
  EliminarMesesCon0Tr = FALSE
){

  ## Librerías ----

  library(data.table, quietly = TRUE)
  library(rlang, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(tidyr, quietly = TRUE)
  library(dplyr, quietly = TRUE)

  ## Parámetros ----
  tolerancia <- 0.05  #tolerancia al error de prima calculada

  ## Origenes de datos ----

  emiorig.colfinales <- c(
    "CONTRATO", "SUC", "MES", "MPYME", "TIPO",
    "FIJA", "VBLE", "TRABAJADORES", "MASA", "PRIMA",
    "PREMIO", "COMI"
  )

  rectificativa.colfinales <- c(
    "CONTRATO", "SUCRECT", "MESEMI", "MESRECTIFICATIVA",
    "TRABAJADORESRECT", "MASARECT", "PRIMARECT", "PREMIORECT",
    "COMIRECT"
  )

  domesticas.colfinales <- c(
    "CONTRATO", "MES",
    "TRABAJADORESDOM", "PRIMADOM",
    "PREMIODOM"
  )

  cobranza.colfinales <- c(
    "CONTRATO", "MESEMI", "MESREC", "CONCEPTO", "COBRANZA", "FFEE_REC"
  )

  contratos.colfinales <- c(
    "CONTRATO", "CUIT", "CLIENTE", "GRUPO_EC", "CLASE",
    "INIVIG", "RESCISION", "ESTADO", "ESTATAL",
    "SUC", "REG", "PCIA", "UC", "EC",
    "CIIU_R2", "CIIU_R3", "CIIU_R4",
    "MOTIVO", "DETALLE_MOTIVO", "MOTIVO_ALTA", "DETALLE_MOTIVO_ALTA"
  )

  sucursales.colfinales <- c(
    "SUC_ID", "REG_ID"
  )

  comisiones.colfinales <- c(
    "CONTRATO", "MES", "ADICIONAL"
  )

  ## funciones ----

  source(paste0(repositorio[["misc"]], "funciones_comunes.R"))
  if(!exists("AAAAMM_diferido", mode = "function"))
    source(paste0(repositorio[["fechas"]], "AAAAMM_diferido.R"))
  if(!exists("AAAAMM_ejercicio", mode = "function"))
    source(paste0(repositorio[["fechas"]], "AAAAMM_ejercicio.R"))
  if(!exists("proporcion_mes", mode = "function"))
    source(paste0(repositorio[["fechas"]], "proporcion_mes.R"))
  if(!exists("factor_estacionalidad", mode = "function"))
    source(paste0(repositorio[["emision"]], "factor_estacionalidad.R"))

  ## Parámetros por default ----

  # controlando el cierre de mes sea nro entre 1 y 12
  if (cierreDeMes > 12L) {
    stop(
      paste0("El argumento cierreDeMes debe ser un mes entre 1 y 12")
    )
  }

  ## bases ----

  ## ** Filtro Contratos ----
  if (!is.null(filtroContratos)) {
    contratos <- contratos[
      which(contratos$CONTRATO %in% filtroContratos), ]
    emiorig <- emiorig[
      which(emiorig$CONTRATO %in% filtroContratos), ]
    domesticas <- domesticas[
      which(domesticas$CONTRATO %in% filtroContratos), ]
  }

  ## ** sucursales y regiones ----
  if (length(setdiff(sucursales.colfinales, names(sucursal))) > 0) {
    stop(
      paste0(c(
        "El argumento sucursal no tiene la/s columna/s",
        setdiff(sucursales.colfinales, names(sucursal))
      ), collapse = " ")
    )
  }
  sucursal <- sucursal %>%
    select(
      all_of(sucursales.colfinales)
    ) %>%
    rename(
      SUC = SUC_ID,
      REG = REG_ID
    )

  ## ** emision original ----
  if (length(setdiff(emiorig.colfinales, names(emiorig))) > 0) {
    stop(
      paste0(c(
        "El argumento emiorig no tiene la/s columna/s",
        setdiff(emiorig.colfinales, names(emiorig))
      ), collapse = " ")
    )
  }
  emiorig <- select(emiorig, all_of(emiorig.colfinales))

  # Filtro meses. Se toma un mes previo más para obtener métricas del mes anterior
  emiorig <- emiorig %>%
    filter(
      MES >= AAAAMM_diferido(!!mesMin, -1L)
    ) %>% 
    filter(
      MES <= AAAAMM_diferido(!!mesMax, 1L)
    )

  ## ** cobranza ----

  if(
    any(reportes %in% c(1,2,3, "rpt1", "rpt2", "rpt3")) &&
    !is.null(cobranza)
  ){
    if (length(setdiff(cobranza.colfinales, names(cobranza))) > 0) {
      stop(
        paste0(c(
          "El argumento cobranza no tiene la/s columna/s",
          setdiff(cobranza.colfinales, names(cobranza))
        ), collapse = " ")
      )
    }
    cobranza <- select(cobranza, all_of(cobranza.colfinales))


    # Filtro meses de cobranza por mes de emision
    cobranza <- filter(
      cobranza,
      cobranza$MESEMI >= mesMin
      & cobranza$MESEMI <= mesMax
      & cobranza$CONCEPTO == "CP"
    )

    # Filtro Contratos
    if (!is.null(filtroContratos)) {
      cobranza <- cobranza[
        which(cobranza$CONTRATO %in% filtroContratos),
      ]
      # cobranza <- cobranza %>%
      #   filter(
      #     CONTRATO %in% filtroContratos
      #   )
    }
    # agrupo cobranza por mes de emisión
    cobranza <- cobranza %>%
      group_by(CONTRATO, MESEMI) %>% # profvis:cuello de botella
      summarise(
        across(
          c(COBRANZA, FFEE_REC),
          ~sum(.)
        ),
        .groups = "drop"
      )
  }

  ## ** domésticas ----

  if (length(setdiff(domesticas.colfinales, names(domesticas))) > 0) {
    stop(
      paste0(c(
        "El argumento domesticas no tiene la/s columna/s",
        setdiff(domesticas.colfinales, names(domesticas))
      ), collapse = " ")
    )
  }
  domesticas <- select(domesticas, all_of(domesticas.colfinales))


  # Filtro meses. Se toma un mes previo más para obtener métricas del mes anterior
  domesticas <- domesticas %>%
    filter(
      MES >= AAAAMM_diferido(!!mesMin, -1L) &
        MES <= AAAAMM_diferido(!!mesMax, 1L)
    )

  ## ** rectificativas ----
  if(
    any(reportes %in% c(1,2,3, "rpt0", "rpt1", "rpt2", "rpt3"))
  ){
    if(!is.null(rectificativa)){
    
      if (length(setdiff(rectificativa.colfinales, names(rectificativa))) > 0) {
        stop(
          paste0(c(
            "El argumento rectificativa no tiene la/s columna/s",
            setdiff(rectificativa.colfinales, names(rectificativa))
          ), collapse = " ")
        )
      }
      
      if (is.null(modoRectificativa)) {
        warning("modoRectificativa no indicado (A=anticuado , C=contable.
              Utilizando A por default")
        modoRectificativa <- "A"
      } else {
        if (modoRectificativa %!in% c("A", "C")) {
          stop("modoRectificativa incorrecto. Ingrese A=anticuado o C=contable")
        }
      }
  
      rectificativa <- rectificativa %>%
        select(all_of(rectificativa.colfinales))
  
      # Filtro Contratos
      if (!is.null(filtroContratos)) {
        rectificativa <- rectificativa[
          which(rectificativa$CONTRATO %in% filtroContratos),
        ]
      }

      # Filtro meses. Se toma un mes previo más para obtener
      # métricas del mes anterior
      if (modoRectificativa == "A") {
        rectificativa <- rectificativa %>%
          filter(
            MESEMI >= AAAAMM_diferido(!!mesMin, -1L) &
              MESEMI <= AAAAMM_diferido(!!mesMax, 1L)
          )
      } else {
        rectificativa <- rectificativa %>%
          filter(
            MESRECTIFICATIVA >= AAAAMM_diferido(!!mesMin, -1L) &
              MESRECTIFICATIVA <= !!mesMax
            #MESRECTIFICATIVA <= AAAAMM_diferido(!!mesMax, 1L)
          )
      }

      # agrupo según modo seleccionado

      MES_AGRUPACION = ifelse(
        !!modoRectificativa == "A",
        sym("MESEMI"),
        sym("MESRECTIFICATIVA")
      )
  
      rectificativa.agregada <- rectificativa %>%
        mutate(
          MES = !!MES_AGRUPACION
        ) %>%
        group_by(
          CONTRATO, MES
        ) %>%
        summarise(
          across(
            c(TRABAJADORESRECT, MASARECT, PRIMARECT, PREMIORECT, COMIRECT),
            .fns = ~sum(., na.rm = TRUE)
          ),
          .groups = "drop"
        )
    } else {
      warning("los reportes saldrán sin rectificativas")
    }
  }

  ## ** comisiones adicionales ----

  if(
    any(reportes %in% c(1,2,3, "rpt1", "rpt2", "rpt3")) &&
    !is.null(comisiones)
  ){
    if (length(setdiff(comisiones.colfinales, names(comisiones))) > 0) {
      stop(
        paste0(c(
          "El argumento comisiones no tiene la/s columna/s",
          setdiff(comisiones.colfinales, names(comisiones))
        ), collapse = " ")
      )
    }

    comisiones <- select(comisiones, all_of(comisiones.colfinales))

    # Filtro Contratos
    if (!is.null(filtroContratos)) {
      comisiones <- comisiones[
        which(comisiones$CONTRATO %in% filtroContratos),
      ]
    }
  }

  ## ** contratos ----

  if (length(setdiff(contratos.colfinales, names(contratos))) > 0) {
    stop(
      paste0(c(
        "El argumento contratos no tiene la/s columna/s",
        setdiff(contratos.colfinales, names(contratos))
      ), collapse = " ")
    )
  }
  contratos <- select(contratos, all_of(contratos.colfinales))

  # columnas auxiliares
  contratos <- contratos %>%
    mutate(
      # armo CIIU a un dígito para controlar máximo MPYME
      CIIU = CIIU_R2 %/% 100000,
      #máximo MPYME
      # max.MPYME = case_when(
      #   CIIU <= 2 ~ 11
      #   , CIIU == 5 ~ 12
      #   , CIIU %in% c(3, 4, 7) ~ 5
      #   , CIIU %in% c(6, 8, 9) ~ 3
      # ),
      # CIIU = NULL,
      # CIIU_R2 = NULL,
      # AAAAMM
      INIVIG_AAAAMM = year(INIVIG) * 100L + month(INIVIG),
      RESCISION_AAAAMM = year(RESCISION) * 100L + month(RESCISION)
    )

  ## emisión completa ----

  # agrego período anual
  emiorig <- emiorig %>%
    mutate(
      PER = AAAAMM_ejercicio(MES, cierreDeMes)
    )

  # agrego región

  emiorig <- emiorig %>%
    left_join(
      sucursal,
      by = "SUC"
    )

  # emisión + domésticas

  emitotal <- left_join(
    emiorig,
    domesticas,
    by = c("CONTRATO", "MES")
  ) %>%
    mutate(
      across(
        .cols = c(TRABAJADORESDOM),
        .fns = list(~replace_na(.,0L)),
        .names = "{col}"
      ),
      across(
        .cols = c(PREMIODOM, PRIMADOM),
        .fns = list(~replace_na(.,0)),
        .names = "{col}"
      ),
      TRABAJADORESDOM = if_else(
        TIPO == "D",
        TRABAJADORES,
        TRABAJADORESDOM
      ),
      PRIMADOM = if_else(
        TIPO == "D",
        PRIMA,
        PRIMADOM
      ),
      PREMIODOM = if_else(
        TIPO == "D",
        PREMIO,
        PREMIODOM
      ),
      TRABAJADORESsinDOM = TRABAJADORES - TRABAJADORESDOM,
      PRIMAsinDOM = PRIMA - PRIMADOM,
      PREMIOsinDOM = PREMIO - PREMIODOM
    )

  # libero memoria
  rm(list = Filter(exists, c("emiorig", "domesticas", "sucursal")))
  
  # proporcionalidad y desestacionalización
  emitotal <- emitotal %>%
    left_join(
      contratos %>% rename(
        SUCACTUAL = SUC, REGACTUAL = REG
      ),
      by = "CONTRATO"
    ) %>%
    mutate(
      PROP_MES = proporcion_mes(
        mes = ymd(
          MES * 100 + 1
        ),
        inivig = INIVIG,
        rescision = RESCISION
      ),
      # a veces la facturación no viene proporcional y se ajusta con una
      # rectificativa. Verifico con un IF cuáles son esos casos para aplicar
      # PROP_MES = PREMIO_TEORICO / PREMIO
      PREMIOsinDOM_DIFF = PROP_MES * (
        MASA * VBLE / 100 + TRABAJADORESsinDOM * FIJA) - PREMIOsinDOM,
      PROP_MES = if_else(
        abs(PREMIOsinDOM_DIFF) > tolerancia,
        round(
          PREMIOsinDOM / (MASA * VBLE / 100 + TRABAJADORESsinDOM * FIJA),
          4
        ),
        # no pasa si nrow(df) == 0
        # signif(
        #   PREMIOsinDOM / (MASA * VBLE / 100 + TRABAJADORESsinDOM * FIJA),
        #   digits = nchar(
        #     sub('^\\d+\\.', '', sub('0+$', '', as.character(PROP_MES)))
        #   )
        # ),
        PROP_MES
      ),
      # fija de domésticas calculada a mano
      FIJA_DOM = if_else(
        TRABAJADORESDOM * PROP_MES == 0,
        0,
        PREMIODOM / (TRABAJADORESDOM * PROP_MES)
      ),
      # corrección de alícuota FIJA sin domésticas
      FIJA = if_else(
        TRABAJADORESDOM * PROP_MES == 0 |
          abs(PREMIOsinDOM_DIFF) < tolerancia |
          FIJA == 0L,
        FIJA,
        (PREMIO - MASA * PROP_MES * VBLE / 100
         - FIJA_DOM * TRABAJADORESDOM * PROP_MES)/
          (TRABAJADORESsinDOM * PROP_MES)
      ),
      FACTOR_EST = factor_estacionalidad(MES, REG),
      MASA_desest = MASA / FACTOR_EST,
      SALPROM =
        MASA_desest / if_else(   ## condición para casos de masa>0 y trab=0
          TRABAJADORESsinDOM == 0L,
          1L,
          TRABAJADORESsinDOM
        ),
      EST = SALPROM * (FACTOR_EST - 1),
      MASA_EST = MASA_desest * (FACTOR_EST - 1),
      CUOTA = if_else(
        MASA == 0,
        0,
        PREMIOsinDOM / MASA
      ),
      # cálculo de altas y bajas
      # ALTA O BAJA según fechas de INIVIG y RESCISION
      ALTA = (MES == INIVIG_AAAAMM),
      # |(MES == AAAAMM_diferido(INIVIG_AAAAMM, -1L) & ALTA_EMI),
      # BAJA: MES que tiene los datos con que se arma la baja
      BAJA = (RESCISION_AAAAMM != 190001) &
        case_when(
          (DETALLE_MOTIVO == "F") & day(RESCISION) == 1L ~
            (MES == AAAAMM_diferido(RESCISION_AAAAMM, -1L)),
          TRUE ~ (MES == RESCISION_AAAAMM)
        ),
      #mes en el que aplico las bajas
      MES_BAJA = if_else(
        BAJA,    #si hay rescición en el mes
        AAAAMM_diferido(MES, 1L),
        NA_integer_,
        NA_integer_
      ),
      MES_CUBIERTO = if_else(
        MES < INIVIG_AAAAMM |
          MES >= MES_BAJA,
        0L, 1L,
        missing = 1L
      )
    ) %>%
    mutate_at(
      .vars = vars(FIJA_DOM, FIJA, MASA_desest, SALPROM, EST, CUOTA),
      .funs = list(~round(., digits = 3L))
    )

  # emisión + domesticas + rectificativas A o C
  if(
    any(reportes %in% c(1,2,3, "rpt0", "rpt1", "rpt2", "rpt3")) &&
    exists("rectificativa.agregada")
  ){
    emitotal <- left_join(
      emitotal,
      rectificativa.agregada,
      by = c("CONTRATO", "MES")
    ) %>%
      mutate_at(
        .vars = vars(ends_with("RECT")),
        .funs = list(~replace_na(.,0))
      ) %>%
      mutate(
        TRABAJADORES_r = TRABAJADORES + TRABAJADORESRECT,
        MASA_r = MASA + MASARECT,
        PRIMA_r = PRIMA + PRIMARECT,
        PREMIO_r = PREMIO + PREMIORECT,
        COMI_r = COMI + COMIRECT,
        TRABAJADORESRECT = NULL,
        MASARECT = NULL,
        PRIMARECT = NULL,
        PREMIORECT = NULL,
        COMIRECT = NULL,
        AAMMMIN = NULL,
        AAMMMAX = NULL
      )
  }

  # emisión + rectificativas + domésticas + comi adicional
  if(
    any(reportes %in% c(1,2,3, "rpt1", "rpt2", "rpt3")) &&
    !is.null(comisiones)
  ){
    emitotal <- emitotal %>% 
      left_join(
        comisiones,
        by = c("CONTRATO", "MES")
      ) %>%
      mutate(
        ADICIONAL = replace_na(ADICIONAL, 0)
      )
  }

  # emisión + rectificativas + domésticas + comi adicional + cobranza
  if(
    any(reportes %in% c(1,2,3, "rpt1", "rpt2", "rpt3")) &&
    !is.null(cobranza)
  ){
    emitotal <- emitotal %>% 
      left_join(
        cobranza,
        by = c("CONTRATO", "MES" = "MESEMI")
      ) %>%
      mutate(
        COBRANZA = replace_na(COBRANZA, 0) + replace_na(FFEE_REC, 0), 
        FFEE_REC = NULL
      )
  }
  
  # emisión anterior
  if(any(reportes %in% c(4, 5, "rpt4", "rpt5"))){
    emitotal <- emitotal %>%
      left_join(
        emitotal %>%
          mutate(
            MES = AAAAMM_diferido(MES, 1L),
          ) %>%
          select(
            c(
              CONTRATO, MES,
              MPYME, TIPO, SUC, REG, INIVIG, RESCISION,
              PROP_MES, FIJA, VBLE, FIJA_DOM,
              TRABAJADORES, MASA, MASA_desest, PREMIO,
              TRABAJADORESsinDOM, PREMIOsinDOM,
              TRABAJADORESDOM, PREMIODOM,
              SALPROM, EST, MASA_EST, MES_CUBIERTO, PREMIOsinDOM_DIFF
            )
          ) %>%
          mutate( # alta de la emisión, paso 1 del cálculo
            ALTA_EMI = FALSE
          ),
        by = c("CONTRATO", "MES"),
        suffix = c("", ".ant")
      ) %>%
      mutate(
        across(
          c(
            MES_CUBIERTO.ant,
            TRABAJADORES.ant, TRABAJADORESsinDOM.ant, TRABAJADORESDOM.ant
          ),
          .fns = ~replace_na(., 0L)
        )
      ) %>%
      mutate(
        across(
          c(
            PROP_MES.ant,
            MASA.ant, MASA_desest.ant, SALPROM.ant, EST.ant, MASA_EST.ant,
            PREMIO.ant, PREMIOsinDOM.ant, PREMIODOM.ant, PREMIOsinDOM_DIFF.ant
          ),
          .fns = ~replace_na(., 0)
        )
      ) %>%
      mutate(
        SUC.ant = if_else(is.na(SUC.ant), SUC, SUC.ant)
      ) %>%
      left_join(
        emitotal %>%
          mutate(
            MES = AAAAMM_diferido(MES, -1L)
          ) %>%
          select(
            CONTRATO, MES, SUC
          ) %>%
          mutate( # baja de la emisión, paso 1 del cálculo
            BAJA_EMI = FALSE
          ),
        by = c("CONTRATO", "MES"),
        suffix = c("", ".post")
      ) %>%
      mutate(
        # completo las columnas de alta-baja por emision (paso 2)
        # ALTA_EMI = is.na(ALTA_EMI),
        ALTA_EMI = is.na(ALTA_EMI) & MES != !!mesMin,
        BAJA_EMI = is.na(BAJA_EMI) & MES != !!mesMax
      ) %>%
      filter( # retiro meses adicionales, ya no los necesito
        MES >= !!mesMin &
          MES <= !!mesMax
      ) %>%
      arrange( # ordeno para luego hacer unos fillings
        CONTRATO, MES
      ) %>%
      group_by(
        CONTRATO
      ) %>%
      fill(
        MES_BAJA, .direction = c("down")
      ) %>%
      ungroup()

    # limpio memoria de variables que no se usan más
    quitar_columnas <- c(    ## No sacar variables INIVIG ni RESCISION
      "DETALLE_MOTIVO",
      "INIVIG_AAAAMM", "RESCISION_AAAAMM",
      "CUOTA", "PRIMAsinDOM"
    )
    quitar_columnas <- quitar_columnas[
      (quitar_columnas %in% factorContrato) == FALSE
    ]

    emitotal <- emitotal %>%
      select(
        -one_of(quitar_columnas)
      )

    # Cálculo de Aumentos y Rebajas
    emitotal <- as.data.table(emitotal)
    emitotal[,
      ATO := replace_na (
      (emitotal$VBLE / emitotal$VBLE.ant - 1) * 100, 0)
    ]
    emitotal[, ATO.tipo := "NAP"]
    emitotal[
      which(
        (ATO > 0 | ATO == 0 & FIJA > FIJA.ant)
      ),
      ATO.tipo := "ATO"
    ]
    emitotal[
      which(
        (ATO < 0 | ATO == 0 & FIJA < FIJA.ant)
      ),
      ATO.tipo := "REB"
    ]
    emitotal[
      which(
        MPYME != MPYME.ant &
          MPYME == T
      ),
      ATO.tipo := "MPYMEin"
    ]
    emitotal[
      which(
        MPYME != MPYME.ant &
          MPYME == F
      ),
      ATO.tipo := "MPYMEout"
    ]
    emitotal[
      which(
        (TIPO == "D" | TIPO == "M") & (TIPO.ant == "D" | TIPO.ant == "M") &
          (FIJA_DOM > FIJA_DOM.ant) &
          ATO.tipo != "ATO" & ATO.tipo != "REB"
      ),
      ATO.tipo := "ATODOM"
    ]
    emitotal[
      which(
        (TIPO == "D" | TIPO == "M") & (TIPO.ant == "D" | TIPO.ant == "M") &
          (FIJA_DOM < FIJA_DOM.ant) &
          ATO.tipo != "ATO" & ATO.tipo != "REB"
      ),
      ATO.tipo := "REBDOM"
    ]
    emitotal[
      # se define como mes acreedor todo aquel mes emitido posterior
      # al mes de baja y anterior al mes de inicio de vigencia.
      which(
        MES_CUBIERTO != 1L
      ),
      ATO.tipo := "SALDO_AC"
    ]
  }


  # filtro emisiones con trabajadores rectificados en cero si se solicita

  if (EliminarMesesCon0Tr == T) {
    emitotal <- emitotal[
      which(TRABAJADORES != 0L)
    ]
  }

  # agrego factor de agrupamiento

  if (factorContrato == "CONTRATO") {
    factor <- "CONTRATO"
  } else {
    if (factorContrato != "CONTRATO"){
      factor <- names(factorContrato)[
        grep("CONTRATO", names(factorContrato), invert = T)
      ]

      emitotal <- left_join(
        emitotal,
        factorContrato,
        by = "CONTRATO"
      )
    } else {
      factor <- "CONTRATO"
    }
  }

  factor <- sym(factor)

  ## reportes ----
  ## ** rpt0 ----
  # detalle emisión mensual.

  if (any(c(0, "rpt0") %in% reportes)) {
    rpt0 <- emitotal %>%
      filter( # retiro meses adicionales, ya no los necesito
        MES >= !!mesMin &
          MES <= !!mesMax
      )
  }

  ## ** rpt1 ----
  # detalle emisión mensual con cobranza y adicionales.

  if (any(c(1, "rpt1") %in% reportes)) {
    rpt1 <- emitotal %>%
      filter( # retiro meses adicionales, ya no los necesito
        MES >= !!mesMin &
          MES <= !!mesMax
      ) %>%
      select(CONTRATO, CLIENTE, GRUPO_EC, MES, everything())
  }

  ## ** rpt2 ----
  # emisión anual básico
  if (any(c(2,"rpt2") %in% reportes)) {

    rpt2 <- emitotal %>%
      filter( # retiro meses adicionales, ya no los necesito
        MES >= !!mesMin &
          MES <= !!mesMax
      )
    
    if(
      !is.null(comisiones)
    ){
      # unifico comisiones y comisiones adicionales
      rpt2 <- rpt2 %>% 
        mutate(
          COMI = COMI + ADICIONAL,
          COMI_r = COMI_r + ADICIONAL,
          ADICIONAL = NULL
        )
    }
    
    if(
      !is.null(rectificativa)
    ){
      rpt2 <- rpt2 %>% 
        mutate(
          TRABAJADORESMES_r = TRABAJADORESsinDOM
        )
    }
    
    rpt2 <- rpt2 %>% 
      mutate(
        # duplico algunas variables
        TRABAJADORESMES = TRABAJADORES,
        TRABAJADORESMESsinDOM = TRABAJADORESsinDOM,
      ) %>%
      group_by(
        # cambiar. colocar CLIENTE y GRUPO_EC en función a factor
        !!factor, PER
      ) %>%
      mutate(
        across(
          any_of(c(
            "TRABAJADORESMES", "TRABAJADORESMESsinDOM", "TRABAJADORESMES_r",
            "MASA", "MASA_r", "EST", "MASA_desest",
            "PRIMA", "PRIMA_r",
            "PREMIO", "PREMIO_r",
            "COMI", "COMI_r",
            "COBRANZA"
          )),
          .fns = ~sum(.)
        ),
        MESES = n_distinct(MES)
      ) %>%
      filter(MES == my.max(MES)) %>%
      mutate(
        SALARIO = if_else(
          TRABAJADORESsinDOM == 0,
          0,
          MASA_desest / TRABAJADORESsinDOM
        ),
        TRABAJADORESPROM_xCTO = TRABAJADORESMES / MESES,
        across(
          c(MPYME, FIJA, VBLE),
          .fns = ~last(.)
        )
      ) %>%
      ungroup() %>%
      select(!!factor, PER, MES, MESES, everything())

  }

  ## ** rpt4 ----

  # ABM Premio mensual
  if (any(c(4,"rpt4", 5, "rpt5") %in% reportes)) {

    rpt4_0 <- emitotal %>%
      mutate(
        # exposicion real
        PROP_MES = MES_CUBIERTO * PROP_MES,
        PROP_MES.ant = MES_CUBIERTO * PROP_MES.ant,
        # contratos
        C_VIGENTES = MES_CUBIERTO * as.integer(TIPO != "D"),
        C_VIGENTES_DOM = MES_CUBIERTO * as.integer(TIPO == "D"),
        # altas contratos
        C_ALTAS_EMI = MES_CUBIERTO * as.integer(ALTA_EMI == TRUE),
        C_ALTAS_DOM = MES_CUBIERTO * as.integer(
          TIPO == "D" & ALTA_EMI == TRUE),
        # C_ALTAS_MPYME = MES_CUBIERTO * as.integer(
        #   TIPO != "D" & MPYME == TRUE & ALTA_EMI == TRUE),
        C_ALTAS_DIR = MES_CUBIERTO * as.integer(
          MOTIVO_ALTA == "A1" & ALTA_EMI == TRUE & TIPO != "D"),
        C_ALTAS_TIN = MES_CUBIERTO * as.integer(
          MOTIVO_ALTA == "A2" & ALTA_EMI == TRUE & TIPO != "D"),
        C_ALTAS_OTRAS = C_ALTAS_EMI - C_ALTAS_DOM - C_ALTAS_DIR - C_ALTAS_TIN,
        C_ALTAS_PROP = PROP_MES * (
          C_ALTAS_DOM + C_ALTAS_DIR + C_ALTAS_TIN + C_ALTAS_OTRAS),
        # bajas contratos
        C_BAJAS_EMI = - as.integer(BAJA_EMI == TRUE),
        C_BAJAS_DOM = - MES_CUBIERTO * as.integer(
          TIPO == "D" & BAJA_EMI == TRUE),
        # C_BAJAS_MPYME = - MES_CUBIERTO * as.integer(
        #   TIPO != "D" & MPYME == TRUE & BAJA == TRUE),
        C_BAJAS_TOUT = - MES_CUBIERTO * as.integer(
          MOTIVO == "B2" & BAJA_EMI == TRUE & TIPO != "D"),
        C_BAJAS_FP_OTRAS = - MES_CUBIERTO * as.integer(
          MOTIVO == "B1" & BAJA_EMI == TRUE & TIPO != "D"),
        C_BAJAS_NT_ERROR = C_BAJAS_EMI
        - C_BAJAS_DOM - C_BAJAS_TOUT - C_BAJAS_FP_OTRAS,
        C_BAJAS_TEMPRANAS = - as.integer(BAJA == TRUE & BAJA_EMI == FALSE),
        C_BAJAS_PROP = PROP_MES * (
          C_BAJAS_DOM + C_BAJAS_TOUT + C_BAJAS_FP_OTRAS + C_BAJAS_NT_ERROR),
        # reasignaciones contratos
        C_REASIGNADOS_SUC = MES_CUBIERTO * if_else(
          SUC != SUC.ant & TIPO != "D", 1L, 0L
        ),
        C_REASIGNADOS_DOM_SUC = MES_CUBIERTO * if_else(
          SUC != SUC.ant & TIPO == "D", 1L, 0L
        ),
        # otros movimientos contratos
        C_SALDO_ACREEDOR = (1L - MES_CUBIERTO),
        # altas trabajadores
        T_ALTAS_EMI = TRABAJADORES * C_ALTAS_EMI,
        T_ALTAS_DOM = MES_CUBIERTO * (ALTA_EMI == TRUE) * TRABAJADORESDOM,
        # T_ALTAS_MPYME = TRABAJADORES * C_ALTAS_MPYME,
        T_ALTAS_DIR = MES_CUBIERTO *
          (MOTIVO_ALTA == "A1" & ALTA_EMI == TRUE) * TRABAJADORESsinDOM,
        T_ALTAS_TIN = MES_CUBIERTO *
          (MOTIVO_ALTA == "A2" & ALTA_EMI == TRUE) * TRABAJADORESsinDOM,
        # T_ALTAS_DIR = TRABAJADORES * C_ALTAS_DIR,
        # T_ALTAS_TIN = TRABAJADORES * C_ALTAS_TIN,
        T_ALTAS_OTRAS = T_ALTAS_EMI - T_ALTAS_DOM - T_ALTAS_DIR - T_ALTAS_TIN,
        # bajas trabajadores
        T_BAJAS_EMI = TRABAJADORES * C_BAJAS_EMI,
        T_BAJAS_DOM = - MES_CUBIERTO * (BAJA_EMI == TRUE) * TRABAJADORESDOM,
        T_BAJAS_TOUT = - MES_CUBIERTO *
          (MOTIVO == "B2" & BAJA_EMI == TRUE) * TRABAJADORESsinDOM,
        T_BAJAS_FP_OTRAS = - MES_CUBIERTO *
          (MOTIVO == "B1" & BAJA_EMI == TRUE) * TRABAJADORESsinDOM,
        # T_BAJAS_DOM = TRABAJADORES * C_BAJAS_DOM,
        # T_BAJAS_MPYME = TRABAJADORES * C_BAJAS_MPYME,
        # T_BAJAS_TOUT = TRABAJADORES * C_BAJAS_TOUT,
        # T_BAJAS_FP_OTRAS = TRABAJADORES * C_BAJAS_FP_OTRAS,
        T_BAJAS_NT_ERROR = T_BAJAS_EMI
        - T_BAJAS_DOM - T_BAJAS_TOUT - T_BAJAS_FP_OTRAS,
        # variación de trabajadores
        T_MOVS_sinDOM = MES_CUBIERTO * if_else(
          ALTA_EMI == TRUE,
          0L,
          TRABAJADORESsinDOM - TRABAJADORESsinDOM.ant
        ),
        T_MOVS_DOM = MES_CUBIERTO * if_else(
          ALTA_EMI == TRUE,
          0L,
          TRABAJADORESDOM - TRABAJADORESDOM.ant
        ),
        # reasignaciones trabajadores
        T_REASIGNADOS_SUC = MES_CUBIERTO * if_else(
          SUC != SUC.ant, TRABAJADORESsinDOM, 0L
        ),
        T_REASIGNADOS_DOM_SUC = MES_CUBIERTO * if_else(
          SUC != SUC.ant, TRABAJADORESDOM, 0L
        ),
        # T_REASIGNADOS_SUC = MES_CUBIERTO * if_else(
        #   SUC != SUC.ant, TRABAJADORESsinDOM.ant, 0L
        # ),
        # T_REASIGNADOS_DOM_SUC = MES_CUBIERTO * if_else(
        #   SUC != SUC.ant, TRABAJADORESDOM.ant, 0L
        # ),
        # régimen MPYME
        T_REG_MPYME = MES_CUBIERTO * (T_MOVS_sinDOM + T_MOVS_DOM) *
          case_when(
            ATO.tipo == "MPYMEin" ~ 1L,
            ATO.tipo == "MPYMEout" ~ -1L,
            TRUE ~ 0L
          ),
        # otros movimientos trabajadores
        T_SALDO_ACREEDOR = (1L - MES_CUBIERTO) * (
          TRABAJADORESsinDOM - TRABAJADORESsinDOM.ant
        ),
        T_SALDO_ACREEDOR_DOM = (1L - MES_CUBIERTO) * (
          TRABAJADORESDOM - TRABAJADORESDOM.ant
        ),
        T_ERROR = (
          TRABAJADORES
          - TRABAJADORES.ant
          # ec0: no hay que restar reasignaciones. En este estadío no se agrupa
          # datos. Siempre existen trabajadores anteriores a nivel contrato
          #- (TRABAJADORES.ant - T_REASIGNADOS_DOM_SUC - T_REASIGNADOS_SUC)
          - T_MOVS_sinDOM - T_MOVS_DOM
          - T_ALTAS_DOM - T_ALTAS_TIN - T_ALTAS_DIR - T_ALTAS_OTRAS
          - T_SALDO_ACREEDOR - T_SALDO_ACREEDOR_DOM
        ),
        #- T_SALDO_ACREEDOR,
        #- T_BAJAS_MPYME - T_BAJAS_DOM - T_BAJAS_TOUT - T_BAJAS_FP_OTRAS,
        # altas masa salarial
        M_ALTAS_EMI = MASA_desest * C_ALTAS_EMI,
        M_ALTAS_EMI_prop = M_ALTAS_EMI * PROP_MES,
        M_ALTAS_DIR = MES_CUBIERTO *
          (MOTIVO_ALTA == "A1" & ALTA_EMI == TRUE) * MASA_desest,
        M_ALTAS_DIR_prop = M_ALTAS_DIR * PROP_MES,
        M_ALTAS_TIN = MES_CUBIERTO *
          (MOTIVO_ALTA == "A2" & ALTA_EMI == TRUE) * MASA_desest,
        M_ALTAS_TIN_prop = M_ALTAS_TIN * PROP_MES,
        M_ALTAS_OTRAS = M_ALTAS_EMI - M_ALTAS_DIR - M_ALTAS_TIN,
        M_ALTAS_OTRAS_prop = M_ALTAS_OTRAS * PROP_MES,
        # bajas masa
        M_BAJAS_EMI = C_BAJAS_EMI * MASA, #* MASA_desest,
        M_BAJAS_EMI_prop = M_BAJAS_EMI * PROP_MES,
        M_BAJAS_TOUT = - MES_CUBIERTO *
          (MOTIVO == "B2" & BAJA_EMI == TRUE) * MASA, #* MASA_desest,
        M_BAJAS_TOUT_prop = M_BAJAS_TOUT * PROP_MES,
        M_BAJAS_FP_OTRAS = - MES_CUBIERTO *
          (MOTIVO == "B1" & BAJA_EMI == TRUE) * MASA, #* MASA_desest,
        M_BAJAS_FP_OTRAS_prop = M_BAJAS_FP_OTRAS * PROP_MES,
        M_BAJAS_NT_ERROR = M_BAJAS_EMI - M_BAJAS_TOUT - M_BAJAS_FP_OTRAS,
        M_BAJAS_NT_ERROR_prop = M_BAJAS_NT_ERROR * PROP_MES,
        # movimientos masa
        M_MOVS = T_MOVS_sinDOM * SALPROM, #diferencia de masa directa
        M_MOVS_prop = MES_CUBIERTO * if_else( #diferencia de masa aplicada
          ALTA_EMI == TRUE,
          0,
          TRABAJADORESsinDOM * PROP_MES
          - TRABAJADORESsinDOM.ant * PROP_MES.ant
        ) * SALPROM,
        M_VSAL = MES_CUBIERTO * ( #diferencia salarial
          SALPROM - SALPROM.ant
        ) * TRABAJADORESsinDOM.ant,
        M_VSAL_prop = M_VSAL * PROP_MES.ant, #dif salarial aplicada
        M_EST = MES_CUBIERTO * MASA_EST,
        M_EST.ant = MES_CUBIERTO * MASA_EST.ant,
        M_EST_prop = M_EST * PROP_MES,
        M_EST_prop.ant = M_EST.ant * PROP_MES.ant,
        # reasignaciones masa salarial
        M_REASIGNADOS_SUC = MES_CUBIERTO * if_else(
          SUC != SUC.ant, MASA, 0
        ),
        M_REG_MPYME = MES_CUBIERTO * MASA *
          case_when(
            ATO.tipo == "MPYMEin" ~ 1L,
            ATO.tipo == "MPYMEout" ~ -1L,
            TRUE ~ 0L
          ),
        M_REG_MPYME_prop = M_REG_MPYME * PROP_MES,
        # otros movimientos masa salarial
        M_SALDO_ACREEDOR = (1L - MES_CUBIERTO) * (MASA - MASA.ant),
        M_ERROR = (
          MASA - MASA.ant
          - M_MOVS - M_VSAL
          - (M_EST - M_EST.ant)
          - M_ALTAS_DIR - M_ALTAS_TIN - M_ALTAS_OTRAS
          - M_SALDO_ACREEDOR
        ),
        # altas premio
        P_ALTAS_EMI = M_ALTAS_EMI_prop * VBLE / 100
        + (T_ALTAS_EMI - T_ALTAS_DOM) * FIJA * PROP_MES
        + T_ALTAS_DOM * FIJA_DOM * PROP_MES,
        P_ALTAS_DOM = T_ALTAS_DOM * FIJA_DOM * PROP_MES,
        # P_ALTAS_MPYME = M_ALTAS_MPYME_prop * VBLE / 100
        # + T_ALTAS_MPYME * FIJA * PROP_MES,
        P_ALTAS_DIR = M_ALTAS_DIR_prop * VBLE / 100
        + T_ALTAS_DIR * FIJA * PROP_MES,
        P_ALTAS_TIN = M_ALTAS_TIN_prop * VBLE / 100
        + T_ALTAS_TIN * FIJA * PROP_MES,
        P_ALTAS_OTRAS = P_ALTAS_EMI - P_ALTAS_DOM - P_ALTAS_DIR - P_ALTAS_TIN,
        # bajas premio
        P_BAJAS_EMI = M_BAJAS_EMI_prop * VBLE / 100
        + (T_BAJAS_EMI - T_BAJAS_DOM) * FIJA * PROP_MES
        + T_BAJAS_DOM * FIJA_DOM * PROP_MES,
        P_BAJAS_DOM = T_BAJAS_DOM * FIJA_DOM * PROP_MES,
        P_BAJAS_TOUT = M_BAJAS_TOUT_prop * VBLE / 100
        + T_BAJAS_TOUT * FIJA * PROP_MES,
        P_BAJAS_FP_OTRAS = M_BAJAS_FP_OTRAS_prop * VBLE / 100
        + T_BAJAS_FP_OTRAS * FIJA * PROP_MES,
        P_BAJAS_NT_ERROR = P_BAJAS_EMI
        - P_BAJAS_DOM - P_BAJAS_TOUT - P_BAJAS_FP_OTRAS,
        # movimientos premio
        P_MOVS_DOM = MES_CUBIERTO * FIJA_DOM * if_else(
          ALTA_EMI == TRUE,
          0,
          TRABAJADORESDOM * PROP_MES - TRABAJADORESDOM.ant * PROP_MES.ant
        ),
        P_MOVS_sinDOM = MES_CUBIERTO * if_else(
          ALTA_EMI == TRUE,
          0,
          M_MOVS_prop * VBLE / 100 + (
            TRABAJADORESsinDOM * PROP_MES
            - TRABAJADORESsinDOM.ant * PROP_MES.ant
          ) * FIJA
        ),
        P_VSAL = M_VSAL_prop * VBLE / 100,
        P_EST = M_EST_prop * VBLE / 100,
        P_EST.ant = if_else(
          ALTA_EMI == TRUE,
          0,
          M_EST_prop.ant * VBLE.ant / 100
        ),
        # aumentos de alícuota premio
        P_ALIC_DOM = MES_CUBIERTO * if_else(
          ALTA_EMI == TRUE,
          0,
          (FIJA_DOM - FIJA_DOM.ant) * TRABAJADORESDOM.ant * PROP_MES.ant
        ),
        P_ATO = MES_CUBIERTO * if_else(
          ATO.tipo == "ATO",
          ((VBLE - VBLE.ant) / 100 * SALPROM.ant + (FIJA - FIJA.ant))
          * TRABAJADORESsinDOM.ant * PROP_MES.ant,
          0
        ),
        P_REB = MES_CUBIERTO * if_else(
          ATO.tipo == "REB",
          ((VBLE - VBLE.ant) / 100 * SALPROM.ant + (FIJA - FIJA.ant))
          * TRABAJADORESsinDOM.ant * PROP_MES.ant,
          0
        ),
        # asumimos que la alícuota de MPyme no cambia nunca ni cambia de CIIU.
        # Por lo tanto calculamos sólo por movimiento
        P_REG_MPYME = MES_CUBIERTO * if_else(
          ATO.tipo == "MPYMEout" | ATO.tipo == "MPYMEin",
          ((VBLE - VBLE.ant) / 100 * SALPROM.ant
           + (FIJA - FIJA.ant)) * TRABAJADORESsinDOM.ant * PROP_MES.ant,
          0
        ),
        # reasignaciones premio
        P_REASIGNADOS_SUC = MES_CUBIERTO * if_else(
          SUC != SUC.ant, PREMIOsinDOM, 0
        ),
        P_REASIGNADOS_DOM_SUC = MES_CUBIERTO * if_else(
          SUC != SUC.ant, PREMIODOM, 0
        ),
        # otros movimientos premio
        P_SALDO_ACREEDOR = (1L - MES_CUBIERTO) * (
          PREMIOsinDOM - PREMIOsinDOM.ant
        ),
        P_SALDO_ACREEDOR_DOM = (1L - MES_CUBIERTO) * (PREMIODOM - PREMIODOM.ant),
        P_ERROR = (
          PREMIO - PREMIO.ant
          - P_ALTAS_DOM - P_ALTAS_DIR - P_ALTAS_TIN - P_ALTAS_OTRAS
          - P_MOVS_DOM - P_MOVS_sinDOM - P_VSAL
          - (P_EST - P_EST.ant)
          - P_ALIC_DOM - P_ATO - P_REB - P_REG_MPYME
          - P_SALDO_ACREEDOR - P_SALDO_ACREEDOR_DOM
        )
      )

    if(!!factor != "CONTRATO"){
      rpt4_0 <- rpt4_0 %>%
        group_by(
          !!factor,
          MES, SUC, REG,
          # MES_BAJA,
          FIJA, VBLE, FIJA_DOM
        ) %>%
        summarise(
          across(
            matches(
              "^(PROP_MES|TRABAJADORESsinDOM|TRABAJADORESDOM|MASA|PREMIO|(C|T|M|P)_)"
            ),
            .fns = ~sum(., na.rm = TRUE)
          ),
          .groups = "drop"
        )
    }

    rpt4_0 <- bind_rows(
      rpt4_0 %>% select(
        -matches("BAJAS")
      ),
      rpt4_0 %>%
        filter(
          C_BAJAS_EMI < 0L
        ) %>%
        mutate(
          MES = AAAAMM_diferido(MES, 1L)
        ) %>%
        select(
          as_name(factor),
          MES, PER, SUC, REG, INIVIG, RESCISION,
          matches("BAJAS"),
          TRABAJADORESDOM.ant = TRABAJADORESDOM,
          TRABAJADORESsinDOM.ant = TRABAJADORESsinDOM,
          PREMIOsinDOM.ant = PREMIOsinDOM,
          PREMIODOM.ant = PREMIODOM,
          PREMIO.ant = PREMIO,
          PREMIOsinDOM_DIFF.ant = PREMIOsinDOM_DIFF
        )
    )

    rpt4_0 <- rpt4_0  %>%
      mutate(
        across(
          where(is_integer),
          ~replace_na(., 0L)
        ),
        across(
          where(~is_double(.) & !is.Date(.) ),
          ~replace_na(., 0)
        )
      ) %>%
      arrange(
        CONTRATO, MES
      )

    # agrego columnas adicionales
    #
    if(factor == "CONTRATO"){
      rpt4 <- rpt4_0 %>%
        # este paso no es necesario siendo que ya se joinea contratos al armar emitotal
        select(
          CONTRATO, CLIENTE, GRUPO_EC, REGACTUAL, INIVIG, RESCISION,
          MES, PER, REG, SUC, everything()
        )
    } else {
      rpt4 <- rpt4_0
    }

  }

  ## ** rpt5 ----
  # ABM Premio anual
  if (any(c(5,"rpt5") %in% reportes)) {

    rpt5 <- rpt4_0 %>%
      # quito variables que cambian por mes
      select(
        -MES, -FACTOR_EST, -PROP_MES.ant
      ) %>%
      group_by(
        !!factor, PER
      ) %>%
      mutate_at(
        .vars = vars(
          C_VIGENTES, C_VIGENTES_DOM, P_EST,
          TRABAJADORESsinDOM, TRABAJADORESDOM
        ),
        .funs = list(~last(.))  # SE SUPONE ESTÁ ORDENADO POR MES
      ) %>%
      mutate_at(
        .vars = vars(
          TRABAJADORESDOM.ant, TRABAJADORESsinDOM.ant, P_EST.ant
        ),
        .funs = list(~first(.))  # SE SUPONE ESTÁ ORDENADO POR MES
      ) %>%
      mutate(
        PREMIO_MENSUAL.ant = first(PREMIO.ant),
        PREMIO_MENSUAL = last(PREMIO),
      ) %>%
      mutate(
        across(
          # regex todo lo que comienza con PROP_MES, MASA,
          # PREMIO (y qu eno continpua con _MENSUAL),
          # C_, T_, M_, P_, excepto qu esiga con VIGENTES.
          # Tuve problemas de tiempo de resolución si no uso el motor PERL
          matches(
            "^(PROP_MES|MASA|PREMIO(?!_MENSUAL)|(C|T|M|P)_(?!VIG|EST))", perl = TRUE
          ),
          .fns = ~sum(., na.rm = TRUE),
          .names = "{col}"
        )
      ) %>%
      ungroup() %>%
      distinct(
        !!factor, PER,
        .keep_all = TRUE
      ) %>%
      select(
        !!factor, CONTRATO, CLIENTE, GRUPO_EC, REGACTUAL, PER, everything()
      )
  }

  ## final ----
  out <-  mget(ls(pattern = "rpt[0-9]"))

  return (out)

}
