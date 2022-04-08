#' Formatear columnas por su nombre
#'
#' @param objeto                Objeto a formatear
#' @param metodo                character. Tipo de objeto
#' @param columnas_logicas      character. vector de nombres columnas TRUE/FALSE
#' @param columnas_fechas       character. vector de nombres columnas fecha 
#' @param columnas_numerosID    character. vector de nombres columnas enteros sin punto
#' @param columnas_enteros      character. vector de nombres columnas enteros
#' @param columnas_numeros2dec  character. vector de nombres columnas reales 2 dec
#' @param columnas_numeros3dec  character. vector de nombres columnas reales 3 dec
#' @param columnas_pesos0dec    character. vector de nombres columnas pesos 2 dec
#' @param columnas_pesos2dec    character. vector de nombres columnas pesos 3 dec
#' @param columnas_texto        character. vector de nombres columnas texto
#' @param formatos_custom       character. vector de nombres columnas otro formato
#' @param ...                   listado de parámetros adicionales
#' 
#' @description 
#' !Objeto refiere a un tipo deobjeto que contenga un data.frame a formatear.
#' Puede ser un objeto DT::datatables, un objeto openxlsx::Worksheet, u otro tipo
#' evaluado por !metodo.
#' Los parámetros !columnas_? determinan el tipo de formato a dar a cada columna.
#' En el argumento !... se pasan los paráetros adicionales que requiera cada tipo
#' de objeto, sobre todo aquellso qu eno pueden recuperar el data.frame embebido,
#' como es el caso de openxlsx, que debe indicarse:
#' hoja_wb: nombre de l ahoja en el objeto Workbook
#' fila_titulos_wb: fila número donde están los títulos en el objeto Workbook
#' filas_wb: filas del data.frame
#' columnas_wb: propiedad names del data.frame
#' hoja_estilos_wb
#' 
#' @return
#' @export
#'
#' @examples
formatear_columnas <- function(
  objeto,
  metodo,
  columnas_logicas,
  columnas_fechas,
  columnas_enteros,
  columnas_numeros2dec,
  columnas_numeros3dec,
  columnas_numerosID,
  columnas_pesos0dec,
  columnas_pesos2dec,
  columnas_texto,
  formatos_custom,
  ...
){

  args <- list(...)
  metodos <- c("DT", "openxlsx", "ninguno")
  metodo <- match.arg(metodo, metodos)

  if(is_missing(columnas_logicas)){
    columnas_logicas <- c(
      "BIT_INTEROCURRENCIA", "CMA", "ESTATAL", "INCIDENCIA_GRATUIDAD_LETRADOS",
      "MODIFICADO", "MPYME", "MULTIPLICIDAD", "PCIA_JUDICIALIZADA",
      "REGION_JUDICIAL", "SUC_JUDICIAL", "ULTIMO_JUICIO"
    )
  } 

  if(is_missing(columnas_fechas)){
    columnas_fechas <- c(
      "F_ACC", "F_ALTA", "F_BAJA", "F_CIERRE_JUICIO", "F_DEMANDA_JUICIO",
      "F_FINALIZACION_JUICIO", "F_GRAB", "F_INGRESO", "F_NOTIFICACION_JUICIO",
      "FECHA_ALTA_UC", "INIVIG", "LEY_ADHESION_27348", "RENOVACION", "RESCISION",
      "ULTIMO_ATO", "VIGENCIA_ADHESION_27348", "VTO"
    )
  } 

  if(is_missing(columnas_enteros)){
    columnas_enteros <- c(
      "CAPITAS",
      "DIAS", "DIAS_CB", "DIAS_GR",
      "Q_MESES", "SINIESTROS_GRAVES_MDO", "SINIESTROS_MDO",
      "TRABAJADORES", "TRABAJADORES_MES_MDO", "TRABAJADORESDOM", "TRABAJADORESRECT",
      "TRABAJADORESPROM", "TRABAJADORESMESsinDOM",
      "C_VIGENTES_DOM", "C_ALTAS_EMI", "C_ALTAS_DOM",
      "C_ALTAS_DIR", "C_ALTAS_TIN", "C_ALTAS_OTRAS",
      "C_ALTAS_PROP", "C_REASIGNADOS_SUC", "C_REASIGNADOS_DOM_SUC",
      "C_SALDO_ACREEDOR", "C_VIGENTES", "T_ALTAS_EMI", "T_ALTAS_DOM",
      "T_ALTAS_DIR", "T_ALTAS_TIN", "T_ALTAS_OTRAS",
      "T_MOVS_sinDOM", "T_MOVS_DOM", "T_REASIGNADOS_SUC",
      "T_REASIGNADOS_DOM_SUC", "T_REG_MPYME", "T_SALDO_ACREEDOR",
      "T_SALDO_ACREEDOR_DOM", "T_ERROR",
      "C_BAJAS_EMI","C_BAJAS_DOM","C_BAJAS_TOUT",
      "C_BAJAS_FP_OTRAS","C_BAJAS_NT_ERROR","C_BAJAS_TEMPRANAS",
      "C_BAJAS_PROP","T_BAJAS_EMI","T_BAJAS_DOM",
      "T_BAJAS_TOUT","T_BAJAS_FP_OTRAS","T_BAJAS_NT_ERROR"
    )
  } 
  
  if(is_missing(columnas_numeros2dec)){
    columnas_numeros2dec <- c(
      "ALIC_MANUAL", "ALIC_MDO", "CxT%",
      "ATO%", "REB%",	"ATO_NETO",	"EFDAD%",
      "C.Mg%", "CONTR_MARGINAL",
      "COMI%", "COMI_TASA",
      "FRSA_GRAVES_MDO", "FRSA_MANUAL", "FRSA_MDO",
      "HS_ASESOR", "HS_LETRADOS",
      "INCOB%",
      "LATITUD", "LONGITUD",
      "PORINC", "PUNTOS_EXTRA_PERITOS",
      "SDAD%",
      "VBLE", "VBLEC"
    )
  } 

  if(is_missing(columnas_numeros3dec)){
    columnas_numeros3dec <- c(
      "FIJA", "FIJAC", "PROBAJUI_ULT"
    )
  } 

  if(is_missing(columnas_numerosID)){
    columnas_numerosID <- c(
      "AÑO", "ART_IN", "ART_OUT", "CIIU_R2", "CIIU_R2_1d", "CIIU_R2_2d",
      "CIIU_R2_3d", "CIIU_R3", "CIIU_R4", "COD_ASO", "CONTRATO", "DENUNCIA",
      "EC", "EDAD", "EDOCONS", "ESTADO", "ESTADO_STRO", "ESTADO_UC", "GR_CIIU_R2_2d", "ID",
      "ID_MOTIVO_RECT", "ID_UART", "INSTANCIA", "INTEGRANTE_N1_ID",
      "INTEGRANTE_N1_ROLID", "INTEGRANTE_N1_TIPOCANALID", "INTEGRANTE_N2_ID",
      "INTEGRANTE_N2_ROLID", "INTEGRANTE_N2_TIPOCANALID", "INTEGRANTE_N3_ID",
      "INTEGRANTE_N3_ROLID", "INTEGRANTE_N3_TIPOCANALID", "JUI", "JUZGADO_ID",
      "MES", "MES_FINALIZACION_JUICIO", "MES_NOTIFICACION_JUICIO", "MESACC",
      "MESATO", "MESATOCOMU", "MESCOMU", "MESEMI", "MESGRAB", "MESLIQ", "MESREC",
      "MESRECTIFICATIVA", "MOTIVO_CIERRE_JUICIO", "MOTIVO_FINALIZACION_JUICIO",
      "NIVEL_UC", "PAS", "PAS_ROL", "PCIA", "PCIA_JUZGADO", "PCIA_TDOR", "PER",
      "PERIODO", "PERSONA", "PRODUCTOR", "REG_ID", "REG_UC", "REGION_INDEC",
      "ROL_ID", "SEG", "SEG_ASO", "SEGMENTO_UART", "SUC_ID", "SUC_UC", "SUCRECT",
      "TERMINACION", "TIPO_CANAL", "TIPO_ID", "TRABAJADOR", "UC"
    )
  } 

  if(is_missing(columnas_pesos2dec)){
    columnas_pesos2dec <- c(
      "COMI", "COMI_r", "COMISIONES", "COMIRECT",
      "CUOTA_PACTADA_MDO",
      "ESP_LIQ", "ESP_LIQ_0", "ESP_RVA", "ESP_INC", "ESP_INC", "ESP_ULT",
      "ILT_LIQ", "ILT_LIQ_0", "ILT_RVA", "ILT_INC", "ILT_INC", "ILT_ULT",
      "IND_LIQ", "IND_LIQ_0", "IND_RVA", "IND_INC", "IND_INC", "IND_ULT",
      "ILP_LIQ", "ILP_LIQ_0", "ILP_RVA", "ILP_INC", "ILP_INC", "ILP_ULT",
      "JUI_LIQ", "JUI_LIQ_0", "JUI_RVA", "JUI_INC", "JUI_INC", "JUI_ULT",
      "TOT_LIQ", "TOT_LIQ_0", "TOT_RVA", "TOT_INC", "TOT_INC", "TOT_ULT",
      "ILT_IBNR",	"ESP_IBNR",	"ILP_IBNR",	"IND_IBNR",	"JUI_IBNR", "TOT_IBNR",
      "ILT_IBNER",	"ESP_IBNER", "ILP_IBNER", "IND_IBNER", "JUI_IBNER", "TOT_IBNER",
      "VAR_ILT_RVA", "VAR_ESP_RVA",	"VAR_IND_RVA", "VAR_JUI_RVA",
      "ILT_DEV", "ESP_DEV",	"IND_DEV", "JUI_DEV",	"TOT_DEV",
      "FFEE_REC", "IBM",
      "MASA", "MASA_MDO", "MASARECT",
      "MONTO_DEMANDA",
      "PREMIO", "PREMIODOM", "PREMIORECT",
      "PRIMA", "PRIMA_r","PRIMA_CALCULO", "PRIMADOM", "PRIMARECT",
      "ATO_COMU",	"ATO", "REB",	"ATO_NETO",
      "RDO_TECNICO", "RDO"
    )
  } 

  if(is_missing(columnas_pesos0dec)){
    columnas_pesos0dec <- c(
      "M_ALTAS_DIR", "M_ALTAS_DIR_prop", "M_ALTAS_EMI_prop", "M_ALTAS_OTRAS",
      "M_ALTAS_OTRAS_prop", "M_ALTAS_TIN", "M_ALTAS_TIN_prop", "M_BAJAS_EMI",
      "M_BAJAS_EMI_prop", "M_BAJAS_FP_OTRAS", "M_BAJAS_FP_OTRAS_prop",
      "M_BAJAS_NT_ERROR", "M_BAJAS_NT_ERROR_prop", "M_BAJAS_TOUT",
      "M_BAJAS_TOUT_prop", "M_ERROR", "M_EST", "M_EST.ant", "M_EST_prop",
      "M_EST_prop.ant", "M_MOVS", "M_MOVS_prop", "M_REASIGNADOS_SUC",
      "M_REG_MPYME", "M_REG_MPYME_prop", "M_SALDO_ACREEDOR", "M_VSAL",
      "M_VSAL_prop", "P_ALIC_DOM", "P_ALTAS_DIR", "P_ALTAS_DOM", "P_ALTAS_EMI",
      "P_ALTAS_OTRAS", "P_ALTAS_TIN", "P_ATO", "P_BAJAS_DOM", "P_BAJAS_EMI",
      "P_BAJAS_FP_OTRAS", "P_BAJAS_NT_ERROR", "P_BAJAS_TOUT", "P_DOM",
      "P_ERROR", "P_EST", "P_EST.ant", "P_MOVS_DOM", "P_MOVS_sinDOM",
      "P_OTROS", "P_REASIGNADOS_DOM_SUC", "P_REASIGNADOS_SUC", "P_REB",
      "P_REG_MPYME", "P_SALDO_ACREEDOR", "P_SALDO_ACREEDOR_DOM", "P_VSAL",
      "PREMIO_MENSUAL", "PREMIO_MENSUAL.ant", "SALARIO", "SALARIO_CLASE"
    )
  } 

  if(is_missing(columnas_texto)){
    columnas_texto <- c(
      "CARATULA", "CIE10", "CIIU_R2_1d_DESC", "CIIU_R2_2d_DESC", "CIIU_R2_3d_DESC",
      "CIIU_R2_DESC", "CLASE", "CLASE1", "CLASE1_ETIQUETA", "CLASE3", "CLASE3_ETIQUETA",
      "CLIENTE", "CP", "CUIL", "CUIT", "DEB_O_CRED", "DESCRIPCION", "DESCRIPCION COD3",
      "DETALLE_MOTIVO", "DETALLE_MOTIVO_ALTA", "EJECUTIVO", "ESTADO_JUICIO", "FILE",
      "GR_CIIU_R2_2d_DESCR", "GRUPO_1", "GRUPO_2", "GRUPO_CANAL", "GRUPO_EC",
      "INTEGRANTE_N1_CUIT", "INTEGRANTE_N1_NOMBRE", "INTEGRANTE_N1_ROL",
      "INTEGRANTE_N1_TIPOCANAL", "INTEGRANTE_N2_CUIT", "INTEGRANTE_N2_NOMBRE",
      "INTEGRANTE_N2_ROL", "INTEGRANTE_N2_TIPOCANAL", "INTEGRANTE_N3_CUIT",
      "INTEGRANTE_N3_NOMBRE", "INTEGRANTE_N3_ROL", "INTEGRANTE_N3_TIPOCANAL",
      "JUICIO", "LETRADO_ACTOR", "LETRADO_CIA", "MODALIDAD", "MOTIVO", "MOTIVO_ALTA",
      "PAS_CUIT", "PAS_NOMBRE", "PROVINCIA", "RALLY", "REG", "REGION", "RESPONSABLE",
      "ROL", "SEXO", "SINIESTRO", "SUBREGION", "SUBTIPO_CANAL", "SUC", "SUC_TIPO",
      "SUCURSAL", "SUCURSAL_UC", "TIPO", "TIPO_JUICIO", "TIPO_TASA_JUDICIAL",
      "UNIDAD_COMERCIAL"
    )
  } 

  if(metodo == "DT"){
    
    library(DT)

    if(
      !inherits_any(objeto, "datatables")
    ) stop(
      "formatear_columnas: el objeto no es de clase DT"
    )

    columnas_logicas <- which(names(objeto$x$data) %in% columnas_logicas)
    columnas_fechas <- which(names(objeto$x$data) %in% columnas_fechas)
    columnas_enteros <- which(names(objeto$x$data) %in% columnas_enteros)
    columnas_numeros2dec <- which(names(objeto$x$data) %in% columnas_numeros2dec)
    columnas_numeros3dec <- which(names(objeto$x$data) %in% columnas_numeros3dec)
    columnas_numerosID <- which(names(objeto$x$data) %in% columnas_numerosID)
    columnas_pesos0dec <- which(names(objeto$x$data) %in% columnas_pesos0dec)
    columnas_pesos2dec <- which(names(objeto$x$data) %in% columnas_pesos2dec)
    columnas_texto <- which(names(objeto$x$data) %in% columnas_texto)   
        
    #objeto <- datatable(objeto, ...)
    if(length(columnas_fechas != 0))
      objeto <- formatDate(objeto, columnas_fechas, "toLocaleDateString")
    if(length(columnas_enteros != 0))
      objeto <- formatRound(objeto, columnas_enteros, digits = 0)
    if(length(columnas_numeros2dec != 0))
      objeto <- formatRound(objeto, columnas_numeros2dec, digits = 0)
    if(length(columnas_numeros3dec != 0))
      objeto <- formatRound(objeto, columnas_numeros3dec, digits = 0)
    if(length(columnas_numerosID != 0))
      objeto <- formatRound(objeto, columnas_numerosID, digits = 0, interval = 0)
    if(length(columnas_pesos0dec != 0))
      objeto <- formatCurrency(objeto, columnas_pesos0dec, currency = "$", digits = 0)
    if(length(columnas_pesos2dec != 0)){
      objeto <- formatCurrency(
        objeto, columnas_pesos2dec, 
        currency = '\UFF04', digits = 2
      )
    }

  }

  if(metodo == "openxlsx"){
    
    library(openxlsx)

    if(
      !inherits_any(objeto, "Workbook")
    ) stop(
      "formatear_columnas: el objeto no es de clase DT"
    )

    if(!"hoja" %in% names(args)) stop(
      "formatear_columnas: falta el parámetro hoja"
    )

    if(!"columnas_wb" %in% names(args)) stop(
      glue(
        "formatear_columnas: falta indicar los nombres de columnas con columnas_wb. ",
        "Es necesario cuando se trata de un objeto openxlsx::Workbook"
      )
    )
    
    if(!"filas_wb" %in% names(args)) stop(
      "formatear_columnas: falta indicar el número de filas con filas_wb. ",
      "Es necesario cuando se trata de un objeto openxlsx::Workbook"
    )
    
    if(!"fila_titulos_wb" %in% names(args)){
      args$fila_titulos_wb <- 1L
    } 
    
    if(!"hoja_estilos" %in% names(args)) stop(
      "formatear_columnas: falta indicar hoja de estilos hoja_estilos"
    )

    columnas_logicas <- which(args$columnas_wb %in% columnas_logicas)
    columnas_fechas <- which(args$columnas_wb %in% columnas_fechas)
    columnas_numerosID <- which(args$columnas_wb %in% columnas_numerosID)
    columnas_enteros <- which(args$columnas_wb %in% columnas_enteros)
    columnas_numeros2dec <- which(args$columnas_wb %in% columnas_numeros2dec)
    columnas_numeros3dec <- which(args$columnas_wb %in% columnas_numeros3dec)
    columnas_pesos0dec <- which(args$columnas_wb %in% columnas_pesos0dec)
    columnas_pesos2dec <- which(args$columnas_wb %in% columnas_pesos2dec)
    columnas_texto <- which(args$columnas_wb %in% columnas_texto)   

    # if(length(columnas_fechas != 0))
    # addStyle (wb, hoja, hoja_estilos$fechas, rows = (fila + 1):(fila + n),
    #   cols = columnas_fechas, gridExpand = TRUE, stack = TRUE)

    if(length(columnas_numerosID != 0))
      addStyle(
        objeto, args$hoja, 
        rows = (args$fila_titulos_wb + 1):(args$fila_titulos_wb + args$filas_wb),
        cols = columnas_numerosID, gridExpand = TRUE, 
        args$hoja_estilos$num, stack = TRUE
      )
    
    if(length(columnas_enteros != 0))
      addStyle(
        objeto, args$hoja, 
        rows = (args$fila_titulos_wb + 1):(args$fila_titulos_wb + args$filas_wb), 
        cols = columnas_enteros, gridExpand = TRUE, 
        args$hoja_estilos$dec0, 
        stack = TRUE
      )
    
    if(length(columnas_numeros2dec != 0))
      addStyle(
        objeto, args$hoja, 
        rows = (args$fila_titulos_wb + 1):(args$fila_titulos_wb + args$filas_wb), 
        cols = columnas_numeros2dec, gridExpand = TRUE, 
        args$hoja_estilos$dec2, 
        stack = TRUE
      )
    
    if(length(columnas_numeros3dec != 0))
      addStyle(
        objeto, args$hoja, 
        rows = (args$fila_titulos_wb + 1):(args$fila_titulos_wb + args$filas_wb), 
        cols = columnas_numeros3dec, gridExpand = TRUE, 
        args$hoja_estilos$dec3, 
        stack = TRUE
      )
    
    if(length(columnas_pesos0dec != 0))
      addStyle(
        objeto, args$hoja, 
        rows = (args$fila_titulos_wb + 1):(args$fila_titulos_wb + args$filas_wb), 
        cols = columnas_pesos0dec, gridExpand = TRUE, 
        args$hoja_estilos$peso0, 
        stack = TRUE
      )
    
    if(length(columnas_pesos2dec != 0))
      addStyle(
        objeto, args$hoja, 
        rows = (args$fila_titulos_wb + 1):(args$fila_titulos_wb + args$filas_wb), 
        cols = columnas_pesos2dec, gridExpand = TRUE, 
        args$hoja_estilos$peso2, 
        stack = TRUE
      )
    
    if(length(columnas_texto != 0))
      addStyle(
        objeto, args$hoja, 
        rows = (args$fila_titulos_wb + 1):(args$fila_titulos_wb + args$filas_wb), 
        cols = columnas_texto, gridExpand = TRUE, 
        args$hoja_estilos$txt, 
        stack = TRUE
      )
    
  }  
  
  return(objeto)

}
