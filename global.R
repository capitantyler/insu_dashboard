credibilidad_IBNR_5 <- c(1, 1, 0.85, 0.65, 0.4)
metodos_BF <- c("CL", "BF", "BF_modificado")
nota_al_pie_especial <- ""
## opciones x default ----

opciones <- list(

  sin_covid19_default = TRUE,
  modo_moneda_default = "prima actual", #"historica",
  mes_corte_datos_default = mes_corte_datos_max,
  mes_nro_cierre_default = mes_corte_datos_max %% 100,
  metodo_IBNER_default = "BF_modificado",
  moneda_homogenea_default = TRUE,
  inflacion_futura_anual_default = 0,
  siniestralidad_target = c(0.62, 0.56, 0.74, 0.75, 0.69, 0.67)

)

# frecuencia de IBNR de casos
patron_fda <- c(
  1.18128, 1.048189, 1.028861, 1.01834, 1.010708, 1.005068
)
# número de períodos
periodos_n <- 15L


## lista de grupos ----

lista_grupo_tipo <- list(

  opciones = c(
    "contratos",
    "cuit",
    "grupo_ec",
    "sucursales",
    "regiones",
    "provincias",
    "uc",
    "pas",
    "clase_3",
    "clase_1",
    "CIIUR2",
    "CIIUR2_1d",
    "Asociart"
  ),

  opciones_label = c(
    "contratos",
    "CUITs contratos",
    "grupo_ec",
    "sucursal",
    "región",
    "provincia",
    "uc",
    "pas",
    "clase",
    "actividad x20",
    "actividad xCIIU",
    "actividad xCIIU_V1",
    "Asociart"
  ),

  default = "grupo_ec"

)

lista_grupo_id <- list(

  grupo_ec = list(

    choices = contratos.data %>%
      distinct(GRUPO_EC) %>%
      arrange(GRUPO_EC),

    default = "CARGILL"
  ),

  sucursales = list(

    choices = sucursal.data %>%
      filter(SUC_ID %!in% c(99, 999999)) %>%
      arrange(SUC_ID) %>%
      mutate(sucursal = paste(sprintf("%2s", SUC_ID), SUCURSAL, sep = "-")) %>%
      select(SUC_ID, sucursal),

    default = 14
  ),

  regiones = list(

    choices = sucursal.data %>%
      filter(SUC_ID %!in% c(99, 999999)) %>%
      arrange(REG_ID) %>%
      mutate(region = paste(sprintf("%1s", REG_ID), REGION, sep = "-")) %>%
      select(REG_ID, region) %>%
      distinct(),

    default = "0-METPAT"
  ),

  provincias = list(

    choices = provincia.data %>%
      filter(ID %!in% c(0, 25, 99)) %>%
      select(ID, PROVINCIA),

    default = "CAPITAL FEDERAL"
  ),

  CIIUR2 = list(

    choices = CIIU.data %>%
      arrange(CIIU_R2) %>%
      transmute(
        value = CIIU_R2,
        label = paste(sprintf("%2s", CIIU_R2), CIIU_R2_DESC, sep = "-")
      ),
    default = 111147

  ),


  CIIUR2_1d = list(

    choices = CIIU.data %>%
      transmute(
        value = CIIU_R2_1d,
        label = paste(sprintf("%2s", CIIU_R2_1d), CIIU_R2_1d_DESC, sep = "-")
      ) %>%
      distinct() %>%
      arrange(value)
  ),

  clase_1 = list(

    choices = titulo_clase.data %>%
      transmute(
        value = CLASE1,
        label = paste(sprintf("%2s", CLASE1), CLASE1_ETIQUETA, sep = "-")
      ) %>%
      distinct()
  ),

  clase_3 = list(

    choices = titulo_clase.data %>%
      transmute(
        value = CLASE3,
        label = paste(sprintf("%2s", CLASE3), CLASE3_ETIQUETA, sep = "-")
      ) %>%
      distinct()
  ),

  uc = list(

    default = "9944 29913"
  ),

  pas = list(

    default = "30541336288"
  ),

  contratos = list(

    default = "423792"  #"188909 345807"
  ),

  cuit = list(

    default = "30632632645 30613665079"
  )

)

## opciones método IBNER ----

modos_moneda <- list(
  "histórica" = "historica",
  "prima historica, stros actual" = "prima historica",
  "prima y siniestros actual" = "prima actual",
  "prima y siniestros al emitir" = "prima emitida"
)

## opciones método IBNER ----

metodo_IBNER <- list(
  "Salario" = "Salario",
  "BF" = "BF",
  "BF-modificado" = "BF_modificado",
  "CL:Chain_Ladder" = "CL",
  "sin-IBNER" = "NO"
)

js_export <- "
var $table = $('#lista_siniestros').find('table');
var instance = $table.tableExport({
  formats: ['xlsx'],
  exportButtons: false,
  filename: 'myTable',
  sheetname: 'Sheet1'
});
var exportData0 = instance.getExportData();
var exportData = exportData0[Object.keys(exportData0)[0]]['xlsx'];
instance.export2file(exportData.data, exportData.mimeType, exportData.filename,
                     exportData.fileExtension, exportData.merges,
                     exportData.RTL, exportData.sheetname);
"
