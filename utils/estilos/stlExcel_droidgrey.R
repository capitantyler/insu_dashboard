# opciones de estilo Excel

library(openxlsx)

if(!exists("stl", mode = "environment")) {stl <- new.env(stl)}

with(stl, {

  # opciones generales ----

  options("openxlsx.numFmt" = NULL) ## For default style rounding of numeric columns
  options("openxlsx.minWidth" = 3)
  options("openxlsx.maxWidth" = 250)
  options("openxlsx.paperSize" = 9) ## A4
  options("openxlsx.orientation" = "portrait")


  # estilos de centrado

  options("openxlsx.halign" = "center")

  halignCenter <- createStyle(halign = "center")
  halignLeft <- createStyle(halign = "left")
  halignRight <- createStyle(halign = "right")
  valignCenter <- createStyle(valign = "center")
  valignTop <- createStyle(valign = "top")
  valignBottom <- createStyle(valign = "bottom")

  # estilos de fuente----

  # modifyBaseFont ( #fuente estándar
  #     wb
  #   , fontSize = 10
  #   , fontName = "Arial Narrow"
  # )

  fuente.calibri8 <- createStyle(fontSize = 8, fontName = "calibri")
  fuentenegrita <- createStyle(textDecoration = "Bold")


  # estilo de títulos de tabla ----
  hs <- createStyle (
    fontColour = "#ffffff"
    , fgFill = "#4F80BD"
    , halign = "CENTER"
    , textDecoration = "Bold"
    , border = "TopBottomLeftRight"
    , borderColour = "#4F81BD"
  )

  # estilos numéricos ----

  options ("openxlsx.dateFormat" = "dd/mm/yyyy")
  options ("openxlsx.datetimeFormat" = "dd/mm/yyyy hh:mm:ss")

  txt   <- createStyle (numFmt = "@")
  num   <- createStyle (numFmt = "0")
  dec0  <- createStyle (numFmt = "#,##0")
  dec1  <- createStyle (numFmt = "#,##0.0")
  dec2  <- createStyle (numFmt = "#,##0.00")
  dec3  <- createStyle (numFmt = "#,##0.000")
  colordec0  <- createStyle (numFmt = "#,##0;[red]-#,##0")
  colordec1  <- createStyle (numFmt = "#,##0.0;[red]-#,##0.0")
  colordec2  <- createStyle (numFmt = "#,##0.00;[red]-#,##0.00")
  colordec3  <- createStyle (numFmt = "#,##0.000;[red]-#,##0.000")
  porc0 <- createStyle (numFmt = "#,##0%")
  porc1 <- createStyle (numFmt = "#,##0.0%")
  porc2 <- createStyle (numFmt = "#,##0.00%")
  colorporc0 <- createStyle (numFmt = "#,##0%;[red]-#,##0%")
  colorporc1 <- createStyle (numFmt = "#,##0.0%;[red]-#,##0.0%")
  colorporc2 <- createStyle (numFmt = "#,##0.00%;[red]-#,##0.00%")
  peso0 <- createStyle (numFmt = "$ #,##0")
  peso2 <- createStyle (numFmt = "$ #,##0.00")
  colorpeso0 <- createStyle (numFmt = "$ #,##0;[red]-$ #,##0")
  colorpeso2 <- createStyle (numFmt = "$ #,##0.00;[red]-$ #,##0.00")
  colorpesosincero0 <- createStyle (numFmt = "$ #,##0;[red]-$ #,##0;")
  colorpesosincero2 <- createStyle (numFmt = "$ #,##0.00;[red]-$ #,##0.00;")
  logico <- createStyle (numFmt = "'Sí';;'No';")

  # estilos de borde y relleno de celdas ----

  options ("openxlsx.borderColour" = "#000000")
  options ("openxlsx.borderStyle" = "thin")

  is <- createStyle ( # estilo de intercalado de clases/contratos
    fgFill = "#BCD2EE"    # "LightSteelBlue2"
  )

  subrayado <- createStyle(border = "bottom")
  borde_medio <- createStyle(border = "bottom", borderStyle = "medium")
  borde_doble <- createStyle(border = "bottom", borderStyle = "double")

  titulo_tabla_flotante_1 <- createStyle(
    fgFill = "#E6E6E7",
    borderStyle = "medium",
    border = "bottom"
  )

})
