ui_grupo_id <- function(id){
  
  ns <- NS(id)
  
  tagList(
    # Grupo Tipo
    selectizeInput(
      inputId = ns("grupo_tipo"),
      label = "Tipo:",
      choices = setNames(
        lista_grupo_tipo$opciones,
        lista_grupo_tipo$opciones_label
      ),
      options = list(
        placeholder = 'Seleccionar',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    uiOutput(ns("grupo_id")) 
  )

}

srv_grupo_id <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      output$grupo_id <- renderUI (expr ={
        # Si se devuelve vacío, return para evitar error de función
        # if (is.null (input$grupo_tipo))
        #   return ()
        req(input$grupo_tipo)
        # cannot use updateinput because it's not always a selectinput
        switch (
          input$grupo_tipo,
          "Asociart" = {
          },
          "grupo_ec" = {
            selectInput (
              inputId = "grupo_id",
              label = "Grupo económico",
              choices = lista_grupo_id$grupo_ec$choices,
              selected = lista_grupo_id$grupo_ec$default,
              multiple = TRUE
            )
          },
          "sucursales" = {
            selectInput(
              "grupo_id",
              label = "Sucursal",
              choices = setNames(
                lista_grupo_id$sucursales$choices$SUC_ID,
                lista_grupo_id$sucursales$choices$sucursal
              ),
              selected = lista_grupo_id$sucursales$default
            )
          },
          "regiones" = {
            selectInput(
              "grupo_id",
              label = "Región",
              choices = setNames(
                lista_grupo_id$regiones$choices$REG_ID,
                lista_grupo_id$regiones$choices$region
              ),
              selected = lista_grupo_id$regiones$default
            )
          },
          "provincias" = {
            selectInput(
              "grupo_id",
              label = "Provincia",
              choices = setNames(
                lista_grupo_id$provincias$choices$ID,
                lista_grupo_id$provincias$choices$PROVINCIA
              ),
              selected = lista_grupo_id$provincias$default
            )
          },
          "uc" = {
            textInput (
              "grupo_id",
              label = "Nro de Unidad Comercial",
              value = lista_grupo_id$uc$default
            )
          },
          "pas" = {
            textInput (
              "grupo_id",
              "CUIT del PAS/socia/organizador/productor/asesor/broker",
              value = lista_grupo_id$pas$default
            )
          },
          "clase_3" = {
            selectInput (
              "grupo_id",
              label = "clase",
              choices = setNames(
                lista_grupo_id$clase_3$choices$value,
                lista_grupo_id$clase_3$choices$label
              )
            )
          },
          "clase_1" = {
            selectInput (
              "grupo_id",
              label = "Actividad x20",
              choices = setNames(
                lista_grupo_id$clase_1$choices$value,
                lista_grupo_id$clase_1$choices$label
              )
            )
          },
          "CIIUR2" = {
            selectInput (
              "grupo_id",
              label = "Actividad",
              choices = setNames(
                lista_grupo_id$CIIUR2$choices$value,
                lista_grupo_id$CIIUR2$choices$label
              ),
              selected = lista_grupo_id$CIIUR2$default
            )
          },
          "CIIUR2_1d" = {
            selectInput (
              "grupo_id",
              label = "Actividad x10",
              choices = setNames(
                lista_grupo_id$CIIUR2_1d$choices$value,
                lista_grupo_id$CIIUR2_1d$choices$label
              )
            )
          },
          "contratos" = {
            textInput (
              "grupo_id",
              label = "Contratos",
              value = lista_grupo_id$contratos$default
            )
          },
          "cuit" = {
            textInput (
              "grupo_id",
              label = "CUITS Contratos",
              value = lista_grupo_id$cuit$default
            )
          },
          disabled(
            textInput (
              "grupo_id",
              "Valores",
              ""
            )
          )
        )
        
      })
      
    }
  )
}



