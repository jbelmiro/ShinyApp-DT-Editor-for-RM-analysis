
library(shiny)
library(shinyjs)
## shinysky is to customize buttons
library(shinysky)
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)


rm(list = ls())
useShinyalert()
shinyServer(function(input, output, session){
  
  ### interactive dataset 
  vals_trich<-reactiveValues()
  vals_trich$Data<-readRDS("rm_db.rds")
  
  
  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    fluidPage(
      hr(),
      column(6,offset = 6,
             HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
             ### tags$head() This is to change the color of "Add a new row" button
             tags$head(tags$style(".butt2{background-color:#996666;} .butt2{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
             tags$head(tags$style(".butt4{background-color:#663333;} .butt4{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
             tags$head(tags$style(".butt3{background-color:#993333;} .butt3{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
             ### Optional: a html button 
             # HTML('<input type="submit" name="Add_row_head" value="Add">'),
             HTML('</div>') ),
      
      column(12,dataTableOutput("Main_table_trich")),
      tags$script("$(document).on('click', '#Main_table_trich button', function () {
                   Shiny.onInputChange('lastClickId',this.id);
                   Shiny.onInputChange('lastClick', Math.random()) });")
      
    ) 
  })
  
  #### render DataTable part ####
  output$Main_table_trich<-renderDataTable({
    DT=vals_trich$Data
    datatable(DT,selection = 'single',
              escape=F) })
  
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          textInput(paste0("rm_type", input$Add_row_head), "Raw Material Type:"),
                          selectInput(paste0("color_distribution", input$Add_row_head), "Color Distribution:",choices=c("Uniform", "Variable Degrade", "Variable Splotches")),
                          textInput(paste0("color_munsel", input$Add_row_head), "Color according to Munsel Chart:"),
                          textInput(paste0("color_CIElab", input$Add_row_head), "Color CIElab code:"),
                          selectInput(paste0("surface", input$Add_row_head), "Surface:",choices=c("Regular", "Irregular")),
                          selectInput(paste0("luster", input$Add_row_head), "Type of Luster:",choices=c("Shiny", "Medium", "Dull")),
                          selectInput(paste0("translucency", input$Add_row_head), "Translucency:",choices=c("Highly Translucent", "Translucent", "Sub-translucent", "Opaque")),
                          selectInput(paste0("feel", input$Add_row_head), "Feel:",choices=c("Smooth", "Semi-smooth", "Rough")),
                          selectInput(paste0("grain", input$Add_row_head), "Grain:",choices=c("Coarse", "Medium-coarse", "Medium", "Fine")),
                          selectInput(paste0("distribution", input$Add_row_head), "Distribution of grain, luster or transparency:",choices=c("Even", "Uneven")),
                          selectInput(paste0("patterns", input$Add_row_head), "Pattern type:",choices=c("Spots", "Lines", "Mix", "Other")),
                          selectInput(paste0("spots_type", input$Add_row_head), "Type of Spots:",choices=c("Spotted", "Splotched", "Broad mottling", "Marbled mottling", "Speckling", "Flecks")),
                          selectInput(paste0("spots_distribution", input$Add_row_head), "Spots Distribution:",choices=c("Even", "Uneven")),
                          numericInput(paste0("spots_size", input$Add_row_head), "Average size of spots (mm):",0),
                          selectInput(paste0("spots_percentage", input$Add_row_head), "Spots percentage in surface:",choices=c("1-49%", "50-99%", "100%")),
                          selectInput(paste0("lines_type", input$Add_row_head), "Lines type:",choices=c("Banded", "Streaked", "Laminated", "Finely laminated")),
                          selectInput(paste0("lines_direction", input$Add_row_head), "Lines direction:",choices=c("Horizontal", "Concentric")),
                          selectInput(paste0("cortex_presence", input$Add_row_head), "Is cortex present:",choices=c("Yes", "No")),
                          selectInput(paste0("cortex_type", input$Add_row_head), "Type of cortex:",choices=c("Outcrop", "Pebble", "Unknown")),
                          numericInput(paste0("cortex_maxthick", input$Add_row_head), "Maximum thickness of cortex (mm):",0),
                          numericInput(paste0("cortex_minthick", input$Add_row_head), "Minimum thickness of cortex (mm):",0),
                          selectInput(paste0("cortex_transition", input$Add_row_head), "Cortex transition:",choices=c("Sharp", "Gradual")),
                          selectInput(paste0("inclusions_presence", input$Add_row_head), "Are inclusions present:",choices=c("Yes", "No")),
                          textInput(paste0("inclusions_type", input$Add_row_head), "Classify or describe inclusions:"),
                          selectInput(paste0("knapability_quality", input$Add_row_head), "Quality for knapping:",choices=c("Good", "Acceptable", "Low")),
                          selectInput(paste0("fractures_presence", input$Add_row_head), "Are fractures present:",choices=c("Yes", "No")),
                          selectInput(paste0("alterations", input$Add_row_head), "Type of alteration:",choices=c("Patina", "Thermoalteration")),
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    new_row=data.frame(
      RMtype=input[[paste0("rm_type", input$Add_row_head)]],
      ColorDistribution=input[[paste0("color_distribution", input$Add_row_head)]],
      ColorMunsel=input[[paste0("color_munsel", input$Add_row_head)]],
      ColorCIElab=input[[paste0("color_CIElab", input$Add_row_head)]],
      Surface=input[[paste0("surface", input$Add_row_head)]],
      Luster=input[[paste0("luster", input$Add_row_head)]],
      Translucency=input[[paste0("translucency", input$Add_row_head)]],
      Feel=input[[paste0("feel", input$Add_row_head)]],
      Grain=input[[paste0("grain", input$Add_row_head)]],
      Distribution=input[[paste0("distribution", input$Add_row_head)]],
      Patterns=input[[paste0("patterns", input$Add_row_head)]],
      SpotsType=input[[paste0("spots_type", input$Add_row_head)]],
      SpotsDistribution=input[[paste0("spots_distribution", input$Add_row_head)]],
      SpotsSize=input[[paste0("spots_size", input$Add_row_head)]],
      SpotsPercentage=input[[paste0("spots_percentage", input$Add_row_head)]],
      LinesType=input[[paste0("lines_type", input$Add_row_head)]],
      LinesDirection=input[[paste0("lines_direction", input$Add_row_head)]],
      CortexPresence=input[[paste0("cortex_presence", input$Add_row_head)]],
      CortexType=input[[paste0("cortex_type", input$Add_row_head)]],
      CortexMaxThickness=input[[paste0("cortex_maxthick", input$Add_row_head)]],
      CortexMinThickness=input[[paste0("cortex_minthick", input$Add_row_head)]],
      CortexTransition=input[[paste0("cortex_transition", input$Add_row_head)]],
      InclusionsPresence=input[[paste0("inclusions_presence", input$Add_row_head)]],
      InclusionsType=input[[paste0("inclusions_type", input$Add_row_head)]],
      Quality=input[[paste0("knapability_quality", input$Add_row_head)]],
      Fractures=input[[paste0("fractures_presence", input$Add_row_head)]],
      Alterations=input[[paste0("alterations", input$Add_row_head)]]
    )
    vals_trich$Data<-rbind(vals_trich$Data,new_row )
    removeModal()
  })
  
  
  
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    saveRDS(vals_trich$Data, "rm_db.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete",length(input$Main_table_trich_rows_selected),"rows?" ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delect!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    vals_trich$Data=vals_trich$Data[-input$Main_table_trich_rows_selected]
    removeModal()
  })
  
  ### edit button
  observeEvent(input$mod_row_head,{
    showModal(
      if(length(input$Main_table_trich_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modification"),align="center"),
            hr(),
            dataTableOutput('row_modif'),
            actionButton("save_changes","Save changes"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the row that you want to edit!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  
  
  #### modify part
  output$row_modif<-renderDataTable({
    selected_row=input$Main_table_trich_rows_selected
    old_row=vals_trich$Data[selected_row]
    row_change=list()
    for (i in colnames(old_row))
    {
      if (is.numeric(vals_trich$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      else if( is.Date(vals_trich$Data[[i]])){
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      }
      else 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
    }
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=row_change
    DT 
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none" )
  
  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 colnames(DF)=colnames(vals_trich$Data)
                 vals_trich$Data[input$Main_table_trich_rows_selected]<-DF
                 
               }
  )
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
  
})