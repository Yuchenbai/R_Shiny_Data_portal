library(shiny)
library(ggplot2)
library(stringr)
library(gtable)

##############################User Interface###########################################
ui <- fluidPage(

    fluidRow(column(1, tags$img(height = 120, width = 150, src = "Autolus_logo.jpg"))),

    
    HTML("<h3>Control Panel</h3>"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput("file1", "Step 1: Upload a CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        #tags$hr(),
        
        radioButtons("disp", "Step 2: Display all, part or none of the file",
                     choices = c(Head = "head",
                                 All = "all",
                                 None = "none"),
                     selected = "head"),
      HTML("<B>Step3: Select the study name, then select two parameters/variables to plot</B>"),  
       tabsetPanel(id="study", 
                tabPanel ("AUTO2", value="AUTO2-MM1"),
                tabPanel ("AUTO3", value="AUTO3-DB1"),

       conditionalPanel(
          condition = "input.study == 'AUTO2-MM1'",
          selectInput("Parameter1_A2MM1", label="Parameter1", 
                      choices=list('AUTO2-MM1'=c( "APRIL+CD4+ cells" ="APRILCD4",
                                                  "APRIL+CD3+ cells" = "APRILCD3",
                                                  "APRIL+CD8+ cells" = "APRILCD8",
                                                  "APRIL+ of total T cells" = "APRILTTC",
                                                  "GM-CSF" = "GMCSF",
                                                  "IFNg" = "IFNG",
                                                  "IL-2" = "INTLK2",
                                                  "IL-5" = "INTLK5",
                                                  "IL-6" = "INTLK6",
                                                  "IL-8" = "INTLK8",
                                                  "IL-10" = "INTLK10",
                                                  "TNF-a" = "TNF"
                      )                    
                  )
          ),
          selectInput("Parameter2_A2MM1", label="Parameter2", 
                      choices=list('AUTO2-MM1'=c( "APRIL+CD8+ cells" = "APRILCD8",
                                                  "APRIL+CD4+ cells" ="APRILCD4",
                                                  "APRIL+CD3+ cells" = "APRILCD3",
                                                  "APRIL+ of total T cells" = "APRILTTC",
                                                  "GM-CSF" = "GMCSF",
                                                  "IFNg" = "IFNG",
                                                  "IL-2" = "INTLK2",
                                                  "IL-5" = "INTLK5",
                                                  "IL-6" = "INTLK6",
                                                  "IL-8" = "INTLK8",
                                                  "IL-10" = "INTLK10",
                                                  "TNF-a" = "TNF"

                      )
                      
                  )
              )
          ),
        conditionalPanel(
          condition = "input.study == 'AUTO3-DB1'",
          selectInput("Parameter1_A3DB1", label="Parameter1", 
                      choices=list('AUTO3-DB1'=c("CD19/CD22 CAR+ CD3+ T cells" = "CD19223",
                                                 "CD19/CD22 CAR+ CD4+ T cells" = "CD19224",
                                                 "CD19/CD22 CAR+ CD8+ T cells" = "CD19228",
                                                 "%CD3+ cells CD19/22 CAR+" = "CD3PCNT",
                                                 "MLV vector insert copies" = "MLVVIC",
                                                 "MLV vector inserts" ="MLVVI"
                      )
          
                )
            ),
            selectInput("Parameter2_A3DB1", label="Parameter2", 
                        choices=list('AUTO3-DB1'=c("CD19/CD22 CAR+ CD4+ T cells" = "CD19224",
                                                   "CD19/CD22 CAR+ CD3+ T cells" = "CD19223",
                                                   "CD19/CD22 CAR+ CD8+ T cells" = "CD19228",
                                                   "%CD3+ cells CD19/22 CAR+" = "CD3PCNT",
                                                   "MLV vector insert copies" = "MLVVIC",
                                                   "MLV vector inserts" ="MLVVI"

                      )
                )
            )
        ),
        

        conditionalPanel( condition = "input.study == 'AUTO3-PA1'",
                          HTML("<h5>no AUTO3_PA1 data yet</h5>")
        ),
        conditionalPanel( condition = "input.study == 'AUTO4-TL1'",
                          HTML("<h5>no AUTO4_TL1 data yet</h5>")
        )
       ),
        

                          



# Input: Select number of rows to display ----



HTML("<B>Step 4: Make sure the parameters match the study type then Submit</B>"),
      actionButton("do", "Submit"),
#tags$hr(),
HTML( "<h1>   </h1>"),
HTML("<B>Step 5: Select different paramentes the chart will change accordingly</B>.")
      
      ),
     # actionButton("do", "Submit"),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Data file ----
        #tableOutput("contents")
        textOutput("mytext"),
        plotOutput("allcar")
     
        
      )
      
    ),
     tableOutput("contents")
    
#######Output figure
   # fluidRow(
      
   #   column(10, plotOutput("allcar"))
   #   )
     
   )#
##############################End of User Interface###########################################

###############################Serer function#################################################
server <- function(input, output, session) {

 observeEvent(input$do, {
   
   ##Create a reactive object
   
   data <- reactive({
     req(input$file1)
     
  read.csv(input$file1$datapath)
   })


####Printout table  
output$contents <- renderTable({
    
  #  req(input$file1)
    
  # df <- read.csv(input$file1$datapath)
                  # header = input$header,
                  # sep = input$sep,
                  # quote = input$quote)
   
  df<-data() 
    #study <- input$study
    
    if(input$disp == "head") {
      return(head(df, 10))
    }
    else if(input$disp == "all") {
      return(df)
    }else if(input$disp == "head") {
      retuen(NULL)
      }
    
    
  })
 # study <- input$study

########start of car code
  
  output$allcar <- renderPlot({
    
  error<-FALSE
    
  req(input$file1)
    
   my_data <- read.csv(input$file1$datapath, quote='', stringsAsFactors=FALSE, header=TRUE)
 
    # header = input$header,
    # sep = input$sep,
    # quote = input$quote)
   studyID<-my_data$STUDYID[1]  # save study ID
   subjID<-my_data$SUBJID[1]  # save patient (subject ID) 
   study <- input$study #determine study type
   
    ####Chnage Data format into integres
    my_data$DATE<-as.Date(my_data$MIDT, format = "%d%B%Y")
    my_data$NEWDATE<-as.integer(my_data$DATE)+720000
    my_data$NEWERDATE<-my_data$NEWDATE-my_data$NEWDATE[2]
    studyVariables<-names(my_data)
    
    x<-as.character(input$study)
    
    
    if(x=="AUTO2-MM1"){
      parameter1<-input$Parameter1_A2MM1
      parameter2<-input$Parameter2_A2MM1
    }
    
    
    if(x=="AUTO3-DB1"){
      parameter1<-input$Parameter1_A3DB1
      parameter2<-input$Parameter2_A3DB1
    }
    
    #if(my_data$MITESTCD[my_data$MITESTCD==parameter1][1]==parameter1){
    
    my_data1<-my_data[my_data$MITESTCD %in% c(parameter1, parameter2), ]
  
    
    # save Max values of parameter 1 and 2
    max1<-max(my_data1$MIORRES[my_data1$MITESTCD == parameter1], na.rm = TRUE)
    max2<-max(my_data1$MIORRES[my_data1$MITESTCD == parameter2], na.rm = TRUE)
    
    #Tale scale factor
    scalefactor<-max2/max1
    if(scalefactor==0){ ##### what if scalefactor is infinite
      scalefactor<-1
    }
    
    #Amplify parameter2 values of MIORRES column
    my_data1$MIORRES[my_data1$MITESTCD==parameter2]<-my_data1$MIORRES[my_data1$MITESTCD==parameter2]/scalefactor
    
    #title1<-paste(testname, parameter1, parameter2, collapse=" ")
    title1<-paste(x, subjID, my_data1$MITESTCD[my_data1$MITESTCD==parameter1][1], "vs", my_data1$MITESTCD[my_data1$MITESTCD==parameter2][1], collapse=" ")
    
    ylabel1<-paste(my_data1$MITEST[my_data1$MITESTCD==parameter1][1], " (", my_data1$MIORRESU[my_data1$MITESTCD==parameter1][1], ")", collapse="")
    ylabel2<-paste(my_data1$MITEST[my_data1$MITESTCD==parameter2][1], " (", my_data1$MIORRESU[my_data1$MITESTCD==parameter2][1], ")", collapse="")
    
    
    
    p1_basic<- ggplot(my_data1, aes(x=NEWERDATE, y=MIORRES, shape=MITESTCD, group=MITESTCD, colour=MITESTCD 
    ) )+ geom_line(aes(linetype=MITESTCD),size=1, na.rm=TRUE) + geom_point(size=5,  na.rm=TRUE) +
      guides(fill="")+ggtitle(title1)+
      theme_set(theme_gray(base_size = 18)) + theme_bw() + 
      xlab("Study Day") + ylab(ylabel1) +
      theme(panel.grid.major.x = element_line(NA),panel.grid.minor = element_line(NA), panel.border = element_blank(), axis.line = element_line(colour = "black")) +
      scale_x_continuous(breaks=seq(-21,119,7))+
      scale_y_continuous(sec.axis = sec_axis(~.*scalefactor, ylabel2))
  
    
    my_graph<-p1_basic + theme(axis.text=element_text(size=12)) + theme(axis.title=element_text(size=14)) + theme(legend.title=element_text(size=16)) 
    return(my_graph)
    #} else {
    #return(NULL)
    #}
 })
  
})
}

shinyApp(server = server, ui = ui)