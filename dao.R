#Data access objects



getData<-reactive({
  
  req(input$csvfile)
  
  if (is.null(input$upfile)) {
    showNotification(sprintf("reading %s",input$csvfile))
  }
  
  dat = readStudyCSV(input$csvfile)
  
  
  return(dat)
  
})

filtData<-reactive({
  d=getData()
  if (!is.null(input$ylab1) & !is.null(input$ylab2))  
    d=subset(d,MITESTCD %in% c(input$ylab1,input$ylab2))
  
  if (!is.null(input$subjects))
    d=subset(d,SUBJID %in% input$subjects)
  
  d
})

celltypedata<-reactive({
  unique(getData()[["MITESTCD"]])
})

celltypenames<-reactive({
  unique(getData()[["MITEST"]])
})

studies<-reactive({
  unique(getData()[["STUDYID"]])
})

subjects<-reactive({
  unique(getData()[["SUBJID"]])
})
