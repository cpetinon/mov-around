

shinyServer(function(session,input, output) {

  data <- callModule(server_2,id = "2",session = session)
  
  callModule(server_3,id = "3",session = session, data)

})



