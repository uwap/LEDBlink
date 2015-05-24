connection = new WebSocket "ws://localhost:8080", "test"

setMode = (mode) ->
  connection.send mode

sendColor = ->
  connection.send $("#sliderRed").slider "value"
  connection.send $("#sliderGreen").slider "value"
  connection.send $("#sliderBlue").slider "value"

$ ->
  $(".colorSin").button().click ->
    setMode "colorSin"
    sendColor()
  $(".setColor").button().click ->
    setMode "fillColor"
    sendColor()
