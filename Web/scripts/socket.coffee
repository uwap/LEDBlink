connection = new WebSocket "ws://localhost:8080", "test"

setMode = (mode) ->
  connection.send mode

sendColor = ->
  brightness = $("#brightnessSlider").slider "value"
  connection.send Math.floor(brightness * $("#sliderRed").slider "value")
  connection.send Math.floor(brightness * $("#sliderGreen").slider "value")
  connection.send Math.floor(brightness * $("#sliderBlue").slider "value")

$ ->
  $(".colorSin").button().click ->
    setMode "colorSin"
    sendColor()
  $(".setColor").button().click ->
    setMode "fillColor"
    sendColor()
  $(".centerButton").button().click ->
    setMode "centered"
    sendColor()
  $(".centerSin").button().click ->
    setMode "centerSin"
    sendColor()
