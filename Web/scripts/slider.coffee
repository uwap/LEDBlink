refreshPreview = ->
  red = $("#sliderRed").slider "value"
  blue = $("#sliderBlue").slider "value"
  green = $("#sliderGreen").slider "value"
  $("#colorPreview").css "background-color", "rgb(#{red}, #{green}, #{blue})"
$ ->
  $("#sliderRed, #sliderBlue, #sliderGreen").slider
    orientation: "horizontal"
    range: "min"
    max: 255
    value: 127
    slide: refreshPreview
    change: refreshPreview
  $("#brightnessSlider").slider
    orientation: "horizontal"
    range: "min"
    max: 1
    min: 0
    step: 0.001
