// CUSTOM HANDLERS]
// Add handler for second execution
/*Shiny.addCustomMessageHandler('second_gallery', function(value) {
  var message = {id: "id1", nonce: Math.random()};
  Shiny.onInputChange("secondGallery", message)
});*/

var dimension = [0, 0];
$(document).on("shiny:connected", function (e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function (e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  Shiny.onInputChange("dimension", dimension);
});