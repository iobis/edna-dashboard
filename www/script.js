// CUSTOM HANDLERS]
// Add handler for second execution
Shiny.addCustomMessageHandler('second_gallery', function(value) {
  var message = {id: "id1", nonce: Math.random()};
  Shiny.onInputChange("secondGallery", message)
});