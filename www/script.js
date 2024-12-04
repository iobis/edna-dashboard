// CUSTOM HANDLERS]
$(document).ready(function () {
  // Add handler for second execution
  Shiny.addCustomMessageHandler('change_col_sizes', function(value) {
    const mediaQuery = window.matchMedia("(max-width: 600px)");

    function updateGridColumn() {
      if (!mediaQuery.matches) { // Check if the screen size does not match the media query
        if (value == 'main') {
          //console.log(value)
          document.getElementById("text-column").style.gridColumn = "auto / span 7";
          document.getElementById("stats-column").style.gridColumn = "auto / span 5";
        } else {
          //console.log(value)
          document.getElementById("text-column").style.gridColumn = "auto / span 6";
          document.getElementById("stats-column").style.gridColumn = "auto / span 6";
        };
      };
    };
    
    updateGridColumn();
    
  });

   // Add handler for home
   document.getElementById("home-link").addEventListener("click", function (event) {
    event.preventDefault(); // Prevent default link behavior
    var message = {id: "id1", nonce: Math.random()};
    Shiny.onInputChange("homeTrigger", message)
   });

  // Add handler for second execution
  Shiny.addCustomMessageHandler('second_gallery', function(value) {
    var message = {id: "id1", nonce: Math.random()};
    Shiny.onInputChange("secondGallery", message)
  });

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
});