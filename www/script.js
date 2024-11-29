// CUSTOM HANDLERS]
$(document).ready(function () {
  // Add handler for second execution
  Shiny.addCustomMessageHandler('change_col_sizes', function(value) {
    const mediaQuery = window.matchMedia("(max-width: 600px)");

    function updateGridColumn() {
      if (!mediaQuery.matches) { // Check if the screen size does not match the media query
        if (value == 'main') {
          console.log(value)
          document.getElementById("text-column").style.gridColumn = "auto / span 7";
          document.getElementById("stats-column").style.gridColumn = "auto / span 5";
        } else {
          console.log(value)
          document.getElementById("text-column").style.gridColumn = "auto / span 6";
          document.getElementById("stats-column").style.gridColumn = "auto / span 6";
        };
      };
    };
    
    updateGridColumn();
    
  });

  // not ready
  $(document).on('shiny:connected', function() {
    // Check if the input value is 'site'
    Shiny.addCustomMessageHandler('show_overlay', function(message) {
      if (message == 'show') {
        // Create the overlay element
        const overlay = document.createElement('div');
        overlay.id = 'tabset-overlay';
        
        // Set styles for the overlay
        overlay.style.position = 'absolute'; 
        overlay.style.top = '0'; 
        overlay.style.left = '0'; 
        overlay.style.width = '100%'; 
        overlay.style.height = '100%'; 
        overlay.style.backgroundColor = 'white'; 
        overlay.style.zIndex = '2'; 
        overlay.style.display = 'flex'; 
        //overlay.style.alignItems = 'center'; 
        overlay.style.justifyContent = 'center'; 
        overlay.style.color = 'black'; 
        overlay.style.fontSize = '24px'; 

        // Add content to the overlay
        overlay.innerHTML = '<br><br>Select a site to start at the top of this page.';

        // Append the overlay to the parent div
        document.getElementById('tabset-inside').appendChild(overlay);
      } else {
        const existingOverlay = document.getElementById('tabset-overlay');
        if (existingOverlay) {
          // Remove the overlay if it exists
          existingOverlay.parentNode.removeChild(existingOverlay);
        }
      }
    });
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