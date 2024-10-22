document.addEventListener('DOMContentLoaded', function() {
    var closeButton = document.getElementById('close-btn-sidebar');
    var sidebar = document.getElementById('image-sidebar');

    closeButton.addEventListener('click', function() {
      console.log("Button clicked");
      sidebar.classList.toggle('hidden');
    });
    
    Shiny.addCustomMessageHandler("image_gallery_side", function(message) {
      if (message === "toggleVisibility") {
        sidebar.classList.remove('hidden'); // Ensure sidebar is visible
      }
    });
  });

  
  
/*  
  function toggleHidden(element) {
    var parent = element.parentElement;
    var grandparent = parent.parentElement;
    parent.classList.toggle('hidden');
    grandparent.classList.toggle('hidden');
  }

  document.addEventListener('DOMContentLoaded', function() {
    Shiny.addCustomMessageHandler("image_gallery_side", function(message) {
      var sidebar = document.getElementById('image-sidebar');
      if (sidebar && message === "toggleVisibility") {
        sidebar.classList.remove('hidden'); // Ensure sidebar is visible
        sidebar.parentElement.classList.remove('hidden'); // Ensure grandparent div is also visible
      }
    });
  });
  */