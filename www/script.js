// CUSTOM HANDLERS]
$(document).ready(function () {
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

  // Function to get the currently active tab's name
  $(document).on("shiny:connected", function () {
    function getActiveTab() {
      const activeTab = document.querySelector('.nav-link.active');
      return activeTab ? activeTab.textContent.trim() : null;
    }

    Shiny.onInputChange('activeTab', getActiveTab());

    $('.navbar-nav .nav-link').on('shown.bs.tab', function (event) {
      const newActiveTab = $(event.target).text().trim();
      Shiny.onInputChange('activeTab', newActiveTab);
    });
  });
});