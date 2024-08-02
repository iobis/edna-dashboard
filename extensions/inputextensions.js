// Define the custom input binding
var imageInput = new Shiny.InputBinding();

$.extend(imageInput, {
  find: function(scope) {
    return $(scope).find(".image-butt-div");
  },
  getValue: function(el) {
    return $(el).data("val") || 0;
  },
  setValue: function(el, value) {
    $(el).data("val", value);
  },
  receiveMessage: function(el, value){
    // Not needed since we only care about the click event
  },
  subscribe: function (el, callback) {
    $(el).on("click.imageInputInputBinding",
      function () {
        var $el = $(this);
        var val = $el.data("val") || 0;
        $el.data("val", val + 1);
        callback(false);
      });
  },
  unsubscribe: function(el) {
    $(el).off(".image-butt-div");
  },
  getState: function(el) {
    return this.getValue(el);
  }
});

// Register the custom input binding
Shiny.inputBindings.register(imageInput, 'my.imageinput');

$(document).ready(function() {
  // Bind a click event to all div elements with the class 'image-gallery'
  $(document).on('click', '.image-butt-div', function() {
    var divId = $(this).attr('id'); // Get the ID of the clicked div
    Shiny.setInputValue('clicked_image_id', divId, {priority: 'event'});
  });
});