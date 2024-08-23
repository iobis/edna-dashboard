var myCarousel = document.querySelector('#carouselExample');
var totalImages = document.querySelectorAll('.carousel-item').length;

myCarousel.addEventListener('slid.bs.carousel', function (event) {
  var currentIndex = event.to + 1; // event.to gives the index of the new slide (0-based)
  document.getElementById('carousel-counter').textContent = currentIndex + '/' + totalImages;
});

// Initialize the carousel with the automatic slide interval
var carousel = new bootstrap.Carousel(myCarousel, {
  interval: 5000, // Time in milliseconds for automatic slide change
  ride: 'carousel'
});