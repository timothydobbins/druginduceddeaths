// Initial Tracking Code
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-123602558-1');


// Event Tracking Code
$(document).on('shiny:inputchanged', function(event) {
  if(event.name == 'bins' || event.name == 'col'){
    ga('send', 'event', 'input',
      'updates', event.name, event.value);
  }
});

// User Tracking Code
$(document).one('shiny:idle', function() {
  ga('set','userId', Shiny.user);
});
