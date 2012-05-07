(function() {
  var flattrLoaded = false;
  
  function loadFlattr() {
      if (flattrLoaded)
  	return;
      flattrLoaded = true;
  
      var s = document.createElement('script');
      var t = document.getElementsByTagName('script')[0];
  
      s.type = 'text/javascript';
      s.async = true;
      s.src = 'https://api.flattr.com/js/0.6/load.js?mode=auto&popout=1&button=compact';
  
      t.parentNode.insertBefore(s, t);
  }
  
  function setupButton(buttonEl) {
      var div;
      buttonEl.onclick = function(ev) {
  	if (!div) {
  	    loadFlattr();
  
  	    div = document.createElement('div');
  	    div.innerHTML = buttonEl.getAttribute('data-payment');
  	    buttonEl.parentNode.insertBefore(div, buttonEl.nextSibling.nextSibling);
  
  	    buttonEl.textContent = "Flattr ▴";
              FlattrLoader.setup();
  	} else {
  	    div.parentNode.removeChild(div);
  	    div = null;
  	    buttonEl.textContent = "Flattr ▾";
  	}
      };
      buttonEl.setAttribute('class', buttonEl.getAttribute('class'));
  }
  
  var buttonEls = document.getElementsByClassName('flattrdropdown');
  var i;
  for(i = 0; i < buttonEls.length; i++) {
      setupButton(buttonEls[i]);
  }
})();
