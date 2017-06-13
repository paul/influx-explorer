// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( '../Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );

app.ports.localStorageSet.subscribe(function(pair) {
  var [key, value] = pair

  if (localStorage) {
    localStorage.setItem(key, JSON.stringify(value));
  }
});

app.ports.localStorageGet.subscribe(function(key) {
  if (localStorage) {
    var value = localStorage.getItem(key);
    app.ports.localStorageResponse.send(JSON.parse(value));
  }
});

app.ports.localStorageClear.subscribe(function(_unused) {
  if (localStorage) {
    localStorage.clear();
  }
});

