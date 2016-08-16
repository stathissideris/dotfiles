'use strict';

//works with Phoenix 2.2 except for tasks it seems

var grids = {
  '2 side-by-side': {rows: 1, cols: 2},
  '2 stacked': {rows: 2, cols: 1},
  '3 side-by-side': {rows: 1, cols: 3},
};

function alert(message) {
  var modal = new Modal();
  modal.text = message;
  modal.duration = 2;

  var sFrame = Screen.main().flippedVisibleFrame();
  modal.origin = {x: 20, y: sFrame.height - 20};

  modal.show();
};

function grid(name) {
  var rows = grids[name].rows;
  var cols = grids[name].cols;
  return function applyGrid() {
    alert(name);
    var windows = Window.recent();
    windows.splice(Math.min(windows.length, cols*rows));
    var pre = windows.length;
    var sFrame = Screen.main().flippedVisibleFrame();
    var width = Math.round(sFrame.width / cols);
    var height = Math.round(sFrame.height / rows);

    var x = sFrame.x;
    var y = sFrame.y;
    _.times(cols, function(col) {
      _.times(rows, function(row) {
        var n = col + (row*cols);
        var rect = {x: x + (col*width), y: y + (row*height), width: width, height: height};
        if (windows.length > n) {
          windows[n].setFrame(rect);
        }
      });
    });
  };
}

function swapMostRecent() {
  alert("swap");
  var windows = Window.recent();
  var frame0 = windows[0].frame();
  var frame1 = windows[1].frame();
  windows[0].setFrame(frame1);
  windows[1].setFrame(frame0);
}

var findByName = function(name) {
  return _.find(App.all(), function(app) {
    if (app.name() === name) {
      return true;
    }
  });
};

function focusDevelop() {
  //alert("back to work!");

  var chrome = findByName("Emacs");
  var emacs = findByName("Google Chrome");
  //var iterm = findByName("iTerm");
  //var slack = findByName("Slack");

  chrome.windows()[0].focus();
  emacs.windows()[0].focus();
}

function makeCurrentFullScreen() {
  alert("maximize");
  Window.focused().maximize();
}

function currentTrack() {
  var h = Task.run("/Users/sideris/bin/current-track.sh", [], function () {});
}

function nextTrack() {
  var h = Task.run("/usr/bin/curl", ["http://mucha:8888/default/?cmd=StartNext&param1="], function () {});
  alert("next track");
}

function bluetoothAssistant() {
  alert("bluetooth assistant");
  var h = Task.run("/usr/bin/open", ["/System/Library/CoreServices/Bluetooth Setup Assistant.app"], function () {});
}

var mash = [ 'cmd', 'alt', 'ctrl' ];

var h1 = new Key ('1', mash, grid('2 side-by-side'));
var h2 = new Key ('2', mash, grid('2 stacked'));
var h3 = new Key ('3', mash, grid('3 side-by-side'));
var h0 = new Key ('0', mash, focusDevelop); //mash it, does not work properly with single press
var h9 = new Key ('9', mash, makeCurrentFullScreen);
var hEquals = new Key ('=', mash, swapMostRecent);
var hF11 = new Key ('f11', ['alt'], currentTrack);
var hF12 = new Key ('f12', ['alt'], nextTrack);
var hM = new Key ('m', mash, bluetoothAssistant);
