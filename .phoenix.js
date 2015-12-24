'use strict';

//see https://github.com/watsoncj/phoenix-config/blob/master/phoenix.js#L18

var keys = [];
var mash = [ 'cmd', 'alt', 'ctrl' ];

var grids = {
    '2 side-by-side': {rows: 1, cols: 2},
    '2 stacked': {rows: 2, cols: 1},
};

function alert(message) {
  var modal = new Modal();
  modal.message = message;
  modal.duration = 2;

  var sFrame = Screen.mainScreen().visibleFrameInRectangle();
  modal.origin = {x: 20, y: sFrame.height - 20};
  
  modal.show();
};

function grid(name) {
  var rows = grids[name].rows;
  var cols = grids[name].cols;
  return function applyGrid() {
    alert(name);
    var windows = Window.visibleWindowsInOrder();
    windows.splice(Math.min(windows.length, cols*rows));
    var pre = windows.length;
    var sFrame = Screen.mainScreen().visibleFrameInRectangle();
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
  var windows = Window.visibleWindowsInOrder();
  var frame0 = windows[0].frame();
  var frame1 = windows[1].frame();
  windows[0].setFrame(frame1);
  windows[1].setFrame(frame0);
}

var findByName = function(name) {
  return _.find(App.runningApps(), function(app) {
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
  Window.focusedWindow().maximize();
}

function currentTrack() {
  //doesn't work, can't collect output
  alert(Command.run("curl http://mucha:8888/default | grep 'id=\"track_title\"' | perl -ne '/\"track\">(.+?)<\/span>/; print $1'"));
}

keys.push(Phoenix.bind('1', mash, grid('2 side-by-side')));
keys.push(Phoenix.bind('2', mash, grid('2 stacked')));
keys.push(Phoenix.bind('0', mash, focusDevelop)); //mash it, does not work properly with single press
//keys.push(Phoenix.bind('9', mash, currentTrack));
keys.push(Phoenix.bind('9', mash, makeCurrentFullScreen));
keys.push(Phoenix.bind('=', mash, swapMostRecent));

