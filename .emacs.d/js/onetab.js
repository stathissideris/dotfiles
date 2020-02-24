function log (s) {
	this.console.log(s);
}

function cleanTitle (s) {
  return s.replace("[", "|").replace("]", "|");
}

windows = Application("Google Chrome").windows();

for(var i = 0; i < windows.length; i++) {
	w = windows[i];
	tabs = w.tabs();
	for(var t = 0; t < tabs.length; t++) {
		tab = tabs[t];
    title = cleanTitle(tab.title());
    if(title == "") {
      log("  * [[" + tab.url() + "]]");
    } else {
		  log("  * [[" + tab.url() + "][" + title + "]]");
    }
	}
}
