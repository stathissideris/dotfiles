function log (s) {
	this.console.log(s);
}

function cleanTitle (s) {
  return s.replace("[", "|").replace("]", "|");
}

var out = "";

windows = Application("Google Chrome").windows();

for(var i = 0; i < windows.length; i++) {
	w = windows[i];
	tabs = w.tabs();
	for(var t = 0; t < tabs.length; t++) {
		tab = tabs[t];
    title = cleanTitle(tab.title());
    if(title == "") {
      out += "  * [[" + tab.url() + "]]\n";
    } else {
		  out += "  * [[" + tab.url() + "][" + title + "]]\n";
    }
	}
}

log(out);
