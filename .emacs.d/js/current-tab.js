function log (s) {
	this.console.log(s);
}

function cleanTitle (s) {
  return s.replace("[", "|").replace("]", "|");
}

windows = Application("Google Chrome").windows();
tab = windows[0].activeTab();
title = cleanTitle(tab.title());
if(title == "") {
  log("[[" + tab.url() + "]]");
} else {
	log("[[" + tab.url() + "][" + title + "]]");
}
