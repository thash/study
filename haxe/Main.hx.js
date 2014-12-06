(function ($hx_exports) { "use strict";
var console = (1,eval)('this').console || {log:function(){}};
var Main = $hx_exports.Main = function() { };
Main.main = function() {
};
Main.runOnload = function() {
	console.log("runOnload");
	Main.filter(".a","アマゾン","Aaaaaaaaaaaamaaaaaaaaaazoooooonn");
};
Main.filter = function(selector,target,replace) {
	var txt = new js.JQuery(selector).html();
	new js.JQuery(selector).html(StringTools.replace(txt,target,replace));
};
var StringTools = function() { };
StringTools.replace = function(s,sub,by) {
	return s.split(sub).join(by);
};
var js = {};
var q = window.jQuery;
var js = js || {}
js.JQuery = q;
Main.main();
})(typeof window != "undefined" ? window : exports);

//# sourceMappingURL=Main.hx.js.map