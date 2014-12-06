(function ($hx_exports) { "use strict";
var console = (1,eval)('this').console || {log:function(){}};
var Hoge = $hx_exports.Hoge = function() { };
Hoge.main = function() {
};
Hoge.runOnload = function() {
	new js.JQuery("button.toggle").click(function() {
		new js.JQuery("button.toggle").toggle();
	});
};
var js = {};
var q = window.jQuery;
var js = js || {}
js.JQuery = q;
Hoge.main();
})(typeof window != "undefined" ? window : exports);

//# sourceMappingURL=Hoge.hx.js.map