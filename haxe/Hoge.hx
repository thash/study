package ;

import js.JQuery;

@:expose
class Hoge {
  // main()はDOMロードを待たず即時関数内で実行される
  static public function main() {
    // import js.JQueryすると即時関数内で
    //     var q = window.jQuery;
    //     js.JQuery = q;
    // としてからmain()を呼ぶんだけど,
    // window.jQueryが準備されるのが遅れる事がある

    // 案(1)没:
    // untyped __js__("if (typeof js.JQuery == \"undefined\") { js.JQuery = jQuery }");

    // 案(2)没: jQueryを使う処理はmain外にまとめてwindow.onload時に呼ぶ
    // untyped __js__("window.onload = function() { Hoge.runOnLoad(); } ");
  }

  // 案(3)採用: JQueryでDOM触る処理はmain()とは別に用意しておいて,
  // exposeした上でhtml側のonloadで実行 <body onload="window.Hoge.runOnload();">
  static public function runOnload() {
    new JQuery( "button.toggle" ).click( function() {
        new JQuery( "button.toggle" ).toggle();
      });
  }
}
