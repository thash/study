package ;

import js.Browser;
import js.JQuery;
import StringTools;

@:expose // js側で関数が利用可能(window.Main.*)になる. exposeはclass単位.
class Main {

  // いちいちnew JQueryと書かなくていいように用意...するものだが,
  // 自分の環境だと即時関数の実行時にはwindow.jQueryがundefinedなので使えなかった
  // static inline function _( str:String ):JQuery { return untyped $(str); }

  // 返り値:Voidは省略可能だが学習のため残している
  static public function main():Void {
  }

  // Chromeならdebug toolからHaxeコードを参照可能
  static public function runOnload():Void {
    trace("runOnload");
    filter(".a", "アマゾン", "Aaaaaaaaaaaamaaaaaaaaaazoooooonn");
  }

  // // js側でpublic/privateの差はない
  static private function filter(selector: String, target: String, replace: String):Void {
    // 補完がほしい...
    var txt = new JQuery(selector).html();
    new JQuery(selector).html(StringTools.replace(txt, target, replace));
    // Browser.window.alert(a.innerText());
  }
}
