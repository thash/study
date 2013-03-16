%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 29. Distribunomicon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Erlangの特性 ... 関数型言語, 並列性, 分散. 最後の一つを語る時が来ましたよと.

%% > Erlangは分散に関して書くべき基本的な要素がほとんどありません。
%% >   * お互いにやり取りする多くのノード（仮想マシン）を持ち、
%% >   * やり取りするデータのシリアライズとデシリアライズを行い、
%% >   * マルチプロセスの概念を多くのノードで行うように拡張させ、
%% >   * ネットワークの失敗を監視する方法を実装すればよいのです

%% Erlangにおける分散の取り扱い.
%% "かなり完全なネットワーク透明性"
%% Erlangでは各VMがノードに当たる. ノードを起動したら名前をつけ, EPMDに接続する.
%% EPMD ... Erlang Port Mapper Daemon. Erlangクラスタの一部として1computerあたり1個稼働.
%% ノード同士が接続すると, 自動的にお互いを監視.
%% ノード群の一部となったときはノード群すべてが接続先とみなされる. 2nodeのクラスタと2nodeのクラスタが接続したら, 全員が全員と直接つながった状態に落ち着く.
%% この設定はfault tolerance的には良いけど数百になると接続数, ポート専有などのアレで難しくなる.

%% 一般的に分散コンピューティングの足を引っ張る問題
%     1. ネットワークの信頼性
%     2. 遅延
%     3. 帯域 ... 多ノードの時大きなデータの影響を受けやすい
%     4. ネットワークは安全ではない ... パケット傍受, 乗っ取り. Erlang黎明期は安全な温室にデプロイされていたため言語として機構がない.
%     5. トポロジーが変化する
%     6. 何人もの管理者が弄る
%     7. 転送コストがかかる
%     8. ネットワークは同質ではない ... データ交換にBERTというformatを使う.

%% CAP定理
%%   * 一貫性(Consistency)
%%   * 可用性(Availability)
%%   * 分断耐性(Partition Tolerance)

%% 分断耐性に関連して. 分断が起こり,その後解消された時どんな戦略を取るか.
%% CPの手法: 何も変更されなかったので, なにもしない.
%% APの手法...たとえば以下.
%%   * Last Write Wins
%%   * ランダムに値が選択される
%%   * 基本last write winsだけど相対時間を使って衝突を減らす.
%%   * 衝突した際の判断はアプリケーション側に任せる. Gitのconflict解消に近い.


%%% 例 -- shell同士で通信してみる %%%

neon $ erl -sname ketchup
(ketchup@neon)1> net_kernel:connect(fries@neon).
(ketchup@neon)2> node().
ketchup@neon
(ketchup@neon)3> nodes().
[fries@neon]
(ketchup@neon)4> register(shell, self()).
(ketchup@neon)5> {shell, fries@neon} ! {hello, from, self()}.
{hello,from,<0.37.0>}
(ketchup@neon)6> {shell, fries@neon} ! {hello, from, self()}.
{hello,from,<0.37.0>}

neon $ erl -sname fries
(fries@neon)1> register(shell, self()).
(fries@neon)2> flush().
Shell got {hello,from,<5900.37.0>}
ok
(fries@neon)3> receive {hello, from, OtherShell} -> OtherShell ! <<"hey there!">> end.
%% ここでストップする. ketchupからの通信を受け取ると...
<<"hey there!">>

%% nodeにcookieを持たせる. (以下の例では起動時に設定)
%% $ erl -sname salad -setcookie 'opensesame'

