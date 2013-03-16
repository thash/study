%% つーかこのへんの話Haskellだと口うるさく最初から教えこまれたのにErlangは今頃なんやな

%% シングルトン型の例
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'some atom'
42
[] %% カラリスト
{} %% カラタプル
<<>> %% カラバイナリ


%% モジュール内での型宣言
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 一般形
-type TypeName() :: TypeDefinition.

%% 具体例
-type tree() :: {'node', tree(), tree(), any(), any()}.
%% 再帰的に定義してる.
%% あるいは, 特別な構文として型コメントのために変数名を使うこともできます:
-type tree() :: {'node', Left::tree(), Right::tree(), Key::any(), Value::any()}.
%% が, カラの二分木が作れないのでもうちょっと工夫しないといけない.
-type tree() :: {'node', 'nil'}
| {'node', Key::any(), Val::any(), Left::tree(), Right::tree()}.


%% レコード型定義
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(user, {name="" :: string(),
    notes :: tree(),
    age :: non_neg_integer(),
    friends=[] :: [#user{}],
    bio :: string() | binary()}). %% 文字列またはバイナリ


%% レコード中での型定義
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% トランプを定義してみよう => cards.erl
-type suit() :: spades | clubs | hearts | diamonds.
-type value() :: 1..10 | j | q | k.
-type card() :: {suit(), value()}.

