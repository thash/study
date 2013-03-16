%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 9. エラーと例外
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% erlangには3種類の例外がある.
%    (1). error: ランタイムエラーを引き起こす例外.
%    (2). throw
%    (3). exit

%% 例外処理 try
%     try Exception of
%       SuccessfulPattern1 [Guards] ->
%         Exp1;
%       SuccessfulPattern2 [Guards] ->
%         Exp2;
%     catch
%       TypeOfError:ExceptionPattern1 ->
%         Exp3;
%       TypeOfError:ExceptionPattern2 ->
%         Exp4
%     end.

-module(exceptions).
-compile(export_all).

throws(F) ->
  try F() of
    _ -> ok
  catch
    Throw -> {throw, caught, Throw}
  end.
%% 次のようにthrowだけを受け取る
%    2> exceptions:throws(fun() -> throw(thrown) end).
%    {throw,caught,thrown}
%    3> exceptions:throws(fun() -> erlang:error(pong) end).
%    ** exception error: pong
%         in function  exceptions:throws/1 (exceptions.erl, line 27)


sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

talk() -> "blah blah".

black_knight(Attack) when is_function(Attack, 0) ->
  try Attack() of
    _ -> "None shall pass."
    catch
      throw:slice -> "It is but a scratch.";
      error:cut_arm -> "I've had worse.";
      exit:cut_leg -> "Come on you pansy!";
      _:_ -> "Just a flesh wound."
    end.

%% _:_ はどんなタイプの例外もキャッチする

%% catchの後にafterブロックを入れれば"例外が起ころうが起こるまいが"実行される.
%% Rubyのafter, Javaのfinally


%%% 9.6 catch %%%
%% catch キーワードを使えばあらゆる種類の例外を取得.

%    6> catch doesnt:exist(a,4).
%    {'EXIT',{undef,[{doesnt,exist,[a,4],[]},
%                    {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},
%                    {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,360}]},
%                    {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
%                    {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
%                    {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]}}


%%% 9.7. binary tree, try %%%

has_value(_, {node, 'nil'}) ->
  false;
has_value(Val, {node, {_, Val, _, _}}) ->
  true;
has_value(Val, {node, {_, _, Left, Right}}) ->
  case has_value(Val, Left) of
    true -> true;
    false -> has_value(Val, Right)
  end.

%% > この実装で問題になるのは、分岐した木のすべてのノードが、前の枝の結果をテストしなければいけないという点です。
%% > これはちょっとイライラします。スローを使うことで、ちょっと比較を減らすことができます：

has_valueX(Val, Tree) ->
  try has_value1(Val, Tree) of
    false -> false
    catch
      true -> true
    end.

  has_value1(_, {node, 'nil'}) ->
    false;
  has_value1(Val, {node, {_, Val, _, _}}) ->
    throw(true);
  has_value1(Val, {node, {_, _, Left, Right}}) ->
    has_value1(Val, Left),
    has_value1(Val, Right).



















