instance Read Color where
  -- readsPrec は入力を構文解析するメイン関数
  readsPrec _ value =
    -- tryParse にペアのリストを渡す.
    -- それぞれのペアは文字列と欲しい返り値を持つ.
    -- tryParseは入力をこれらの文字列の中のどれか一つにマッチさせる.
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
      where tryParse [] = [] -- 試すものが残っていなければ[]
            tryParse ((attempt, result):xs) =
                     -- 構文解析する文字列の先頭部分と探してるテキスト比較
                     if (take (length attempt) value) == attempt
                       -- マッチすれば結果と入力の残りを返す
                       then [(result, drop (length attempt) value)]
                       -- マッチしなければ次へ
                       else tryParse xs
