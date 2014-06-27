module Main (main) where

import SimpleJSON

-- 実行可能なHaskellファイルを作るなら main という関数を含むMainモジュールを作るべし
main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
