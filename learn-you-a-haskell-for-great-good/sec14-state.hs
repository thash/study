import Control.Monad.State

-- 状態付きの計算を扱う State Monad.

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

-- Haskell以外のほとんどの言語では, 乱数に添えて新しいジェネレータを返す必要などない。なぜならジェネレータの状態を上書きすれば事足りるので。
-- HaskellたんはState Monadのおかげで、純粋さを保ちつつ状態を扱うことができる。

-- x = 5 という代入操作をHaskellの視点で見ると「状態を取って, 新しい状態と, (5) という結果を返す関数」と見えてくる。

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState


