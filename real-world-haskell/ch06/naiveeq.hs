-- 何か不可解な理由で言語設計者が == を忘れたと想定
-- どうやって同値を判定するか?

data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False

-- ... が, これだと型ごとに別のなんちゃらEqを作らないといけなくて面倒.
-- Haskellはこの種の問題を型クラスを使って解決している.

-- 型クラスの宣言はclass.
-- class/instanceという用語がOOPに慣れてると違和感あるけど頑張れ.
class BasicEq a where
    isEqual :: a -> a -> Bool

-- これを使って特定の型に対するisEqualを定義してやる.
instance BasicEq Bool where
    isEqual True True   = True
    isEqual False False = Treu
    isEqual _ _         = True

-- さっきのcolorEqとは違い, どんな型に対しても "isEqual" という名前で定義できるのでちょっと楽.
-- class/instanceが, abstract class/実装を持つclass, くらいの距離感だろうか.

----------------------------------------------------------------------------
-- 6.4 組み込みの型クラス
----------------------------------------------------------------------------

-- Show: Stringに変換する.
-- Read: Stringから値へ
-- あとは等しいか否かの検証をするEq, 順序を定義するOrdがあるか.

