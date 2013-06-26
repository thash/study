-- 独自のデータ型を定義する. 新しい型はdataを使って定義する.

-- file: ch03/BookStore.hs
-- BookInfo: 型コンストラクタ, Book: 値コンストラクタ
-- id, title, authers. これをスロットと言う.
data BookInfo = Book Int String [String]
                deriving (Show)

-- 次のMagazineInfoはBookInfoと同じ構成要素を持つが, Haskellは異なるものとして扱う.
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

-- derivingを付けないと, 新しい型を作っても何も出来ない.
-- Bookという値コンストラクタを"関数"として扱うことで, BookInfo型の新しい値を生成できる.

-- ghciの中でやるならletを付ける.
-- *Main> :t Book
-- Book :: Int -> String -> [String] -> BookInfo
myInfo = Book 939393939 "Algebra of Programming" ["Richard Bird", "oege de Moor"]

-- :t myInfo
-- myInfo :: BookInfo


-- *Main> :info BookInfo
-- data BookInfo = Book Int String [String]
--         -- Defined at ch03/BookStore.hs:6:6
-- instance Show BookInfo -- Defined at ch03/BookStore.hs:7:27

data Bool = True | False
-- Bool型は, TrueもしくはFalseという値をとる.
-- | は値コンストラクタを区切る.
-- 型の世界と値の世界にわかれている, Boolは型の世界, Trueは値の世界

-- True, と書いた時点で引数0個の関数適用が完了して"値"になっている.

-- Algebraic data types 代数データ型. Abstract data typeもあるので略さないように.
-- とはなんぞや.

type CustomerID = Int
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- *Main> :t CreditCard
-- CreditCard :: CardNumber -> CardHolder -> Address -> BillingInfo
-- *Main> CreditCard "20202020220" "Thomas Grandgrind" ["Dickens", "Englshnd"]
-- CreditCard "20202020220" "Thomas Grandgrind" ["Dickens","Englshnd"]
-- *Main> :t it
-- it :: BillingInfo
-- *Main> InVoice
--
-- <interactive>:19:1:
--     Not in scope: data constructor `InVoice'
--     Perhaps you meant `Invoice' (line 46)

-- ふつうHaskellでは関数を表示できない(= 関数をShow)
