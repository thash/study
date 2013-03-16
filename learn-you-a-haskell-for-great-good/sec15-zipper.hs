import Data.List (break)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x



-- 背後に残った道しるべ
type Breadcrumbs' = [Direction] -- 後に改良するので名前回避

goLeft' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goLeft' (Node _ l _, bs) = (l, L:bs)

goRight' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goRight' (Node _ _ r, bs) = (r, R:bs)


-- Breadcrumbsを改良し、左右に移動するとき無視してきた情報をすべて含む
data Crumb a = LeftCrumb a (Tree a)
             | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs a)


modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

-- 見た目のためだけにこんな関数を定義。これ型とかいらんのな...
x -: f = f x

-- *Main>  let newFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')
-- *Main> newFocus
-- (Node 'P' (Node 'S' Empty Empty) (Node 'A' Empty Empty),[RightCrumb 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- *Main> let farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
-- *Main> farLeft
-- (Empty,[LeftCrumb 'N' Empty,LeftCrumb 'L' (Node 'T' Empty Empty),LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])
--
-- *Main> let newFocus = farLeft -: attach (Node 'Z' Empty Empty)
-- *Main> newFocus
-- (Node 'Z' Empty Empty,[LeftCrumb 'N' Empty,LeftCrumb 'L' (Node 'T' Empty Empty),LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])

-- てっぺんまで行く関数
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)


-- リストと部分リストに対するZipperを作る
type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

-- *Main> let xs = [1,2,3,4]
-- *Main> goForward (xs, [])
-- ([2,3,4],[1])
-- *Main> goForward ([2,3,4], [1])
-- ([3,4],[2,1])


-----------------------------------------------------------------------
--              15.3. ファイルシステムを作ろうぜ！
-----------------------------------------------------------------------

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_likne.wav" "baaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_th.jpg" "blaegth"
            , File "watermelon_smash.gif" "Smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best must"
        , Folder "programs"
            [ File "farw.exe" "10gotofart"
            , File "owl_bandig.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = pring (fix error)"
                , File "random.hs" "main = pringt 4"
                ]
            ]
        ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) =
    (Folder name (ls ++ [item] ++ rs), bs)

-- ここでimprot Data.List (break) した。一番上に置いてるけど

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName


-- *Main> let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
-- *Main> newFocus
-- (File "skull_man(scary).bmp" "Yikes!",[FSCrumb "pics" [File "ape_th.jpg" "blaegth",File "watermelon_smash.gif" "Smash!!"] [],FSCrumb "root" [File "goat_yelling_likne.wav" "baaa",File "pope_time.avi" "god bless"] [File "dijon_poupon.doc" "best must",Folder "programs" [File "farw.exe" "10gotofart",File "owl_bandig.dmg" "mov eax, h00t",File "not_a_virus.exe" "really not a virus",Folder "source code" [File "best_hs_prog.hs" "main = pring (fix error)",File "random.hs" "main = pringt 4"]]]])

-- 求める結果だけならfstでいいね
-- *Main> fst newFocus
-- File "skull_man(scary).bmp" "Yikes!"

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)

-- *Main> let newFocus = (myDisk,[]) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "hogeeee") -: fsUp
-- *Main> newFocus
-- (Folder "root" [File "goat_yelling_likne.wav" "baaa",File "pope_time.avi" "god bless",Folder "pics" [File "heh.jpg" "hogeeee",File "ape_th.jpg" "blaegth",File "watermelon_smash.gif" "Smash!!",File "skull_man(scary).bmp" "Yikes!"],File "dijon_poupon.doc" "best must",Folder "programs" [File "farw.exe" "10gotofart",File "owl_bandig.dmg" "mov eax, h00t",File "not_a_virus.exe" "really not a virus",Folder "source code" [File "best_hs_prog.hs" "main = pring (fix error)",File "random.hs" "main = pringt 4"]]],[])


