module SplitLines ( splitLines ) where
-- module SplitLines where, だけにすれば全部export.

splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of
             ('\r':'\n':rest) -> splitLines rest
             ('\r':rest) -> splitLines rest
             ('\n':rest) -> splitLines rest
             _           -> []

isLineTerminator c = c == '\r' || c == '\n'

-----------------------------------------------
-- break --
-- Prelude> :t break
-- break :: (a -> Bool) -> [a] -> ([a], [a])
--
-- 最初にぶつかるところまでしか面倒見ない.
-- Prelude> break odd [2,3,4,5,6,7]
-- ([2],[3,4,5,6,7])

-- *Main> break isLineTerminator "foo\r\nhoge"
-- ("foo","\r\nhoge")
