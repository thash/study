-- read from file, and write to file
--
-- *Main> import System.IO
-- *Main System.IO> :t openFile
-- openFile :: FilePath -> IOMode -> IO Handle
-- *Main System.IO> :t writeFile
-- writeFile :: FilePath -> String -> IO ()
--    ↑ 本では writeFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a になってたけど。
--    ...間違えた。本で説明してたのは"with"Fileだった。
-- *Main System.IO> :info withFile
-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

--  FilePathは単なるStringのalias.
-- *Main System.IO> :info FilePath
-- type FilePath = String  -- Defined in `GHC.IO'

--  IOModeはこんな型クラスです。
-- type FilePath = String  -- Defined in `GHC.IO'
-- *Main System.IO> :info IOMode
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
--         -- Defined in `GHC.IO.IOMode'
-- instance Enum IOMode -- Defined in `GHC.IO.IOMode'
-- instance Eq IOMode -- Defined in `GHC.IO.IOMode'
-- instance Ord IOMode -- Defined in `GHC.IO.IOMode'
-- instance Read IOMode -- Defined in `GHC.IO.IOMode'
-- instance Show IOMode -- Defined in `GHC.IO.IOMode'


