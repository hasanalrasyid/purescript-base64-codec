module Data.Base64
       ( encodeBase64
       , decodeBase64
       , Base64(..)
       , runBase64
       , fromString
       , arrayBufferToString
       , reverse
       ) where

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude ((<<<), class Show, class Eq, (<>), ($), bind, pure, show, (<$>), map)
import Simple.JSON as JSON
import Data.String as S
--import Database.PostgreSQL.Value as SQL
import Data.Bifunctor (lmap)
import Control.Monad.Except (runExcept)
import Foreign (readString)
import Data.Either (either)

-- | A boxed Base64 type to prevent accidental misuse
newtype Base64 = Base64 String

derive instance eqBase64 :: Eq Base64

-- | Show instance is for textual representations, not data representation
instance showBase64 :: Show Base64 where
  show (Base64 "") = "Base64 ()"
  show (Base64 s) =
    let r = reverse $ S.take 30 $ reverse s
     in "Base64 (..." <> r <> ")"

foreign import encodeBase64Impl :: ArrayBuffer -> String
foreign import arrayBufferToString :: ArrayBuffer -> String
foreign import reverse :: String -> String

-- | Encodes an ArrayBuffer into the base64 representation thereof
encodeBase64 :: ArrayBuffer -> Base64
encodeBase64 = Base64 <<< encodeBase64Impl

foreign import decodeBase64Impl :: Fn3 (ArrayBuffer -> Maybe ArrayBuffer) (Maybe ArrayBuffer) String (Maybe ArrayBuffer)

-- | Decode base64 content to the array buffer(byte) representation it stored internally.
decodeBase64 :: Base64 ->  ArrayBuffer
-- Uses unsafe calls to coerce types, but because Base64 can only be constructed in this module,
-- The function with this type signature is total. The underlying ffi function must however be defined in terms of Maybe
decodeBase64 (Base64 content) = unsafePartial $ fromJust $ runFn3 decodeBase64Impl Just Nothing content

-- |
runBase64 :: Base64 -> String
runBase64 (Base64 s) = s

foreign import fromStringImpl :: Fn3 (Base64 -> Maybe Base64) (Maybe Base64) String (Maybe Base64)

fromString :: String -> Maybe Base64
fromString = runFn3 fromStringImpl Just Nothing

instance readForeignBase64 :: JSON.ReadForeign Base64 where
  readImpl f = do
    s <- JSON.readImpl f
    pure $ case fromString s of
            Just a -> a
            Nothing -> Base64 "ERROR:unknown"

instance writeForeignBase64 :: JSON.WriteForeign Base64 where
  writeImpl (Base64 s) = JSON.writeImpl (s :: String)

--instance fromSQLValueBase64 :: SQL.FromSQLValue Base64 where
--  fromSQLValue s = map Base64 $ lmap show $ runExcept $ readString s
--    --r <- SQL.fromSQLValue s
--    --pure $ Base64 r
--instance toSQLValueBase64 :: SQL.ToSQLValue Base64 where
--  toSQLValue (Base64 s) = SQL.toSQLValue s
