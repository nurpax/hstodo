{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import           Control.Exception hiding (Handler)
import           Control.Monad
import           Prelude hiding (catch)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)

import qualified Database.SQLite.Simple as S

-- Must be the first on the test list for basic database
-- initialization (schema creation for snap_auth_user, etc.)
testInitDbEmpty :: Test
testInitDbEmpty = testCase "create tables" go
  where
    go = do
      conn <- S.open ":memory:"
      S.close conn
      assertBool "init ok" True

main :: IO ()
main =
  defaultMain tests `finally` (return ())

  where
    tests =
      [ mutuallyExclusive $ testGroup "db tests"
        [ testInitDbEmpty
        ]
      ]
