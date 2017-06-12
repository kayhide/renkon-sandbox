{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Maybe
import Data.Either
import Data.FileEmbed
import Data.Text as Text
import Data.Text.Encoding
import Data.ByteString
import Text.Inflections
import Text.Mustache
import Control.Monad
import Control.Monad.IO.Class
import Control.Arrow
import Options.Declarative
import Formatting

template = $(embedDir "template")


main :: IO ()
main = do
  run_ generate'
  return ()

generate'
  :: Arg "NAME" String
  -> Flag "s" '["src"] "DIR" "source dir (default: src)" (Def "src" String)
  -> Cmd "Run sandbox generator" ()
generate' name srcDir = liftIO $ do
  let name' = get name
      srcDir' = get srcDir
      binding = Binding (Text.pack name')
  mapM_ (print' . process binding) template
  where
    print' (x, y) = fprint format' x y
    format' = (stext % ln % stext % ln)



process :: Binding -> (String, ByteString) -> (Text, Text)
process binding (key, content) = (substitute key' &&& substitute content') binding
  where
    Right key' = compileTemplate (key ++ "_key") $ Text.pack key
    Right content' = compileTemplate (key ++ "_content") $ decodeUtf8 content


data Binding = Binding
  { _name :: Text
  }

instance ToMustache Binding where
  toMustache binding = object
    [ "name" ~> object
      [ "camelized" ~> camelize name'
      , "underscored" ~> underscore name'
      ]
    ]
    where
      Right name' = parseCamelCase [] $ _name binding


-- * Formatting

-- | @ln@ formatter
ln :: Format r r
ln = "\n"
