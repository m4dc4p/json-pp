{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (fmap)
import System.IO (stdin, stdout)
import Data.List (sortBy)
    
import Data.Aeson
import Data.Aeson.Encode (encode)
import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as T hiding (map)
import qualified Data.Text.Lazy.IO as TIO 
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.PrettyPrint.Leijen.Text
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H (toList)
import qualified Data.Vector as V
    
main = do
  input <- B.getContents
  let result = fmap ppJson (decode input :: Maybe Value)
  TIO.putStrLn $ case result of
    Just doc -> displayT (renderPretty 0.9 100 doc)
    _ -> "Error"
    
ppJson :: Value -> Doc
ppJson (Object o) = group $ text "{" <> nest 2 (linebreak <> ppObject o) <> linebreak <> text "}"
ppJson (Array a) = case (lastIsObject, firstIsObject) of
       (True, True) -> group $ text "[" <> nest 2 (ppArr a <> text "]")
       (True, False) -> group $ text "[" <> nest 2 (linebreak <> ppArr a <> text "]")
       (False, True) -> group $ text "[" <> nest 2 (ppArr a) <> linebreak <> text "]"
       (False, False) -> group $ text "[" <> nest 2 (linebreak <> ppArr a) <> linebreak <> text "]"
  where
    lastIsObject = isObject (reverse elems)
    firstIsObject = isObject elems
    elems = elements a                                
    isObject elements = case elements of
                     Object _ : _ -> True
                     _ -> False
ppJson v = text . decodeUtf8 . encode $ v

ppArr :: Array -> Doc
ppArr = ppCommaSep . map ppJson . elements

ppObject :: Object -> Doc
ppObject = ppCommaSep . map ppProperty . sortBy (\(l, _) (r, _) -> compare l r) . properties

elements :: Array -> [Value]
elements = V.toList
           
properties :: Object -> [(Strict.Text, Value)]
properties = H.toList

ppProperty :: (Strict.Text, Value) -> Doc
ppProperty (name, value) =
    let key = T.fromStrict name
    in (dquotes (text key) <> char ':' <+> ppJson value)
                           
escapeNewlines :: Strict.Text -> Text
escapeNewlines str = T.intercalate "\\n" $ T.lines (T.fromStrict str)

ppCommaSep :: [Doc] -> Doc
ppCommaSep = group . hcat . punctuate (char ',' <> line)
