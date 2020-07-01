import Test.Hspec

import Markup
import BasicElements
import Data.Void
import qualified Data.Text as T

main :: IO ()
main = hspec basicElements

basicElements :: SpecWith ()
basicElements = describe "Testing basic elements" $ do

  it "Text" $ do
    let helloWorld = runMarkup basicRunner handleVoid $ (text "Hello World!")
    helloWorld `shouldBe` "Hello World!"
  
  it "Empty List" $ do
    let emptyList = runMarkup basicRunner handleVoid $ list $ 
          emptyMarkupBuilder
    emptyList `shouldBe` "[]"

  it "List with Text" $ do
    let emptyList = runMarkup basicRunner handleVoid $ list $ emptyMarkupBuilder
          +-> text "Hello"
          +-> text "World!"
    emptyList `shouldBe` "[Hello,World!]"

  it "Recursive List" $ do
    let markup = runMarkup basicRunner handleVoid $ list $ emptyMarkupBuilder
          +-> text "hello"
          +-> (list $ emptyMarkupBuilder
            +-> text "text"
            +-> (list $ emptyMarkupBuilder
              +-> text "text"))
    markup `shouldBe` "[hello,[text,[text]]]"

  it "Overwrite Runner" $ do
    let overwritingRunner = basicRunner |-> (\(Text str) _ _ -> str <> "!")
        markup = runMarkup overwritingRunner handleVoid $ list $ emptyMarkupBuilder
          +-> text "Hello"
          +-> (list $ emptyMarkupBuilder +-> text "Hello")
    markup `shouldBe` "[Hello!,[Hello!]]"
  
  it "Recursively overwrite Runner" $ do
    let overwritingRunner = basicRunner 
          |-> (\(List markupBuilder) runner handleEvent -> 
            let f = if (length ((markupBuilder)) == 0)
                  then id 
                  else T.init
            in f (foldl (\a b -> a <> b <> ",") "[" $ flip fmap (markupBuilder) $
                  runMarkup (runner |-> (\elem@(Text str) _ handleEvent -> runMarkup runner handleEvent (toMarkup elem)  <> "!")) handleEvent) 
                <> "]")
        markup = runMarkup overwritingRunner (\_ -> pure ()) $ list $ emptyMarkupBuilder
          +-> text "Hello"
          +-> (list $ emptyMarkupBuilder +-> text "Hello")
    markup `shouldBe` "[Hello!,[Hello!!]]"

  where
    basicRunner :: Runner '[Text, List] T.Text
    basicRunner = emptyRunner
      |-> (\(Text str) _ _ -> str)
      |-> (\(List markupBuilder) (runner) handleEvent -> 
        let f = if (length (markupBuilder) == 0)
              then id 
              else T.init
        in f (foldl (\a b -> a <> b <> ",") "[" (runMarkup runner handleEvent <$> markupBuilder)) <> "]")

handleVoid :: Void -> IO ()
handleVoid _ = pure ()