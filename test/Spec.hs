import Test.Hspec

import ReactiveMarkup.Markup
import ReactiveMarkup.Elements.Basic
import Data.Void
import qualified Data.Text as T
import Data.Functor.Identity

main :: IO ()
main = hspec basicElements

basicElements :: SpecWith ()
basicElements = describe "Testing basic elements" $ do

  it "Text" $ do
    let helloWorld = runMarkup basicRunner handleVoid $ (text "Hello World!")
    helloWorld `shouldBe` Identity "Hello World!"
  
  it "Empty List" $ do
    let emptyList = runMarkup basicRunner handleVoid $ list $ 
          emptyMarkupBuilder
    emptyList `shouldBe` Identity "[]"

  it "List with Text" $ do
    let emptyList = runMarkup basicRunner handleVoid $ list $ emptyMarkupBuilder
          +-> text "Hello"
          +-> text "World!"
    emptyList `shouldBe` Identity "[Hello,World!]"

  it "Recursive List" $ do
    let markup = runMarkup basicRunner handleVoid $ list $ emptyMarkupBuilder
          +-> text "hello"
          +-> (list $ emptyMarkupBuilder
            +-> text "text"
            +-> (list $ emptyMarkupBuilder
              +-> text "text"))
    markup `shouldBe` Identity "[hello,[text,[text]]]"

  it "Overwrite Runner" $ do
    let overwritingRunner = basicRunner |-> (\(Text str) _ _ -> pure $ str <> "!")
        markup = runMarkup overwritingRunner handleVoid $ list $ emptyMarkupBuilder
          +-> text "Hello"
          +-> (list $ emptyMarkupBuilder +-> text "Hello")
    markup `shouldBe` Identity "[Hello!,[Hello!]]"
  
  -- Not supported anymore. Practical testing is required to see if this feature would have been useful.
  -- it "Recursively overwrite Runner" $ do
  --   let overwritingRunner = basicRunner 
  --         |-> (\(List markupBuilder) (runner) handleEvent -> 
  --           let removeLast = if (length (markupBuilder) == 0)
  --                 then id 
  --                 else T.init
  --           in pure $ (<> "]") $ removeLast $ foldl (\a b -> a <> b <> ",") "[" $ 
  --               let newRunner = runner |-> 
  --                     (\elem@(Text str) _ handleEvent -> runMarkup  handleEvent (text str) <> Identity "!")
  --               in runIdentity . runMarkup newRunner handleEvent <$> markupBuilder 
  --           )
  --       markup = runMarkup overwritingRunner (\_ -> pure ()) $ list $ emptyMarkupBuilder
  --         +-> text "Hello"
  --         +-> (list $ emptyMarkupBuilder +-> text "Hello")
  --   markup `shouldBe` "[Hello!,[Hello!!]]"

  where
    basicRunner :: Runner '[Text, List] Identity T.Text
    basicRunner = emptyRunner
      |-> (\(Text str) _ _ -> pure str)
      |-> (\(List markupBuilder) (runner) handleEvent -> 
        let removeLast = if (length (markupBuilder) == 0)
              then id 
              else T.init
        in pure $ (<> "]") $ removeLast $ foldl (\a b -> a <> b <> ",") "[" $ 
            runIdentity . runMarkup runner handleEvent <$> markupBuilder
        )

handleVoid :: Void -> Identity ()
handleVoid _ = pure ()