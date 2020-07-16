import Data.Void
import qualified Data.Text as T
import Data.Functor.Identity
import Test.Hspec

import ReactiveMarkup.Markup
import ReactiveMarkup.Elements.Basic

main :: IO ()
main = hspec basicElements

basicElements :: SpecWith ()
basicElements = describe "Testing basic elements" $ do

  it "Label" $ do
    let helloWorld = runMarkup basicRunner handleVoid $ (label "Hello World!")
    helloWorld `shouldBe` "Hello World!"
  
  it "Empty List" $ do
    let emptyList = runMarkup basicRunner handleVoid $ list $ 
          emptyMarkupBuilder
    emptyList `shouldBe` "[]"

  it "List with Label" $ do
    let emptyList = runMarkup basicRunner handleVoid $ list $ emptyMarkupBuilder
          +-> label "Hello"
          +-> label "World!"
    emptyList `shouldBe` "[Hello,World!]"

  it "Recursive List" $ do
    let markup = runMarkup basicRunner handleVoid $ list $ emptyMarkupBuilder
          +-> label "hello"
          +-> (list $ emptyMarkupBuilder
            +-> label "label"
            +-> (list $ emptyMarkupBuilder
              +-> label "label"))
    markup `shouldBe` "[hello,[label,[label]]]"

  it "Overwrite Runner" $ do
    let overwritingRunner = basicRunner |-> (\(Label str) _ _ -> str <> "!")
        markup = runMarkup overwritingRunner handleVoid $ list $ emptyMarkupBuilder
          +-> label "Hello"
          +-> (list $ emptyMarkupBuilder +-> label "Hello")
    markup `shouldBe` "[Hello!,[Hello!]]"
  
  -- Not supported anymore. Practical testing is required to see if this feature would have been useful.
  -- it "Recursively overwrite Runner" $ do
  --   let overwritingRunner = basicRunner 
  --         |-> (\(List markupBuilder) (runner) handleEvent -> 
  --           let removeLast = if (length (markupBuilder) == 0)
  --                 then id 
  --                 else T.init
  --           in pure $ (<> "]") $ removeLast $ foldl (\a b -> a <> b <> ",") "[" $ 
  --               let newRunner = runner |-> 
  --                     (\elem@(Label str) _ handleEvent -> runMarkup  handleEvent (label str) <> "!")
  --               in runIdentity . runMarkup newRunner handleEvent <$> markupBuilder 
  --           )
  --       markup = runMarkup overwritingRunner (\_ -> pure ()) $ list $ emptyMarkupBuilder
  --         +-> label "Hello"
  --         +-> (list $ emptyMarkupBuilder +-> label "Hello")
  --   markup `shouldBe` "[Hello!,[Hello!!]]"

  where
    basicRunner :: Runner '[Label, List] Identity T.Text
    basicRunner = emptyRunner
      |-> (\(Label str) _ _ -> str)
      |-> (\(List markupBuilder) (runner) handleEvent -> 
        let removeLast = if (length (markupBuilder) == 0)
              then id 
              else T.init
        in (<> "]") $ removeLast $ foldl (\a b -> a <> b <> ",") "[" $ 
            runMarkup runner handleEvent <$> markupBuilder
        )

handleVoid :: Void -> Identity ()
handleVoid _ = pure ()