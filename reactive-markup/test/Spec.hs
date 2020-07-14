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
    let helloWorld = runMarkup basicRunner handleVoid $ (label mempty "Hello World!")
    helloWorld `shouldBe` Identity "Hello World!"
  
  it "Empty List" $ do
    let emptyList = runMarkup basicRunner handleVoid $ list mempty $ 
          emptyMarkupBuilder
    emptyList `shouldBe` Identity "[]"

  it "List with Label" $ do
    let emptyList = runMarkup basicRunner handleVoid $ list mempty $ emptyMarkupBuilder
          +-> label mempty "Hello"
          +-> label mempty "World!"
    emptyList `shouldBe` Identity "[Hello,World!]"

  it "Recursive List" $ do
    let markup = runMarkup basicRunner handleVoid $ list mempty $ emptyMarkupBuilder
          +-> label mempty "hello"
          +-> (list mempty $ emptyMarkupBuilder
            +-> label mempty "label"
            +-> (list mempty $ emptyMarkupBuilder
              +-> label mempty "label"))
    markup `shouldBe` Identity "[hello,[label,[label]]]"

  it "Overwrite Runner" $ do
    let overwritingRunner = basicRunner |-> (\(Label _ str) _ _ -> pure $ str <> "!")
        markup = runMarkup overwritingRunner handleVoid $ list mempty $ emptyMarkupBuilder
          +-> label mempty "Hello"
          +-> (list mempty $ emptyMarkupBuilder +-> label mempty "Hello")
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
  --                     (\elem@(Label str) _ handleEvent -> runMarkup  handleEvent (label str) <> Identity "!")
  --               in runIdentity . runMarkup newRunner handleEvent <$> markupBuilder 
  --           )
  --       markup = runMarkup overwritingRunner (\_ -> pure ()) $ list $ emptyMarkupBuilder
  --         +-> label "Hello"
  --         +-> (list $ emptyMarkupBuilder +-> label "Hello")
  --   markup `shouldBe` "[Hello!,[Hello!!]]"

  where
    basicRunner :: Runner '[Label, List] Identity T.Text
    basicRunner = emptyRunner
      |-> (\(Label _ str) _ _ -> pure str)
      |-> (\(List _ markupBuilder) (runner) handleEvent -> 
        let removeLast = if (length (markupBuilder) == 0)
              then id 
              else T.init
        in pure $ (<> "]") $ removeLast $ foldl (\a b -> a <> b <> ",") "[" $ 
            runIdentity . runMarkup runner handleEvent <$> markupBuilder
        )

handleVoid :: Void -> Identity ()
handleVoid _ = pure ()