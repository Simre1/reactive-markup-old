{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (find)
import qualified Data.Text as T
import ReactiveMarkup.Markup
import Examples.BasicElements
import Examples.GridLayout
import ReactiveMarkup.Runners.Gtk

main :: IO ()
main = do
  chosenMarkup <- chooseMarkup
  case chosenMarkup of
    Nothing -> pure ()
    Just (name, markup) -> basicGtkSetup (T.pack name) (\_ -> pure ()) markup
  where
    chooseMarkup :: IO (Maybe (String, SimpleMarkup GtkElements Void))
    chooseMarkup = case presetMarkup of
      Just x -> pure $ find (\(name, _) -> name == x) examples
      Nothing -> do
        putStrLn "You can choose an example by typing out its name. These examples are available:"
        sequence $ putStrLn . ("- " <>) . fst <$> examples
        putStrLn "You can also exit now by typing quit."
        let askForExampleName = do
              input <- getLine
              case input of
                "quit" -> pure Nothing
                str -> case lookup str examples of
                  Just x -> pure $ Just (str, x)
                  Nothing -> do
                    putStrLn "That example does not exist. Choose one from the list or type quit."
                    askForExampleName
        askForExampleName

examples :: [(String, SimpleMarkup GtkElements Void)]
examples = 
  [ ("basic-elements", expandMarkup basicElements)
  , ("grid-layout", expandMarkup grid)
  ]

presetMarkup :: Maybe String
presetMarkup = Nothing