{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.HotKey where

import qualified Data.Text as T
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Input
import ReactiveMarkup.Elements.Options
import ReactiveMarkup.Markup

hotKeyExample :: Markup '[DynamicState] '[DynamicMarkup, HotKey, Label '[Text]] outerEvent
hotKeyExample =
  dynamicState
    (0 :: Int)
    (\i _ -> (Just $ succ i, Nothing))
    ( \dynI ->
        ( dynamicMarkup
            dynI
            ( \i ->
                hotKey
                  KeyQ
                  []
                  ()
                  (label (text $ T.pack (show i)))
            )
        )
    )