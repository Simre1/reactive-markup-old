# Reactive Markup

**Reactive-Markup is still in a testing/development phase!**

The main goal of Reactive Markup is to finally allow high-level GUI-programming in Haskell. Gone are the days of manually interfacing with C frameworks!

Here are some (planned) features:
- Declarative
- State/Event handling directly within components
- Easy to create additional elements
- Choose GUI framework depending on the environment

Here is a code example:
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.BasicElements where

import Data.Colour
import Data.Colour.Names
import qualified Data.Text as T
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Options
import ReactiveMarkup.Markup
import ReactiveMarkup.Runners.Gtk

basicElements :: SimpleMarkup '[Label '[Text, FontWeight, FontStyle, FontSize, FontColour], List '[], DynamicState, DynamicMarkup, Button '[Click, Text]] e
basicElements =
  expandMarkup $
    list
      noOps
      ( label (text "Some text")
          +: label (italicStyle // text "Italic text")
          +: list noOps [label (bold // text "Bold text"), label (bold // text "Another bold text")]
          +: [ dynamicState
                 0
                 (\i _ -> (Just $ succ i, Nothing))
                 ( flip dynamicMarkup $ \i ->
                     list
                       noOps
                       $ button (onClick () // text "Change Colour")
                         +: [label (fontSizePx 20 // fontColour (rainbowColour i) // text "Colourful!")]
                 )
             ]
      )

rainbowColour :: Int -> Colour Double
rainbowColour i = blend factor3 (blend factor2 red yellow) (blend factor2 (blend factor1 red yellow) blue)
  where
    factor1 = sin (0.35 * fromIntegral i)
    factor2 = sin (0.2 * fromIntegral i)
    factor3 = 0.5 + sin (0.25 * fromIntegral i) / 2
```

Here is a screenshot of the above example:
![](./screenshot.png)

I have already added some **documentation** of the source code! Take a look if you are interested.

## How to build yourself

First and foremost, you need to have **GTK 3** installed!

This projects currently consists of:
- reactive-markup: Main library which defines the markup and some basic components
- reactive-markup-gtk: A gtk implementation of the current components
- examples: Examples on how to use this library
- gtk-hotreload: Hot-Reloading for reactive-markup-gtk

I personally use stack as my build tool and you can build all of these 4 sub-projects with:
```bash
git clone https://github.com/Simre1/reactive-markup.git
cd reactive-markup
stack build
```

Other build systems (cabal/nix) should also work fine, but I have not set up configuration files for those.

This library depends on **gi-gtk**, so keep in mind that compilation takes quite a while.

## How to try yourself

After successfully building the project, you can start the examples by typing:
```
stack exec examples
```

If you want to edit an example, be sure to use hot-reloading:
```bash
ghcid --command="stack ghci" --run="hotReloadMarkupWithoutAsking (\_ -> pure ()) Examples.*.*"
```
Just replace _*_ with the correct path to the example.

## What is the Markup datatype?

**Markup** is an extensible datatype which can be extended with an arbitrary amount of elements.
Elements used in this example are
- Label
- List
- Button
- DynamicState
- DynamicMarkup

**Label**, **List**, **Button** directly correlate with Gtk components that you can see in the screenshot. **Dynamic State** and **Dynamic Markup** allow elements to have internal state and are used to change the font colour on a button press. Most elements also have additional type arguments (for example **Label '[Text, FontWeight, FontStyle, FontSize, FontColour]**). Those are used to determine additional capabilities of that element.

## How does my Markup get rendered?

**Markup** holds the information needed to create a GUI, but not the actual GUI itself. How the **Markup** gets interpreted depends on the used **Runner**. Currently, the only available **Runner** is for GTK, which transforms **Markup** into a GTK widget. However, it is also possible to create **Runner**s for other frameworks.

## Hot Reloading !

I am currently testing hot reloading with the help of ghcid.

You can try it out with:
```bash
ghcid --command="stack ghci" --run="hotReloadMarkupWithoutAsking $ runMarkup widgetRunner (\_ -> pure ()) Examples.BasicElements.basicElements
```

This will hot-reload the example in _examples/Examples/BasicElements.hs_.

If you are using another build system, make sure that _ghci_ loads _gtk-hotreload/src/HotReload.hs_ and _reactive-markup-gtk/src/ReactiveMarkup/Runners/Gtk.hs_. You can then hot-reload any **Markup** that is in scope in _ghci_ and that is within the capabilities of the GTK **Runner**!

## Call for participation

This library is rather ambitious and every helping hand is greatly appreciated. Feel free to tinker with this library and share your thoughts!