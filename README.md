# Reactive Markup

The main goal of Reactive Markup is to provide a declarative way to program GUIs and to provide control structures within the markup to simplify event handling. Additionally, the markup is not limited to a fixed set of elements and can be easily extended with further components. The implementation of the markup can also be freely decided, even at runtime!

The library ist currently still in a design stage and this minimal example only shows what could be possible. I would love to hear what others think about my ideas!

Here is a code example:
```haskell
import qualified GI.Gtk as Gtk

import Markup
import BasicElements
import Runners.Gtk
import qualified Data.Text as T

main :: IO ()
main = do
  Gtk.init Nothing

  win <- Gtk.new Gtk.Window [ #title Gtk.:= "Example" ]

  Gtk.on win #destroy Gtk.mainQuit

  myWidget <- runMarkup gtkRunner (\_ -> pure ()) myMarkup

  #add win myWidget

  #showAll win

  Gtk.main

myMarkup :: SimpleMarkup [Text, List, LocalState, Button] Void
myMarkup = expandMarkup $ list $ emptyMarkupBuilder
  +-> text "First element"
  +-> text "Second element"
  +-> list (emptyMarkupBuilder +-> text "Element sub list")
  +-> local (2,1) 
    (\(i,sum) _ -> ( (succ i, sum * i), Nothing)) 
    (\(_,sum) -> list $ emptyMarkupBuilder +-> button +-> text (T.pack $ show sum))
```

For now, I am working on an implementation (**gtkRunner** in _Runners/Gtk.hs_) of the Markup for GTK. I have created a simple implementation for the four components **Text**, **List**, **LocalState** and **Button**, which you can find in _BasicElements.hs_.

Here is a screenshot of the above example with my simplistic implementation:
![](./screenshot.png)

## How to build yourself

First and foremost, you need to have **GTK 3** installed!

To build this library, I personally use stack.

```bash
stack build
```

However, cabal ought to also work just fine.

This library depends on **gi-gtk**, so keep in mind that compilation takes quite a while.

## How it works

I have already added some **documentation** of the source code! Take a look if you are interested.

**Text**, **List**, **Button**, **LocalState** are all elements. **SimpleMarkup** is just a wrapper for those elements and the list parameter of **SimpleMarkup** keeps track of all used elements. The second parameter of **SimpleMarkup** is the event type that this **SimpleMarkup** can emit. Since I did not want to handle any events at the top-level, I just chose **Void** so that it is impossible to emit any events at the top-level.

### Explanation of each element

- **Text**: An element to render text
- **List**: An element which can contain other elements
- **Button**: An element which emits an event
- **LocalState**: An element handles events and allows for state within markup

### Used Runner determines the result

By itself, *myMarkup* is really just markup which can be executed in various ways. In order to use this markup, you have to provide a **Runner** which transforms this markup into GUI elements. In this example, I have used **gtkRunner** which converts *myMarkup* into GTK widgets.

The transformation happens here: `myWidget <- runMarkup gtkRunner (\_ -> pure ()) myMarkup`
The rest of the main function just sets up GTK and creates a window.

This means that you can define your markup once while using different runners depending on the situation.

## Call for participation

This library is still in its design phase and I would like to gather as many opinions as possible. So feel free to tinker with this library and share your thoughts about this concept.

