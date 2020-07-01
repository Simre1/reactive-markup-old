# Magic Markup


The main goal of magic markup is to provide a declarative way to program GUIs and to provide control structures within the markup to simplify event handling. Additionally, the markup is not limited to a fixed set of elements and can be easily extended with further components. The implementation of the markup can also be freely decided, even at runtime!

The library ist currently still in a design stage and this minimal example only shows what could be possible. I would love to hear how others feel about my ideas!

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

  widget <- runMarkup gtkRunner (\_ -> pure ()) myMarkup

  #add win widget

  #showAll win

  Gtk.main

myMarkup :: SimpleMarkup [Text, List, Local, Button] Void
myMarkup = expandMarkup $ list $ emptyMarkupBuilder
  +-> text "First element"
  +-> text "Second element"
  +-> list (emptyMarkupBuilder +-> text "Element sub list")
  +-> local (2,1) 
    (\(i,sum) _ -> ( (succ i, sum * i), Nothing)) 
    (\(_,sum) -> list $ emptyMarkupBuilder +-> button +-> text (T.pack $ show sum))
```

For now, I am working on an implementation (**gtkRunner** in _Runners/Gtk.hs_) of the Markup for GTK. I have done so for the four components **Text**, **List**, **Local** and **Button**, which you can find in _BasicElements.hs_. It is by no means a good implementation, but I just wanted to get a working example.

**Text** and **List** do what you expect, but **Button** and **Local** are more interesting. Markup generally has the ability to emit an event. This event can be handled by a **Local** element, which can change its state depending on the event and/or transmit the event further up the chain. For this example, I did not want to process any events that are not handled within the markup by a **Local**. That is why my top-level events have the type **Void**, which obviously can never be emitted.