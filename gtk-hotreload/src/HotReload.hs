module HotReload where


import System.IO.Unsafe (unsafePerformIO)
import qualified GI.Gtk as Gtk
import Data.Foldable (forM_)
import Control.Concurrent
import Data.IORef
import qualified GI.GLib as GLib

import ReactiveMarkup.Markup
import ReactiveMarkup.Runners.Gtk
import Control.Monad (when)

import System.IO (hPutStrLn, stderr)
import Control.Concurrent.Async
import Data.Maybe
 
gtkWidget :: MVar (IO Gtk.Widget)
gtkWidget = unsafePerformIO $ newEmptyMVar

shouldQuit :: IORef Bool
shouldQuit = unsafePerformIO $ newIORef False

gtkThread :: IORef (Maybe (Async ()))
gtkThread = unsafePerformIO $ (newIORef Nothing)

cleanUp :: IO ()
cleanUp = do
  tryTakeMVar gtkWidget
  writeIORef shouldQuit False
  writeIORef gtkThread Nothing


stopGtk :: IO ()
stopGtk = do
  async <- readIORef gtkThread
  case async of 
    Nothing -> pure ()
    Just x -> writeIORef shouldQuit True

setupHotReloading :: IO ()
setupHotReloading = do
  async <- asyncBound action
  writeIORef gtkThread (Just async)
  pure ()
  where action = do
          Gtk.init Nothing
          win <- Gtk.new Gtk.Window [#title Gtk.:= "HotReload"]
          Gtk.on win #destroy Gtk.mainQuit
          #showAll win
          let loop = do
                shouldQuit <- readIORef shouldQuit
                if shouldQuit
                  then #close win >> pure False
                  else do
                    maybeNewWidget <- tryReadMVar gtkWidget
                    case maybeNewWidget of
                      Nothing -> pure True
                      Just _ -> do
                        makeWidget <- takeMVar gtkWidget
                        widgets <- Gtk.containerGetChildren win
                        forM_ widgets $ \w -> Gtk.containerRemove win w
                        widget <- makeWidget
                        Gtk.containerAdd win widget
                        #showAll widget
                        pure True
          timer <- GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT 1 loop 
          Gtk.main
          GLib.sourceRemove timer
          cleanUp


hotReloadMarkup :: SubList (Merge elems children) GtkRunner => Markup elems children e -> (e -> IO ()) -> IO ()
hotReloadMarkup markup handleEvent = do
  maybeThread <- readIORef gtkThread
  case maybeThread of
    Nothing -> do
      putStrLn "GTK is not set up! Should I start GTK now and try again? (y/n)"
      let handleUserInput = do
            answer <- getLine
            case answer of
              "y" -> do
                putStrLn "Setting up GTK. Please wait for a moment." 
                setupHotReloading
                threadDelay 1000000
                hotReloadMarkup markup handleEvent
              "n" -> putStrLn "Hot-Reloading is not possible without setting up GTK. Therefore, there is nothing to do."
              _ -> putStrLn "You need to either input the character y or n and no other characters." *> handleUserInput
      handleUserInput
    Just _ -> do
      gtkState <- defaultGtkState
      putMVar gtkWidget $ runGtk gtkState $ runMarkup gtkRunner handleEvent markup

hotReloadMarkupWithoutAsking :: SubList (Merge elems children) GtkRunner => Markup elems children e -> (e -> IO ()) -> IO ()
hotReloadMarkupWithoutAsking markup handleEvent = do
  maybeThread <- readIORef gtkThread
  case maybeThread of
    Nothing -> do
                putStrLn "Setting up GTK. Please wait for a moment." 
                setupHotReloading
                threadDelay 1000000
                hotReloadMarkup markup handleEvent
    Just _ -> do
      gtkState <- defaultGtkState
      putMVar gtkWidget $ runGtk gtkState $ runMarkup gtkRunner handleEvent markup
