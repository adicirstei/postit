{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad          (void)
-- import           Control.Monad.Trans.Reader (runReaderT)
-- import           Data.IORef                 (newIORef, readIORef, writeIORef)
-- import           Data.Maybe                 (isJust)

import           Control.Monad.IO.Class (MonadIO)
import           Data.Foldable          (forM_)
import           Data.GI.Base
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Word              (Word32)

-- import qualified GI.Gdk                     as Gdk
-- import qualified GI.GdkPixbuf               as GP
-- import qualified GI.GLib                    as GLib
import qualified GI.Gtk                 as Gtk

type ColorSet = (Text, Text)

data WD = WD
  { color  :: Int
  , width  :: Int
  , height :: Int
  , text   :: Text
  , css    :: Text
  }

type Model = [ WD ]



w = WD 0 initialSize initialSize "" "textview.view text {background-color: rgb(137, 209, 189);}"

b = WD 0 initialSize initialSize "" "textview.view text {background-color: rgb(174, 174, 212);}"


initialModel :: Model
initialModel = [w, b]

loadData :: MonadIO m => m Model
loadData = do
  return initialModel


initialSize :: Int
initialSize = 256

createPostitWindow :: MonadIO m => WD -> m Gtk.Window
createPostitWindow (WD c w h t s) = do
  win <- new Gtk.Window [ #defaultWidth := fromIntegral w
                        , #defaultHeight := fromIntegral h
                        ]

  textBuf <- new Gtk.TextBuffer [ #text := t]
  textAr <- new Gtk.TextView [ #buffer := textBuf ]
  Gtk.textViewSetWrapMode textAr Gtk.WrapModeWordChar
  on textBuf #changed (set win [ #title := "miau" ])
  on win #destroy Gtk.mainQuit


  #add win textAr

  ctx <- Gtk.widgetGetStyleContext textAr

  p <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p (encodeUtf8 s)
  Gtk.styleContextAddProvider ctx p (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32)
  return win


main :: IO ()
main = do
  Gtk.init Nothing

  d <- loadData
  wins <- traverse createPostitWindow d

  forM_ wins (#showAll)

  Gtk.main
