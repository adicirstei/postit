{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad          (void, when)

import           Control.Monad.IO.Class (MonadIO)
import           Data.Foldable          (forM_)
import           Data.GI.Base
import           Data.Hashable          (Hashable (..))
import           Data.HashMap.Lazy      (HashMap, fromList, lookupDefault)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Word              (Word32)

import           Data.Monoid            ((<>))

import qualified GI.Gtk                 as Gtk

import           Data.IORef
import           Debug.Trace
import qualified GI.Gio                 as Gio


data Msg
  = NoteDeleted Int
  | NoteUpdated Int WD

type ColorSet = (Text, Text)
data Style = Green | Yellow | Red | Purple | Gray
  deriving (Eq)

instance Hashable Style where
  hashWithSalt s Green  = s
  hashWithSalt s Yellow = s + 1
  hashWithSalt s Red    = s + 2
  hashWithSalt s Purple = s + 3
  hashWithSalt s Gray   = s + 4


data WD = WD
  { id     :: Int
  , color  :: Style
  , width  :: Int
  , height :: Int
  , text   :: Text
  }

type Model = [ WD ]

colorMap :: HashMap Style ColorSet
colorMap = fromList [ (Green, ("rgb(137, 209, 189)", "rgb(32, 123, 53)"))
                    , (Yellow, ("rgb(253, 222, 127)", "rgb(229, 90, 18)"))
                    , (Red, ("rgb(251, 173, 113)", "rgb(203, 26, 25)"))
                    , (Purple, ("rgb(141, 173, 209)", "rgb(115, 42, 139)"))
                    , (Gray, ("rgb(175, 175, 175)", "rgb(65, 65, 65)"))
                    ]

w = WD 0 Yellow initialSize initialSize ""

b = WD 1 Purple initialSize initialSize ""

defaultColorSet :: ColorSet
defaultColorSet = ("rgb(137, 209, 189)", "rgb(32, 123, 53)")


onWindowClose :: IO ()
onWindowClose = do
  traceIO "buu"
  Gtk.mainQuit


styleCss :: Text -> Text
styleCss color =
  "*, * * {background-color: "
  <> color
  <> ";}"

getStyleColorSet :: Style -> ColorSet
getStyleColorSet s = lookupDefault defaultColorSet s colorMap


getTitleColor :: Style -> Text
getTitleColor = snd . getStyleColorSet
getBodyColor = fst . getStyleColorSet


initialModel :: Model
initialModel = [w, b, WD 2 Gray initialSize initialSize "", WD 3 Red initialSize 400 ""]

loadData :: MonadIO m => m Model
loadData = do
  return initialModel


initialSize :: Int
initialSize = 256

createPostitWindow :: IORef Int -> WD -> IO Gtk.Window
createPostitWindow ref (WD i s w h t) = do
  win <- new Gtk.Window [ #defaultWidth := fromIntegral w
                        , #defaultHeight := fromIntegral h
                        , #decorated := False
                        ]

  let bgCss = styleCss $ getBodyColor s
      tlCss = styleCss $ getTitleColor s


  textBuf <- new Gtk.TextBuffer [ #text := t]
  textAr <- new Gtk.TextView [ #buffer := textBuf ]
  Gtk.textViewSetWrapMode textAr Gtk.WrapModeWordChar
  on textBuf #changed (set win [ #title := "miau" ])
  on win #destroy $ do
    count <- atomicModifyIORef' ref (\c -> (c -1, c-1))
    when (count <= 0) Gtk.mainQuit

  buttAdd <- new Gtk.Button [ #label := "+" ]
  buttColor <- new Gtk.Button [ #label := "..." ]
  buttDel <- new Gtk.Button [ #label := "X" ]

  on buttDel #clicked (#close win)
  _ <- atomicModifyIORef' ref (\c -> (c + 1, ()))

  box <- new Gtk.VBox []

  toolbar <- new Gtk.Box []


  Gtk.boxPackStart toolbar buttAdd False False 0
  Gtk.boxPackEnd toolbar buttDel False False 0
  Gtk.boxPackEnd toolbar buttColor False False 0


  Gtk.boxPackStart box toolbar False False 3
  Gtk.boxPackStart box textAr True True 0

  #add win box

  ctx <- Gtk.widgetGetStyleContext textAr
  bgp <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData bgp (encodeUtf8 bgCss)
  Gtk.styleContextAddProvider ctx bgp (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32)


  tctx <- Gtk.widgetGetStyleContext toolbar
  tlp <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData tlp (encodeUtf8 tlCss)
  Gtk.styleContextAddProvider tctx tlp (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32)

  return win


main :: IO ()
main = do
  Gtk.init Nothing

  app <- new Gtk.Application [ #flags := [ Gio.ApplicationFlagsFlagsNone ] ]
  winCount <- newIORef 0

  d <- loadData
  wins <- traverse (createPostitWindow winCount) d

  forM_ wins (#showAll)

  Gtk.main
