{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.IORef
import qualified Data.Map as Map

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

type ImageMap = Map.Map FilePath Surface

data GlobalData = GlobalData { _screen   :: Surface
                             , _images   :: ImageMap
                             , _quitFlag :: Bool }
    deriving (Show)

screen :: Simple Lens GlobalData Surface
screen f gd = fmap (\x -> gd { _screen = x }) (f $ _screen gd)

images :: Simple Lens GlobalData ImageMap
images f gd = fmap (\x -> gd { _images = x }) (f $ _images gd)

quitFlag :: Simple Lens GlobalData Bool
quitFlag f gd = fmap (\x -> gd { _quitFlag = x }) (f $ _quitFlag gd)

getScreen :: IO GlobalData
getScreen = do
    screen <- setVideoMode 640 480 32 [DoubleBuf, SWSurface]
    return $ GlobalData screen Map.empty False

loadImage :: FilePath -> ImageMap -> IO ImageMap
loadImage path m = do
    surface <- load path
    return $ Map.insert path surface m

mainLoop :: StateT GlobalData IO ()
mainLoop = do
    event <- lift pollEvent
    case event of
        NoEvent -> return ()
        KeyDown (Keysym SDLK_ESCAPE _ _) -> quitFlag .= True
        _ -> lift . putStrLn $ show event
    s <- use screen
    lift $ SDL.flip s
    q <- use quitFlag
    unless q $ mainLoop

actualMain :: IO ()
actualMain = do
    initial <- getScreen
    runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
