{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array
import qualified Data.Map as M
import Data.Maybe
import Data.Word

import Codec.Image.STB
import Data.Bitmap.OpenGL
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL

import Working.Color
import Working.GL

data RawTile = Off | On
    deriving (Eq, Enum, Ord, Show)

data Tile = Sky | Ground | Impass
    deriving (Eq, Ord, Show)

tileColors :: M.Map Tile RGB
tileColors = M.fromList [ (Sky, skyBlue)
                        , (Ground, grassGreen)
                        , (Impass, stoneGray) ]

type RawTiles = Array (Int, Int) RawTile
type Tiles = Array (Int, Int) Tile

basicTiles :: RawTiles
basicTiles = array ((0, 0), (15, 15)) xs
    where
    coords = [(x, y) | y <- [0..15], x <- [0..15]]
    stuff = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0
            ,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0
            ,0,0,0,0,0,1,1,1,0,1,1,1,0,0,0,0
            ,0,1,1,0,0,1,1,1,0,1,1,1,0,0,0,0
            ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
    xs = zip (reverse coords) (map toEnum stuff)

-- | Center, Left, Right, Down, Up
convolve :: RawTile -> RawTile -> RawTile -> RawTile -> RawTile -> Tile
convolve On  On On On On = Impass
convolve On  _  _  _  _  = Ground
convolve Off _  _  _  _  = Sky

colorTiles :: RawTiles -> Tiles
colorTiles rt = let
    bounds' = bounds rt
    check i e = if inRange bounds' i then e else On
    l i = check i $ rt ! i
    convolved (x, y) = convolve (l (x, y)) (l (x - 1, y)) (l (x + 1, y)) (l (x, y - 1)) (l (x, y + 1))
    in array bounds' [((x, y), convolved (x, y)) | (x, y) <- range bounds']

data Box v = Box (Vertex2 v) (Vertex2 v)
    deriving (Show)

data Sprite v = Colored RGB (Box v)
              | Textured TextureObject (Box v)
    deriving (Show)

data Globals = Globals { _gScreen    :: Surface
                       , _gTimestamp :: Word32
                       , _gFps       :: Int
                       , _gCharacter :: Sprite GLfloat
                       , _gTiles     :: RawTiles
                       , _gShowTiles :: Bool
                       , _gQuitFlag  :: Bool }
    deriving (Show)

makeLenses ''Globals

type Loop = StateT Globals IO ()

sBox :: Simple Lens (Sprite v) (Box v)
sBox f (Colored c b) = fmap (Colored c) (f b)
sBox f (Textured t b) = fmap (Textured t) (f b)

bX, bY, bX', bY' :: Simple Lens (Box a) a
bX f (Box (Vertex2 x y) v) = fmap (\x' -> Box (Vertex2 x' y) v) (f x)
bY f (Box (Vertex2 x y) v) = fmap (\y' -> Box (Vertex2 x y') v) (f y)
bX' f (Box v (Vertex2 x y)) = fmap (\x' -> Box v (Vertex2 x' y)) (f x)
bY' f (Box v (Vertex2 x y)) = fmap (Box v . Vertex2 x)           (f y)

bW, bH, bW', bH' :: (Num a) => Simple Lens (Box a) a
bW f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box (Vertex2 x y) (Vertex2 (x + w) y')) (f (x' - x))
bH f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box (Vertex2 x y) (Vertex2 x' (y + h))) (f (y' - y))
bW' f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box (Vertex2 (x' - w) y) (Vertex2 x' y')) (f (x' - x))
bH' f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box (Vertex2 x (y' - h)) (Vertex2 x' y)) (f (y' - y))

-- Resize the viewport such that:
--  * The smallest dimension still corresponds to at least [-1, 1]
--  * The viewport is centered on (0, 0)
resizeViewport :: GLsizei -> GLsizei -> IO ()
resizeViewport w h
    | w > h     = viewport $= (Position ((w - h) `div` 2) 0, Size h h)
    | otherwise = viewport $= (Position 0 ((h - w) `div` 2), Size w w)

resizeScreen :: GLsizei -> GLsizei -> IO Surface
resizeScreen w h = let
    flags = [OpenGL, DoubleBuf, Resizable]
    in do
        screen <- setVideoMode (fromIntegral w) (fromIntegral h) 32 flags
        resizeViewport w h
        return screen

getInitialState :: IO Globals
getInitialState = do
    screen <- resizeScreen 1 1
    let box = Colored blue $ Box (Vertex2 0.9 0.9) (Vertex2 (-0.9) (-0.9))
    return $ Globals screen 0 0 box basicTiles False False

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w _ dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

drawSprite :: Sprite GLfloat -> IO ()
drawSprite (Colored c b) = renderPrimitive Quads quad
    where
    x = b ^. bX
    y = b ^. bY
    x' = b ^. bX'
    y' = b ^. bY'
    quad = do
        color c
        vertex (Vertex2 x y)
        vertex (Vertex2 x' y)
        vertex (Vertex2 x' y')
        vertex (Vertex2 x y')
drawSprite (Textured texobj b) = do
    enableTextures
    renderPrimitive Quads quad
    disableTextures
    where
    x = b ^. bX
    y = b ^. bY
    x' = b ^. bX'
    y' = b ^. bY'
    r = 0 :: GLfloat
    s = 0 :: GLfloat
    r' = 1
    s' = 1
    enableTextures = do
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just texobj
        textureFunction $= Replace
    disableTextures = texture Texture2D $= Disabled
    quad = do
        texCoord (TexCoord2 r s)
        vertex (Vertex2 x y)
        texCoord (TexCoord2 r' s)
        vertex (Vertex2 x' y)
        texCoord (TexCoord2 r' s')
        vertex (Vertex2 x' y')
        texCoord (TexCoord2 r s')
        vertex (Vertex2 x y')

drawRawTiles :: RawTiles -> IO ()
drawRawTiles t = forM_ (assocs t) $ \((x, y), tile) -> do
    let x' = -1 + (realToFrac x / 8)
        y' = -1 + (realToFrac y / 8)
        c = if tile == On then green else red
        box = Colored c (Box (Vertex2 x' y') (Vertex2 (x' + (1 / 8)) (y' + (1 / 8))))
    drawSprite box

drawTiles :: Tiles -> IO ()
drawTiles t = forM_ (assocs t) $ \((x, y), tile) -> do
    let x' = -1 + (realToFrac x / 8)
        y' = -1 + (realToFrac y / 8)
        c = fromMaybe white $ M.lookup tile tileColors
        box = Colored c (Box (Vertex2 x' y') (Vertex2 (x' + (1 / 8)) (y' + (1 / 8))))
    drawSprite box

mma :: Int -> Int -> Int
mma new old = (19 * old + new) `div` 20

updateTimestamp :: Loop
updateTimestamp = do
    ticks <- use gTimestamp
    ticks' <- lift getTicks
    let diff = ticks' - ticks
    let fps = fromIntegral $ 1000 `div` diff
    adjusted <- gFps <%= mma fps
    lift . putStrLn $ "Ticks: " ++ show diff ++ " (FPS: " ++ show adjusted ++ ")"
    gTimestamp .= ticks'

handleEvent :: Event -> Globals -> Globals
handleEvent (KeyDown (Keysym SDLK_ESCAPE _ _)) = gQuitFlag .~ True
handleEvent (KeyDown (Keysym SDLK_t _ _)) = gShowTiles %~ not
handleEvent _ = id

mainLoop :: Loop
mainLoop = makeShine >> loop
    where
    makeShine = do
        texobj <- lift . loadTexture $ "shine2.png"
        gCharacter .= Textured texobj (Box (Vertex2 0.8 0.8) (Vertex2 0.7 0.7))
    box = Colored blue $ Box (Vertex2 0.9 0.9) (Vertex2 (-0.9) (-0.9))
    loop = do
        event <- lift pollEvent
        modify $ handleEvent event
        case event of
            VideoResize w h -> do
                s <- lift $ resizeScreen (fromIntegral w) (fromIntegral h)
                gScreen .= s
            KeyDown (Keysym SDLK_DOWN _ _) -> do
                gCharacter . sBox . bY -= 0.1
                gCharacter . sBox . bY' -= 0.1
            KeyDown (Keysym SDLK_UP _ _) -> do
                gCharacter . sBox . bY += 0.1
                gCharacter . sBox . bY' += 0.1
            KeyDown (Keysym SDLK_LEFT _ _) -> do
                gCharacter . sBox . bX -= 0.1
                gCharacter . sBox . bX' -= 0.1
            KeyDown (Keysym SDLK_RIGHT _ _) -> do
                gCharacter . sBox . bX += 0.1
                gCharacter . sBox . bX' += 0.1
            _ -> lift . putStrLn $ show event
        lift clearScreen
        lift . drawSprite $ box
        whether <- use gShowTiles
        tiles <- use gTiles
        if whether
            then lift . drawTiles . colorTiles $ tiles
            else lift . drawRawTiles $ tiles
        shine <- use gCharacter
        lift . drawSprite $ shine
        lift finishFrame
        updateTimestamp
        q <- use gQuitFlag
        unless q loop

loadTexture :: FilePath -> IO TextureObject
loadTexture path = do
    ei <- loadImage path
    let b = case ei of
            Left x  -> error x
            Right x -> x
    makeSimpleBitmapTexture b

actualMain :: IO ()
actualMain = do
    initial <- getInitialState
    checkExtensions
    _ <- runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
