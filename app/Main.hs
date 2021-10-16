
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified SDL as SDL
import Foreign.Ptr ( castPtr, nullPtr )
import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL.Input.Keyboard.Codes as SDL

-- diagrams-cairo
import Diagrams.Backend.Cairo as Cairo

-- diagrams
import Diagrams.Prelude hiding (view, Vector, (*^), (^+^))
import Diagrams.TwoD.Text (text)

-- base
import Data.Int (Int32)
import Data.Word (Word8)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar, modifyMVar_) -- (newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (forM, forM_, replicateM)
import Data.Maybe (listToMaybe)
import Data.Complex
import Data.List (intersect,sortOn, groupBy)

-- palette
-- import Data.Colour.Palette.ColorSet

type NormalDiagram = Diagram V2

type GenericDiagram a = QDiagram V2 Double a

type SelectableDiagram = GenericDiagram [String]

-- rasterize :: SizeSpec V2 Int -> Diagram V2 -> Diagram V2
-- rasterize sz d = sizedAs d $ imageEmb $ ImageRGBA8 $ renderImage sz d

modifyMVarPure_ :: MVar a -> (a -> a) -> IO ()
modifyMVarPure_ var f = modifyMVar_  var $ return . f

value :: Monoid m => m -> QDiagram v n Any -> QDiagram v n m
value m = fmap fromAny
  where fromAny (Any True)  = m
        fromAny (Any False) = mempty

resetValue :: (Eq m, Monoid m) => QDiagram v n m -> QDiagram v n Any
resetValue = fmap toAny
  where toAny m | m == mempty = Any False
                | otherwise   = Any True

clearValue :: QDiagram v n m -> QDiagram v n Any
clearValue = fmap (const (Any False))

-----------------------------

data Model = Model
    { playerPos :: (Double, Double)
    , velocity :: Vector
    }

data Playar = Character


--　使う関数を下記に定義
type Vector = (Double, Double)

projectX :: Vector -> Vector
projectX (x, y) = (x, 0)

projectY :: Vector -> Vector
projectY (x, y) = (0, y)

gravity :: Vector 
gravity = (0, -0.1)

leftDash :: Vector 
leftDash =  (0.5, 0)

rightDash :: Vector 
rightDash = ( -0.5, 0)

jump :: Vector 
jump = (0, 0.1)

k :: Double
k = 0.5

(^+^) :: Vector -> Vector -> Vector
(x1, y1) ^+^ (x2, y2) = (x1 + x2, y1 + y2)

(^-^) :: Vector -> Vector -> Vector
(x1, y1) ^-^ (x2, y2) = (x1 - x2, y1 - y2)

nagateV :: Vector -> Vector
nagateV v =  (-1) *^ v

normV :: Vector -> Double
normV (x, y) = sqrt (x^2 + y^2)

signorm :: Vector -> Vector
signorm v = (1 / (normV v)) *^ v

(*^) :: Double -> Vector -> Vector
a *^ (x, y) = (a * x, a * y)

(^/) :: Vector -> Double -> Vector
v ^/ a = (1 / a) *^ v

isColideTop :: Position -> Bool
isColideTop pos = not $ null $ stage `intersect` listPos pos
 where listPos :: Position -> [(Int, Int)]
       listPos (x ,y) =  [(ceiling x, ceiling y) ,(floor x, ceiling y)]


-- where 
       
-- where posX_up player = (ceiling x , y)
-- vを受け取って、当たり判定をする条件をする範囲を指定する



isColideGround :: Position -> Bool
isColideGround pos = not $ null $ stage `intersect` listPos pos
 where listPos :: Position -> [(Int, Int)]
       listPos (x ,y) =  [(ceiling x, floor y) ,(floor x, floor y)]
 

isColideRight :: Position -> Bool
isColideRight pos =  not $ null $ stage `intersect` listPos pos
 where listPos :: Position -> [(Int, Int)]
       listPos (x ,y) =  [(ceiling x, ceiling y) ,(ceiling x, floor y)]


isColideLeft :: Position -> Bool
isColideLeft pos =  not $ null $ stage `intersect` listPos pos
 where listPos :: Position -> [(Int, Int)]
       listPos (x ,y) =  [(floor x, ceiling y) ,(floor x, floor y)]
 



initialModel :: Model
initialModel = Model
    { playerPos = initialPos
    , velocity  = (0, 0)
    }

type Position = (Double, Double)

initialPos :: Position
initialPos = (6, 10)


stage :: [(Int, Int)]
stage = concat
     [  map (  , 0) [5 , 6 .. 18]
      , map ( 5,  ) [1 .. 10] -- 左の壁
      , map (20,  ) [1 .. 10] -- 右の壁
     ]

mapPair ::(a -> b) -> (a, a) -> (b, b)
mapPair f (l, r) = (f l, f r) 

view :: Model -> SelectableDiagram
view Model{..} = scale 20 $ center $ 
    center $ hsep 3
    [ value [] $ center $ mconcat $ map drawOneBlock $ concat
        [ [playerPos] 
        , map (mapPair fromIntegral) stage 
        ]
    ]
    where drawOneBlock :: (Double, Double) -> NormalDiagram       
          drawOneBlock ( x , y ) =  translate (V2 x y) r -- 一個のブロックを作成中
          r = rect 0.8 0.8

updateWithTimer :: Model -> Model
updateWithTimer model = Model (shiftTop $ shiftDown $ shiftLeft $ shiftRight $  playerPos') velocity'
 where (vx, vy) = velocity model
       velocity' :: Vector
       velocity' = undefined
       shiftTop :: Position -> Position
       shiftTop
           | isColideTop playerPos'= floorY
           | otherwise = id
       shiftDown :: Position -> Position
       shiftDown
           | isColideGround playerPos'= ceilingY
           | otherwise = id
       shiftLeft :: Position -> Position
       shiftLeft
           | isColideLeft playerPos'= floorX
           | otherwise = id
       shiftRight :: Position -> Position
       shiftRight
           | isColideRight playerPos'= ceilingX
           | otherwise = id
       floorY :: Position -> Position
       floorY = undefined
       ceilingY :: Position -> Position
       ceilingY = undefined
       floorX :: Position -> Position
       floorX = undefined
       ceilingX :: Position -> Position
       ceilingX = undefined
       playerPos' = playerPos model ^+^ velocity model

       newVelocity :: Vector -> Vector
       newVelocity = undefined



updateWithKeyPress :: SDL.Keycode -> Model -> Model
-- updateWithKeyPress SDL.KeycodeRight =  updateWithClick "right"
-- updateWithKeyPress SDL.KeycodeLeft =  updateWithClick "left"
updateWithKeyPress _ = id


updateWithClick :: String -> Model -> Model
updateWithClick _ model = undefined
    
--     | isColide model  = model
--     | otherwise       = model -- '
--  where model'     = Model playerPos' (velocity model)
--        playerPos' = [(x', y)]
--        y  = snd( playerPos model) 
--        x' = fst( playerPos model) + 1 

-- currentBlocks :: [Position] --　viewに対してだけ定義している。関数定義の順番は関係ない
--        currentBlocks = reify (cursorPos model) (orientation model) (currentTetrmino model)
--        model'     = Model (score model) cursorPos' (orientation model) (currentTetrmino model) (piledBlocks model)
--        cursorPos' = x' :+ y
--        y          = imagPart (cursorPos model)
--        x'         = realPart (cursorPos model) - 1 

-----------------------------

fullHDRect :: NormalDiagram
fullHDRect = rect screenWidth screenHeight # fc white

screenWidth :: Num a => a
screenWidth = 800
screenHeight :: Num a => a
screenHeight = 600

main :: IO ()
main = do
    -- 編集の初期化
    vModel <- newMVar initialModel
    vRender <- newMVar $ view initialModel
    -- SDL初期化
    SDL.initialize [ SDL.InitVideo ]
    window <- SDL.createWindow
        "SDL / Cairo Example"
        SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 screenWidth screenHeight}
    SDL.showWindow window
    
    screenSdlSurface <- SDL.getWindowSurface window

    sdlSurface <- SDL.createRGBSurface (SDL.V2 screenWidth screenHeight) SDL.ARGB8888
    buffer <- fmap castPtr $ SDL.surfacePixels sdlSurface
    cairoSurface <- Cairo.createImageSurfaceForData buffer Cairo.FormatRGB24 screenWidth screenHeight (screenWidth * 4)

    SDL.updateWindowSurface window

    -- Userイベントの登録
    mRegisteredEventType <- SDL.registerEvent decodeUserEvent encodeUserEvent
    let pushCustomEvent :: CustomEvent -> IO ()
        pushCustomEvent userEvent = forM_ mRegisteredEventType $ \ regEventType -> SDL.pushRegisteredEvent regEventType userEvent
        getCustomEvent :: SDL.Event -> IO (Maybe CustomEvent)
        getCustomEvent event = case mRegisteredEventType of
            Nothing -> return $ Nothing
            Just regEventType -> SDL.getRegisteredEvent regEventType event

    -- 定周期の処理
    _ <- SDL.addTimer 1000 $ const $ do
--        modifyMVarPure_ vModel-- $ updateWithTimer
        pushCustomEvent CustomExposeEvent
        return $ SDL.Reschedule 1000

    pushCustomEvent CustomExposeEvent
    
    -- Eventハンドラ
    let loop :: IO ()
        loop = do
            event <- SDL.waitEvent
            mUserEvent <- getCustomEvent event
            forM_ mUserEvent $ \case
                CustomExposeEvent -> do
                    model <- readMVar vModel
--                     putStrLn $ show $ triangleClickCount model
                    let selectableDiagram :: SelectableDiagram
                        selectableDiagram = toSDLCoord $ view model

                    SDL.surfaceFillRect sdlSurface Nothing whiteRect
                    Cairo.renderWith cairoSurface $ Cairo.toRender mempty $ clearValue selectableDiagram
                    SDL.surfaceBlit sdlSurface Nothing screenSdlSurface Nothing

                    SDL.updateWindowSurface window
                    swapMVar vRender selectableDiagram
                    return ()
            case SDL.eventPayload event of
                SDL.MouseButtonEvent SDL.MouseButtonEventData{..} -> do
                    case mouseButtonEventMotion of
                        SDL.Pressed -> do
                            selectableDiagram <- readMVar vRender
                            let mClickedObj = listToMaybe $ reverse $ sample selectableDiagram $ toFloatingPoint $ mouseButtonEventPos
                            case mClickedObj of
                                Nothing -> return ()
                                Just obj -> modifyMVarPure_ vModel $ updateWithClick obj
                            pushCustomEvent CustomExposeEvent
                            loop
                        _           -> loop
                SDL.KeyboardEvent SDL.KeyboardEventData{..} | keyboardEventKeyMotion == SDL.Pressed -> do
                    let SDL.Keysym _ key SDL.KeyModifier{..} = keyboardEventKeysym
                    modifyMVarPure_ vModel  $ updateWithKeyPress key
                    pushCustomEvent CustomExposeEvent
                    loop
                SDL.QuitEvent       -> return ()
                _                   -> loop
    loop
    putStrLn "Exitting"

data CustomEvent = CustomExposeEvent

decodeUserEvent :: SDL.RegisteredEventData -> SDL.Timestamp -> IO (Maybe CustomEvent)
decodeUserEvent SDL.RegisteredEventData{..} _ = case registeredEventCode of
    0 -> return $ Just CustomExposeEvent
    _ -> return Nothing

encodeUserEvent :: CustomEvent -> IO SDL.RegisteredEventData
encodeUserEvent CustomExposeEvent = return $ SDL.RegisteredEventData Nothing 0 nullPtr nullPtr

toSDLCoord :: SelectableDiagram -> SelectableDiagram
toSDLCoord = translate (V2 (screenWidth / 2) (screenHeight / 2)) . reflectY

toFloatingPoint :: Point V2 Int32 -> Point V2 Double
toFloatingPoint p = fmap fromIntegral p

whiteRect :: SDL.V4 Word8
whiteRect = SDL.V4 maxBound maxBound maxBound maxBound

alphaRect :: SDL.V4 Word8
alphaRect = SDL.V4 maxBound maxBound maxBound minBound

