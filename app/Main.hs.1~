{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified SDL as SDL
import Foreign.Ptr ( castPtr, nullPtr )
import qualified Graphics.Rendering.Cairo as Cairo

-- diagrams-cairo
import Diagrams.Backend.Cairo as Cairo

-- diagrams
import Diagrams.Prelude hiding (view)
import Diagrams.TwoD.Text (text)

-- base
import Data.Int (Int32)
import Data.Word (Word8)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar, modifyMVar_) -- (newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (forM, forM_, replicateM)
import Data.Maybe (listToMaybe)
import Data.Complex

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
    { score :: Int
    , currentBlocks :: [Complex]
    , carsorPos :: Complex
    , blocks :: [Complex]
    }

data Tetrimino
    = BlockL
    | BlockL2
    | BlockT
    | BlockO
    | BlockI

tetriminoToBlocks :: Tetrimino -> [Complex]
tetriminoToBlocks BlockL = [0 :+ 0, 0 :+ 1, 0 :+ 2, 1 :+ 0]
tetriminoToBlocks BlockL2 = undefined
tetriminoToBlocks BlockT = undefined
tetriminoToBlocks BlockO = undefined
tetriminoToBlocks BlockI = undefined

isColideWall :: Model -> Bool
isColideWall Model{..} = any isColide' currentBlocks
 where currentBlocks = map (carsorPos +) currentBlocks
       isColide' (x :+ y) = or [x <= 0, x >= 10]

isColideBottom :: Model -> Bool
isColideBottom Model{..} = or
    [ blocks `intersection` currentBlocks
    , any isColide' currentBlocks
    ]
 where currentBlocks = map (carsorPos +) currentBlocks
       isColide' (x :+ y) = y <= 0

clearBlock :: [Complex] -> [Complex]
clearBlock = undefined

routateBlock :: Model -> Model
routateBlock Model{..} = Model score currentBlocks' carsorPos blocks
 where currentBlocks' = map ((+ cursorPos) . (* i) . (- cursorPos)) currentBlocks

initialModel :: Model
initialModel = Model
    { clockCount = 0
    , triangleClickCount = 0
    , squareClickCount = 0
    }

view :: Model -> SelectableDiagram
view (Model clockCount triangleClickCount squareClickCount) = toSDLCoord $ mconcat
    [ scale 50 $ center $ vsepEven 10
        [ value [] $ vsepEven 3
            [ text ("Clock count: " ++ show clockCount) <> textPhantom
            , text ("Triangle click count: " ++ show triangleClickCount) <> textPhantom
            , text ("Square click count: " ++ show squareClickCount) <> textPhantom
            ]
        , center $ hsep 1
            [ triangle 1 # fc red # value ["triangle"]
            , rect 1 1 # fc blue # value ["square"]
            , circle 1 # fc green # value ["circle"]
            ]
        ]
    , sized (mkHeight screenHeight) $ center $ vcat $ replicate triangleClickCount $ hcat $ replicate triangleClickCount $ circle 1 # fc blue # value []
    ]
 where textPhantom = phantom (rect 10 1 :: NormalDiagram)



updateWithClick :: String -> Model -> Model
updateWithClick "triangle" (Model{..}) = Model clockCount (triangleClickCount + 1) squareClickCount
updateWithClick "square" (Model t n m) = Model t n (m + 1)
updateWithClick "circle" _ = initialModel -- Model t n (m + 1)
updateWithClick _ model = model

updateWithTimer :: Model -> Model
updateWithTimer (Model clockCount triangleClickCount squareClickCount) = Model (clockCount + 1) triangleClickCount squareClickCount

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
        modifyMVarPure_ vModel $ updateWithTimer
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
                        selectableDiagram = view model

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

