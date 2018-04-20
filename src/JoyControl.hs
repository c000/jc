{-# LANGUAGE Arrows #-}
module JoyControl
    ( joyControlMain
    ) where

import Prelude hiding (init, (.))
import Control.Exception
import Control.Monad
import Data.Bits

import Foreign.C.Types
import System.Win32.Types
import Graphics.Win32.Key

import Control.Auto
import Control.Auto.Run (run)
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Auto.Interval
import qualified Data.Map as M

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL

foreign import ccall "mouse_event" mouseEvent :: DWORD -> DWORD -> DWORD ->DWORD -> CULong -> IO ()
foreign import ccall "keybd_event" keybdEvent :: BYTE -> BYTE -> DWORD -> CULong -> IO ()

-- config
speedFactor :: Double
speedFactor = 10
joyMouseButtons :: [Int]
joyMouseButtons = [4, 2]
joyKeyboardButtons :: [(Int, VKey)]
joyKeyboardButtons =
    [ (3 , vK_RETURN)
    , (1 , vK_ESCAPE)
    , (6 , vK_SPACE)
    , (8 , vK_CONTROL)
    , (5 , vK_PRIOR)
    , (7 , vK_NEXT)
    , (13, vK_UP)
    , (14, vK_RIGHT)
    , (15, vK_DOWN)
    , (16, vK_LEFT)
    ]

joyControlMain :: IO ()
joyControlMain = do
    bracket_ init terminate $ withWindow $ \maybeWindow -> do
        case maybeWindow of
            Nothing -> putStrLn "Window open error!"
            Just window -> do
                makeContextCurrent (Just window)
                swapInterval 10
                void $ run (return window) return mainWindowAuto

withWindow f = bracket create destroy f
  where
    create = do
        windowHint $ WindowHint'Resizable False
        createWindow 256 256 "JoyControl" Nothing Nothing
    destroy (Just w) = destroyWindow w
    destroy _ = return ()

-- joystick autos

mainWindowAuto :: Auto IO Window (Maybe Window)
mainWindowAuto = proc window -> do
    -- input
    (x, y) <- effect . fmap mergeJoystick $ getJoystickAxes (toEnum 0) -< ()
    Just (buttons) <- effect $ getJoystickButtons (toEnum 0) -< ()
    -- accumulate
    (xCurrent, xr) <- deltaSigma   -< (x^3) * speedFactor
    (yCurrent, yr) <- deltaSigma   -< (y^3) * speedFactor
    kr             <- edgeDetector -< M.fromAscList . zip [1..] . map fromEnum $ buttons
    -- result io
    arrM mouseIO    -< (kr, xr, yr)
    arrM keyboardIO -< kr
    arrM renderIO   -< (window, x, y, xr /= 0 || yr /= 0)
    arrM windowIO   -< window
  where
    mouseIO (kr, x, y) = do
        let clicks = map (kr M.!) joyMouseButtons
            leftClick = case clicks of Just 0 : _ -> 2
                                       Just 1 : _ -> 4
                                       _          -> 0
            rightClick = case clicks of _: Just 0 : _ -> 8
                                        _: Just 1 : _ -> 16
                                        _             -> 0
            move = case (x, y) of (0, 0) -> 0
                                  _      -> 1
            evNum = leftClick .|. rightClick .|. move
        mouseEvent evNum x y 0 0
    keyboardIO kr = mapM_ (joy2Kbd kr) joyKeyboardButtons
    joy2Kbd kr (index, vKey) = do
        case kr M.! index of
            Nothing -> return ()
            Just 0 -> keybdEvent (fromIntegral vKey) 0 0 0
            Just 1 -> keybdEvent (fromIntegral vKey) 0 2 0
    mergeJoystick (Just (x1:y1:x2:y2:_)) = (x1 + 0.5 * x2, y1 + 0.5 * y2)
    mergeJoystick (Just (x :y :_)) = (x, y)
    mergeJoystick _ = (0, 0)
    renderIO (w, x, y, moved) = do
        makeContextCurrent $ Just w
        let bg = if moved then 0.5 else 0
        clearColor $= Color4 bg bg bg 0
        clear [ColorBuffer]
        renderPrimitive Lines $ do
            vertex $ Vertex2 0 (0::GLfloat)
            vertex $ Vertex2 x (-y)
    windowIO w = do
        swapBuffers w
        pollEvents
        closeFlag <- windowShouldClose w
        return $ case closeFlag of
            True  -> Nothing
            False -> Just w

deltaSigma :: Monad m => Auto m Double (Double, DWORD)
deltaSigma = accum_ core (0, 0) >>> forcer
  where
    core (sigma, _) delta = case delta + sigma of
        s | 1 <= s    -> (s - fromInteger (floor   s), floor s)
          | s <= (-1) -> (s - fromInteger (ceiling s), ceiling s)
          | otherwise -> (s, 0)

edgeDetector :: Monad m => Auto m (M.Map Int Int) (M.Map Int (Maybe Int))
edgeDetector = muxMany_ (const core) >>> forcer
  where
    core = onChange_ >>> holdFor_ 1
