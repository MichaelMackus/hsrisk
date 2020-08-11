module Graphics.Pixels
    ( surfaceToColors
    ) where

import Control.Monad (when)
import Data.Bits ((.&.), (.|.))
import GHC.Word (Word8(..), Word32(..))
import SDL
import SDL.Vect (V4(..), V2(..), Point(..))
import SDL.Video.Renderer
import qualified SDL.Raw as Raw
import SDL.Raw.Video (getPixelFormatName)
import Foreign.C.Types
import Foreign.C.String (peekCString)
import Foreign.Ptr
import Foreign.Storable

-- surfaceToPaletteColors :: Surface -> IO (Maybe (Vector (V4 Word8)))
-- surfaceToPaletteColors s = do
--         fmt     <- surfaceFormat s
--         palette <- formatPalette fmt
--         case palette of
--             (Just p) -> paletteColors p
--             Nothing  -> return Nothing

{-- TODO handle different pixel formats --}
-- surfaceToColors :: Surface -> IO (Vector (V4 Word8))
surfaceToColors :: Surface -> IO [Word8]
surfaceToColors s = do
        surfaceToColorsNaive s
        -- colors <- surfaceToPaletteColors s
        -- case colors of
        --     (Just r)  -> return r
        --     otherwise -> do
        --         error "Unable to get colors without palette"

surfaceToColorsNaive s = do
        lockSurface s
        ptr      <- surfacePixels s
        (V2 w h) <- surfaceDimensions s
        -- {-- inspect pixel format of surface --}
        -- f        <- rawSurfaceFormat s
        -- putStr "SDL pixel format: "
        -- putStrLn =<< peekCString =<< getPixelFormatName (Raw.pixelFormatFormat f)
        let len = fromIntegral (w*h)
        pixels   <- getPixelData (castPtr ptr) len
        -- TODO convert to RGBA values depending on format
        unlockSurface s
        return pixels
    where
        surfacePtr (Surface s _) = peek s

rawSurfaceFormat :: Surface -> IO Raw.PixelFormat
rawSurfaceFormat (Surface s _) = do 
    f <- peek s
    peek (Raw.surfaceFormat f)

getPixelData :: Ptr Word8 -> Int -> IO [Word8]
getPixelData ptr len = sequence . take len $ map (\i -> pixel i ptr) [0..]

-- pixelLen :: Ptr Word8 -> Int
-- pixelLen ptr = takeWhile (/=nullPtr) $ map (\i -> plusPtr ptr i) [0..]

pixel :: Storable a => Int -> Ptr a -> IO a
pixel i ptr = peek (plusPtr ptr i)
