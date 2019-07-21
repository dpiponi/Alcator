--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Display where

import Control.Monad
-- import Data.Dequeue
-- import Data.IORef
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW
-- import Keys
import Metrics
import System.Exit
import System.IO
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL

-- | Inform OpenGL about new pixel data.
updateTexture :: GL.TextureObject -> Ptr Word8 -> IO ()
updateTexture texName textureData = do
    GL.textureBinding GL.Texture2D $= Just texName
    GL.texSubImage2D
        GL.Texture2D
        0
        (GL.TexturePosition2D 0 0)
        (GL.TextureSize2D 128 128)
        (GL.PixelData GL.Red GL.UnsignedByte textureData)
    return ()

setTextureMode :: IO ()
setTextureMode = do
    GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

-- | Create OpenGL texture map to represent TV screen.
createImageTexture :: GL.TextureObject -> IO (Ptr Word8)
createImageTexture texName = do
    GL.textureBinding GL.Texture2D $= Just texName

    textureData <- mallocBytes 16384 :: IO (Ptr Word8)

    -- Allocate 16K of video RAM
    -- as width 128, height 128
    forM_ [0..16383::Int] $ \i ->
        pokeElemOff textureData (fromIntegral i) 0

    GL.textureBinding GL.Texture2D $= Just texName
    GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.R8
        (GL.TextureSize2D 128 128)
        0
        (GL.PixelData GL.Red GL.UnsignedByte textureData)

    setTextureMode

    return textureData

createFontTexture :: GL.TextureObject -> Ptr Word8 -> IO ()
createFontTexture texName font_data = do
    GL.textureBinding GL.Texture2D $= Just texName

    GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.R8
        -- Non-square, probably needs fixing XXX
        (GL.TextureSize2D 256 96)
        0
        (GL.PixelData GL.Red GL.UnsignedByte font_data)

    setTextureMode

-- | Compile and link vertex and fragment shaders.
createShaderProgram :: IO GL.Program
createShaderProgram = do
    -- compile vertex shader
    vs <- GL.createShader GL.VertexShader
    vsSource <- BS.readFile "shaders/vertex.glsl"
    GL.shaderSourceBS vs $= vsSource
    GL.compileShader vs
    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        exitFailure

    -- Do it again for the fragment shader
    fs <- GL.createShader GL.FragmentShader
    fsSource <- BS.readFile "shaders/fragment.glsl"
    GL.shaderSourceBS fs $= fsSource
    GL.compileShader fs
    fsOK <- GL.get $ GL.compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        msg <- GL.get $ GL.shaderInfoLog fs
        hPutStrLn stderr msg
        exitFailure

    program <- GL.createProgram
    GL.attachShader program vs
    GL.attachShader program fs
    GL.attribLocation program "coord2d" $= GL.AttribLocation 0
    GL.linkProgram program
    linkOK <- GL.get $ GL.linkStatus program

    unless linkOK $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure

    return program

-- | Bind textures to appropriate locations in shader program.
connectProgramToTextures :: GL.Program -> Float -> GL.TextureObject ->
                            GL.TextureObject -> IO ()
connectProgramToTextures program mode
                         current_frame_tex font_tex = do
    GL.currentProgram $= Just program
    current_screen_tex_loc <- GL.uniformLocation program "current_frame"
--     last_screen_tex_loc <- GL.uniformLocation program "last_frame"
    font_tex_loc <- GL.uniformLocation program "table"
    mode_loc <- GL.uniformLocation program "mode"

    GL.uniform mode_loc $= mode

    GL.activeTexture $= GL.TextureUnit 0
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just current_frame_tex

    GL.activeTexture $= GL.TextureUnit 1
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just font_tex

    -- What does this do?
    GL.activeTexture $= GL.TextureUnit 2
    GL.texture GL.Texture2D $= GL.Enabled
--     GL.textureBinding GL.Texture2D $= Just last_frame_tex

    GL.uniform current_screen_tex_loc $= GL.Index1 (0::GL.GLint)
    GL.uniform font_tex_loc $= GL.Index1 (1::GL.GLint)
--     GL.uniform last_screen_tex_loc $= GL.Index1 (2::GL.GLint)

    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless status $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure
    GL.currentProgram $= Just program

-- | Create all OpenGL objects required including shaders and textures.
initResources :: Float -> Ptr Word8 -> IO (GL.Program, GL.AttribLocation, GL.TextureObject, Ptr Word8)
initResources mode font_data = do
    [current_frame_tex, font_tex] <- GL.genObjectNames 2 :: IO [GL.TextureObject]

    textureData <- createImageTexture current_frame_tex
    createFontTexture font_tex font_data

    program <- createShaderProgram
    connectProgramToTextures program mode current_frame_tex font_tex

    return (program, GL.AttribLocation 0, current_frame_tex, textureData)

-- | Render VCS screen as pair of triangles.
draw :: Word8 -> Int -> Int -> GL.Program -> GL.AttribLocation -> IO ()
draw mode windowWidth windowHeight program attrib = do
--     print "Draw 1"
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight))
--     (w, h) <- getFramebufferSize window
--     GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

    GL.currentProgram $= Just program

    mode_loc <- GL.uniformLocation program "mode"
    GL.uniform mode_loc $= (fromIntegral mode :: Float)

    GL.vertexAttribArray attrib $= GL.Enabled
    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer attrib $=
          (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
    GL.drawArrays GL.Triangles 0 6
    GL.vertexAttribArray attrib $= GL.Disabled

-- | Pair of triangles filling window.
vertices :: V.Vector Float
vertices = V.fromList [ -1.0, -1.0
                      ,  1.0, -1.0 
                      ,  1.0,  1.0 

                      , -1.0, -1.0 
                      ,  1.0,  1.0 
                      , -1.0,  1.0
                      ]

makeMainWindow :: Int -> Int -> IO Window
makeMainWindow screenScaleX' screenScaleY' = do
    
    windowHint (WindowHint'OpenGLProfile OpenGLProfile'Any)
    windowHint (WindowHint'DoubleBuffer True)
    windowHint (WindowHint'ContextVersionMajor 2)
    windowHint (WindowHint'ContextVersionMinor 1)

    mWindow <- createWindow (fromIntegral $ screenScaleX'*screenWidth)
                            (fromIntegral $ screenScaleY'*screenHeight)
                            "Alcator"
                            Nothing
                            Nothing
    case mWindow of
        Nothing -> die "Couldn't create window"
        Just createdWindow -> do
            makeContextCurrent (Just createdWindow)
            return createdWindow
