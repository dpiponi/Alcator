--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Display where

import Control.Monad
import Data.Dequeue
import Data.IORef
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW
import Keys
import Metrics
import System.Exit
import System.Exit (exitFailure)
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

-- | Create OpenGL texture map to represent TV screen.
createImageTexture :: GL.TextureObject -> IO (Ptr Word8)
createImageTexture texName = do
    GL.textureBinding GL.Texture2D $= Just texName

    textureData <- mallocBytes 16384 :: IO (Ptr Word8)

    -- Allocate 16K of video RAM
    -- as width 128, height 128
    forM_ [0..16383::Int] $ \i ->
        pokeElemOff textureData (fromIntegral $ i) 0

    GL.textureBinding GL.Texture2D $= Just texName
    GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.R8
        (GL.TextureSize2D 128 128)
        0
        (GL.PixelData GL.Red GL.UnsignedByte textureData)

    GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

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

    GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

-- | Compile and link vertex and fragment shaders.
createShaderProgram :: IO GL.Program
createShaderProgram = do
    -- compile vertex shader
    vs <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vs $= vsSource
    GL.compileShader vs
    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        exitFailure

    -- Do it again for the fragment shader
    fs <- GL.createShader GL.FragmentShader
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

vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [
                "#version 110",
                "",
                "attribute vec2 position;",
                "varying vec2 texcoord;",
                "",
                "void main()",
                "{",
                "    gl_Position = vec4(position, 0.0, 1.0);",
                "    texcoord = vec2(0.5+0.5*position.x, 0.5-0.5*position.y);",
                "}"
           ]

-- Acorn atom graphics modes
-- Mode:       Resolution:          Memory:   #B000
--             X:       Y:
--  0          64       48           0.5 K    #00
--  1a         64       64             1 K    #10
--  1         128       64             1 K    #30
--  2a        128       64             2 K    #50
--  2         128       96           1.5 K    #70
--  3a        128       96             3 K    #90
--  3         128      192             3 K    #B0
--  4a        128      192             6 K    #D0
--  4         256      192             6 K    #F0

fsSource = BS.intercalate "\n"
           [
                "#version 110",
                "",
                "float testbit(int byte, int bit) {",
                "  int px = int(pow(2., float(bit)));",
                "  return mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;",
                "}",
                "uniform sampler2D current_frame;",
                "uniform sampler2D table;",
                "uniform sampler2D last_frame;",
                "uniform float mode;",
                "varying vec2 texcoord;",
                "",
                "void main()",
                "{",
                "",
                "    if (mode == 0.0) {",
                "        int x = int(32.*8.*texcoord.x);",
                "        int y = int(16.*12.*texcoord.y);",
                "        int ix = x/8;",
                "        int iy = y/12;",
                "        int fx = x-8*ix;",
                "        int fy = y-12*iy;",
                "        int addr = 32*iy+ix;",
                "        int ty = addr/128;",
                "        int tx = addr-128*ty;",
                "        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));",
                "        int character = int(256.0*last_index.x);",
                "        int cy = character/32;",
                "        int cx = character-32*cy;",
                "        int px = 8*cx+fx;",
                "        int py = 12*cy+fy;",
                "        float z = texture2D(table, vec2(float(px)/255.0, float(py)/95.0)).x;",
                "        gl_FragColor = vec4(z, z, z, 1.0);",
                "    } else if (mode == 48.0) {",
                "        int x = int(128.*texcoord.x);",
                "        int y = int(64.*texcoord.y);",
                "        int ix = x/8;",
                "        int fx = 7-(x-8*ix);",
                "        int addr = 16*y+ix;",
                "        int ty = addr/128;",
                "        int tx = addr-128*ty;",
                "        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));",
                "        int byte = int(256.0*last_index.x);",
                "        int px = int(pow(2., float(fx)));",
                "        float z = mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;",
                "        gl_FragColor = vec4(z, z, z, 1.0);",
                "    } else if (mode == 112.0) {",
                "        int x = int(128.*texcoord.x);",
                "        int y = int(96.*texcoord.y);",
                "        int ix = x/8;",
                "        int fx = 7-(x-8*ix);",
                "        int addr = 16*y+ix;",
                "        int ty = addr/128;",
                "        int tx = addr-128*ty;",
                "        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));",
                "        int byte = int(256.0*last_index.x);",
                "        int px = int(pow(2., float(fx)));",
                "        float z = mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;",
                "        gl_FragColor = vec4(z, z, z, 1.0);",
                "    } else if (mode == 176.0) {",
                "        int x = int(128.*texcoord.x);",
                "        int y = int(192.*texcoord.y);",
                "        int ix = x/8;",
                "        int fx = 7-(x-8*ix);",
                "        int addr = 16*y+ix;",
                "        int ty = addr/128;",
                "        int tx = addr-128*ty;",
                "        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));",
                "        int byte = int(256.0*last_index.x);",
                "        int px = int(pow(2., float(fx)));",
                "        float z = mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;",
                "        gl_FragColor = vec4(z, z, z, 1.0);",
                "    } else if (mode == 240.0) { // 4",
                "        int x = int(256.*texcoord.x);",
                "        int y = int(192.*texcoord.y);",
                "        int ix = x/8;",
                "        int fx = 7-(x-8*ix);",
                "        int addr = 32*y+ix;",
                "        int ty = addr/128;",
                "        int tx = addr-128*ty;",
                "        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));",
                "        int byte = int(256.0*last_index.x);",
                "        int px = int(pow(2., float(fx)));",
                "        float z = mod(float(byte), float(2*px)) >= float(px) ? 1.0 : 0.0;",
                "        gl_FragColor = vec4(z, z, z, 1.0);",
                "    } else if (mode == 208.0 || true) { // 4a",
                "        int x = int(128.*texcoord.x);",
                "        int y = int(192.*texcoord.y);",
                "        int ix = x/4;",
                "        int fx = 3-(x-4*ix);",
                "        int addr = 32*y+ix;",
                "        int ty = addr/128;",
                "        int tx = addr-128*ty;",
                "        vec4 last_index = texture2D(current_frame, vec2(float(tx)/128., float(ty)/128.));",
                "        int byte = int(256.0*last_index.x);",
                "        int px = int(pow(2., float(2*fx)));",
                "        int bits = mod(float(byte), float(2*px)) >= float(px) ? 1 : 0;",
                "        bits += mod(float(byte), float(4*px)) >= float(2*px) ? 2 : 0;",
                "        if (bits == 0) {",
                "            gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0);",
                "        } else if (bits == 1) {",
                "            gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0);",
                "        } else if (bits == 2) {",
                "            gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);",
                "        } else {",
                "            gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);",
                "        }",
                "    }",
                "}"
           ]
-- 0 green
-- 1 yellow
-- 2 blue
-- 3 red

-- | Pair of triangles filling window.
vertices :: V.Vector Float
vertices = V.fromList [ -1.0, -1.0
                      ,  1.0, -1.0 
                      ,  1.0,  1.0 

                      , -1.0, -1.0 
                      ,  1.0,  1.0 
                      , -1.0,  1.0
                      ]

makeMainWindow :: Int -> Int -> IORef (BankersDequeue UIKey) -> IO Window
makeMainWindow screenScaleX' screenScaleY' queue = do
    
    windowHint (WindowHint'OpenGLProfile OpenGLProfile'Any)
    windowHint (WindowHint'DoubleBuffer True)
    windowHint (WindowHint'ContextVersionMajor 2)
    windowHint (WindowHint'ContextVersionMinor 1)

    mWindow <- createWindow (fromIntegral $ screenScaleX'*screenWidth)
                            (fromIntegral $ screenScaleY'*screenHeight)
                            "Stellarator"
                            Nothing
                            Nothing
    case mWindow of
        Nothing -> die "Couldn't create window"
        Just createdWindow -> do
            makeContextCurrent (Just createdWindow)
            return createdWindow
