{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import Graphics.Blank.Canvas

instance Show Command where
  show (Arc (a1,a2,a3,a4,a5,a6)) = "c.arc(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ "," ++ showJ a5 ++ "," ++ showB a6 ++ ");"
  show BeginPath = "c.beginPath();"
  show (BezierCurveTo (a1,a2,a3,a4,a5,a6)) = "c.bezierCurveTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ "," ++ showJ a5 ++ "," ++ showJ a6 ++ ");"
  show (ClearRect (a1,a2,a3,a4)) = "c.clearRect(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ ");"
  show ClosePath = "c.closePath();"
  show (Custom str) = str
  show Fill = "c.fill();"
  show (FillRect (a1,a2,a3,a4)) = "c.fillRect(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ ");"
  show (FillStyle (a1)) = "c.fillStyle = (" ++ show a1 ++ ");"
  show (FillText (a1,a2,a3)) = "c.fillText(" ++ show a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ ");"
  show (Font (a1)) = "c.font = (" ++ show a1 ++ ");"
  show (GlobalAlpha (a1)) = "c.globalAlpha = (" ++ showJ a1 ++ ");"
  show (LineCap (a1)) = "c.lineCap = (" ++ show a1 ++ ");"
  show (LineJoin (a1)) = "c.lineJoin = (" ++ show a1 ++ ");"
  show (LineTo (a1,a2)) = "c.lineTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show (LineWidth (a1)) = "c.lineWidth = (" ++ showJ a1 ++ ");"
  show (MiterLimit (a1)) = "c.miterLimit = (" ++ showJ a1 ++ ");"
  show (MoveTo (a1,a2)) = "c.moveTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show Restore = "c.restore();"
  show (Rotate (a1)) = "c.rotate(" ++ showJ a1 ++ ");"
  show (Scale (a1,a2)) = "c.scale(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show Save = "c.save();"
  show Stroke = "c.stroke();"
  show (StrokeRect (a1,a2,a3,a4)) = "c.strokeRect(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ ");"
  show (StrokeText (a1,a2,a3)) = "c.strokeText(" ++ show a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ ");"
  show (StrokeStyle (a1)) = "c.strokeStyle = (" ++ show a1 ++ ");"
  show (TextAlign (a1)) = "c.textAlign = (" ++ show a1 ++ ");"
  show (TextBaseline (a1)) = "c.textBaseline = (" ++ show a1 ++ ");"
  show (Transform (a1,a2,a3,a4,a5,a6)) = "c.transform(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ "," ++ showJ a5 ++ "," ++ showJ a6 ++ ");"
  show (Translate (a1,a2)) = "c.translate(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"

-- DSL

-- | see <http://www.w3schools.com/tags/canvas_arc.asp>
arc :: (Float,Float,Float,Float,Float,Bool) -> Canvas ()
arc = Command . Arc

-- | see <http://www.w3schools.com/tags/canvas_beginpath.asp>
beginPath :: Canvas ()
beginPath = Command BeginPath

-- | see <http://www.w3schools.com/tags/canvas_beziercurveto.asp>
bezierCurveTo :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
bezierCurveTo = Command . BezierCurveTo

-- | see <http://www.w3schools.com/tags/canvas_clearrect.asp>
clearRect :: (Float,Float,Float,Float) -> Canvas ()
clearRect = Command . ClearRect

-- | see <http://www.w3schools.com/tags/canvas_closepath.asp>
closePath :: Canvas ()
closePath = Command ClosePath

-- | sends command (JS) unchanged. useful for extending this library with functionality it doesn't currently have. example: 
--
-- > custom $ unlines $ [
-- >       "var grd=c.createRadialGradient(0,0,3,20,20,10); "
-- >      ,"grd.addColorStop(0,\"white\");"
-- >      ,"grd.addColorStop(1,\"red\");"
-- >      ,"c.fillStyle=grd;"]
custom :: String -> Canvas ()
custom = Command . Custom

-- | see <http://www.w3schools.com/tags/canvas_fill.asp>
fill :: Canvas ()
fill = Command Fill

-- | see <http://www.w3schools.com/tags/canvas_fillrect.asp>
fillRect :: (Float,Float,Float,Float) -> Canvas ()
fillRect = Command . FillRect

-- | see <http://www.w3schools.com/tags/canvas_fillstyle.asp>
fillStyle :: String -> Canvas ()
fillStyle = Command . FillStyle

-- | see <http://www.w3schools.com/tags/canvas_filltext.asp>
fillText :: (String,Float,Float) -> Canvas ()
fillText = Command . FillText

-- | see <http://www.w3schools.com/tags/canvas_font.asp>
font :: String -> Canvas ()
font = Command . Font

-- | see <http://www.w3schools.com/tags/canvas_globalalpha.asp>
globalAlpha :: Float -> Canvas ()
globalAlpha = Command . GlobalAlpha

-- | see <http://www.w3schools.com/tags/canvas_linecap.asp>
lineCap :: String -> Canvas ()
lineCap = Command . LineCap

-- | see <http://www.w3schools.com/tags/canvas_linejoin.asp>
lineJoin :: String -> Canvas ()
lineJoin = Command . LineJoin

-- | see <http://www.w3schools.com/tags/canvas_lineto.asp>
lineTo :: (Float,Float) -> Canvas ()
lineTo = Command . LineTo

-- | see <http://www.w3schools.com/tags/canvas_linewidth.asp>
lineWidth :: Float -> Canvas ()
lineWidth = Command . LineWidth

-- | see <http://www.w3schools.com/tags/canvas_miterlimit.asp>
miterLimit :: Float -> Canvas ()
miterLimit = Command . MiterLimit

-- | see <http://www.w3schools.com/tags/canvas_moveto.asp>
moveTo :: (Float,Float) -> Canvas ()
moveTo = Command . MoveTo

-- | see bottom of <http://www.w3schools.com/tags/ref_canvas.asp>
restore :: Canvas ()
restore = Command Restore

-- | see <http://www.w3schools.com/tags/canvas_rotate.asp>
rotate :: Float -> Canvas ()
rotate = Command . Rotate

-- | <http://www.w3schools.com/tags/canvas_scale.asp>
scale :: (Float,Float) -> Canvas ()
scale = Command . Scale

-- | see bottom of <http://www.w3schools.com/tags/ref_canvas.asp>
save :: Canvas ()
save = Command Save

-- | see <http://www.w3schools.com/tags/canvas_stroke.asp>
stroke :: Canvas ()
stroke = Command Stroke

-- | see <http://www.w3schools.com/tags/canvas_strokerect.asp>
strokeRect :: (Float,Float,Float,Float) -> Canvas ()
strokeRect = Command . StrokeRect

-- | see <http://www.w3schools.com/tags/canvas_stroketext.asp>
strokeText :: (String,Float,Float) -> Canvas ()
strokeText = Command . StrokeText

-- | see <http://www.w3schools.com/tags/canvas_strokestyle.asp>
strokeStyle :: String -> Canvas ()
strokeStyle = Command . StrokeStyle

-- | see <http://www.w3schools.com/tags/canvas_textalign.asp>
textAlign :: String -> Canvas ()
textAlign = Command . TextAlign

-- | see <http://www.w3schools.com/tags/canvas_textbaseline.asp>
textBaseline :: String -> Canvas ()
textBaseline = Command . TextBaseline

-- | see <http://www.w3schools.com/tags/canvas_transform.asp>
transform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
transform = Command . Transform

-- | see <http://www.w3schools.com/tags/canvas_translate.asp>
translate :: (Float,Float) -> Canvas ()
translate = Command . Translate

