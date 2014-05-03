{-# LANGUAGE PackageImports #-}
 
module SimpleListBrowser (simpleListBrowser) where

import Control.Monad
import "mtl" Control.Monad.State
import System.Exit
import UI.NCurses

type Vector2 = (Int, Int)

simpleListBrowser :: [String]
                  -> (Int -> Curses ())
                  -> Curses ()
simpleListBrowser xs act = do
  let loop top sel = do
        (by, bx) <- (\(a,b) -> (a-1,b)) `fmap` screenSize'
        draw $ do 
          clear ((1,1), (bx+1, by-2))
          zipWithM_ (\n x -> 
                 if n == sel - top + 1
                   then withReverse $ drawStringAt (1, n) x
                   else               drawStringAt (1, n) x) [1..] (take by (drop (top - 1) xs))
        c <- waitForChrs "qQjk "
        case c of
          'q' -> return ()
          'Q' -> liftIO $ exitWith ExitSuccess
          'j' -> if sel > length xs - 1
                   then loop top sel
                   else if sel - top >= by - 1
                          then loop (top + 1) (sel + 1)
                          else loop top (sel + 1)
          'k' -> if sel > 1
                   then if sel - top <= 0
                          then loop (top - 1) (sel - 1)
                          else loop top (sel - 1)
                   else loop top sel
          ' ' -> do act sel
                    -- reset the screen as best as possible
                    draw $ fill  ((1,1), (bx+1, by-2))
                    draw $ clear ((1,1), (bx+1, by-2))
                    loop top sel
          _   -> loop top sel

  loop 1 1

----------------------------------

draw :: Update () -> Curses ()
draw ioact = do
  w <- defaultWindow
  updateWindow w ioact
  render

drawStringAt :: Vector2 -> String -> Update ()
drawStringAt (x, y) s = do
  moveCursor' (y - 1, x - 1)
  drawString s

screenSize' :: Curses Vector2
screenSize' = screenSize >>= \(y, x) ->
  return (fromIntegral y, fromIntegral x)

moveCursor' :: Vector2 -> Update ()
moveCursor' (y, x) = moveCursor (fromIntegral y) (fromIntegral x)

waitForChrs :: String -> Curses Char
waitForChrs cs = loop' where
  loop' = do
    w <- defaultWindow
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop'
      Just (EventCharacter c) -> if c `elem` cs then return c else loop'
      _ -> loop'

withReverse :: Update () -> Update ()
withReverse upact = do
  setAttribute AttributeReverse True
  upact
  setAttribute AttributeReverse False

clear :: (Vector2, Vector2) -> Update ()
clear ((x1, y1), (x2, y2)) =
   let cs = replicate (x2-x1) ' '
    in forM_ [y1..y2+2] $ \y ->
        drawStringAt (x1, y) cs

fill :: (Vector2, Vector2) -> Update ()
fill ((x1, y1), (x2, y2)) =
   let cs = replicate (x2-x1) '.'
    in forM_ [y1..y2+2] $ \y ->
        drawStringAt (x1, y) cs

