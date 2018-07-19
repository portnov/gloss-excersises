
module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Model = Model {
    mX :: Float,
    mY :: Float,
    mRadius :: Float,
    mColor :: Color,
    mVelocity :: Vector,
    mRandom :: StdGen
  }

initModel :: StdGen -> Model
initModel gen = Model  100 100 20 red (0,0) gen

showModel :: Model -> Picture
showModel m = color (mColor m) $ translate (mX m) (mY m) $ circleSolid (mRadius m)

rho :: Point -> Point -> Float
rho (x1, y1) (x2, y2) = sqrt $ (x1-x2)**2 + (y1-y2)**2

maxVelocity :: Float
maxVelocity = 100

handleEvent :: Event -> Model -> Model
handleEvent (EventKey (MouseButton LeftButton) Up _ p@(x, y)) m =
  if rho (mX m, mY m) p < mRadius m
    then let (velocity, rnd) = randomR (0, maxVelocity) (mRandom m)
             (phi, rnd') = randomR (0, 2*pi) rnd
         in  m { mVelocity = polar velocity phi, mRandom = rnd' }
    else m
handleEvent (EventMotion p) m = 
  if rho (mX m, mY m) p < mRadius m
    then m {mColor = green}
    else m {mColor = red}
handleEvent _ m = m

polar :: Float -> Float -> Vector
polar r phi = (r * cos phi, r * sin phi)

slowdown :: Float -> Float
slowdown seconds = 0.9 ** seconds

scaleV :: Float -> Vector -> Vector
scaleV coef (x,y) = (coef * x, coef * y)

doStep :: Float -> Model -> Model
doStep seconds m = 
  let (vx, vy) = mVelocity m
      x = mX m
      y = mY m
  in m {
         mX = x + seconds * vx,
         mY = y + seconds * vy,
         mVelocity = scaleV (slowdown seconds) (mVelocity m)
       }
  

main :: IO ()
main = do
  let dpy = InWindow "Test" (1024, 768) (0,0)
      background = white
      stepsPerSecond = 10
  gen <- newStdGen
  play dpy background stepsPerSecond (initModel gen) showModel handleEvent doStep

