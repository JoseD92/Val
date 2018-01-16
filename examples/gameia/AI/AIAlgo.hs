module AI.AIAlgo (
    getNewOrientation,
    seek,
    arrive
) where

import           AI.AIData
import           EasyGL.Util
import           Graphics.Rendering.OpenGL (Vector2 (..), Vector3 (..))


getNewOrientation :: Double -> Vector2 Double -> Double
getNewOrientation currentOrientation (Vector2 0 0) = currentOrientation
getNewOrientation _ (Vector2 x z)                  = (180 / pi) * atan2 (-z) x

vec a = Vector2 a a

divV (Vector2 x y) a = Vector2 (x/a) (y/a)

seek :: Double -> Kinematic -> Kinematic -> Steering
seek maxSpeed character target = SteeringOutput steeringVel 0 (Just newOrientation)
    where
    dir0 = position target - position character
    dir = case dir0 of
        Vector2 0 0 -> Vector2 0 0
        _           -> normalizeVec2 dir0
    steeringVel = vec maxSpeed * dir
    newOrientation = getNewOrientation (orientation character) steeringVel

arrive :: Double -> Double -> Kinematic -> Kinematic -> Steering
arrive timeToTarget radius character target = if normVec2 dir0 < radius
    then SteeringOutput 0 0 Nothing
    else SteeringOutput steeringVel 0 (Just newOrientation)
    where
        dir0 = position target - position character
        steeringVel = divV dir0 timeToTarget
        newOrientation = getNewOrientation (orientation character) steeringVel

