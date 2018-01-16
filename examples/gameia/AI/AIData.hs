module AI.AIData (
    Kinematic(..),
    Steering(..),
    updateKinematic,
    toTansform
)
where

import           Data.Maybe                (fromMaybe)
import           Data.Maybe                (fromJust)
import           EasyGL.Util
import           Graphics.Rendering.OpenGL (Vector2 (..), Vector3 (..))
import qualified Val.Strict                as Val

data Kinematic = Kinematic {
    position    :: Vector2 Double,
    orientation :: Double,
    speed       :: Vector2 Double,
    speedBound  :: Maybe Double,
    rotation    :: Double
} deriving (Show)

data Steering = SteeringOutput {
    linear           :: Vector2 Double,
    angular          :: Double,
    forceOrientation :: Maybe Double
} deriving Show

updateKinematic :: Double -> Steering -> Kinematic -> Kinematic
updateKinematic time steering kin = kin{
    position=position kin + speed kin * vec time,
    speed=defSpeed,

    rotation=rotation kin + angular steering * time,
    orientation=fromMaybe (orientation kin + rotation kin * time) (forceOrientation steering)
} where
    newSpeed = speed kin + linear steering * vec time
    newSpeedModule = normVec2 newSpeed
    vec a = Vector2 a a
    bound = (abs . fromJust . speedBound) kin
    speedBounded = if newSpeedModule > bound
        then bound / newSpeedModule
        else 1
    defSpeed = maybe newSpeed (const $ newSpeed * vec speedBounded) $ speedBound kin


toTansform :: Double -> Kinematic -> Val.Transform
toTansform yPos Kinematic{position=Vector2 x z,orientation=rot} =
    Val.Transform (Vector3 x yPos z) (Val.Quaternion rot (Vector3 0 1 0)) 1 1 1
