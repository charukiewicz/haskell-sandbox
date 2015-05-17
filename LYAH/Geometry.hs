{-

    The Geometry module

    Made in Chapter 7 of Learn You A Hakell (see ch07_modules.hs)

    This file is not actually imported in ch07_modules.hs, as we import
    the submodules of the Geometry/ directory instead

-}

-- We say that a module exports functions.  This means that when a module is
-- imported, we can use the functions that that module exports.

module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cubeoidArea
, cubeoidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0/3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cubeoidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cubeoidArea side side side

cubeoidVolume :: Float -> Float -> Float -> Float
cubeoidVolume a b c = rectangleArea a b * c

cubeoidArea :: Float -> Float -> Float -> Float
cubeoidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

-- Note that we create the helper function rectangleArea even though we do not
-- export it. This helps us with the other functions that we do export, and because
-- the implementation is abstracted away from the user, our helper function does
-- not concern them.

-- Now we can import our module in our main file (ch07_modules.hs)

-- This module must be in the same folder of the file which is importing it.
