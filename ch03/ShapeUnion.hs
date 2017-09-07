-- file: ch03/ShapeUnion.hs
--
-- Remember 'type' is just a synonym!!!
type Point = (Double, Double)

data Shape = Circle Point Double
            | Poly [Point]
