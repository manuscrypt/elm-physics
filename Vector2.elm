module Vector2 exposing (..)

{-| Represents a two dimensional vector.

# Types and Constructors
@docs Vector, origin, xUnit, yUnit

# Type Conversions
@docs toFloatVector, toTuple, toList

# Mathematical Operations
@docs negate, add, subtract, multiply, divide, dotMultiply, scaleBy, normalize, center

# Norms and Distances
@docs length, lengthSquared, distance, norm, manhattanNorm, manhattanDistance, maximumNorm, chebyshevDistance, canberraDistance

# Useful aliases and operators
@docs (<+>), (<->), (<*>), (</>), (<.>), sub, mul, div, dot, taxicabNorm, taxicabDistance, euclideanNorm, euclideanDistance
-}

-- TYPES AND CONSTRUCTORS


{-| The Vector type for 2-d vector operations. Useful for 2d graphics and vector math
-}
type alias Vector =
    { x : Float, y : Float }


{-| Default constructor at x = 0, y = 0
-}
origin : Vector
origin =
    Vector 0 0


{-| Simple constructor at x = 1, y = 0
-}
xUnit : Vector
xUnit =
    Vector 1 0


{-| Simple Constructor a x = 0, y = 1
-}
yUnit : Vector
yUnit =
    Vector 0 1



-- Type Conversions


{-| Convert an integer point or vector to a float Vector.
-}
toFloatVector : { a | x : Int, y : Int } -> Vector
toFloatVector p =
    Vector (toFloat p.x) (toFloat p.y)


fromIntTuple : ( Int, Int ) -> Vector
fromIntTuple ( x, y ) =
    toFloatVector { x = x, y = y }


toIntTuple : Vector -> ( Int, Int )
toIntTuple p =
    ( round p.x, round p.y )


toIntPoint : Vector -> { x : Int, y : Int }
toIntPoint p =
    { x = round p.x, y = round p.y }


{-| Convert a vector to a tuple.
-}
toTuple : Vector -> ( Float, Float )
toTuple v =
    ( v.x, v.y )


{-| Convert a tuple to a vector.
-}
fromTuple : ( Float, Float ) -> Vector
fromTuple ( x, y ) =
    Vector x y


{-| Convert a vector to a list.
-}
toList : Vector -> List Float
toList v =
    [ v.x, v.y ]



-- MATHEMATICAL OPERATIONS


{-| Negates a vector.

        negate (Vector 3 4) == Vector -3 -4
-}
negate : Vector -> Vector
negate =
    scaleBy (-1)


{-| Vector addition.

        (Vector 2 1) <+> (Vector 4 4) == Vector 6 5
-}
(<+>) : Vector -> Vector -> Vector
(<+>) p q =
    Vector (p.x + q.x) (p.y + q.y)


{-| Vector addition

        add (Vector 2 1) (Vector 4 4) == Vector 6 5
-}
add : Vector -> Vector -> Vector
add =
    (<+>)


{-| Vector subtraction

        (Vector 3 4) <-> (Vector 2 2) == Vector 1 2
-}
(<->) : Vector -> Vector -> Vector
(<->) p q =
    Vector (p.x - q.x) (p.y - q.y)


{-| Vector subtraction

        subtract (Vector 3 4) (Vector 2 2) == Vector 1 2
-}
subtract : Vector -> Vector -> Vector
subtract =
    (<->)


{-| Vector subtraction

        sub (Vector 3 4) (Vector 2 2) == Vector 1 2
-}
sub : Vector -> Vector -> Vector
sub =
    subtract


{-| Vector multiplication. Element by element multiplication.
    For dot product see "<.>".

        (Vector 3 4) <*> (Vector 2 2) == Vector 6 8
-}
(<*>) : Vector -> Vector -> Vector
(<*>) p q =
    Vector (p.x * q.x) (p.y * q.y)


{-| Vector multiplication. Element by element multiplication.
    For dot product see "dotMultiply".

        multiply (Vector 3 4) (Vector 2 2) == Vector 6 8
-}
multiply : Vector -> Vector -> Vector
multiply =
    (<*>)


{-| Vector multiplication. Element by element multiplication.
    For dot product see "dot".

        mul (Vector 3 4) (Vector 2 2) == Vector 6 8
-}
mul : Vector -> Vector -> Vector
mul =
    multiply


{-| Vector division. Element by element division.

        (Vector 6 4) </> (Vector 2 2) == Vector 3 2
-}
(</>) : Vector -> Vector -> Vector
(</>) p q =
    Vector (p.x / q.x) (p.y / q.y)


{-| Vector division. Element by element division.

        divide (Vector 6 4) (Vector 2 2) == Vector 3 2
-}
divide : Vector -> Vector -> Vector
divide =
    (</>)


{-| Vector division. Element by element division.

        div (Vector 6 4) (Vector 2 2) == Vector 3 2
-}
div : Vector -> Vector -> Vector
div =
    divide


{-| Dot product.

        (Vector 3 4) <.> (Vector 2 2) == 14
-}
(<.>) : Vector -> Vector -> Float
(<.>) p q =
    p.x * q.x + p.y * q.y


{-| Dot product.

        dotMultiply (Vector 3 4) (Vector 2 2) == 14
-}
dotMultiply : Vector -> Vector -> Float
dotMultiply =
    (<.>)


{-| Dot product.

        dot (Vector 3 4) (Vector 2 2) == 14
-}
dot : Vector -> Vector -> Float
dot =
    dotMultiply



-- USEFUL FUNCTIONS


{-| Scalar multiplication. Scale a vector by some value

        scaleBy 3 (Vector 2 8) == Vector 6 24
-}
scaleBy : Float -> Vector -> Vector
scaleBy n v =
    Vector (v.x * n) (v.y * n)


{-| Normalizes a vector. (i.e. scales the vector such that it has unit length)
-}
normalize : Vector -> Vector
normalize v =
    scaleBy (1 / (length v)) v


{-| Find the center of two points (vectors).
-}
center : Vector -> Vector -> Vector
center p q =
    scaleBy 0.5 (p <+> q)



-- NORMS AND DISTANCES


{-| Find the nth norm of a vector. This is a generalized form of length or distance.
    Note that typical pythagorean length or euclidean distance is just "norm 2".

        norm 2 (Vector 5 12) == 13
-}
norm : Float -> Vector -> Float
norm n v =
    ((v.x ^ n) + (v.y ^ n)) ^ (1 / n)


{-| Find the manhattan (or taxicab) norm of a vector.
-}
manhattanNorm : Vector -> Float
manhattanNorm v =
    (abs v.x) + (abs v.y)


{-| Find the taxicab (or manhattan) norm of a vector.
-}
taxicabNorm : Vector -> Float
taxicabNorm =
    manhattanNorm


{-| Find the euclidean length of a vector.
    This is the most typical measure for the length of a vector.
    Use this if unsure.

        length (Vector 5 12) == 13
-}
length : Vector -> Float
length =
    norm 2


{-| Find the length squared of a vector.

    This is usually a convenience function but nevertheless very useful.

-}
lengthSquared : Vector -> Float
lengthSquared v =
    (v.x * v.x) + (v.y * v.y)


{-| Find the euclidean norm of a vector.
-}
euclideanNorm : Vector -> Float
euclideanNorm =
    length


{-| Finds the euclidean distance between two vectors.
    This is the most typical measure of distance between two vectors.
    Use this if unsure
-}
distance : Vector -> Vector -> Float
distance p q =
    length (p <-> q)


{-| Finds the euclidean distance between two vectors.
-}
euclideanDistance : Vector -> Vector -> Float
euclideanDistance =
    distance


{-| Finds the manhattan (or taxicab) distance between two vectors.
-}
manhattanDistance : Vector -> Vector -> Float
manhattanDistance p q =
    manhattanNorm (p <-> q)


{-| Finds the taxicab (or manhattan) distance between two vectors.
-}
taxicabDistance : Vector -> Vector -> Float
taxicabDistance =
    manhattanDistance


{-| Finds the maximum norm of a vector.

        maximumNorm (Vector -2 1) == 2
-}
maximumNorm : Vector -> Float
maximumNorm v =
    max (abs v.x) (abs v.y)


{-| Finds the Chebyshev distance between two vectors.
-}
chebyshevDistance : Vector -> Vector -> Float
chebyshevDistance p q =
    max (abs (q.x - p.x)) (abs (q.y - p.y))


{-| Finds the Canberra distance between two vectors.
-}
canberraDistance : Vector -> Vector -> Float
canberraDistance p q =
    let
        numX =
            abs (p.x - q.x)

        numY =
            abs (p.y - q.y)

        denX =
            (abs p.x) + (abs q.x)

        denY =
            (abs p.y) + (abs q.y)
    in
        (numX / denX) + (numY / denY)


perp : Vector -> Vector
perp v =
    Vector v.y -v.x


flip : Vector -> Vector
flip v =
    Vector v.y v.x
