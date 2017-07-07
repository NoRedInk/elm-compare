module Order exposing (Comparator, compareBy, concat, max, maximum, min, minimum, reverse)

{-| Tools for composing comparison functions.

A comparison function is the type of function you can pass to `List.sortWith`.


## Comparator type

@docs Comparator


## Using compare functions

@docs min, max, minimum, maximum


## Composing compare functions

@docs compareBy, concat, reverse

-}


{-| A function that returns the relative ordering of two elements of a type.
-}
type alias Comparator a =
    a -> a -> Order


{-| Take a function meant for `List.sortBy` and turn it into a Comparator.

    List.sortWith (compareBy .firstName) people

-}
compareBy : (a -> comparable) -> Comparator a
compareBy fn a b =
    compare (fn a) (fn b)


{-| Compare by multiple criteria in order. The first criterium that distinguishes two elements is used.

    List.sortWith (concat [ byLastName, byFirstName ]) people

-}
concat : List (Comparator a) -> Comparator a
concat comparators a b =
    case comparators of
        [] ->
            EQ

        comparator :: rest ->
            case comparator a b of
                EQ ->
                    concat rest a b

                order ->
                    order


{-| Reverse an ordering function.

    List.sortWith (reverse byLastName) people

-}
reverse : Comparator a -> Comparator a
reverse comparator a b =
    case comparator a b of
        EQ ->
            EQ

        GT ->
            LT

        LT ->
            GT


{-| Like `List.minimum` but using a custom comparator.
-}
minimum : Comparator a -> List a -> Maybe a
minimum comparator xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            List.foldl (min comparator) x rest
                |> Just


{-| Like `List.maximum` but using a custom comparator.
-}
maximum : Comparator a -> List a -> Maybe a
maximum comparator xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            List.foldl (max comparator) x rest
                |> Just


{-| Like `Basics.min` but using a custom comparator.
-}
min : Comparator a -> a -> a -> a
min comparator x y =
    case comparator x y of
        EQ ->
            x

        GT ->
            y

        LT ->
            x


{-| Like `Basics.max` but using a custom comparator.
-}
max : Comparator a -> a -> a -> a
max comparator x y =
    case comparator x y of
        EQ ->
            x

        GT ->
            x

        LT ->
            y
