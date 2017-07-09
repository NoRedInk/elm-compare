module Compare exposing (Comparator, by, concat, max, maximum, min, minimum, reverse)

{-| Tools for composing comparison functions.

A comparison function is the type of function you can pass to `List.sortWith`.
You pass it two elements of a type and it returns an Order (defined in the Basics module and imported by deault).


## Comparator type

@docs Comparator


## Using compare functions

@docs min, max, minimum, maximum


## Composing compare functions

@docs by, concat, reverse

-}


{-| A function that returns the relative ordering of two elements of a type.
-}
type alias Comparator a =
    a -> a -> Order


{-| Take a function meant for `List.sortBy` and turn it into a Comparator.

    List.sortWith (by String.length) [ "longer", "short", "longest" ]
    --> [ "short", "longer", "longest" ]

-}
by : (a -> comparable) -> Comparator a
by fn a b =
    compare (fn a) (fn b)


{-| Compare by multiple criteria in order. The first criterium that distinguishes two elements is used.

    kara : { firstName : String, lastName : String }
    kara = { firstName = "Kara", lastName = "Thrace" }

    william : { firstName : String, lastName : String }
    william = { firstName = "William", lastName = "Adama" }

    lee : { firstName : String, lastName : String }
    lee = { firstName = "Lee", lastName = "Adama" }

    List.sortWith (concat [ by .lastName, by .firstName ]) [ kara, william, lee  ]
    --> [ lee, william, kara ]

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

    List.sortWith (by String.length |> reverse) [ "longer", "short", "longest" ]
    --> [ "longest", "longer", "short" ]

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

    minimum (by String.length) [ "longer", "short", "longest" ]
    --> Just "short"

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

    maximum (by String.length) [ "longer", "short", "longest" ]
    --> Just "longest"

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

    Compare.min (by String.length) "short" "longer"
    --> "short"

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

    Compare.max (by String.length) "short" "longer"
    --> "longer"

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
