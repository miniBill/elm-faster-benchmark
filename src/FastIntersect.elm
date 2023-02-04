module FastIntersect exposing (intersect)

import DictDotDot as DDD


intersect : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
intersect l r =
    intersectFromZipper
        ( 0, [] )
        (unconsBiggest [ l ])
        (unconsBiggest [ r ])
        |> fromSortedList


type alias State comparable v =
    Maybe ( comparable, v, List (DDD.Dict comparable v) )


unconsBiggest : List (DDD.Dict comparable v) -> State comparable v
unconsBiggest queue =
    case queue of
        [] ->
            Nothing

        h :: t ->
            case h of
                DDD.RBNode_elm_builtin _ key value childLT DDD.RBEmpty_elm_builtin ->
                    Just ( key, value, childLT :: t )

                DDD.RBNode_elm_builtin color key value childLT childGT ->
                    unconsBiggest (childGT :: DDD.RBNode_elm_builtin color key value childLT DDD.RBEmpty_elm_builtin :: t)

                DDD.RBEmpty_elm_builtin ->
                    unconsBiggest t

                DDD.RBBlackMissing_elm_builtin c ->
                    -- This doesn't happen in practice, performance is irrelevant
                    unconsBiggest (c :: t)


unconsBiggestWhileDroppingGT : comparable -> List (DDD.Dict comparable v) -> State comparable v
unconsBiggestWhileDroppingGT compareKey queue =
    case queue of
        [] ->
            Nothing

        h :: t ->
            case h of
                DDD.RBNode_elm_builtin color key value childLT childGT ->
                    if key > compareKey then
                        unconsBiggestWhileDroppingGT compareKey (childLT :: t)

                    else if key == compareKey then
                        Just ( key, value, childLT :: t )

                    else
                        case childGT of
                            DDD.RBEmpty_elm_builtin ->
                                Just ( key, value, childLT :: t )

                            _ ->
                                unconsBiggestWhileDroppingGT compareKey (childGT :: DDD.RBNode_elm_builtin color key value childLT DDD.RBEmpty_elm_builtin :: t)

                DDD.RBEmpty_elm_builtin ->
                    unconsBiggestWhileDroppingGT compareKey t

                DDD.RBBlackMissing_elm_builtin c ->
                    -- This doesn't happen in practice, performance is irrelevant
                    unconsBiggestWhileDroppingGT compareKey (c :: t)


intersectFromZipper : ( Int, List ( comparable, v ) ) -> State comparable v -> State comparable v -> ( Int, List ( comparable, v ) )
intersectFromZipper (( dsize, dlist ) as dacc) lleft rleft =
    case lleft of
        Nothing ->
            dacc

        Just ( lkey, lvalue, ltail ) ->
            case rleft of
                Nothing ->
                    dacc

                Just ( rkey, _, rtail ) ->
                    if lkey > rkey then
                        intersectFromZipper dacc (unconsBiggestWhileDroppingGT rkey ltail) rleft

                    else if lkey < rkey then
                        intersectFromZipper dacc lleft (unconsBiggestWhileDroppingGT lkey rtail)

                    else
                        intersectFromZipper ( dsize + 1, ( lkey, lvalue ) :: dlist ) (unconsBiggest ltail) (unconsBiggest rtail)


fromSortedList : ( Int, List ( comparable, v ) ) -> DDD.Dict comparable v
fromSortedList ( len, arr ) =
    let
        redLayer : Int
        redLayer =
            floor (logBase 2 (toFloat len))

        go : Int -> Int -> Int -> List ( comparable, v ) -> ( DDD.Dict comparable v, List ( comparable, v ) )
        go layer fromIncluded toExcluded acc =
            if fromIncluded >= toExcluded then
                ( DDD.RBEmpty_elm_builtin, acc )

            else
                let
                    mid : Int
                    mid =
                        fromIncluded + (toExcluded - fromIncluded) // 2

                    ( lchild, accAfterLeft ) =
                        go (layer + 1) fromIncluded mid acc
                in
                case accAfterLeft of
                    [] ->
                        ( DDD.RBEmpty_elm_builtin, acc )

                    ( k, v ) :: tail ->
                        let
                            ( rchild, accAfterRight ) =
                                go (layer + 1) (mid + 1) toExcluded tail

                            color : DDD.NColor
                            color =
                                if layer == redLayer then
                                    DDD.Red

                                else
                                    DDD.Black
                        in
                        ( DDD.RBNode_elm_builtin color k v lchild rchild
                        , accAfterRight
                        )
    in
    go 0 0 len arr
        |> Tuple.first
