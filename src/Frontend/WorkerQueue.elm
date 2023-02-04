module Frontend.WorkerQueue exposing (WorkerQueue, addFree, areAllFree, freeCount, getOne, init, totalSize)

import Common.Types exposing (Index)
import Deque exposing (Deque)
import Set exposing (Set)


type WorkerQueue
    = WorkerQueue
        { queue : Deque Index
        , free : Set Index
        , size : Int
        }


init : Int -> WorkerQueue
init size =
    let
        list : List Index
        list =
            List.range 0 (size - 1)
    in
    WorkerQueue
        { size = size
        , free = Set.fromList list
        , queue = Deque.fromList list
        }


freeCount : WorkerQueue -> Int
freeCount (WorkerQueue { queue }) =
    Deque.length queue


totalSize : WorkerQueue -> Int
totalSize (WorkerQueue { size }) =
    size


addFree : Index -> WorkerQueue -> WorkerQueue
addFree index ((WorkerQueue { queue, free, size }) as wq) =
    if Set.member index free then
        wq

    else
        WorkerQueue
            { queue = Deque.pushBack index queue
            , free = Set.insert index free
            , size = size
            }


areAllFree : WorkerQueue -> Bool
areAllFree wq =
    freeCount wq == totalSize wq


getOne : WorkerQueue -> ( Maybe Index, WorkerQueue )
getOne ((WorkerQueue { queue, free, size }) as wq) =
    let
        ( got, rest ) =
            Deque.popFront queue

        newQueue : WorkerQueue
        newQueue =
            case got of
                Nothing ->
                    wq

                Just index ->
                    WorkerQueue
                        { queue = rest
                        , free = Set.remove index free
                        , size = size
                        }
    in
    ( got, newQueue )
