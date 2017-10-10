module Tests.IntDict exposing (all)

{-| Copied and modified from `Dict`s test suite. -}

import Basics exposing (..)
import IntDict exposing (IntDict)
import List
import Maybe exposing (..)

import Test exposing (..)
import Expect

numbers : IntDict String
numbers = IntDict.fromList [ (2, "2"), (3, "3") ]

moreNumbers : IntDict String
moreNumbers = IntDict.fromList [ (2, "2"), (5, "5"), (7, "7"), (45, "45") ]

all : Test
all =
  describe "The IntDict module"
    [ describe "build"
        [ test "empty" <| \() -> Expect.equal (IntDict.fromList []) (IntDict.empty)
        , test "singleton" <| \() -> Expect.equal (IntDict.fromList [(1,"v")]) (IntDict.singleton 1 "v")
        , test "insert" <| \() -> Expect.equal (IntDict.fromList [(1,"v")]) (IntDict.insert 1 "v" IntDict.empty)
        , test "insert replace" <| \() -> Expect.equal (IntDict.fromList [(1,"vv")]) (IntDict.insert 1 "vv" (IntDict.singleton 1 "v"))
        , test "update" <| \() -> Expect.equal (IntDict.fromList [(1,"vv")]) (IntDict.update 1 (\v->Just "vv") (IntDict.singleton 1 "v"))
        , test "update Nothing" <| \() -> Expect.equal IntDict.empty (IntDict.update 1 (\v->Nothing) (IntDict.singleton 1 "v"))
        , test "remove" <| \() -> Expect.equal IntDict.empty (IntDict.remove 1 (IntDict.singleton 1 "v"))
        , test "remove not found" <| \() -> Expect.equal (IntDict.singleton 1 "v") (IntDict.remove 342 (IntDict.singleton 1 "v"))
        ]
    , describe "query"
        [ test "isEmpty - empty is empty" <| \() -> Expect.equal True (IntDict.isEmpty IntDict.empty)
        , test "isEmpty - non-empty dict is not empty" <| \() -> Expect.equal False (IntDict.isEmpty numbers)
        , test "size 1" <| \() -> Expect.equal 0 (IntDict.size IntDict.empty)
        , test "size 2" <| \() -> Expect.equal 2 (IntDict.size numbers)
        , test "size 3" <| \() -> Expect.equal 5 (IntDict.size (IntDict.union numbers moreNumbers))
        , test "member 1" <| \() -> Expect.equal True (IntDict.member 2 numbers)
        , test "member 2" <| \() -> Expect.equal False (IntDict.member 5234 numbers)
        , test "get 1" <| \() -> Expect.equal (Just "2") (IntDict.get 2 numbers)
        , test "get 2" <| \() -> Expect.equal Nothing (IntDict.get 5234 numbers)
        , test "findMin" <| \() -> Expect.equal (Just (2, "2")) (IntDict.findMin numbers)
        , test "findMax" <| \() -> Expect.equal (Just (3, "3")) (IntDict.findMax numbers)
        , test "before 2 empty" <| \() -> Expect.equal Nothing (IntDict.before 2 IntDict.empty)
        , test "before 2" <| \() -> Expect.equal Nothing (IntDict.before 2 numbers)
        , test "before 3" <| \() -> Expect.equal (Just (2, "2")) (IntDict.before 3 numbers)
        , test "before 4" <| \() -> Expect.equal (Just (3, "3")) (IntDict.before 4 numbers)
        , test "before 2'" <| \() -> Expect.equal (Nothing) (IntDict.before 2 moreNumbers)
        , test "before 5" <| \() -> Expect.equal (Just (2, "2")) (IntDict.before 5 moreNumbers)
        , test "before 6" <| \() -> Expect.equal (Just (5, "5")) (IntDict.before 6 moreNumbers)
        , test "before 7" <| \() -> Expect.equal (Just (5, "5")) (IntDict.before 7 moreNumbers)
        , test "before 44" <| \() -> Expect.equal (Just (7, "7")) (IntDict.before 44 moreNumbers)
        , test "before 45" <| \() -> Expect.equal (Just (7, "7")) (IntDict.before 45 moreNumbers)
        , test "before 46" <| \() -> Expect.equal (Just (45, "45")) (IntDict.before 46 moreNumbers)
        , test "after 2 empty" <| \() -> Expect.equal Nothing (IntDict.after 2 IntDict.empty)
        , test "after 2" <| \() -> Expect.equal (Just (3, "3")) (IntDict.after 2 numbers)
        , test "after 3" <| \() -> Expect.equal Nothing (IntDict.after 3 numbers)
        , test "after 1" <| \() -> Expect.equal (Just (2, "2")) (IntDict.after 1 numbers)
        , test "after 1'" <| \() -> Expect.equal (Just (2, "2")) (IntDict.after 1 moreNumbers)
        , test "after 2'" <| \() -> Expect.equal (Just (5, "5")) (IntDict.after 2 moreNumbers)
        , test "after 5" <| \() -> Expect.equal (Just (7, "7")) (IntDict.after 5 moreNumbers)
        , test "after 6" <| \() -> Expect.equal (Just (7, "7")) (IntDict.after 6 moreNumbers)
        , test "after 7" <| \() -> Expect.equal (Just (45, "45")) (IntDict.after 7 moreNumbers)
        , test "after 44" <| \() -> Expect.equal (Just (45, "45")) (IntDict.after 44 moreNumbers)
        , test "after 45" <| \() -> Expect.equal Nothing (IntDict.after 45 moreNumbers)
        ]
    , describe "combine"
        [ test "union" <| \() -> Expect.equal numbers (IntDict.union (IntDict.singleton 3 "3") (IntDict.singleton 2 "2"))
        , test "union collison" <| \() -> Expect.equal (IntDict.singleton 2 "2") (IntDict.union (IntDict.singleton 2 "2") (IntDict.singleton 2 "3"))
        , test "intersect" <| \() -> Expect.equal (IntDict.singleton 2 "2") (IntDict.intersect numbers (IntDict.singleton 2 "2"))
        , test "diff" <| \() -> Expect.equal (IntDict.singleton 3 "3") (IntDict.diff numbers (IntDict.singleton 2 "2"))
        ]
    , describe "transform"
        [ test "filter" <| \() -> Expect.equal (IntDict.singleton 2 "2") (IntDict.filter (\k v -> k == 2) numbers)
        , test "partition" <| \() -> Expect.equal (IntDict.singleton 2 "2", IntDict.singleton 3 "3") (IntDict.partition (\k v -> k == 2) numbers)
        ]
    , describe "merge" <|
        let
          insertBoth key leftVal rightVal dict =
            IntDict.insert key (leftVal ++ rightVal) dict
          s1 =
            IntDict.empty |> IntDict.insert 1 [ 1 ]
          s2 =
            IntDict.empty |> IntDict.insert 2 [ 2 ]
          s23 =
            IntDict.empty |> IntDict.insert 2 [ 3 ]
          b1 =
            (List.range 1 10) |> List.map (\i -> (i, [i])) |> IntDict.fromList
          b2 =
            (List.range 5 15) |> List.map (\i -> (i, [i])) |> IntDict.fromList
          bExpected =
            [(1,[1]),(2,[2]),(3,[3]),(4,[4]),(5,[5,5]),(6,[6,6]),(7,[7,7]),(8,[8,8]),(9,[9,9]),(10,[10,10]),(11,[11]),(12,[12]),(13,[13]),(14,[14]),(15,[15])]
        in
          [ test "merge empties" <| \() -> Expect.equal (IntDict.empty)
            (IntDict.merge IntDict.insert insertBoth IntDict.insert IntDict.empty IntDict.empty IntDict.empty)
          , test "merge singletons in order" <| \() -> Expect.equal [(1, [1]), (2, [2])]
            ((IntDict.merge IntDict.insert insertBoth IntDict.insert s1 s2 IntDict.empty) |> IntDict.toList)
          , test "merge singletons out of order" <| \() -> Expect.equal [(1, [1]), (2, [2])]
            ((IntDict.merge IntDict.insert insertBoth IntDict.insert s2 s1 IntDict.empty) |> IntDict.toList)
          , test "merge with duplicate key" <| \() -> Expect.equal [(2, [2, 3])]
            ((IntDict.merge IntDict.insert insertBoth IntDict.insert s2 s23 IntDict.empty) |> IntDict.toList)
          , test "partially overlapping" <| \() -> Expect.equal bExpected
            ((IntDict.merge IntDict.insert insertBoth IntDict.insert b1 b2 IntDict.empty) |> IntDict.toList)
          ]
    , describe "regressions"
        [ describe "issue #1" <|
            let
              a =
                IntDict.fromList [(4,20),(6,11)]
              b =
                IntDict.fromList [(1,0),(2,7),(3,9),(4,22),(6,14)]
            in
              [ test "a union b" <| \() ->
                  Expect.equal
                    (IntDict.union a b)
                    (IntDict.fromList [(1,0),(2,7),(3,9),(4,20),(6,11)])
              , test "b union a" <| \() ->
                  Expect.equal
                    (IntDict.union b a)
                    (IntDict.fromList [(1,0),(2,7),(3,9),(4,22),(6,14)])
              ]
        ]
    ]
