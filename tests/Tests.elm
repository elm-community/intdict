module Tests exposing (all)

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
moreNumbers = IntDict.fromList [ (2, "2"), (5, "3"), (7, "7"), (45, "45") ]

all : Test
all =
  describe "The IntDict module"
    [ buildTests
    , queryTests
    , combineTests
    , transformTests
    , mergeTests
    , regressionTests
    ]

buildTests =
  describe "build"
    [ test "empty" <| \() -> Expect.equal (IntDict.fromList []) (IntDict.empty)
    , test "singleton" <| \() -> Expect.equal (IntDict.fromList [(1,"v")]) (IntDict.singleton 1 "v")
    , test "insert" <| \() -> Expect.equal (IntDict.fromList [(1,"v")]) (IntDict.insert 1 "v" IntDict.empty)
    , test "insert replace" <| \() -> Expect.equal (IntDict.fromList [(1,"vv")]) (IntDict.insert 1 "vv" (IntDict.singleton 1 "v"))
    , test "update" <| \() -> Expect.equal (IntDict.fromList [(1,"vv")]) (IntDict.update 1 (\v->Just "vv") (IntDict.singleton 1 "v"))
    , test "update Nothing" <| \() -> Expect.equal IntDict.empty (IntDict.update 1 (\v->Nothing) (IntDict.singleton 1 "v"))
    , test "remove" <| \() -> Expect.equal IntDict.empty (IntDict.remove 1 (IntDict.singleton 1 "v"))
    , test "remove not found" <| \() -> Expect.equal (IntDict.singleton 1 "v") (IntDict.remove 342 (IntDict.singleton 1 "v"))
    ]

queryTests =
  describe "query"
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
    ]

combineTests =
  describe "combine"
    [ test "union" <| \() -> Expect.equal numbers (IntDict.union (IntDict.singleton 3 "3") (IntDict.singleton 2 "2"))
    , test "union collison" <| \() -> Expect.equal (IntDict.singleton 2 "2") (IntDict.union (IntDict.singleton 2 "2") (IntDict.singleton 2 "3"))
    , test "intersect" <| \() -> Expect.equal (IntDict.singleton 2 "2") (IntDict.intersect numbers (IntDict.singleton 2 "2"))
    , test "diff" <| \() -> Expect.equal (IntDict.singleton 3 "3") (IntDict.diff numbers (IntDict.singleton 2 "2"))
    ]

transformTests =
  describe "transform"
    [ test "filter" <| \() -> Expect.equal (IntDict.singleton 2 "2") (IntDict.filter (\k v -> k == 2) numbers)
    , test "partition" <| \() -> Expect.equal (IntDict.singleton 2 "2", IntDict.singleton 3 "3") (IntDict.partition (\k v -> k == 2) numbers)
    ]

mergeTests =
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
    describe "merge"
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

regressionTests =
  describe "regressions Tests"
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
