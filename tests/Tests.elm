module Tests exposing (suite)

import Test exposing (..)
import Tests.IntDict


suite : Test
suite =
  describe "intdict"
    [ Tests.IntDict.all
    ]
