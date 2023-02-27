{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SnippetsCheck where

import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Utils.CalculateDistance (everySnippetIs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

snippetsCheckTests :: TestTree
snippetsCheckTests =
  testGroup
    "Testing everySnippetIs function"
    [ testSnippetsCheckInCorrectRealRoute,
      testFailIfLastSnippetBad,
      testFailIfFirstSnippetBad,
      testFailIfSnippetsInTheMiddleBad,
      testZeroPointsShouldReturnTrue,
      testOnePointShouldReturnTrue
    ]

testSnippetsCheckInCorrectRealRoute :: TestTree
testSnippetsCheckInCorrectRealRoute =
  testCase "Calling everySnippetIs with good snippets route should return true" $
    everySnippetIs (< 300) goodSnippetsRoute @?= True

testFailIfLastSnippetBad :: TestTree
testFailIfLastSnippetBad =
  let badLastSnippetRoute = goodSnippetsRoute ++ [LatLong 9.9737605 76.284097]
   in testCase "Calling everySnippetIs with bad last snippet should return false" $
        everySnippetIs (< 300) badLastSnippetRoute @?= False

testFailIfFirstSnippetBad :: TestTree
testFailIfFirstSnippetBad =
  let badFirstSnippetRoute = LatLong 9.9737605 76.244097 : goodSnippetsRoute
   in testCase "Calling everySnippetIs with bad first snippet should return false" $
        everySnippetIs (< 300) badFirstSnippetRoute @?= False

testFailIfSnippetsInTheMiddleBad :: TestTree
testFailIfSnippetsInTheMiddleBad =
  testCase "Calling everySnippetIs with bad snippets in the middle should return false" $
    everySnippetIs (< 300) badSnippetsInTheMiddleRoute @?= False

testZeroPointsShouldReturnTrue :: TestTree
testZeroPointsShouldReturnTrue =
  testCase "Calling everySnippetIs with zero points should return true" $
    everySnippetIs (error "Should be never reached") [] @?= True

testOnePointShouldReturnTrue :: TestTree
testOnePointShouldReturnTrue =
  testCase "Calling everySnippetIs with one point should return true" $
    everySnippetIs (error "Should be never reached") [LatLong 20.0 20.0] @?= True

badSnippetsInTheMiddleRoute :: [LatLong]
badSnippetsInTheMiddleRoute =
  [ LatLong 9.9398944 76.2544582,
    LatLong 9.9398909 76.2544567,
    LatLong 9.939888 76.254453,
    LatLong 9.9398883 76.2544531,
    LatLong 9.9398829 76.2544371,
    LatLong 9.9398768 76.2544257,
    LatLong 9.9398604 76.2544371,
    LatLong 9.9398524 76.2544493,
    LatLong 9.939908 76.2543671,
    LatLong 9.939908 76.2843671, -- bad point
    LatLong 9.9399302 76.2543952,
    LatLong 9.9400302 76.2544405,
    LatLong 9.9400567 76.2544309,
    LatLong 9.9401069 76.2543531,
    LatLong 9.9401888 76.2543246,
    LatLong 9.940274 76.2543199,
    LatLong 9.9403908 76.2543252,
    LatLong 9.9403922 76.254325
  ]

goodSnippetsRoute :: [LatLong]
goodSnippetsRoute =
  [ LatLong 9.9398944 76.2544582,
    LatLong 9.9398909 76.2544567,
    LatLong 9.939888 76.254453,
    LatLong 9.9398883 76.2544531,
    LatLong 9.9398829 76.2544371,
    LatLong 9.9398768 76.2544257,
    LatLong 9.9398604 76.2544371,
    LatLong 9.9398524 76.2544493,
    LatLong 9.939908 76.2543671,
    LatLong 9.9399302 76.2543952,
    LatLong 9.9400302 76.2544405,
    LatLong 9.9400567 76.2544309,
    LatLong 9.9401069 76.2543531,
    LatLong 9.9401888 76.2543246,
    LatLong 9.940274 76.2543199,
    LatLong 9.9403908 76.2543252,
    LatLong 9.9403922 76.254325,
    LatLong 9.9403912 76.2543255,
    LatLong 9.9403894 76.2543261,
    LatLong 9.9403933 76.2543236,
    LatLong 9.9404058 76.254299,
    LatLong 9.940435 76.2543254,
    LatLong 9.9404082 76.2544591,
    LatLong 9.9404217 76.2545058,
    LatLong 9.9404428 76.2545107,
    LatLong 9.9404445 76.2545127,
    LatLong 9.9404456 76.254513,
    LatLong 9.9404458 76.2545127,
    LatLong 9.9404459 76.2545135,
    LatLong 9.940446 76.2545139,
    LatLong 9.9404465 76.254514,
    LatLong 9.9404489 76.2545165,
    LatLong 9.940447 76.254507,
    LatLong 9.9404535 76.2545367,
    LatLong 9.940485 76.2544063,
    LatLong 9.9404856 76.2544054,
    LatLong 9.9404867 76.2544041,
    LatLong 9.9404873 76.2544039,
    LatLong 9.9404885 76.2544044,
    LatLong 9.9404831 76.2544035,
    LatLong 9.9405054 76.254401,
    LatLong 9.9404917 76.2543863,
    LatLong 9.9405356 76.2543739,
    LatLong 9.9404698 76.2543737,
    LatLong 9.9405352 76.2548744,
    LatLong 9.940589 76.2553461,
    LatLong 9.9401319 76.2555534,
    LatLong 9.9395652 76.2555812,
    LatLong 9.9392106 76.2556376,
    LatLong 9.9385047 76.2556366,
    LatLong 9.9375899 76.2556057,
    LatLong 9.9366767 76.2556794,
    LatLong 9.9363119 76.2556921,
    LatLong 9.9351912 76.2558317,
    LatLong 9.9351557 76.2560704,
    LatLong 9.9353335 76.256792,
    LatLong 9.9354633 76.2573945,
    LatLong 9.9355591 76.2581132,
    LatLong 9.9355043 76.258261,
    LatLong 9.9353529 76.25832,
    LatLong 9.9347847 76.2584784,
    LatLong 9.934107 76.2586099,
    LatLong 9.9329574 76.2588132,
    LatLong 9.9323185 76.2591149,
    LatLong 9.9320081 76.2593105,
    LatLong 9.9322084 76.2599253,
    LatLong 9.9324095 76.2608791,
    LatLong 9.932475 76.2617346,
    LatLong 9.932479 76.2618825,
    LatLong 9.9326096 76.2621351,
    LatLong 9.9330211 76.262919,
    LatLong 9.9335388 76.2640871,
    LatLong 9.9337354 76.2644983,
    LatLong 9.9334285 76.264736,
    LatLong 9.9330363 76.2653446,
    LatLong 9.9328202 76.2658018,
    LatLong 9.9322172 76.2661768,
    LatLong 9.9317685 76.2664856,
    LatLong 9.9315057 76.2670869,
    LatLong 9.9315215 76.267064,
    LatLong 9.9315072 76.2670681,
    LatLong 9.9315695 76.2669322,
    LatLong 9.9319097 76.2662313,
    LatLong 9.9322136 76.2660685,
    LatLong 9.9329054 76.2656673,
    LatLong 9.9335088 76.2648101,
    LatLong 9.9337261 76.2646656,
    LatLong 9.9338295 76.2645363,
    LatLong 9.9336978 76.264466,
    LatLong 9.9336755 76.2644644,
    LatLong 9.9336972 76.2643114,
    LatLong 9.9333984 76.2636898,
    LatLong 9.9330346 76.2628183,
    LatLong 9.9325948 76.2619272,
    LatLong 9.9324081 76.2611435,
    LatLong 9.9321841 76.2602847,
    LatLong 9.9320771 76.2595885,
    LatLong 9.9321323 76.2592183,
    LatLong 9.9327819 76.258968,
    LatLong 9.9333936 76.2587619,
    LatLong 9.9338434 76.2587383,
    LatLong 9.9342633 76.2586622,
    LatLong 9.934598 76.2585258,
    LatLong 9.9352644 76.2583415,
    LatLong 9.935577 76.2582758,
    LatLong 9.9356161 76.258063,
    LatLong 9.9355687 76.2573506,
    LatLong 9.9354151 76.2566043,
    LatLong 9.9352069 76.2560722,
    LatLong 9.9352094 76.255868,
    LatLong 9.9354833 76.2557625,
    LatLong 9.9361751 76.2557119,
    LatLong 9.9371143 76.2555911,
    LatLong 9.9379369 76.2555983,
    LatLong 9.938632 76.2556986,
    LatLong 9.9389736 76.255741,
    LatLong 9.9394014 76.2557381,
    LatLong 9.9399124 76.2556095,
    LatLong 9.9407917 76.2557176,
    LatLong 9.9418247 76.2558436,
    LatLong 9.9424599 76.2558964,
    LatLong 9.942946 76.255968,
    LatLong 9.9435518 76.2559449,
    LatLong 9.9438331 76.2559526,
    LatLong 9.9440321 76.256003,
    LatLong 9.9444502 76.2560629,
    LatLong 9.9446595 76.2560492,
    LatLong 9.9448985 76.2560086,
    LatLong 9.945373 76.2559084,
    LatLong 9.9460681 76.2558452,
    LatLong 9.9467327 76.2558197,
    LatLong 9.947248 76.2559258,
    LatLong 9.9477598 76.2558551,
    LatLong 9.948272 76.2556759,
    LatLong 9.949095 76.2556278,
    LatLong 9.9493114 76.2557103,
    LatLong 9.9494124 76.2555746,
    LatLong 9.9494283 76.2549797,
    LatLong 9.9494653 76.2541804,
    LatLong 9.9494084 76.253778,
    LatLong 9.9494707 76.2534851,
    LatLong 9.9495028 76.2532594,
    LatLong 9.9495729 76.2529304,
    LatLong 9.9496094 76.2524137,
    LatLong 9.9496595 76.2519506,
    LatLong 9.9496786 76.2511073,
    LatLong 9.949829 76.2504006,
    LatLong 9.949746 76.2502432,
    LatLong 9.9496946 76.2498126,
    LatLong 9.9497162 76.2493365,
    LatLong 9.9488983 76.2493126,
    LatLong 9.9482119 76.2493073,
    LatLong 9.9476222 76.2493323,
    LatLong 9.947077 76.2493599,
    LatLong 9.9465928 76.2494519,
    LatLong 9.9461945 76.2495408,
    LatLong 9.9460593 76.2494892,
    LatLong 9.9457926 76.2492072,
    LatLong 9.9457317 76.2485888,
    LatLong 9.9458146 76.2478109,
    LatLong 9.9459228 76.2474178,
    LatLong 9.9458006 76.2468397,
    LatLong 9.945748 76.2459569,
    LatLong 9.945839 76.245265,
    LatLong 9.9466031 76.2450449,
    LatLong 9.9475667 76.2449813,
    LatLong 9.9479818 76.2450643,
    LatLong 9.9487161 76.2449397,
    LatLong 9.949558 76.2448184,
    LatLong 9.9501572 76.2446354,
    LatLong 9.9505948 76.2446262,
    LatLong 9.951299 76.244686,
    LatLong 9.9515957 76.2446221,
    LatLong 9.9516244 76.2441339,
    LatLong 9.9516122 76.2434132,
    LatLong 9.9516515 76.2426795,
    LatLong 9.9521181 76.2424165,
    LatLong 9.9533909 76.2423739,
    LatLong 9.9544857 76.242439,
    LatLong 9.9553803 76.2424938,
    LatLong 9.9569953 76.2424815,
    LatLong 9.9580443 76.2425291,
    LatLong 9.9590512 76.2426587,
    LatLong 9.9597582 76.2427202,
    LatLong 9.9600314 76.2427758,
    LatLong 9.960336 76.242884,
    LatLong 9.9610344 76.2429602,
    LatLong 9.9614003 76.2429196,
    LatLong 9.9619112 76.2429355,
    LatLong 9.9626063 76.2430359,
    LatLong 9.9630785 76.2430546,
    LatLong 9.963587 76.2429774,
    LatLong 9.9641474 76.2429025,
    LatLong 9.96467 76.2432158,
    LatLong 9.9650579 76.2434864,
    LatLong 9.9652889 76.2435833,
    LatLong 9.9662792 76.2440292,
    LatLong 9.9668321 76.2442162,
    LatLong 9.9671789 76.244374,
    LatLong 9.9680518 76.2443197,
    LatLong 9.9684303 76.2444205,
    LatLong 9.9684431 76.2445717,
    LatLong 9.9684486 76.2446346,
    LatLong 9.9684292 76.244613,
    LatLong 9.968421 76.2447441,
    LatLong 9.968836 76.2449155,
    LatLong 9.9688942 76.2449145,
    LatLong 9.968915 76.2448988,
    LatLong 9.9688228 76.2449572,
    LatLong 9.9687608 76.2449791,
    LatLong 9.9688348 76.2449417,
    LatLong 9.9689061 76.2449002,
    LatLong 9.9689559 76.2449258,
    LatLong 9.9698299 76.245046,
    LatLong 9.971594 76.245025,
    LatLong 9.9730622 76.24485,
    LatLong 9.9734527 76.2446998,
    LatLong 9.9736689 76.2446262,
    LatLong 9.9736669 76.2446251,
    LatLong 9.9736669 76.2446252,
    LatLong 9.9736662 76.244619,
    LatLong 9.9736642 76.2446193,
    LatLong 9.9736628 76.2446195,
    LatLong 9.9736608 76.2446171,
    LatLong 9.9736529 76.2446106,
    LatLong 9.9736523 76.2446029,
    LatLong 9.9736433 76.2445917,
    LatLong 9.9737532 76.2445318,
    LatLong 9.9737765 76.2442782,
    LatLong 9.9737356 76.244145,
    LatLong 9.9737354 76.244145,
    LatLong 9.9737354 76.2441457,
    LatLong 9.9737351 76.2441477,
    LatLong 9.9737354 76.2441479,
    LatLong 9.9737338 76.2441522,
    LatLong 9.9737343 76.2441547,
    LatLong 9.9737346 76.2441575,
    LatLong 9.9737341 76.2441575,
    LatLong 9.9737331 76.2441583,
    LatLong 9.9737331 76.244156,
    LatLong 9.9737332 76.2441554,
    LatLong 9.9737377 76.2441512,
    LatLong 9.9737365 76.244149,
    LatLong 9.9737362 76.2441471,
    LatLong 9.9737393 76.2441443,
    LatLong 9.9737415 76.2441467,
    LatLong 9.9737408 76.2441508,
    LatLong 9.9737433 76.2441611,
    LatLong 9.9737324 76.2441815,
    LatLong 9.9737368 76.244183,
    LatLong 9.9737373 76.2441826,
    LatLong 9.9737316 76.2441844,
    LatLong 9.9737302 76.244185,
    LatLong 9.9737362 76.2441849
  ]
