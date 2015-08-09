structure U = HUrUnit
structure D = Deq

fun unitTest (testdata: list int, iRnd: int): transaction (xbody * list int) =
    let
        val myDeq = D.fromList testdata
        val fromTo = D.toList myDeq
    in
        tst0 <- U.assertBool "propConsViewL fails" (D.propConsViewL iRnd myDeq) ;
        tst1 <- U.assertBool "propSnocViewR fails" (D.propSnocViewR iRnd myDeq) ;
        tst2 <- U.assertBool "propFromToList fails" (D.propFromToList testdata) ;
        tst3 <- U.assertBool "propNthSameElements fails" (D.propNthSameElements myDeq) ;
        tst4 <- U.assertBool "propTakeDropSplitAt fails" (D.propTakeDropSplitAt myDeq) ;
        let
            val testsResults = tst0 :: tst1 :: tst2 :: tst3 :: tst4 :: []
            val xmlJoinedResults = List.foldr join <xml/> testsResults
        in
            return (xmlJoinedResults, fromTo)
        end
     end

