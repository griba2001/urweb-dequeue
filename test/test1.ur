
structure HSR = HSRandom

structure T = Deq_UnitTest

fun main () =
        testdata <- HSR.getSysRandomIntList 30 0 100 ;
        let val iRnd = case testdata of
                        [] => 1
                        | x :: _ => x
        in
           (failedResults, listFromTo) <- T.unitTest (testdata, iRnd) ;
           return <xml>
<body><br/>
<p>
         Data1       : {[testdata]}<br/>
         Through Deq : {[listFromTo]}<br/>
</p>
<p>Failed tests: <br/> {failedResults}</p>
</body></xml>

end
