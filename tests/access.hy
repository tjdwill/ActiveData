"
@author: tjdwill
@date: 21 March 2022
@title: Active Data Testing
@description:
    Tests data access in the new container type.

Run from top-level directory as a module 
    >>> cd active_data
    >>> hy -m tests.access
"
(import
    os
    random
    time
    activedata [ActiveArray])


(if (= __name__ "__main__")
(do
    ;(random.seed 42)
    (setv ELEMENTS 100)
    (setv TRIALS 100)
    (print "Compilation Success!\n")
    (try
        ; (setv lst (lfor x (range ELEMENTS) (random.randint 0 1000)))
        (setv lst (lfor x (range ELEMENTS) x))
        (setv arr (ActiveArray lst))
        (print "Live Data created.\n")
    (except [e[]]
        (print e))
    )
    

    (try

        (setv [counter correct totaltime max_time min_time] [0 0 0 0 Inf])
        (for [i (range TRIALS)]
            (setv idx (random.randint 0 (- (len arr) 1)))
            (setv starttime (time.perf_counter))
            (setv answer (. arr [idx]))
            (print f"Index Query Response {counter}\nTarget: {idx}, Answer: {answer}")
            (setv new_time (- (time.perf_counter) starttime))
            (print "Elapsed Time (s): " new_time)
            (setv counter (+ counter 1))
            ; Time Data
            (if (< new_time min_time)
                (setv min_time new_time)
                None)
            (if (> new_time max_time)
                (setv max_time new_time)
                None)            
            (setv totaltime (+ totaltime new_time))
            ; Accuracy
            (if (and answer (= (. answer [0])  (. lst [idx])))
                (setv correct (+ correct 1))
                None))
        (print f"\nNumber of Elements in Array: {(len arr)}")
        (print f"Retrieval Accuracy: {(* (/ correct counter) 100) :.02f}%")
        (print f"Average Access Time (s): {(/ totaltime TRIALS) :.04f}")
        (print f"Min Access Time (s): {min_time :.04f}")
        (print f"Max Access Time (s): {max_time :.04f}")
#[CHECK[]CHECK]
        ; Try new form of selection
        (setv func '(fn [x] (and (< x (/ ELEMENTS 4)) (= (% x 2) 0))))
        (setv starttime (time.perf_counter))
        (print f"\nValues in which\n{func}:\n{(get arr (hy.eval func))}")
        (setv new_time (- (time.perf_counter) starttime))
        (print "Elapsed Time (s): " new_time)

        ; Get Indices
        ;(setv func '(fn [x] (>= x 75)) )
        (setv starttime (time.perf_counter))
        (print f"\nIndices Where\n{func}:\n{(arr.where (hy.eval func))}")
        (setv new_time (- (time.perf_counter) starttime))
        (print "Elapsed Time (s): " new_time)

    (except [e[BaseException]]
        (print "Something happened.\n" e)
        (arr.__del__))
    (else
        (del arr))))
    
None)
