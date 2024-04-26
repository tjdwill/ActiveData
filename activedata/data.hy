(import 
    threading
    typing [Any]
    activedata.noticeboard [NoticeBoard])

(defclass ActiveData []
    "A data class that is expected to be 'alive'"

    (setv NO_MATCH False)
    (setv SHUTDOWN_VAL None)

    (defn __init__ [
            self 
            #^ int idx 
            #^ Any value
            #^ (get list threading.Event) event_flags
            #^ NoticeBoard notice_board
            #^ (get dict #(int Any)) response_board
        ]

        ; Checks
        (cond
            (< idx 0)
                (raise (ValueError "<ActiveData>: Unacceptable index."))
            (not (isinstance idx int))
                (raise (TypeError "<ActiveData>: index must be an integer.")))

        (setv self.idx idx)
        (setv self.value value)
        (setv [self.fl_command self.fl_resume self.fl_write] event_flags) ; event flags
        (setv self.notice_board notice_board) ; To view commands
        (setv self.response_board response_board) ; To write output
        ; Command mappings
        (setv self.command_mappings {
            -1 self._shutdown
            0 self.val-from-idx
            1 self.idx-from-bool
            2 self.val-from-bool})
        )
    
    (defn val-from-idx [self query]
        "Returns the value if this data has the correct index."
        (if (query self.idx)
            self.value
            self.NO_MATCH))

    (defn idx-from-bool [self query]
        "Returns the index if the data's value fits the query.'"
        (if (query self.value)
            (do 
            ;(print f"Thread {(id self)}: Index Found {self.idx}.\n")
            self.idx)
            self.NO_MATCH))

    (defn val-from-bool [self query]
        "Returns the value if this data fits the query."
        (if (query self.value)
            self.value
            self.NO_MATCH))
    
    (defn _shutdown [self query]
        "Triggers the shutdown process"
        self.SHUTDOWN_VAL)

    (defn spin [self]
        (while True
            (self.fl_command.wait)
            ; Command was issued
            (setv [command query] self.notice_board.content)
            ;(print f"Thread {(id self)}: Command {command}; Query {query}\n")
            (setv answer ((get self.command_mappings command) query))
            ;(print f"Thread {(id self)}: Query Answer is {answer}")
            ;(print (not (is answer None)))

            ; Use `is` comparisons to guard against valid False values (ex. 0, [], etc.)
            (if (is answer self.SHUTDOWN_VAL)
                (break)
                (if (is answer self.NO_MATCH)
                    None
                    (setv (get self.response_board (id self)) answer)))
            ;(print f"Thread {(id self)} Curr Dict\n{self.response_board} with ID {( id self.response_board)}\n"))
            (self.fl_write.set)
            (self.fl_resume.wait)
            (self.fl_write.clear))))
            
            ;(print f"Thread {(id self)}: {self.response_board}\n"))))
