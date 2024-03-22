"
@author: tjdwill
@date: 18 March 2022
@title: Active Data
@description:
    A data container in which the data is 'alive', meaning the contained data
    has an active role in container queries.
@version: 0.0.5
"

(import 
    collections [namedtuple]
    threading)

#[TODO[
    - Investigate Race condition: Why do I get the incorrect answer at times?
    - Why does a larger number of threads result in super slow performance?
    - What are the weak points of the program implementation?
    - How can I make it faster than the current list implementation? (It's currently over 10x slower.) 
]TODO]

(defclass NoticeBoard []
    "A class that allows data to be Noneed down to threads in one direction."
    (defn __init__ [self author_key canvas]
        (setv self._canvas-type (type (canvas))) ; Type of container the data lives in
        (setv #^ [namedtuple tuple] self._board (canvas)) ; The actual "board"
        (setv self.author_key author_key)) ; Authentication Key
    
    (defn [property] content [self]
        self._board)
    "
    Setter for the NoticeBoard. Data should be in format:
    [authentication_id, new_data]
    "
    (defn [content.setter] content [self data]
        (setv [key new_data] data)
        (if (not (= (type new_data) self._canvas-type))
            (raise (TypeError "Incorrect canvas type.\n"))
            (if (not (= key self.author_key))
                (raise (PermissionError "Authentication Error: No write access.\n"))
                (setv self._board new_data))))
                
    (defn __str__ [self]
        (str (self.content))))
    

(defclass ActiveData []
    "A dataclass that is expected to be alive"

    (setv NO_MATCH False)
    (setv SHUTDOWN_VAL None)

    (defn __init__ [
            self 
            #^ int idx 
            value
            event_flags
            notice_board
            thread_dict
        ]

        (if (< idx 0)
            (raise (ValueError "<ActiveData>: Unacceptable index.\n"))
            (setv self.idx idx))
        (setv self.value value)
        (setv [self.fl_command self.fl_resume self.fl_write] event_flags) ; event flags
        (setv self.notice_board notice_board) ; To view commands
        (setv self.thread_dict thread_dict) ; To write output
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
                    (setv (get self.thread_dict (id self)) answer)))
            ;(print f"Thread {(id self)} Curr Dict\n{self.thread_dict} with ID {( id self.thread_dict)}\n"))
            (self.fl_write.set)
            (self.fl_resume.wait)
            (self.fl_write.clear))))
            
            ;(print f"Thread {(id self)}: {self.thread_dict}\n"))))


(defclass ActiveArray []
    "A container class to coordinate ActiveData instances."

    ; Define mappings from desired command to ActiveData command-map key.
    (setv command_mappings {
        "shutdown" -1
        "val_from_idx" 0 
        "idx_from_bool" 1 
        "val_from_bool" 2})

    (defn __init__ [self #^ [list tuple] data]
        (setv self.canvas_type (namedtuple "CommandBoard" ["command" "query"] :defaults [None None]))
        (setv self.notice_board (NoticeBoard (id self) self.canvas_type))
        "Thread setup"
        ; thread_dict {obj_id relevant_data}
        (setv self.thread_dict {})
        (setv self._key_pool [])  ; (Debug) list of threads
        (setv self.fl_write_pool []) ; A Pool of write event flags
        (setv self.fl_command (threading.Event)) ; For sending commands
        (setv self.fl_resume (threading.Event)) ; Makes threads sit but not write.
        (setv self.flags [self.fl_command self.fl_resume])
        (setv self._len (len data))
        ; Initialize data members
        (for [[idx value] (enumerate data)]
            (setv fl_write (threading.Event))
            (self.fl_write_pool.append fl_write)
            (setv livedata (ActiveData #* [
                idx
                value
                [#* self.flags fl_write]
                self.notice_board
                self.thread_dict
            ]))
            (setv thread (threading.Thread :target livedata.spin))
            (thread.start)
            (self._key_pool.append (id livedata)))
        ; Initialize writeable dict.
        (self.thread_dict.update (dfor key self._key_pool key None)))
    
    (defn send-command [self command query]
        "
        Publish command and query to the notice board.
        Command Mapping:
            -1: Shutdown
            0: Get value from index
            1: Get idx from value
            2: Get values from boolean
        "
        (setv self.notice_board.content [(id self) (self.canvas_type :command command :query query)])
        (self.fl_command.set))

    (defn send-response [self]
        ; Wait for writes to finish
        (while (not (all (gfor flag self.fl_write_pool (flag.is_set))))
            None)
        (self.fl_command.clear)
        (self.fl_resume.set)
        (setv response (list (filter (fn [x] (not (is x None))) (self.thread_dict.values))))
        (self.thread_dict.update (self.thread_dict.fromkeys self.thread_dict None))
        (self.fl_resume.clear)
        response)
    
    (defn shutdown-data [self]
        (self.fl_resume.set)  ; wake any blocking threads
        (self.send_command (get self.command_mappings "shutdown") (fn [x] None)))

    (defn #^ type _val-from-idx [self #^ int index]
        (cond
            (< index 0) (do
                (if (<= 1 (abs index) self._len)
                (setv index (+ self._len index))
                (raise (IndexError "Index out of range.\n"))))
            
            (>= index self._len) (raise (IndexError "Index out of range.\n")))
        ; Send requisite signal
        (self.send_command (get self.command_mappings "val_from_idx") (fn [x] (= x index))))

    (defn #^ type _val-from-bool [self query]
        ; query: an anonymous boolean predicate
        (self.send_command (get self.command_mappings "val_from_bool") query))

    (defn #^ int where [self query]
        ; Returns the indices where the condition is satisfied
        (self.send_command (get self.command_mappings "idx_from_bool") query)
        (self.send-response))

    (defn __len__ [self]
        (return self._len))

    (defn __getitem__ [self query]
        (if (= (type query) int)
            (self._val-from-idx query)
            (self._val-from-bool query))
        (self.send-response))
        
    (defn __str__ [self]
        ; Return all data elements
        (str (. self [(fn [x] True)])))

    (defn __del__ [self]
        (self.shutdown-data)))
