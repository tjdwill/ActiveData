#[COPYRIGHT[
    This file is part of ActiveData. ActiveData is free software: you can
    redistribute it and/or modify it under the terms of the GNU General Public
    License v2.0 as published by the Free Software Foundation.

    ActiveData is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
    A PARTICULAR PURPOSE. See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with
    ActiveData. If not, see <https://www.gnu.org/licenses/>. 
]COPYRIGHT]
"
@author: tjdwill
@date: 18 March 2024
@last_modified: 22 May 2024
@title: ActiveArray
@description:
    A data container in which the data is 'alive', meaning the contained data
    has an active role in container queries.
@version: 0.0.6
"
(import 
    collections [namedtuple]
    threading
    typing [Any]
    activedata.noticeboard [NoticeBoard]
    activedata.data [ActiveData])


#[TODO[
    - What are the weak points of the program implementation?
    - Investigate Race condition: Why do I get the incorrect answer at times for larger array lengths?
    - Why does a larger number of threads result in super slow performance?
        * Answer: GIL + Context Switching
    - How can I make it faster than the current list implementation? (It's currently over 10x slower.)
        * It's going to be slower because the list operations are implemented in C. You won't outspeed it by nature of 
        the Active data structures being implemented in Python (CPython).
]TODO]

(setv cat (. "" join))

(defclass ActiveArray []
    "A container class to coordinate ActiveData instances."

    "Define mappings from desired command to ActiveData command-map key.
    Command Mapping:
            -1: Shutdown
            0: Get value from index
            1: Get idx from value
            2: Get values from boolean
    "
    (setv command_mappings {
        "shutdown" -1
        "val_from_idx" 0 
        "idx_from_bool" 1 
        "val_from_bool" 2})

    (defn __init__ [self #^ (get tuple Any) data]
        (setv self.canvas_type (namedtuple "CommandBoard" ["command" "query"] :defaults [None None]))
        (setv self.notice_board (NoticeBoard (id self) self.canvas_type))
        
        ; response_board {obj_id relevant_data}
        (setv #^ (get dict #(int Any)) self.response_board {})
        (setv self._key_pool [])  ; (Debug) list of ActiveData IDs
        (setv self.fl_write_pool []) ; A Pool of write event flags
        (setv self.fl_command (threading.Event)) ; For sending commands
        (setv self.fl_resume (threading.Event)) ; Makes threads sit but not write.
        (setv self.flags [self.fl_command self.fl_resume])
        (setv self._len (len data))
        (setv self._dtype (type (get data 0)))  ; inner dtype

        ; Initialize data members
        (for [[idx value] (enumerate data)]
            (setv fl_write (threading.Event))
            (self.fl_write_pool.append fl_write)
            (setv livedata (ActiveData #* [
                idx
                value
                [#* self.flags fl_write]
                self.notice_board
                self.response_board
            ]))
            (setv thread (threading.Thread :target livedata.spin))
            (thread.start)
            (self._key_pool.append (id livedata)))
        ; Initialize writeable dict.
        (self.response_board.update (dfor key self._key_pool key None)))
    
    (defn _send-command [self command query]
        "Publish command and data query to the notice board."
        (setv self.notice_board.content [(id self) (self.canvas_type :command command :query query)])
        (self.fl_command.set))

    (defn _send-response [self]
        "Output the coalesced responses from the data objects"
        ; Wait for writes to finish
        (while (not (all (gfor flag self.fl_write_pool (flag.is_set))))
            None)
        (self.fl_command.clear)
        (self.fl_resume.set)
        (setv response (list (filter (fn [x] (not (is x None))) (self.response_board.values))))
        (self.response_board.update (self.response_board.fromkeys self.response_board None))
        (self.fl_resume.clear)
        response)
    
    (defn _shutdown-data [self]
        (self.fl_resume.set)  ; wake any blocking threads
        (self._send-command (get self.command_mappings "shutdown") (fn [x] None)))

    (defn #^ type _val-from-idx [self #^ int index]
        (cond
            (< index 0) (do
                (if (<= 1 (abs index) self._len)
                (setv index (+ self._len index))
                (raise (IndexError "Index out of range.\n"))))
            
            (>= index self._len) (raise (IndexError "Index out of range.\n")))
        ; Send requisite signal
        (self._send-command (get self.command_mappings "val_from_idx") (fn [x] (= x index))))

    (defn #^ type _val-from-bool [self query]
        ; query: an anonymous boolean predicate
        (self._send-command (get self.command_mappings "val_from_bool") query))

    (defn #^ int where [self query]
        ; Returns the indices where the condition is satisfied
        (self._send-command (get self.command_mappings "idx_from_bool") query)
        (self._send-response))

    (defn __len__ [self]
        (return self._len))

    (defn __getitem__ [self query]
        (if (= (type query) int)
            (self._val-from-idx query)
            (self._val-from-bool query))
        (self._send-response))

    (defn __repr__ [self]
        (cat [f"ActiveArray:\n\tInner datatype: {self._dtype}\n\t"
        f"num_elements: {self._len}"]))

    (defn __str__ [self]
        ; Return all data elements
        (str (. self [(fn [x] True)])))

    (defn __enter__ [self]
        self)

    (defn __exit__ [self #* args #** kwargs]
        (self._shutdown-data))

    (defn __del__ [self]
        (self._shutdown-data)))
