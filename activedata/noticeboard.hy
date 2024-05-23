"
@author: tjdwill
@date: 18 March 2024
@last_modified: 22 May 2024
@title: ActiveData
@description:
    A one-way communication channel that, in the context of this project, allows the ActiveArray to issue commands to the ActiveData.
"

#[PROPOSALS[
    1. Should I move the response board to be an element of this structure?
]PROPOSALS]

(import collections [namedtuple])


(defclass NoticeBoard []
    "A class that allows data to be Noneed down to threads in one direction."
    (defn __init__ [self author_key canvas]
        (setv self._canvas-type (type (canvas))) ; Type of container the data lives in
        (setv #^ namedtuple self._board (canvas)) ; The actual "board"
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

    (defn __repr__ [self]
        (str self.content))

    (defn __str__ [self]
        (str self.content)))
