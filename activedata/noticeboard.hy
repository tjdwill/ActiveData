#[LICENSE[
    This file is part of ActiveData. ActiveData is free software: you can
    redistribute it and/or modify it under the terms of the GNU General Public
    License v2.0 as published by the Free Software Foundation.

    ActiveData is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
    A PARTICULAR PURPOSE. See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with
    ActiveData. If not, see <https://www.gnu.org/licenses/>. 
]LICENSE]
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
