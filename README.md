# ActiveData
A very impractical container for live data. The stored data can respond to queries, potentially speeding up access time (that's the goal, anyway).

## To-Do
- [x] Add Exception handling for ActiveData objects.
- [x] Experiment with container instantiation and access
  - [x] Querying numeric data
  - [x] Querying for dictionaries with certain values within a given key
  - [x] Querying by index (+/-)
  - [x] Querying by boolean expression
  - [x] Querying by Numpy values? As in, "retrieve all elements arr such that np.mean(arr) <= 7" or something.

## Examples

```hy
>>> (import
    	threading
    	activedata [ActiveArray]
    	numpy :as np)
>>> (setv data (
    	lfor _ (range 3) 
    		(np.random.randint 0 50 :size #(2 2))))
>>> (print #* data :sep "\n\n")
[[39 33]
[25 24]]

[[32 30]
[42 22]]

[[25  7]
[29 19]]
>>> (threading.active_count)
1
>>> (setv actv_arr (ActiveArray data))
>>> (threading.active_count)
4

>>> ; Get all arrays whose entries are all even.

>>> (get actv_arr (fn [x]
	    (. (np.equal (% x 2) 0) (all))))
[array([[32, 30],
[42, 22]])]

>>> ; Get all arrays that have at least one even entry

>>> (get actv_arr (fn [x]
	    (. (np.equal (% x 2) 0) (any))))
[array([[39, 33],
[25, 24]]) array([[32, 30],
[42, 22]])]	
>>> (setv a (get actv_arr 1))
>>> (setv b (get
      actv_arr (fn [x] (. (np.equal (% x 2) 0) (all)))))
>>> (= a b)
True

>>> ; Get all arrays that sum to less than 100.

>>> (get actv_arr (fn [x] (< (x.sum) 100)))
[array([[25,  7],
[29, 19]])]
>>> (del actv_arr)
>>> (threading.active_count)
1
```
