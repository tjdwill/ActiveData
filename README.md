# ActiveData
A container for live data. The stored data can respond to queries, potentially speeding up access time (that's the goal, anyway).

## To-Do
- [ ] Add Exception handling for ActiveData objects.
- [ ] Write and record tests of container instantiation and access
  - [ ] Querying numeric data
  - [ ] Querying for dictionaries with certain values within a given key
  - [ ] Querying by index (+/-)
  - [ ] Querying by boolean expression
  - [ ] Querying by Numpy values? As in, "retrieve all elements arr such that np.mean(arr) <= 7" or something.
