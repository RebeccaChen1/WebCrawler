# Pset 5 Writeup: Rebecca Chen & Michael Horton

### DictSet Times
1. Average Time of 3 runs of Simple: 0.000870 seconds
2. Average Time of 3 runs of HTML: 0.247806 seconds
3. Average Time of 3 runs of Wiki: 6.100379 seconds
4. Queries (224)
   * "Turtle" returned 42 results in 0.00571 seconds
   * "Turtle Shell" returned 28 results in 0.00351 seconds
   * "Turtle OR Pizza" returned 52 results in 0.00787 seconds
5. Queries (42)
   * "Turtle" returned 12 results in 0.00034 seconds
   * "Turtle Shell" returned 11 results in 0.00034 seconds
   * "Turtle OR Pizza" returned 13 results in 0.00041 seconds
6. Average Time of 3 runs of Wiki (224 Pages): 180.767660 seconds

### ListSet Times
1. Average Time of 3 runs of Simple: 0.000833 seconds
2. Average Time of 3 runs of HTML: 0.261918 seconds
3. Average Time of 3 runs of Wiki: 187.163942 seconds
4. Queries (42)
  * "Turtle" returned 5 results in 0.00011 seconds
  * "Turtle Shell" returned 4 results in 0.00011 seconds
  * "Turtle OR Pizza" returned 7 results in 0.00020 seconds

### About Run Times
DictSet is substantially faster than ListSet. The functions in the Dict
implementation have faster runtimes than the functions in the List
implementation. Member is O(n) for List but O(log n) for Dict. List
therefore does not scale well whereas Dict does scale well because of its
efficient abstractions and use of a binary search tree. Each was run on
macOS Sierra v10.12.3 at about 13% of CPU. This shows that the crawler is
efficient and does not take over all the resources of the computer.

### Methods
We used time_test.ml to test the run time of indexing the various directories.
Each directory was crawled separately three times and then the sum of these
times was averaged. Because ListSet was taking a inordinate amount of time to
run the average times reflected are crawling 42 pages of the wiki, so that
ListSet would not take hours to finish. Number 6 of DictSet Times reflects
three runs of DictSet over 224 pages.

### Analysis
The times recorded show the significance of Big O notation as a measure of
efficiency instead of relying on runtimes. While the DictSet
implementation runs significantly faster for more pages and larger directories,
the ListSet implementation ran faster on average for the simple-html directory.
This shows that for small values of n, the ListSet implementation is faster.
However, as n increases, DictSet becomes faster than ListSet.
