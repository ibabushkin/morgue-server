* Add consistent `data` prefixes
* decide upon a clear approach regarding verification.
  * Right now, we move it upfront, which leads to numerous advantages. 
  * However, this also urges the usage of ugly things like `head` and `fromJust`
  * look into how we want to handle discrepancy between the types as expected in `Main.hs`
    and how they are (partly) implemented in the modules.
* add merging and other useful features still needed
