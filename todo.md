# Our project agenda as of now
* Add consistent `data` prefixes
* decide upon a clear approach regarding verification.
  * Right now, we move it upfront, which leads to numerous advantages. 
  * However, this also urges the usage of ugly things like `head` and `fromJust`
  * look into how we want to handle discrepancy between the types as expected in `Main.hs`
    and how they are (partly) implemented in the modules.
    * This is especially important considering the fact that the current `master`-version
      makes the API functions return a `IO (ApiResponse a)`, while most of those just wrap
      whatever they return into a call to `success`, which is definitely a bad sign.
* add merging and other useful features still needed

## Thoughts on verification
* We can keep it "up front" and make it return the data needed for whatever calculation we do
  * For instance, if we verify a signup by `Credentials`, we check whether it is valid (a user exists)
    and return said user. This scales well with the `Either` datatype which we used anyway.
  * This is simple, and makes it possible to encode some guarantees about our data into their types,
    avoiding the usage of unsafe functions.
  * To summarize, we have a new layering concept now: A request gets passed to a layer verifying it's
    integrity, and optionally returning data. Awesome.
