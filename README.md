# Morgue server
A JSON-RPC server bringing the functionality of [morgue](https://github.com/ibabushkin/morgue)
to the web. Offers all library functions and a file management layer.

## Features
* Users can save their own, private files, as well as share them amongst groups of users
* Files can be deleted, uploaded, downloaded, listed and patched
* Simple, functional approach
* Clean, logical API
* Well-tested, carefully designed code striving to make extending and editing easy and reliable
* Based on [Happstack](http://www.happstack.com/page/view-page-slug/1/happstack)
  and [acid-state](http://hackage.haskell.org/package/acid-state),
  intended to be put behind [nginx](https://www.nginx.com/)

## API docs
See [this file](https://github.com/ibabushkin/morgue-server/blob/master/docs/api.md).

## Installation
As of now, I recommend this workflow (using
[stack](http://docs.haskellstack.org/en/stable/README.html)):
```sh
$ mkdir morgue && cd morgue
$ git clone https://github.com/ibabushkin/morgue
$ git clone https://github.com/ibabushkin/morgue-server
$ cd morgue-server
$ stack build  # this might take a while
$ stack install
```
Then, you should have a working binary (consult the stack docs for details and tweaks).
If you have issues, please file an issue on GitHub.

To run tests,
```sh
$ cd morgue-server
$ stack test
```
