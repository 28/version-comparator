# version-comparator

Current version: 0.1.0-SNAPSHOT

A Clojure library that compares version strings.

Currently it works only for number versions - versions without strings
like "final", "alpha" etc.

## Usage

```clojure
user=> (require '[version-comparator.core :as c])
nil

user=> (c/compare-versions "1.1" "1.0")
1

user=> (c/compare-versions "1.1" "1.1")
0

user=> (c/compare-versions "1.0" "1.1")
-1

```

## License

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

Copyright © Dejan Josifović 2016.
