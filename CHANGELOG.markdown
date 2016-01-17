0.4
---
* Drop the `generic-deriving` requirement (and GHC 7.0 support)
* Support `transformers` 0.5
* Support GHC 8

0.2.0.1
-------
* Wider bounds for `generic-deriving` so we can maintain compatibility with GHC HEAD.

0.2
---
* Split `Data.Hashable.Extras` into a separate `hashable-extras` package, to avoid it incurring quite so many dependencies.

0.1
---
* Added `Data.Hashable.Extras` in the style of `prelude-extras` and `syb-extras` for higher rank hashing.
* Split from [https://github.com/analytics/analytics](analytics)
