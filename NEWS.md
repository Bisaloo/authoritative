# authoritative 0.2.0

## New features

* New `invert_names()` function will return a cleaned vector of names
  potentially inverted occurrences of 'Lastname Firstname' to
  'Firstname Lastname' (or the reverse) based on a list of `cleaned_names`.
  Same as for the `expand_names()` function, the `invert_names(x, x)` can be
  used to deduplicate names from a vector without an external source of truth
  (@Bisaloo, #25).


# authoritative 0.1.0

* This project now includes a
   [`NEWS.md`](https://r-pkgs.org/other-markdown.html#sec-news) file to inform
   users about changes and new features.
