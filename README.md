This crates provides a way to create a `&String`
without actually allocating a new string.

It is usually helpful as a workaround for the `&String`-taking API,
especially when the `String` is nested in some structure.

# Examples

Indexing a `(String, String)`-keyed hashmap with a `(&str, &str)` key.

```rust
use std::collections::HashMap;
use fakestring::prelude::*;
use fakestring::FakeString;

fn search<'a, T>(
   map: &'a HashMap<(String, String), T>,
   (needle_left, needle_right): (&str, &str)
) -> Option<&'a T> {

    // Create a tuple with the same layout as the hashmap key
    let needle = (FakeString::from(needle_left), FakeString::from(needle_right));

    // Convert from `&(FakeString, FakeString)` to `&(String, String)`
    map.get(needle.unfake())
}

let mut m = HashMap::new();
m.insert(("a".to_owned(), "b".to_owned()), 5);
assert_eq!( search(&m, ("a", "b")), Some(&5) );
```

# Installation

Cargo.toml:

```toml
[dependencies]
fakestring = { git = "https://github.com/krdln/fakestring" }
```
