//! This crates provides a way to create a `&String`
//! without actually allocating a new string.
//!
//! It is usually helpful as a workaround for the `&String`-taking API,
//! especially when the `String` is nested in some structure.
//!
//! # Examples
//!
//! Indexing a `(String, String)`-keyed hashmap with a `(&str, &str)` key.
//!
//! ```
//! use std::collections::HashMap;
//! use fakestring::prelude::*;
//! use fakestring::FakeString;
//!
//! fn search<'a, T>(
//!    map: &'a HashMap<(String, String), T>,
//!    (needle_left, needle_right): (&str, &str)
//! ) -> Option<&'a T> {
//!
//!     // Create a tuple with the same layout as the hashmap key
//!     let needle = (FakeString::from(needle_left), FakeString::from(needle_right));
//!
//!     // Convert from `&(FakeString, FakeString)` to `&(String, String)`
//!     map.get(needle.unfake())
//! }
//!
//! let mut m = HashMap::new();
//! m.insert(("a".to_owned(), "b".to_owned()), 5);
//! assert_eq!( search(&m, ("a", "b")), Some(&5) );
//! ```
//!
//! # Safety
//!
//! `FakeString` is implemented using `ManuallyDrop<String>`.
//!
//! This crate relies on the fact that all structures containing
//! `FakeString` will have the same layout as the ones containing `String`.
//! This is quite reasonable assumption, but:
//!
//! * A field order randomization (if ever implemented in Rust) may break it,
//! * Using "optimization fuel" to selectively reorder structs may break it.

use std::mem::ManuallyDrop;
use std::mem::transmute;
use std::marker::PhantomData;
use std::ops;
use std::fmt;

/// A "non-owned" variant of `String`
///
/// Provides a way to use an API that requires `&String`
/// (or a reference to a structure containing a `String`)
/// but without actually creating an owned string.
///
/// You can create a `FakeString` from a `&str` or `&String`
/// using the `From::from`. Conversions in the other way
/// is also provided.
///
/// To "unfake" a more complicated structure, use the
/// `Unfake::unfake` method.
///
/// See the top-level example.
#[derive(Debug)]
pub struct FakeString<'s> {
    // repr(transparent)?
    s: ManuallyDrop<String>,
    _phantom: PhantomData<&'s str>,
}

// Is there any need to Deref to String instead of str,
// and expose From(&String) constructor?

impl<'s> ops::Deref for FakeString<'s> {
    type Target = str;

    fn deref(&self) -> &str { &self.s }
}

impl<'s> From<&'s str> for FakeString<'s> {
    fn from(s: &'s str) -> FakeString<'s> {
        unsafe {
            let s = String::from_raw_parts(s.as_ptr() as *mut _, s.len(), s.len());
            FakeString { s: ManuallyDrop::new(s), _phantom: PhantomData }
        }
    }
}

// In theory &String should coerce to &str, and this impl won't be necessary.
impl<'s> From<&'s String> for FakeString<'s> {
    fn from(s: &'s String) -> FakeString<'s> { From::from(s.as_str()) }
}

impl<'x, 's> From<&'x FakeString<'s>> for &'s str {
    fn from(fake: &'x FakeString<'s>) -> &'s str {
        unsafe {
            transmute::<&str, &str>(&fake)
        }
    }
}

impl<'s> From<FakeString<'s>> for &'s str {
    fn from(fake: FakeString<'s>) -> &'s str {
        From::from(&fake)
    }
}

impl<'s> FakeString<'s> {
    pub fn as_str(&self) -> &'s str { From::from(self) }
}

impl<'s> Clone for FakeString<'s> {
    fn clone(&self) -> Self {
        Self::from(<&str>::from(self))
    }
}

impl<'s> fmt::Display for FakeString<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.s.fmt(f)
    }
}

/// TODO (?) Eq, Ord, Hash, Borrow, AsRef

#[test]
fn test_from_impls() {
    let s = "abc";
    assert!(FakeString::from(s).as_str() == "abc");
    assert!(FakeString::from(&s.to_owned()).as_str() == "abc");

    assert!(<&str>::from(FakeString::from(s)) == "abc");

    let fs = FakeString::from(s);
    assert!(fs.as_str() == fs.clone().as_str());

    assert!(fs.to_string() == "abc");
}

/// Converts all `FakeString`s to `String`s in a structure passed by reference.
///
/// This trait is implemented for `FakeString`, `String`, primitives, tuples and collections.
///
/// See the top-level example.
pub unsafe trait Unfake {
    // type Unfaked: SameSize<Self>
    type Unfaked;

    fn unfake(&self) -> &Self::Unfaked;
}

// macro_rules! unsafe_impl_unfake {
//     (impl $(< $('$region:ident),+ >)* $T:type -> $U:type) => {
//         unsafe impl $(< $('$region),+ >)* Unfake for $T {
//             type Target = $U;

mod impls {

    use *;
    use std::collections::*;

    macro_rules! unsafe_impl {
        ( $( ( $( $region:tt ),* ) < $( $X:ident ),* > )* & $T:ty => & $U:ty) => {
            unsafe impl $( < $( $region , )* $( $X : Unfake , )* > )* Unfake for $T {
                type Unfaked = $U;

                fn unfake(&self) -> &Self::Unfaked { unsafe { transmute(self) } }
            }
        }
    }

    macro_rules! unsafe_impl_tuple {
        ( $($X:ident),* ) => {
            unsafe_impl!{
                () < $($X),* > &( $($X,)* ) => &( $($X::Unfaked,)* )
            }
        };
    }

    macro_rules! unsafe_impls {
        ( $($X:ty),* ) => {
            $(
                unsafe_impl!{ () <> &$X => &$X }
            )*
        };
    }

    unsafe_impl!{ ('s) <> &FakeString<'s> => &String }

    unsafe_impl!{ ('a) <A>  & &'a A => & &'a A::Unfaked }

    unsafe_impl!{ () <A>  &Vec<A> => &Vec<A::Unfaked> }
    unsafe_impl!{ () <A>  &LinkedList<A> => &Vec<A::Unfaked> }
    unsafe_impl!{ () <A>  &BinaryHeap<A> => &BinaryHeap<A::Unfaked> }
    unsafe_impl!{ () <A>  &VecDeque<A> => &VecDeque<A::Unfaked> }
    unsafe_impl!{ () <A, S>  &HashSet<A, S> => &HashSet<A::Unfaked, S> }
    unsafe_impl!{ () <A>     &BTreeSet<A> => &HashSet<A::Unfaked> }
    unsafe_impl!{ () <K, V, S> &HashMap<K, V, S> => &HashMap<K::Unfaked, V::Unfaked, S> }
    unsafe_impl!{ () <K, V>    &BTreeMap<K, V> => &BTreeMap<K::Unfaked, V::Unfaked> }

    unsafe_impl!{ ('a) <>  & &'a str => & &'a str }

    unsafe_impls!{ (), char, u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, String }

    unsafe_impl_tuple!{ A }
    unsafe_impl_tuple!{ A, B }
    unsafe_impl_tuple!{ A, B, C }
    unsafe_impl_tuple!{ A, B, C, D }
    unsafe_impl_tuple!{ A, B, C, D, E }
    unsafe_impl_tuple!{ A, B, C, D, E, F }
    unsafe_impl_tuple!{ A, B, C, D, E, F, G }
    unsafe_impl_tuple!{ A, B, C, D, E, F, G, H }
    unsafe_impl_tuple!{ A, B, C, D, E, F, G, H, I }
    unsafe_impl_tuple!{ A, B, C, D, E, F, G, H, I, J }
    unsafe_impl_tuple!{ A, B, C, D, E, F, G, H, I, J, K }
    unsafe_impl_tuple!{ A, B, C, D, E, F, G, H, I, J, K, L }

    #[test]
    fn test_unfake_impls() {
        let buffer = "b".to_owned();
        let fake = vec![(FakeString::from("a"), &buffer, "c", 'd', 3usize)];
        let real: &Vec<(String, &String, &str, char, usize)> = fake.unfake();
        assert_eq!(real, &vec![("a".to_owned(), &"b".to_owned(), "c", 'd', 3)]);
    }
}

/// Traits usually imported when using this crate
///
/// # Usage
///
/// ```
/// pub use fakestring::prelude::*;
/// ```
pub mod prelude {
    pub use Unfake;
}

// Trying to use From instead of Unfake fails due to coherence problems:
//
// impl<T: SafeTransmute> From<T> for T::Target {
//     fn from(x: T) -> Self { unsafe { transmute(x) } }
// }
