use std::{
    borrow::Borrow,
    collections::HashSet,
    fmt::Display,
    ops::Deref,
    sync::{Arc, Mutex},
};

use once_cell::sync::Lazy;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct InternedStr(Arc<str>);
impl Display for InternedStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_ref())
    }
}

impl Deref for InternedStr {
    type Target = Arc<str>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Borrow<str> for InternedStr {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Clone for InternedStr {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl From<&str> for InternedStr {
    fn from(value: &str) -> Self {
        if let Ok(mut lock) = INTERNEDSTR.lock() {
            if let Some(s) = lock.get(value) {
                InternedStr(s.clone())
            } else {
                let val: Arc<str> = value.into();
                lock.insert(val.clone());
                InternedStr(val)
            }
        } else {
            InternedStr(value.into())
        }
    }
}

static INTERNEDSTR: Mutex<Lazy<HashSet<Arc<str>>>> = Mutex::new(Lazy::new(HashSet::default));
