use crate::bindings::{
    hs_exit, hs_init, lake_generator_free, lake_generator_has_next, lake_generator_new,
    lake_generator_next, lake_generator_value, HsGenerator,
};
use std::{ffi::CStr, ptr::null_mut};

pub struct Generator {
    raw: HsGenerator,
}

impl Generator {
    pub fn new() -> Generator {
        HaskellRuntime::init();
        Generator {
            raw: unsafe { lake_generator_new() },
        }
    }
    pub fn has_next(&self) -> bool {
        let value = unsafe { lake_generator_has_next(self.raw) };
        value > 0
    }
}

impl Drop for Generator {
    fn drop(&mut self) {
        unsafe { lake_generator_free(self.raw) };
    }
}

impl Iterator for Generator {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.has_next() {
            return None;
        } else {
            self.raw = unsafe { lake_generator_next(self.raw) };
            let value = unsafe { lake_generator_value(self.raw) };
            if value.is_null() {
                return None;
            } else {
                let value = unsafe { CStr::from_ptr(value) };
                Some(String::from(value.to_str().unwrap()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Generator;

    #[test]
    fn it_works() {
        let generator = Generator::new();
        for value in Iterator::take(generator, 10) {
            println!("{}", value)
        }
    }
}

// Haskell RTS

static mut HASKELL_RUNTIME: Option<HaskellRuntime> = None;

struct HaskellRuntime {}

impl HaskellRuntime {
    fn init() {
        if unsafe { HASKELL_RUNTIME.is_some() } {
            return;
        }
        unsafe { hs_init(null_mut(), null_mut()) };
        unsafe { HASKELL_RUNTIME.insert(HaskellRuntime {}) };
    }
}

impl Drop for HaskellRuntime {
    fn drop(&mut self) {
        unsafe { hs_exit() };
    }
}
