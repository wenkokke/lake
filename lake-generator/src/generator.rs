use crate::ast::AST;
use crate::bindings;
use serde_json::de;
use std::{ffi::CStr, ptr::null_mut};

pub struct Generator {
    generator_ptr: bindings::Generator,
}

impl Generator {
    pub fn new() -> Generator {
        HaskellRuntime::init();
        Generator {
            generator_ptr: unsafe { bindings::lake_generator_new() },
        }
    }
    pub fn has_next(&self) -> bool {
        let value = unsafe { bindings::lake_generator_has_next(self.generator_ptr) };
        value > 0
    }
}

impl Drop for Generator {
    fn drop(&mut self) {
        unsafe { bindings::lake_generator_free(self.generator_ptr) };
    }
}

impl Iterator for Generator {
    type Item = AST;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.has_next() {
            return None;
        } else {
            self.generator_ptr = unsafe { bindings::lake_generator_next(self.generator_ptr) };
            let value_ptr: bindings::Value =
                unsafe { bindings::lake_generator_value(self.generator_ptr) };
            if value_ptr.is_null() {
                return None;
            } else {
                let value: AST =
                    de::from_slice(unsafe { CStr::from_ptr(value_ptr) }.to_bytes()).unwrap();
                unsafe { bindings::lake_generator_value_free(value_ptr) };
                Some(value)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Generator;

    #[test]
    fn it_works() {
        for value in Generator::new().take(100) {
            println!("{:?}", value)
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
        unsafe { bindings::hs_init(null_mut(), null_mut()) };
        unsafe { HASKELL_RUNTIME.insert(HaskellRuntime {}) };
    }
}

impl Drop for HaskellRuntime {
    fn drop(&mut self) {
        unsafe { bindings::hs_exit() };
    }
}
