use std::{
    any::TypeId,
    cmp::Ordering,
    fmt::Debug,
    sync::{atomic::AtomicUsize, Arc},
};

use crate::{
    objects::{TryIntoValue, ValueType},
    ExecutionError, ResolveResult, Value,
};

#[allow(unused_variables)]
pub trait AsValue: Send + Sync {
    #[inline]
    fn value_field(&self, name: &str) -> ResolveResult {
        Err(ExecutionError::NoSuchKey(Arc::new(
            name.to_string(),
        )))
    }

    #[inline]
    fn value_index(&self, index: &Value) -> ResolveResult {
        Err(ExecutionError::UnsupportedIndexExternal(
            index.clone(),
            std::any::type_name::<Self>(),
        ))
    }

    #[inline]
    fn to_value(&self, hint: ValueType) -> Option<Value> {
        None
    }

    /// Called when other [`Value`] is added to this type.
    #[inline]
    fn value_add(&self, second: &Value) -> ResolveResult {
        Err(ExecutionError::UnsupportedBinaryOperatorExternal(
            "+",
            std::any::type_name::<Self>(),
            "Value",
        ))
    }

    /// Called when this type is added to other [`Value`].
    ///
    /// Override if addition is non-commutative.
    #[inline]
    fn value_add_other(&self, first: &Value) -> ResolveResult {
        AsValue::value_add(self, first).map_err(|err| match err {
            ExecutionError::UnsupportedBinaryOperatorExternal(op, a, b) => {
                ExecutionError::UnsupportedBinaryOperatorExternal(op, b, a)
            }
            other => other,
        })
    }

    /// Called when other [`Value`] is subtracted from this type.
    #[inline]
    fn value_sub(&self, second: &Value) -> ResolveResult {
        Err(ExecutionError::UnsupportedBinaryOperatorExternal(
            "-",
            std::any::type_name::<Self>(),
            "Value",
        ))
    }

    /// Called when this type is subtracted from other [`Value`].
    ///
    /// Override if subtraction is non-commutative.
    #[inline]
    fn value_sub_other(&self, first: &Value) -> ResolveResult {
        AsValue::value_sub(self, first).map_err(|err| match err {
            ExecutionError::UnsupportedBinaryOperatorExternal(op, a, b) => {
                ExecutionError::UnsupportedBinaryOperatorExternal(op, b, a)
            }
            other => other,
        })
    }

    /// Called when this type is multiplied with other [`Value`].
    #[inline]
    fn value_mul(&self, second: &Value) -> ResolveResult {
        Err(ExecutionError::UnsupportedBinaryOperatorExternal(
            "*",
            std::any::type_name::<Self>(),
            "Value",
        ))
    }

    /// Called when other [`Value`] is multiplied with this type.
    ///
    /// Override if multiplication is non-commutative.
    #[inline]
    fn value_mul_other(&self, first: &Value) -> ResolveResult {
        AsValue::value_mul(self, first).map_err(|err| match err {
            ExecutionError::UnsupportedBinaryOperatorExternal(op, a, b) => {
                ExecutionError::UnsupportedBinaryOperatorExternal(op, b, a)
            }
            other => other,
        })
    }

    /// Called when other [`Value`] is divided by this type.
    #[inline]
    fn value_div(&self, second: &Value) -> ResolveResult {
        Err(ExecutionError::UnsupportedBinaryOperatorExternal(
            "/",
            std::any::type_name::<Self>(),
            "Value",
        ))
    }

    /// Called when this type divided by other [`Value`].
    #[inline]
    fn value_div_other(&self, first: &Value) -> ResolveResult {
        Err(ExecutionError::UnsupportedBinaryOperatorExternal(
            "/",
            "Value",
            std::any::type_name::<Self>(),
        ))
    }

    /// Called when modulo of other [`Value`] is applied to this type.
    #[inline]
    fn value_rem(&self, second: &Value) -> ResolveResult {
        Err(ExecutionError::UnsupportedBinaryOperatorExternal(
            "%",
            std::any::type_name::<Self>(),
            "Value",
        ))
    }

    /// Called when modulo of this type is applied to other [`Value`].
    #[inline]
    fn value_rem_other(&self, first: &Value) -> ResolveResult {
        Err(ExecutionError::UnsupportedBinaryOperatorExternal(
            "%",
            "Value",
            std::any::type_name::<Self>(),
        ))
    }

    /// Called when this type is compared to other [`Value`].
    #[inline]
    fn value_cmp(&self, second: &Value) -> Result<Ordering, ExecutionError> {
        Err(ExecutionError::UnsupportedBinaryOperatorExternal(
            "cmp",
            std::any::type_name::<Self>(),
            "Value",
        ))
    }

    /// Called when this type is checked for equality to other [`Value`].
    #[inline]
    fn value_eq(&self, second: &Value) -> bool {
        false
    }
}

impl<V: AsValue + 'static> TryIntoValue for V {
    type Error = Infalliable;

    fn try_into_value(self) -> Result<Value, Self::Error> {
        unsafe { Ok(Value::External(ExternalValue::new(self))) }
    }
}

pub struct ExternalValue {
    r#type: TypeId,
    data: *mut (),
    drop_fn: DropFn,
    use_counter: *mut AtomicUsize,
    external: Option<*const ()>,
}

impl ExternalValue {
    /// # Safety
    ///
    /// Type `T` must be kept alive for as long as returned `ExternalValue`
    /// exists.
    pub unsafe fn new_opaque<T>(value: T) -> Self
    where
        T: Sized + Send + Sync + 'static,
    {
        Self {
            r#type: TypeId::of::<T>(),
            data: Box::leak(Box::new(value)) as *mut T as *mut (),
            drop_fn: drop_fn::<T>(),
            use_counter: Box::leak(Box::new(AtomicUsize::new(1))) as *mut AtomicUsize,
            external: None,
        }
    }

    /// # Safety
    ///
    /// Type `T` must be kept alive for as long as returned `ExternalValue`
    /// exists.
    pub unsafe fn new<T>(value: T) -> Self
    where
        T: AsValue + Sized + Send + Sync + 'static,
    {
        let vtable = unsafe {
            let value = &value as &dyn AsValue;
            let (_, vtable): (*const (), *const ()) = std::mem::transmute(value);
            vtable
        };

        Self {
            r#type: TypeId::of::<T>(),
            data: Box::leak(Box::new(value)) as *mut T as *mut (),
            drop_fn: drop_fn::<T>(),
            use_counter: Box::leak(Box::new(AtomicUsize::new(1))) as *mut AtomicUsize,
            external: Some(vtable),
        }
    }

    pub fn as_ref<T>(&self) -> Option<&T>
    where
        T: 'static + Sized + Send + Sync,
    {
        if self.r#type != TypeId::of::<T>() {
            return None;
        }
        let data = self.data as *const T;
        Some(unsafe { data.as_ref().unwrap() })
    }

    pub fn as_value(&self) -> Option<&dyn AsValue> {
        let vtable = self.external?;
        Some(unsafe { std::mem::transmute((self.data, vtable)) })
    }

    /// # Safety
    /// 
    /// Calling this function is unsafe if `ExternalValue` was constructed from
    /// opaque type.
    pub unsafe fn as_value_unchecked(&self) -> &dyn AsValue {
        let vtable = unsafe {
            // SAFETY: This operation
            self.external.unwrap_unchecked()
        };
        unsafe { std::mem::transmute((self.data, vtable)) }
    }

    pub fn is_opaque(&self) -> bool {
        !self.external.is_some()
    }
}

impl Clone for ExternalValue {
    fn clone(&self) -> Self {
        unsafe {
            let uses = match self.use_counter.as_mut() {
                Some(it) => it,
                None => unreachable!("use_counter is null"),
            };
            uses.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        }

        Self {
            r#type: self.r#type,
            data: self.data,
            drop_fn: self.drop_fn,
            use_counter: self.use_counter,
            external: self.external,
        }
    }
}

impl Debug for ExternalValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Requires specialization#31844 to show useful information
        f.write_str("ExternalValue")
    }
}

impl Drop for ExternalValue {
    fn drop(&mut self) {
        unsafe {
            let uses = match self.use_counter.as_mut() {
                Some(it) => it,
                None => unreachable!("use_counter is null"),
            };
            let use_count = uses.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
            if use_count == 0 {
                (self.drop_fn)(self.data);
                drop(Box::from_raw(self.use_counter));
            }
        }
    }
}

type DropFn = fn(*mut ());
const fn drop_fn<T>() -> DropFn {
    if core::mem::needs_drop::<T>() {
        |ptr: *mut ()| unsafe { core::ptr::drop_in_place(ptr as *mut T) }
    } else {
        |_: *mut ()| {}
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Infalliable {}
