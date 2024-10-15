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
        Err(ExecutionError::NoSuchKey(Arc::new(name.to_string())))
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

/// External value is a wrapper for external types that aren't directly
/// supported by supported CEL primitives.
#[derive(Clone)]
pub struct ExternalValue {
    inner: Arc<Data>,
}

struct Data {
    r#type: TypeId,
    name: &'static str,
    data: *mut (),
    drop_fn: DropFn,
    as_value_vtable: Option<*const ()>,
}

impl ExternalValue {
    /// Constructs a new opaque `ExternalValue`.
    /// 
    /// Opaque external values act like [_abstract types_] as documented in
    /// specification.
    /// 
    /// # Safety
    ///
    /// Type `T` must be kept alive for as long as returned `ExternalValue`
    /// exists.
    /// 
    /// [_abstract types_]:
    ///     https://github.com/google/cel-spec/blob/master/doc/langdef.md#abstract-types
    pub unsafe fn new_opaque<T>(value: T) -> Self
    where
        T: Sized + Send + Sync + 'static,
    {
        Self {
            inner: Arc::new(Data {
                r#type: TypeId::of::<T>(),
                name: std::any::type_name::<T>(),
                data: Box::leak(Box::new(value)) as *mut T as *mut (),
                drop_fn: drop_fn::<T>(),
                as_value_vtable: None,
            }),
        }
    }

    /// Constructs a new non-opaque `ExternalValue`.
    /// 
    /// Non-opaque external values behave like type safe
    /// [`Map`][crate::objects::Map]s that allow operator specialization and
    /// function implementations to check their underlying types.
    /// 
    /// # Safety
    ///
    /// Type `T` must be kept alive for as long as returned `ExternalValue`
    /// exists.
    pub unsafe fn new<T>(value: T) -> Self
    where
        T: AsValue + Sized + Send + Sync + 'static,
    {
        let as_value_vtable = unsafe {
            let value = &value as &dyn AsValue;
            let (_, vtable): (*const (), *const ()) = std::mem::transmute(value);
            Some(vtable)
        };

        Self {
            inner: Arc::new(Data {
                r#type: TypeId::of::<T>(),
                name: std::any::type_name::<T>(),
                data: Box::leak(Box::new(value)) as *mut T as *mut (),
                drop_fn: drop_fn::<T>(),
                as_value_vtable,
            }),
        }
    }

    /// Returns a reference to contained value if the contained type is `T`.
    pub fn as_ref<T>(&self) -> Option<&T>
    where
        T: 'static + Sized + Send + Sync,
    {
        if self.inner.r#type != TypeId::of::<T>() {
            return None;
        }
        let data = self.inner.data as *const T;
        Some(unsafe { data.as_ref().unwrap() })
    }

    /// Returns a reference to contained value if the contained type is `T`, or
    /// returns an [`UnexpectedType`][ExecutionError::UnexpectedType] error.
    pub fn as_ref_or_error<T>(&self) -> Result<&T, ExecutionError>
    where
        T: 'static + Sized + Send + Sync,
    {
        match self.as_ref() {
            Some(it) => Ok(it),
            None => Err(ExecutionError::UnexpectedType {
                got: format!("external({})", self.inner.name),
                want: format!("external({})", std::any::type_name::<T>()),
            }),
        }
    }

    /// Returns `true` if contained type is opaque, i.e. doesn't implement
    /// [`AsValue`].
    pub fn is_opaque(&self) -> bool {
        !self.inner.as_value_vtable.is_some()
    }

    /// Returns [`&dyn AsValue`][AsValue] reference to contained value if it's
    /// not opaque.
    pub fn as_value(&self) -> Option<&dyn AsValue> {
        let vtable = self.inner.as_value_vtable?;
        Some(unsafe { std::mem::transmute((self.inner.data, vtable)) })
    }

    /// Returns [`&dyn AsValue`][AsValue] reference to contained value, or
    /// panics if it's opaque.
    pub fn as_value_expect(&self) -> &dyn AsValue {
        self.as_value().expect("expected external value to be non non-opaque")
    }

    /// Returns [`&dyn AsValue`][AsValue] reference to contained value, assuming
    /// it's not opaque.
    /// 
    /// # Safety
    ///
    /// Calling this function is unsafe if `ExternalValue` was constructed from
    /// opaque type.
    pub unsafe fn as_value_unchecked(&self) -> &dyn AsValue {
        let vtable = unsafe {
            // SAFETY: This operation is unsafe which is reflected in function
            // signature.
            self.inner.as_value_vtable.unwrap_unchecked()
        };
        unsafe { std::mem::transmute((self.inner.data, vtable)) }
    }
}

impl Debug for ExternalValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Requires specialization#31844 to show useful information
        f.write_str("ExternalValue")
    }
}

impl Drop for Data {
    fn drop(&mut self) {
        (self.drop_fn)(self.data);
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
