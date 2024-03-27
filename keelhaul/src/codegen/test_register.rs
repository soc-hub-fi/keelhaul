use crate::{
    codegen,
    model::{self, ArchPtr},
    TestConfig, TestKind,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use thiserror::Error;

/// Type that a test can be generated for
pub trait TestRegister<P> {
    /// The path of the register used for human readable identification of the register as part of a
    /// larger design. Might comprise components such as "peripheral, register cluster, register
    /// name".
    fn path(&self) -> Vec<String>;

    /// A human-readable unique identifier for the register, usually the the path that is used to
    /// access the register.
    fn uid(&self) -> String {
        self.path().join("_")
    }

    /// Name of the top-level element containing this register, usually the first element of the
    /// `path`
    fn periph_name(&self) -> String {
        self.path().first().unwrap().to_owned()
    }

    /// The name of the register, usually the final element of the `path`
    fn name(&self) -> String {
        self.path().last().unwrap().to_owned()
    }

    /// The address of the register
    fn addr(&self) -> P;

    /// The size of the register in bits
    fn size(&self) -> u32;

    /// Whether reading the register has a defined effect
    fn is_readable(&self) -> bool;

    /// An optional, known reset value
    fn reset_value(&self) -> Option<ValueOnReset<u64>>;
}

#[derive(Clone, Debug)]
pub struct ValueOnReset<T> {
    /// Known value on reset
    value: T,
    /// An optional reset mask, indicating which bits have the defined reset value
    mask: Option<T>,
}

impl<T> ValueOnReset<T> {
    pub(crate) fn new(value: T, mask: Option<T>) -> Self {
        Self { value, mask }
    }
}

#[derive(Error, Debug)]
#[error("cannot parse test kind from {0}")]
pub struct ParseTestKindError(pub(crate) String);

impl crate::api::TestKind {
    /// Error variant for an error enumeration, including the comma at end
    ///
    /// # Parameters
    ///
    /// - `arch_ptr_width` - The architecture pointer size.
    /// - `max_value_width` - The maximum pointee width for any register. Determines the size of the
    ///   Error variant.
    pub(crate) fn error_variant_def(&self, max_value_width: model::PtrSize) -> Option<TokenStream> {
        let max_value_width = format_ident!(
            "{}",
            codegen::bit_count_to_rust_uint_type_str(max_value_width.bit_count())
        );
        match self {
            Self::Read => None,
            Self::ReadIsResetVal => Some(
                // We try to avoid generating extra code as much as possible. Therefore we only
                // reference existing static symbols or integers here.
                quote! {
                    /// The read value was not the expected value on reset
                    ///
                    /// This means that the source file (IP-XACT or SVD) claims that the register
                    /// value on reset should be `reset_val` but it was `read_val` instead.
                    ReadValueIsNotResetValue {
                        /// The value that was read from the register
                        read_val: #max_value_width,
                        /// Expected value for this register on reset
                        reset_val: #max_value_width,
                        /// Register identifier or "full path"
                        reg_uid: &'static str,
                        /// Register address
                        reg_addr: usize,
                    },
                },
            ),
        }
    }
}

/// Generates test cases based on a [`TestRegister`] definition and [`TestConfig`]
///
/// Test cases are represented by [`TokenStream`] which can be rendered to text.
/// This text is then compiled as Rust source code.
pub(crate) struct RegTestGenerator<'r, 'c, P: ArchPtr + quote::IdentFragment + 'static>(
    &'r dyn TestRegister<P>,
    &'c TestConfig,
);

impl<'r, 'c, P: ArchPtr + quote::IdentFragment> RegTestGenerator<'r, 'c, P> {
    /// Name for the binding to the pointer to the memory mapped register
    fn ptr_binding() -> TokenStream {
        quote!(reg_ptr)
    }

    /// Name for the binding to the value that was read from the register
    ///
    /// We prefix it with underscore to avoid a warning about unused, since we
    /// may or may not want to re-use the binding depending on which tests get
    /// generated for this particular register
    fn read_value_binding() -> TokenStream {
        quote!(_read_value)
    }

    /// Create a [`RegTestGenerator`] from a register definition
    pub fn from_register(reg: &'r impl TestRegister<P>, config: &'c TestConfig) -> Self {
        Self(reg, config)
    }

    /// Generates a test that reads an appropriate amount of bytes from the
    /// register
    fn gen_read_test(&self) -> TokenStream {
        let ptr_binding = Self::ptr_binding();
        let read_value_binding = Self::read_value_binding();
        quote! {
            let #read_value_binding = unsafe { read_volatile(#ptr_binding) };
        }
    }

    /// Generates a function identifier
    ///
    /// # Examples
    ///
    /// * `test_MCR_0xfff00004`
    /// * `test_MDIO_RD_DATA_0xff40040c`
    fn gen_test_fn_ident(&self) -> Ident {
        let reg = &self.0;
        let addr: P = reg.addr();
        format_ident!("test_{}_{:#x}", reg.name(), addr)
    }

    /// Generates a test function
    ///
    /// Example output:
    ///
    /// ```rust,ignore
    /// pub fn test_something_0xdeadbeef() {
    ///     #[allow(unused)]
    ///     let reg_ptr: *mut u32 = 0xdeadbeef as *mut u32;
    ///
    ///     let _read_value = unsafe { read_volatile(reg_ptr) };
    /// }
    /// ```
    pub fn gen_test_fn(&self) -> TokenStream {
        let (reg, config) = (&self.0, self.1);

        // Name for the variable holding the pointer to the register
        let ptr_binding = Self::ptr_binding();
        let reg_size_ty = format_ident!("{}", codegen::bit_count_to_rust_uint_type_str(reg.size()));
        let addr_hex: TokenStream = format!("{:#x}", reg.addr()).parse().unwrap();

        let fn_name = self.gen_test_fn_ident();

        // Only generate read test if register is readable
        let gen_read_test = reg.is_readable() && config.tests_to_generate.contains(&TestKind::Read);
        let read_test = gen_read_test
            .then(|| self.gen_read_test())
            .unwrap_or_default();

        // Only generate reset value test if register is readable and has a reset value
        let gen_reset_test =
            reg.is_readable() && config.tests_to_generate.contains(&TestKind::ReadIsResetVal);
        let reset_val_test = gen_reset_test
            .then(|| {
                reg.reset_value()
                    .map(|value_on_reset| {
                        gen_read_is_reset_val_test(
                            reg.uid(),
                            reg.addr(),
                            reg.size(),
                            &value_on_reset,
                            config,
                        )
                    })
                    .unwrap_or_default()
            })
            .unwrap_or_default();

        let ret = quote! {
            #[allow(non_snake_case)]
            pub fn #fn_name() -> Result<()> {
                let #ptr_binding: *mut #reg_size_ty = #addr_hex as *mut #reg_size_ty;

                #read_test
                #reset_val_test
                Ok(())
            }
        };
        ret
    }

    /// Generates a test definition that can be put into an array initializer
    ///
    /// Example output:
    ///
    /// ```rust,ignore
    /// TestCase {
    ///     function: foo::test_something_0xdeafbeef,
    ///     addr: 0xdeafbeef,
    ///     uid: "test_something",
    /// }
    /// ```
    pub fn gen_test_def(&self) -> TokenStream {
        let fn_name = self.gen_test_fn_ident();
        let periph_name_lc: TokenStream = self.0.periph_name().to_lowercase().parse().unwrap();
        let func = quote!(#periph_name_lc::#fn_name);
        let addr_hex: TokenStream = format!("{:#x}", self.0.addr()).parse().unwrap();
        let uid = self.0.uid();

        quote! {
            TestCase {
                function: #func,
                addr: #addr_hex,
                uid: #uid,
            }
        }
    }
}

/// Generates a test that verifies that the read value matches with reported
/// reset value
///
/// # Arguments
///
/// * `uid` - Register UID
/// * `size` - The size of the register in bits
/// * `reset_value` - A masked reset value that will be checked against
fn gen_read_is_reset_val_test<P: ArchPtr + quote::IdentFragment + 'static>(
    uid: String,
    addr: P,
    size: u32,
    reset_value: &ValueOnReset<u64>,
    config: &TestConfig,
) -> TokenStream {
    // Reset value test requires read test to be present. Can't check for
    // reset value unless it's been read before.
    debug_assert!(config.tests_to_generate.contains(&TestKind::Read));

    let read_value_binding = RegTestGenerator::<P>::read_value_binding();
    let reset_val_frag = if config.force_ignore_reset_mask || reset_value.mask.is_none() {
        codegen::u_to_hexlit(reset_value.value, size)
    } else {
        // Unwrap: checked on conditional
        codegen::gen_bitand(reset_value.value, reset_value.mask.unwrap())
    };
    let reg_size_ty = format_ident!("{}", codegen::bit_count_to_rust_uint_type_str(size));

    match config.on_fail {
        // If reset value is incorrect, panic
        codegen::FailureImplKind::Panic => quote! {
            assert_eq!(#read_value_binding, #reset_val_frag as #reg_size_ty);
        },
        // If reset value is incorrect, do nothing
        codegen::FailureImplKind::None => quote! {},
        codegen::FailureImplKind::ReturnError => {
            let addr_hex: TokenStream = format!("{:#x}", addr).parse().unwrap();
            let max_val_width = quote!(u64);
            quote! {
                if #read_value_binding != #reset_val_frag as #reg_size_ty {
                    return Err(Error::ReadValueIsNotResetValue {
                            read_val: #read_value_binding as #max_val_width,
                            reset_val: #reset_val_frag,
                            reg_uid: #uid,
                            reg_addr: #addr_hex,
                        })
                }
            }
        }
    }
}
