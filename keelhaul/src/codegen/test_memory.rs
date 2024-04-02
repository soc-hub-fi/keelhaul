use std::ops;

use crate::{bit_count_to_rust_uint_type_str, codegen, FailureImplKind};
use indoc::formatdoc;
use itertools::Itertools;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

// TODO: generate also test arrays (see codegen)

#[derive(Clone, Debug)]
pub enum MemTestStrategy {
    All,
    BoundariesOnly,
}

/// # Arguments
///
/// * `range`- Where does the memory under test start and end
/// * `address_unit_bits` - Number of data bits uniquely selected by each address. Most devices have
///   `address_unit_bits = 8`, i.e., they are byte-addressable.
/// * `strategy` - What strategy is employed to test the memory
/// * `on_fail` - What to do when the test case fails
pub fn gen_memtest_module(
    test_ranges: &[ops::Range<u64>],
    address_unit_bits: u32,
    strategy: &MemTestStrategy,
    on_fail: &FailureImplKind,
) -> TokenStream {
    let preamble = gen_preamble(true);
    let test_cases = test_ranges
        .iter()
        .map(|range| gen_memtest(range.start, range.end, address_unit_bits, strategy, on_fail))
        .collect_vec();

    quote! {
        #preamble

        #(
            #test_cases
        )*
    }
}

fn gen_preamble(use_ptr: bool) -> TokenStream {
    let mod_doc = codegen::gen_mod_desc();

    let imports = if use_ptr {
        quote! {
            use core::{ptr, slice};
        }
    } else {
        quote!()
    };
    quote! {
        // Module documentation is provided via `_DOC` symbol. Do not use inner attributes in
        // generated code: https://github.com/rust-lang/rfcs/issues/752
        #[doc = #mod_doc]
        const _DOC: () = ();

        #imports
    }
}

/// Generate a memory test for the given memory range
///
/// Notably, the generator does not need to know the target architecture, as the compiler will take
/// care of warning us if an address or a value cannot fit the target pointer.
///
/// # Arguments
///
/// * `start`- Where does the memory under test start (inclusive)
/// * `end` - Where does the memory end (exclusive)
/// * `address_unit_bits` - Number of data bits uniquely selected by each address. Most devices have
///   `address_unit_bits = 8`, i.e., they are byte-addressable.
/// * `strategy` - What strategy is employed to test the memory
fn gen_memtest(
    start: u64,
    end: u64,
    address_unit_bits: u32,
    strategy: &MemTestStrategy,
    on_fail: &FailureImplKind,
) -> TokenStream {
    let len = end - start;

    // Generate required tokens
    let fn_name = gen_test_fn_ident(start, start + len);
    let start: TokenStream = format!("{:#x}", start).parse().unwrap();
    let end: TokenStream = format!("{:#x}", end).parse().unwrap();
    let len: TokenStream = format!("{:#x}", len).parse().unwrap();
    // Type of element in a mem cell
    let ty: TokenStream = bit_count_to_rust_uint_type_str(address_unit_bits)
        .parse()
        .unwrap();
    let fail_count = fail_count_binding();

    let frags = match strategy {
        MemTestStrategy::All => gen_test_all(&start, &end, &len, &ty, &fail_count),
        MemTestStrategy::BoundariesOnly => {
            gen_test_boundaries(&start, &end, &len, &ty, &fail_count)
        }
    };
    let doc_lines = frags.doc_lines();
    let test_frag = frags.test_frag;
    let fail_frag = gen_fail_frag(on_fail);

    quote! {
        #( #[doc = #doc_lines] )*
        pub fn #fn_name() {
            #test_frag

            #fail_frag
        }
    }
}

/// Memory test components
struct MemTestFrags {
    doc_str: String,
    test_frag: TokenStream,
}

impl MemTestFrags {
    fn doc_lines(&self) -> Vec<String> {
        self.doc_str.lines().map(ToOwned::to_owned).collect()
    }
}

fn gen_test_all(
    start: &TokenStream,
    end: &TokenStream,
    len: &TokenStream,
    ty: &TokenStream,
    fail_count: &TokenStream,
) -> MemTestFrags {
    let doc_str = formatdoc!(
        r#"
        Memory test for range {start}..{end}

        Tests that all memory cells in range:

        * start: {start} (inclusive),
        * end:   {end} (exclusive),
        * len:   {len},

        are able to retain a value.
        "#
    );

    let test_frag = quote! {
        let mem_slice = unsafe { slice::from_raw_parts_mut(#start as *mut #ty, #len) };

        // Write integers from 0..len
        for (idx, cell) in mem_slice.iter_mut().enumerate() {
            unsafe { ptr::write_volatile(cell as *mut #ty, idx as #ty % #ty::MAX) };
        }

        // Read back integers from 0..len, counting mismatches
        let #fail_count =
            mem_slice.iter().enumerate().filter(|&(idx, cell)| {
                let val = unsafe { ptr::read_volatile(cell) };
                val != idx as #ty & #ty::MAX
            })
            .count();
    };
    MemTestFrags { doc_str, test_frag }
}

fn gen_test_boundaries(
    start: &TokenStream,
    end: &TokenStream,
    len: &TokenStream,
    ty: &TokenStream,
    fail_count: &TokenStream,
) -> MemTestFrags {
    let doc_str = formatdoc!(
        r#"
        Memory boundary test for range {start}..{end}

        Tests that the start and end address for memory between:

        * start: {start} (inclusive),
        * end:   {end} (exclusive),
        * len:   {len},

        are able to retain a value.
        "#
    );

    let test_frag = quote! {
        let first = #start;
        let last = #start + #len - 1;

        ptr::write_volatile(first as *mut #ty, 42);
        ptr::write_volatile(last as *mut #ty, 43);

        let mut #fail_count = 0;
        if ptr::read_volatile(first as *mut #ty) != 42 {
            #fail_count += 1;
        }
        if ptr::read_volatile(last as *mut #ty) != 43 {
            #fail_count += 1;
        }
    };
    MemTestFrags { doc_str, test_frag }
}

fn gen_test_fn_ident(start: u64, end: u64) -> Ident {
    format_ident!("memtest_from_{start:#x}_to_{end:#x}")
}

fn fail_count_binding() -> TokenStream {
    quote!(_fail_count)
}

fn gen_fail_frag(on_fail: &FailureImplKind) -> TokenStream {
    let fail_count = fail_count_binding();
    match on_fail {
        FailureImplKind::None => {
            quote!()
        }
        FailureImplKind::Panic => quote! {
            if #fail_count > 0 {
                panic!("{}", #fail_count);
            }
        },
        FailureImplKind::ReturnError => todo!(),
    }
}
