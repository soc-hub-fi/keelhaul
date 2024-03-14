//! Set panicking behavior to print into UART

use crate::{sprintln, ufmt::uDisplay};
use core::panic::PanicInfo;

pub(crate) struct PanicInfoWrapper<'a>(pub(crate) &'a PanicInfo<'a>);

impl uDisplay for PanicInfoWrapper<'_> {
    fn fmt<W>(&self, f: &mut crate::ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: crate::ufmt::uWrite + ?Sized,
    {
        if let Some(msg) = self.0.payload().downcast_ref::<&str>() {
            f.write_str(&msg)
        } else {
            f.write_str("panic occurred")
        }
    }
}

#[cfg(feature = "panic-uart")]
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    sprintln!("{}", PanicInfoWrapper(info));
    loop {}
}
