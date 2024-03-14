use crate::uart::uart_write;

pub struct Uart;

#[macro_export]
macro_rules! sprint {
    ($s:expr) => {{
        use $crate::{sprintln, ufmt};
        ufmt::uwrite!(sprintln::Uart {}, $s).unwrap()
    }};
    ($($tt:tt)*) => {{
        use $crate::{sprintln, ufmt};
        ufmt::uwrite!(sprintln::Uart, $($tt)*).unwrap()
    }};
}

#[macro_export]
macro_rules! sprintln {
    () => {{
        use $crate::sprint;
        sprint!("\r\n");
    }};
    // IMPORTANT use `tt` fragments instead of `expr` fragments (i.e. `$($exprs:expr),*`)
    ($($tt:tt)*) => {{
        use $crate::{sprint, sprintln};
        sprint!($($tt)*);
        sprint!("\r\n");
    }};
}

impl ufmt::uWrite for Uart {
    type Error = core::convert::Infallible;

    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        uart_write(s);
        Ok(())
    }
}
