use log::{Level, LevelFilter, Metadata, Record};

pub struct SimpleLogger;

static LOGGER: SimpleLogger = SimpleLogger;

pub fn init(max_level: LevelFilter) {
    log::set_logger(&LOGGER)
        .map(|()| log::set_max_level(max_level))
        .unwrap();
}

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("cargo:warning=[{}] {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}
