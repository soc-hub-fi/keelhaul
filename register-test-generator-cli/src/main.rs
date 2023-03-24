use clap::{ArgAction, ArgGroup, Parser, ValueEnum};
use log::LevelFilter;
use simplelog::{Config, SimpleLogger};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
// One type of input is always required. IP-XACT or SVD.
#[command(group(ArgGroup::new("input").required(true)))]
struct Cli {
    /// Input file, SVD or IP-XACT format
    ///
    /// File extension is used to determine input type. Use `--input_svd` or
    /// `--input_ipxact` to specify input format explicitly.
    #[arg(short, long, group = "input")]
    input_file: Option<String>,

    /// Input SVD file
    #[arg(long = "svd", group = "input")]
    input_svd: Option<String>,

    /// Input IP-XACT file
    #[arg(long = "ipxact", group = "input")]
    input_ipxact: Option<String>,

    /// Output lib.rs with test cases will be placed at <OUT_DIR>/lib.rs
    #[arg(short, long, required = true)]
    out_dir: Option<String>,

    /// Generate a <OUT_DIR>/src/lib.rs instead of <OUT_DIR>/lib.rs and also
    /// generate a Cargo.toml manifest file at <OUT_DIR>/Cargo.toml
    #[arg(short = 'p', long, default_value_t = false)]
    gen_package: bool,

    /// Include symbols by regex
    ///
    /// E.g., `--include-syms=clint*`
    #[arg(long)]
    include_syms: Option<String>,

    /// Exclude symbols by regex
    ///
    /// E.g., `--exclude-syms=clint*`
    #[arg(long)]
    exclude_syms: Option<String>,

    /// Include peripherals, comma separated, no space
    ///
    /// E.g., `--include-periphs=clint,plic`
    #[arg(long)]
    include_periphs: Option<String>,

    /// Exclude peripherals, comma separated, no space
    ///
    /// E.g., `--exclude-periphs=clint,plic`
    #[arg(long)]
    exclude_periphs: Option<String>,

    /// Types of tests to generate
    ///
    /// E.g., `-t=read -t=reset`
    #[arg(short, long, value_enum, action = ArgAction::Append)]
    test_kind: Vec<TestKind>,

    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
}

// TODO: use internal test kind type instead
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum TestKind {
    Read,
    Reset,
}

fn main() {
    let cli = Cli::parse();

    // Set log level based on number of -v flags
    let log_level = match cli.verbose {
        0 => LevelFilter::Info,
        1 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    };

    SimpleLogger::init(log_level, Config::default()).unwrap();

    // Continued program logic goes here...
}
