use std::{env, iter, ops, path, str::FromStr};

use anyhow::{anyhow, Context};
use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Parser)]
#[command(version, about, long_about = None, author = clap::crate_authors!(), subcommand_required = true)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,

    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
}

#[derive(Args, Clone)]
#[group(id = "input", required = true)]
struct InputFiles {
    /// CMSIS-SVD source file for memory map metadata
    #[arg(long, requires = "arch", action = clap::ArgAction::Append)]
    svd: Vec<String>,

    /// A directory of IEEE 1685 source files for memory map metadata
    #[arg(long, action = clap::ArgAction::Append)]
    ipxact_dir: Vec<String>,
}

#[derive(Subcommand)]
enum Command {
    /// Run the Keelhaul parser without doing anything
    DryRun {
        #[command(flatten)]
        input: InputFiles,

        /// Number of bits used to represent addresses on the target CPUs architecture
        #[arg(long, value_enum)]
        arch: Option<ArchWidth>,

        #[arg(long = "validate", default_value = ValidateLevel(keelhaul::ValidateLevel::Disabled), requires = "svd")]
        validate_level: ValidateLevel,

        /// Use the original, custom SVD parser instead of the new `svd-parser` based parser
        #[arg(long, action = clap::ArgAction::SetTrue, requires = "svd")]
        use_legacy: bool,
    },
    /// List all top level items (peripherals or subsystems) in the supplied sources
    ///
    /// Peripherals or subsystems containing zero registers are omitted.
    LsTop {
        #[command(flatten)]
        input: InputFiles,

        /// Number of bits used to represent addresses on the target CPUs architecture
        #[arg(long, value_enum)]
        arch: Option<ArchWidth>,

        #[arg(long = "validate", default_value = ValidateLevel(keelhaul::ValidateLevel::Disabled), requires = "svd")]
        validate_level: ValidateLevel,

        /// Use the original, custom SVD parser instead of the new `svd-parser` based parser
        #[arg(long, action = clap::ArgAction::SetTrue, requires = "svd")]
        use_legacy: bool,

        /// Only list peripherals without register counts
        #[arg(long, action = clap::ArgAction::SetTrue)]
        no_count: bool,

        /// Remove the first-line rubric
        #[arg(long, action = clap::ArgAction::SetTrue)]
        no_rubric: bool,

        #[arg(long, default_value = "alpha")]
        sorting: Sorting,
    },
    /// Count the number of registers in the inputs
    CountRegisters {
        #[command(flatten)]
        input: InputFiles,

        /// Number of bits used to represent addresses on the target CPUs architecture
        #[arg(long, value_enum)]
        arch: Option<ArchWidth>,

        #[arg(long = "validate", default_value = ValidateLevel(keelhaul::ValidateLevel::Disabled), requires = "svd")]
        validate_level: ValidateLevel,

        /// Use the original, custom SVD parser instead of the new `svd-parser` based parser
        #[arg(long, action = clap::ArgAction::SetTrue, requires = "svd")]
        use_legacy: bool,
    },
    /// Count the number of readable registers with a known reset value in the inputs
    CountResetValues {
        #[command(flatten)]
        input: InputFiles,

        /// Number of bits used to represent addresses on the target CPUs architecture
        #[arg(long, value_enum)]
        arch: Option<ArchWidth>,

        #[arg(long = "validate", default_value = ValidateLevel(keelhaul::ValidateLevel::Disabled), requires = "svd")]
        validate_level: ValidateLevel,

        /// Use the original, custom SVD parser instead of the new `svd-parser` based parser
        #[arg(long, action = clap::ArgAction::SetTrue, requires = "svd")]
        use_legacy: bool,
    },
    /// Generate metadata tests
    #[command(name = "gen-regtest")]
    GenRegTest {
        #[command(flatten)]
        input: InputFiles,

        /// Number of bits used to represent addresses on the target CPUs architecture
        #[arg(long, value_enum)]
        arch: Option<ArchWidth>,

        #[arg(long = "validate", default_value = ValidateLevel(keelhaul::ValidateLevel::Disabled), requires = "svd")]
        validate_level: ValidateLevel,

        /// Use the original, custom SVD parser instead of the new `svd-parser` based parser
        #[arg(long, action = clap::ArgAction::SetTrue, requires = "svd")]
        use_legacy: bool,

        /// Type of test to be generated. Chain multiple for more kinds.
        #[arg(short = 't', long = "test", required = true, action = clap::ArgAction::Append)]
        tests_to_generate: Vec<TestKind>,

        /// What to do when a test fails
        #[arg(long)]
        on_fail: Option<FailureImplKind>,

        /// Derive debug on possible errors
        ///
        /// May improve output for failing tests but also increases binary size.
        #[arg(long, action = clap::ArgAction::SetTrue)]
        derive_debug: bool,

        /// Ignore the reset mask field when evaluating reset values for correctness
        ///
        /// Can be useful when the reset masks are misconfigured and it's good enough to just check
        /// that the register values match with reset values on reset.
        #[arg(long, action = clap::ArgAction::SetTrue)]
        ignore_reset_masks: bool,

        #[arg(long, action = clap::ArgAction::SetTrue)]
        no_format: bool,

        /// Use zero as the assumed default reset value when a reset value is not provided
        #[arg(long = "reset-defaults-zero", action = clap::ArgAction::SetTrue)]
        use_zero_as_default_reset: bool,

        /// Filter top elements using a regex
        #[arg(long = "filter-top")]
        filter_top_regex: Option<String>,

        /// Filter register paths using a regex. Dash (`-`) is the separator between path elements.
        #[arg(long = "filter-path")]
        filter_path_regex: Option<String>,
    },
    /// Generate memory tests
    #[command(name = "gen-memtest")]
    GenMemTest {
        #[arg(long = "range", required = true, num_args = 2, action = clap::ArgAction::Append, value_parser = clap_num::maybe_hex::<u64>)]
        ranges: Vec<u64>,

        /// How to test the memory
        #[arg(long, default_value = "all")]
        strategy: MemTestStrategy,

        /// What to do when a test fails
        #[arg(long, default_value = FailureImplKind(keelhaul::FailureImplKind::Panic))]
        on_fail: Option<FailureImplKind>,

        #[arg(long, action = clap::ArgAction::SetTrue)]
        no_format: bool,

        /// Validate that supplied memories are representable on the the target CPUs architecture
        #[arg(long, value_enum)]
        arch: Option<ArchWidth>,
    },
}

impl Command {
    fn input(&self) -> Option<&InputFiles> {
        match self {
            Command::DryRun { input, .. } => Some(input),
            Command::LsTop { input, .. } => Some(input),
            Command::CountRegisters { input, .. } => Some(input),
            Command::CountResetValues { input, .. } => Some(input),
            Command::GenRegTest { input, .. } => Some(input),
            Command::GenMemTest { .. } => None,
        }
    }

    fn arch(&self) -> Option<ArchWidth> {
        match self {
            Command::DryRun { arch, .. } => *arch,
            Command::LsTop { arch, .. } => *arch,
            Command::CountRegisters { arch, .. } => *arch,
            Command::CountResetValues { arch, .. } => *arch,
            Command::GenRegTest { arch, .. } => *arch,
            Command::GenMemTest { .. } => None,
        }
    }

    fn validate_level(&self) -> Option<ValidateLevel> {
        match self {
            Command::DryRun { validate_level, .. } => Some(*validate_level),
            Command::LsTop { validate_level, .. } => Some(*validate_level),
            Command::CountRegisters { validate_level, .. } => Some(*validate_level),
            Command::CountResetValues { validate_level, .. } => Some(*validate_level),
            Command::GenRegTest { validate_level, .. } => Some(*validate_level),
            Command::GenMemTest { .. } => None,
        }
    }
}

#[derive(Clone, Copy)]
enum Sorting {
    Preserve,
    Alpha,
}

impl ValueEnum for Sorting {
    fn value_variants<'a>() -> &'a [Self] {
        &[Sorting::Alpha, Sorting::Preserve]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        use clap::builder::PossibleValue;
        match self {
            Sorting::Alpha => Some(PossibleValue::new("alpha")),
            Sorting::Preserve => Some(PossibleValue::new("preserve")),
        }
    }
}

#[derive(Clone)]
struct FailureImplKind(keelhaul::FailureImplKind);

impl From<FailureImplKind> for keelhaul::FailureImplKind {
    fn from(value: FailureImplKind) -> Self {
        value.0
    }
}

impl ops::Deref for FailureImplKind {
    type Target = keelhaul::FailureImplKind;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ValueEnum for FailureImplKind {
    fn value_variants<'a>() -> &'a [Self] {
        &[
            Self(keelhaul::FailureImplKind::None),
            Self(keelhaul::FailureImplKind::Panic),
            Self(keelhaul::FailureImplKind::ReturnError),
        ]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        use clap::builder::PossibleValue;
        match self.0 {
            keelhaul::FailureImplKind::None => Some(PossibleValue::new("ignore")),
            keelhaul::FailureImplKind::Panic => Some(PossibleValue::new("panic")),
            keelhaul::FailureImplKind::ReturnError => Some(PossibleValue::new("error")),
        }
    }
}

impl From<FailureImplKind> for clap::builder::OsStr {
    fn from(value: FailureImplKind) -> Self {
        match value.0 {
            keelhaul::FailureImplKind::None => "none".into(),
            keelhaul::FailureImplKind::Panic => "panic".into(),
            keelhaul::FailureImplKind::ReturnError => "error".into(),
        }
    }
}

#[derive(Clone)]
struct MemTestStrategy(keelhaul::MemTestStrategy);

impl From<MemTestStrategy> for keelhaul::MemTestStrategy {
    fn from(value: MemTestStrategy) -> Self {
        value.0
    }
}

impl ValueEnum for MemTestStrategy {
    fn value_variants<'a>() -> &'a [Self] {
        &[
            Self(keelhaul::MemTestStrategy::All),
            Self(keelhaul::MemTestStrategy::BoundariesOnly),
        ]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        use clap::builder::PossibleValue;
        match self.0 {
            keelhaul::MemTestStrategy::All => Some(PossibleValue::new("all")),
            keelhaul::MemTestStrategy::BoundariesOnly => Some(PossibleValue::new("boundaries")),
        }
    }
}

#[derive(Clone, Copy)]
struct ValidateLevel(keelhaul::ValidateLevel);

impl From<ValidateLevel> for keelhaul::ValidateLevel {
    fn from(value: ValidateLevel) -> Self {
        value.0
    }
}

impl From<ValidateLevel> for clap::builder::OsStr {
    fn from(value: ValidateLevel) -> Self {
        match value.0 {
            keelhaul::ValidateLevel::Disabled => "disabled".into(),
            keelhaul::ValidateLevel::Weak => "weak".into(),
            keelhaul::ValidateLevel::Strict => "strict".into(),
        }
    }
}

impl ValueEnum for ValidateLevel {
    fn value_variants<'a>() -> &'a [Self] {
        &[
            Self(keelhaul::ValidateLevel::Disabled),
            Self(keelhaul::ValidateLevel::Weak),
            Self(keelhaul::ValidateLevel::Strict),
        ]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        use clap::builder::PossibleValue;
        match self.0 {
            keelhaul::ValidateLevel::Disabled => Some(PossibleValue::new("disabled")),
            keelhaul::ValidateLevel::Weak => Some(PossibleValue::new("weak")),
            keelhaul::ValidateLevel::Strict => Some(PossibleValue::new("strict")),
        }
    }
}

#[derive(Clone)]
struct TestKind(keelhaul::TestKind);

impl From<TestKind> for keelhaul::TestKind {
    fn from(value: TestKind) -> Self {
        value.0
    }
}

impl ValueEnum for TestKind {
    fn value_variants<'a>() -> &'a [Self] {
        &[
            Self(keelhaul::TestKind::Read),
            Self(keelhaul::TestKind::ReadIsResetVal),
        ]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        use clap::builder::PossibleValue;
        match self.0 {
            keelhaul::TestKind::Read => Some(PossibleValue::new("read")),
            keelhaul::TestKind::ReadIsResetVal => Some(PossibleValue::new("reset")),
        }
    }
}

/// Like `PtrSize` but limited exactly to supported architecture pointer sizes.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum ArchWidth {
    #[value(name = "32")]
    U32,
    #[value(name = "64")]
    U64,
}

impl From<ArchWidth> for keelhaul::ArchWidth {
    fn from(value: ArchWidth) -> Self {
        match value {
            ArchWidth::U32 => keelhaul::ArchWidth::U32,
            ArchWidth::U64 => keelhaul::ArchWidth::U64,
        }
    }
}

fn string_to_path(s: &str) -> path::PathBuf {
    env::current_dir()
        .expect("cannot access current working dir")
        .join(s)
        // Canonicalize paths for clear output
        .canonicalize()
        .unwrap_or_else(|_| panic!("file does not exist: {s}"))
}

fn get_sources(
    input: &InputFiles,
    vlevel: ValidateLevel,
) -> anyhow::Result<Vec<keelhaul::ModelSource>> {
    let mut sources = Vec::with_capacity(input.svd.len());

    if !input.ipxact_dir.is_empty() {
        anyhow::bail!("IEE 1685/IP-XACT is not supported");
    }
    sources.extend(
        input
            .svd
            .iter()
            .map(|s| (s, keelhaul::SourceFormat::Svd(vlevel.into()))),
    );

    let sources = sources
        .into_iter()
        .map(|(s, f)| keelhaul::ModelSource::new(string_to_path(s), f))
        .collect::<Vec<_>>();

    // Make sure all source paths correspond to a file
    for s in sources.iter() {
        if !s.path().is_file() {
            return Err(anyhow!("file does not exist: {}", s.path().display()));
        }
    }

    Ok(sources)
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Install a logger to print useful messages to stderr
    env_logger::Builder::new()
        .filter_module("keelhaul", cli.verbose.log_level_filter())
        .init();

    let sources = cli
        .command
        .as_ref()
        .and_then(|cmd| {
            cmd.input().map(|input| {
                get_sources(
                    input,
                    cmd.validate_level()
                        .unwrap_or(ValidateLevel(keelhaul::ValidateLevel::Disabled)),
                )
            })
        })
        .transpose()?;
    let arch = cli
        .command
        .as_ref()
        .and_then(|cmd| cmd.arch().map(|arch| arch.into()));

    if let Some(cmd) = &cli.command {
        match cmd {
            Command::DryRun {
                input: _input,
                arch: _arch,
                validate_level: _,
                use_legacy,
            } => {
                let sources = sources.unwrap();
                match keelhaul::dry_run(&sources, arch.unwrap(), *use_legacy).with_context(|| {
                    format!("could not execute dry run for arch: {arch:?}, sources: {sources:?}")
                }) {
                    Ok(_) => println!("keelhaul: dry run completed successfully"),
                    Err(e) => println!("keelhaul: exited unsuccessfully: {e:?}"),
                }
            }
            Command::LsTop {
                input: _input,
                arch: _arch,
                no_count,
                sorting,
                validate_level: _,
                no_rubric,
                use_legacy,
            } => ls_top(
                &sources.unwrap(),
                arch.unwrap(),
                *sorting,
                *no_count,
                *no_rubric,
                *use_legacy,
            )?,
            Command::CountRegisters {
                input: _input,
                validate_level: _,
                arch: _arch,
                use_legacy,
            } => {
                let output = keelhaul::count_registers_svd(
                    &sources.unwrap(),
                    arch.unwrap(),
                    &keelhaul::Filters::all(),
                    *use_legacy,
                )?;
                println!("{output}");
            }
            Command::GenRegTest {
                input: _input,
                arch,
                tests_to_generate,
                on_fail,
                derive_debug,
                ignore_reset_masks,
                no_format,
                use_zero_as_default_reset,
                validate_level: _,
                filter_top_regex,
                filter_path_regex,
                use_legacy,
            } => {
                let mut config = keelhaul::CodegenConfig::default()
                    .tests_to_generate(tests_to_generate.iter().cloned().map(|tk| tk.0).collect())
                    .unwrap()
                    .derive_debug(*derive_debug)
                    .ignore_reset_masks(*ignore_reset_masks);
                if let Some(on_fail) = on_fail {
                    config = config.on_fail(on_fail.clone().into());
                }

                let filters = keelhaul::Filters::from_filters(
                    None,
                    filter_top_regex
                        .as_ref()
                        .map(|s| keelhaul::RegexFilter::from_str(s))
                        .transpose()?
                        .map(|f| -> Box<dyn keelhaul::Filter> { Box::new(f) }),
                    filter_path_regex
                        .as_ref()
                        .map(|s| keelhaul::RegexFilter::from_str(s))
                        .transpose()?
                        .map(|f| -> Box<dyn keelhaul::Filter> { Box::new(f) }),
                );

                generate(
                    arch.unwrap().into(),
                    filters,
                    config,
                    sources.unwrap(),
                    *no_format,
                    *use_zero_as_default_reset,
                    *use_legacy,
                )?
            }
            Command::CountResetValues {
                input: _input,
                arch: _arch,
                validate_level: _,
                use_legacy,
            } => {
                let sources = sources.unwrap();
                let total = keelhaul::count_registers_svd(
                    &sources,
                    arch.unwrap(),
                    &keelhaul::Filters::all(),
                    *use_legacy,
                )?;
                let with_reset = keelhaul::count_readable_registers_with_reset_value(
                    &sources,
                    arch.unwrap(),
                    &keelhaul::Filters::all(),
                    *use_legacy,
                )?;
                let percent = (with_reset as f64 / total as f64) * 100.;
                println!(
                    "{with_reset}/{total} ({percent:.2} %) of readable registers have a known a reset value that can be tested for"
                );
            }
            Command::GenMemTest {
                ranges,
                strategy,
                on_fail,
                no_format,
                arch,
            } => {
                let ranges = ranges
                    .chunks(2)
                    .map(|chunk| chunk[0]..(chunk[0] + chunk[1]))
                    .collect::<Vec<_>>();
                if let Some(arch) = arch {
                    let arch: keelhaul::ArchWidth = (*arch).into();
                    let ptr_size: keelhaul::PtrSize = arch.into();
                    for range in &ranges {
                        if range.start > ptr_size.max_value() || range.end > ptr_size.max_value() {
                            panic!("Supplied memory addresses cannot be represented on this architecture");
                        }
                    }
                }
                let output = if *no_format {
                    keelhaul::generate_memtests(
                        &ranges,
                        &strategy.0,
                        on_fail
                            .as_ref()
                            .unwrap_or(&FailureImplKind(keelhaul::FailureImplKind::ReturnError)),
                    )
                } else {
                    keelhaul::generate_memtests_with_format(
                        &ranges,
                        &strategy.0,
                        on_fail
                            .as_ref()
                            .unwrap_or(&FailureImplKind(keelhaul::FailureImplKind::ReturnError)),
                    )
                };
                println!("{output}");
            }
        }
    } else {
        println!("Nothing to do. Please issue a subcommand.")
    }

    Ok(())
}

fn generate(
    arch: keelhaul::ArchWidth,
    filters: keelhaul::Filters,
    config: keelhaul::CodegenConfig,
    sources: Vec<keelhaul::ModelSource>,
    no_format: bool,
    use_zero_as_default_reset: bool,
    use_legacy: bool,
) -> Result<(), anyhow::Error> {
    let output = if no_format {
        keelhaul::generate_tests(
            &sources,
            arch,
            &config,
            &filters,
            use_zero_as_default_reset,
            use_legacy,
        )?
    } else {
        keelhaul::generate_tests_with_format(
            &sources,
            arch,
            &config,
            &filters,
            use_zero_as_default_reset,
            use_legacy,
        )?
    };
    println!("{output}");
    Ok(())
}

fn ls_top(
    sources: &[keelhaul::ModelSource],
    arch: keelhaul::ArchWidth,
    sorting: Sorting,
    no_count: bool,
    no_rubric: bool,
    use_legacy: bool,
) -> Result<(), anyhow::Error> {
    let mut top_and_count = keelhaul::list_top(sources, arch, use_legacy)?;
    if top_and_count.is_empty() {
        println!("keelhaul: no peripherals found in input");
    }
    match sorting {
        Sorting::Preserve => { /* do nothing */ }
        Sorting::Alpha => top_and_count.sort(),
    };
    let top_title = "Top element";
    let count_title = "Registers";
    let longest = top_and_count
        .iter()
        .map(|(s, _)| s.as_str())
        .chain(iter::once(top_title))
        .map(|s| s.len())
        .max()
        .unwrap();
    if !no_rubric {
        println!("{top_title: <longest$} {count_title}");
        let count_title_len = count_title.len();
        println!("{} {}", "-".repeat(longest), "-".repeat(count_title_len));
    }
    for (top, count) in top_and_count {
        if no_count {
            println!("{top}");
        } else {
            println!("{top: <longest$} {count}");
        }
    }
    Ok(())
}
