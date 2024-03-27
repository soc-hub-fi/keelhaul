use std::{env, io, path};

use anyhow::{anyhow, Context};
use clap::{Parser, Subcommand, ValueEnum};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// CMSIS-SVD source file for memory map metadata
    #[arg(group = "input", long, required = true, action = clap::ArgAction::Append)]
    svd: Vec<String>,

    /// IEEE-1685 source file for memory map metadata
    #[arg(group = "input", long, required = true, action = clap::ArgAction::Append)]
    ipxact: Vec<String>,

    /// Number of bits used to represent addresses on the target CPUs architecture
    #[arg(long, value_enum, required = true)]
    arch: ArchWidth,

    #[arg(long = "validate", default_value = ValidateLevel(keelhaul::ValidateLevel::Disabled), requires = "svd")]
    validate_level: ValidateLevel,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run the Keelhaul parser without doing anything
    DryRun,
    /// Lists all top level items (peripherals or subsystems) in the supplied sources
    ///
    /// Peripherals or subsystems containing zero registers are omitted.
    LsTop {
        /// Only list peripherals without register counts
        #[arg(long, action = clap::ArgAction::SetTrue)]
        no_count: bool,
    },
    CountRegisters {},
    Coverage {
        /// Describes how many of the input registers supply a known reset value which can be tested
        /// for
        #[arg(long)]
        has_reset_value: bool,
    },
    /// Generate metadata tests
    Generate {
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
    },
}

#[derive(Clone)]
struct FailureImplKind(keelhaul::FailureImplKind);

impl From<FailureImplKind> for keelhaul::FailureImplKind {
    fn from(value: FailureImplKind) -> Self {
        value.0
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

#[derive(Clone)]
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

fn string_to_path(s: &String) -> Result<path::PathBuf, io::Error> {
    env::current_dir()
        .expect("cannot access current working dir")
        .join(s)
        // Canonicalize paths for clear output
        .canonicalize()
}

fn get_sources(cli: &Cli) -> anyhow::Result<Vec<keelhaul::ModelSource>> {
    let mut sources = Vec::with_capacity(cli.svd.len() + cli.ipxact.len());

    sources.extend(cli.svd.iter().map(|s| (s, keelhaul::SourceFormat::Svd)));
    sources.extend(
        cli.ipxact
            .iter()
            .map(|s| (s, keelhaul::SourceFormat::Ieee1685)),
    );

    let sources = sources
        .into_iter()
        .map(|(s, f)| string_to_path(s).map(|p| keelhaul::ModelSource::new(p, f)))
        .collect::<Result<Vec<_>, _>>()?;

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

    let sources = get_sources(&cli)?;
    let arch = cli.arch.into();

    match &cli.command {
        Some(Commands::DryRun) => {
            match keelhaul::dry_run(&sources, arch).with_context(|| {
                format!("could not execute dry run for arch: {arch:?}, sources: {sources:?}")
            }) {
                Ok(_) => println!("keelhaul: dry run completed successfully"),
                Err(e) => println!("keelhaul: exited unsuccessfully: {e:?}"),
            }
        }
        Some(Commands::LsTop { no_count }) => {
            let top_and_count = keelhaul::list_top(&sources, arch)?;
            if top_and_count.is_empty() {
                println!("keelhaul: no peripherals found in input");
            }
            let longest = top_and_count.iter().map(|(s, _)| s.len()).max().unwrap();
            for (top, count) in top_and_count {
                if *no_count {
                    println!("{top}");
                } else {
                    println!("{top: <longest$} {count}");
                }
            }
        }
        Some(Commands::CountRegisters {}) => {
            let output = keelhaul::count_registers_svd(&sources, arch, &keelhaul::Filters::all())?;
            println!("{output}");
        }
        Some(Commands::Generate {
            tests_to_generate,
            on_fail,
            derive_debug,
            ignore_reset_masks,
        }) => {
            let mut config = keelhaul::TestConfig::new(arch)
                .tests_to_generate(tests_to_generate.iter().cloned().map(|tk| tk.0).collect())?
                .derive_debug(*derive_debug)
                .ignore_reset_masks(*ignore_reset_masks);
            if let Some(on_fail) = on_fail.as_ref() {
                config = config.on_fail(on_fail.clone().into());
            }
            let output =
                keelhaul::generate_tests(&sources, arch, &config, &keelhaul::Filters::all())?;
            println!("{output}");
        }
        Some(Commands::Coverage { .. }) => {
            todo!()
        }
        None => {}
    }

    Ok(())
}
