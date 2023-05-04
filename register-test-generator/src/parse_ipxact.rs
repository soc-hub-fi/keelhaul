//! IP-XACT-file parser for register test generator.

use crate::{
    error::*,
    read_file_from_env_or_panic,
    xml::{find_text_in_node_by_tag_name, maybe_find_text_in_node_by_tag_name},
    Access, AddrRepr, ArchiPtr, Error, IpxactParseError, Protection, PtrSize, RegPath, Register,
    RegisterPropertiesGroup, Registers, ResetValue,
};
use log::info;
use regex::Regex;
use roxmltree::{Document as XmlDocument, Node as XmlNode};
use std::{fmt, str::FromStr};

enum RegisterAccess {
    ReadOnly,
    WriteOnly,
    ReadWrite,
    ReadWriteOnce,
}

impl FromStr for RegisterAccess {
    type Err = IpxactParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "read-write" => Ok(Self::ReadWrite),
            "read-only" => Ok(Self::ReadOnly),
            "read-writeOnce" => Ok(Self::ReadWriteOnce),
            "write-only" => Ok(Self::WriteOnly),
            other => Err(IpxactParseError::GenericParse(
                CommonParseError::InvalidAccessType(other.to_owned()),
            )),
        }
    }
}

#[allow(dead_code)]
enum TestConstraint {
    /// No restriction on the data that may be written or read from field.
    Unconstrained,
    /// Field's value shall be restored to the original value before accessing another register.
    Restore,
    /// Field shall be written only to a value just previously read from field.
    WriteAsRead,
    /// Field shall only be read.
    ReadOnly,
}

impl TestConstraint {
    #[allow(dead_code)]
    fn from_str(string: &str) -> Self {
        match string {
            "unconstrained" => Self::Unconstrained,
            "restore" => Self::Restore,
            "writeAsRead" => Self::WriteAsRead,
            "readOnly" => Self::ReadOnly,
            other => panic!("Invalid test constraint: {other}."),
        }
    }
}

#[allow(dead_code)]
enum FieldTestable {
    /// Field is testable by automatic register test.
    True(TestConstraint),
    /// Field is not testable by automatic register test.
    False,
}

impl FieldTestable {
    #[allow(dead_code)]
    fn default() -> Self {
        FieldTestable::True(TestConstraint::Unconstrained)
    }
}

#[allow(dead_code)]
enum FieldValueUsage {
    Read,
    Write,
    ReadWrite,
}

impl FieldValueUsage {
    #[allow(dead_code)]
    fn default() -> Self {
        Self::ReadWrite
    }

    #[allow(dead_code)]
    fn from_str(string: &str) -> Self {
        match string {
            "read" => Self::Read,
            "write" => Self::Write,
            "read-write" => Self::ReadWrite,
            other => panic!("Invalid field value usage: {other}."),
        }
    }
}

#[allow(dead_code)]
struct FieldValue<P: num::CheckedAdd> {
    pub name: String,
    pub value: P,
    pub usage: FieldValueUsage,
}

#[allow(dead_code)]
struct Field<P: num::CheckedAdd> {
    pub name: String,
    pub offset: P,
    pub value_reset: Option<P>,
    pub width: P,
    pub testable: FieldTestable,
    pub enumerated_values: Vec<FieldValue<P>>,
}

/// Hierarchical representation of a register's path
///
/// E.g., TODO
#[derive(Clone)]
pub struct RegPathIpxact {
    // TODO: divide to enumerations
    pub component: String,
    pub address_space: Option<String>,
    pub local_memory_map: Option<String>,
    pub memory_map: Option<String>,
    pub address_block: Option<String>,
    pub register: String,
}

impl RegPathIpxact {
    /// Joins the path elements into one string
    pub fn join(&self, sep: &str) -> String {
        let mut v = vec![self.component.clone()];
        if let Some(n) = &self.address_space {
            v.push(n.clone());
        }
        if let Some(n) = &self.local_memory_map {
            v.push(n.clone());
        }
        if let Some(n) = &self.memory_map {
            v.push(n.clone());
        }
        if let Some(n) = &self.address_block {
            v.push(n.clone());
        }
        v.push(self.register.clone());
        v.join(sep)
    }

    pub fn periph(&self) -> String {
        self.component.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AddrReprIpxact<P: num::CheckedAdd> {
    Comps {
        address_space: Option<P>,
        register_offset: P,
    },
    Full(P),
}

impl<P> AddrReprIpxact<P>
where
    P: num::CheckedAdd + Clone,
{
    /// Get register's absolute memory address
    ///
    /// Returns None on address overflow.
    pub fn full(&self) -> Option<P> {
        match self {
            Self::Full(addr) => Some(addr.clone()),
            Self::Comps {
                address_space,
                register_offset,
            } => {
                let addr = register_offset.clone();
                match address_space {
                    Some(address_space) => addr.checked_add(address_space),
                    None => Some(addr),
                }
            }
        }
    }
}

impl<P: num::CheckedAdd + fmt::LowerHex> fmt::Display for AddrReprIpxact<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Full(address) => write!(f, "{{ address: {:#x} }}", address),
            Self::Comps {
                address_space,
                register_offset,
            } => match address_space {
                Some(address_space) => write!(
                    f,
                    "{{ address_space: {:#x}, register_offset: {:#x} }}",
                    address_space, register_offset
                ),
                None => write!(
                    f,
                    "{{ address_space: None, register_offset: {:#x} }}",
                    register_offset
                ),
            },
        }
    }
}

// 64-bit address can be fallibly converted to a 32-bit address. Returns Err on
// overflow.
impl TryFrom<AddrReprIpxact<u64>> for AddrReprIpxact<u32> {
    type Error = <u64 as TryInto<u32>>::Error;
    fn try_from(value: AddrReprIpxact<u64>) -> Result<Self, Self::Error> {
        match value {
            AddrReprIpxact::Comps {
                address_space,
                register_offset,
            } => Ok(AddrReprIpxact::Comps {
                address_space: match address_space {
                    Some(address_space) => Some(address_space.try_into()?),
                    None => None,
                },
                register_offset: register_offset.try_into()?,
            }),
            AddrReprIpxact::Full(a) => Ok(AddrReprIpxact::Full(a.try_into()?)),
        }
    }
}

/*
impl TryFrom<AddrReprIpxact<u32>> for AddrReprIpxact<u64> {
    type Error = <u32 as TryInto<u64>>::Error;
    fn try_from(value: AddrReprIpxact<u32>) -> Result<Self, Self::Error> {
        match value {
            AddrReprIpxact::Comps {
                address_space,
                register_offset,
            } => Ok(AddrReprIpxact::Comps {
                address_space: address_space.try_into()?,
                register_offset: register_offset.try_into()?,
            }),
            AddrReprIpxact::Full(a) => Ok(AddrReprIpxact::Full(a.try_into()?)),
        }
    }
}
*/

impl From<AddrReprIpxact<u32>> for AddrReprIpxact<u64> {
    fn from(value: AddrReprIpxact<u32>) -> Self {
        match value {
            AddrReprIpxact::Comps {
                address_space,
                register_offset,
            } => AddrReprIpxact::Comps {
                address_space: address_space.map(|address_space| address_space.into()),
                register_offset: register_offset.into(),
            },
            AddrReprIpxact::Full(a) => AddrReprIpxact::Full(a.into()),
        }
    }
}

#[allow(dead_code)]
struct IpxactRegister {
    pub path: RegPathIpxact,
    pub access: RegisterAccess,
    pub size: u64,
    pub fields: Vec<Field<u64>>,
    pub address_address_block: Option<u64>,
    pub address_offset_register: u64,
    pub dimension: Option<u64>,
}

impl IpxactRegister {
    pub fn addr_repr<P>(&self) -> AddrRepr<P>
    where
        P: ArchiPtr,
    {
        let address_space = self
            .address_address_block
            .map(|address_space| address_space.try_into().unwrap_or_else(|_err| panic!("")));
        let register_offset = self
            .address_offset_register
            .try_into()
            .unwrap_or_else(|_err| panic!(""));
        AddrRepr::Ipxact(AddrReprIpxact::Comps {
            address_space,
            register_offset,
        })
    }
}

impl<P> From<IpxactRegister> for Register<P>
where
    P: ArchiPtr,
{
    fn from(value: IpxactRegister) -> Self {
        let path = RegPath::Ipxact(value.path.clone());
        let addr = value.addr_repr();

        // TODO
        let properties = RegisterPropertiesGroup::new(
            PtrSize::U64,
            Access::ReadWrite,
            Protection::NonSecureOrSecure,
            ResetValue::U64 { val: 0, mask: 0 },
        );
        // TODO
        let dimensions = None;
        Register {
            path,
            addr,
            properties,
            dimensions,
        }
    }
}

/*
impl<P> From<Vec<IpxactRegister<P>>> for Registers<P>
where
    P: num::CheckedAdd,
{
    fn from(value: Vec<IpxactRegister<P>>) -> Self {
        let mut registers = Vec::new();
        Registers(registers)
    }
}
*/

/// Returns the appropriate multiplier for given character, represented by type parameter `P`
fn binary_size_mult_from_char<P: TryFrom<u64>>(c: char) -> Result<P, IpxactParseError>
where
    IpxactParseError: From<<P as TryFrom<u64>>::Error>,
{
    match c {
        'k' | 'K' => Ok(1024u64.try_into()?),
        'm' | 'M' => Ok((1024 * 1024u64).try_into()?),
        'g' | 'G' => Ok((1024 * 1024 * 1024u64).try_into()?),
        't' | 'T' => Ok((1024 * 1024 * 1024 * 1024u64).try_into()?),
        _ => Err(IpxactParseError::InvalidSizeMultiplierSuffix(c)),
    }
}

/// Parses an integer from `text`
///
/// This implementation is format aware and uses regex to ensure correct behavior.
fn parse_nonneg_int<P: ArchiPtr>(text: &str) -> Result<P, IpxactParseError>
where
    IpxactParseError: From<<P as num::Num>::FromStrRadixErr>
        + From<<P as FromStr>::Err>
        + From<<P as TryFrom<u64>>::Error>,
{
    // Compile Regexes only once as recommended by the documentation of the Regex crate
    use lazy_static::lazy_static;
    lazy_static! {
        // [0x|0X|\#]{1}          # hexadecimal prefix
        /// Regular expression to capture hexadecimal numbers, as defined in CMSIS-SVD schema
        static ref HEX_NONNEG_INT_RE: Regex = Regex::new(
            r"(?x)              # insignificant whitespace
            \+?                 # zero or one plus sign
            (?:0x|0X|\#)        # hexadecimal prefix
            ([[:xdigit:]]+)     # one or more hexadecimal digits (captured as #1)
            [[:space:]]?        # zero or one of whitespace
            ([kmgtKMGT])?       # zero or one of kilo, mega, giga, tera identifier (captured as #2)
        ").unwrap();

        /// Regular expression to capture decimal numbers, as defined in CMSIS-SVD schema
        static ref DEC_NONNEG_INT_RE: Regex = Regex::new(
            r"(?x)              # insignificant whitespace
            \+?                 # zero or one plus sign
            ([[:digit:]]+)      # one or more decimal digits (captured as #1)
            [[:space:]]?        # zero or one of whitespace
            ([kmgtKMGT])?       # zero or one of kilo, mega, giga, tera identifier (captured as #2)
        ").unwrap();
    }

    // Pick either hexadecimal or decimal format based on which fits
    // TODO: pick binary format on '#'

    let (number_part, size_mult_capture) = if HEX_NONNEG_INT_RE.is_match(text) {
        // Safety: we checked above that at least one match exists in text
        let captures = HEX_NONNEG_INT_RE.captures_iter(text).next().unwrap();

        let digits = &captures[1];
        let number = P::from_str_radix(digits, 16)?;

        let size_mult = captures.get(2);
        (number, size_mult)
    } else if DEC_NONNEG_INT_RE.is_match(text) {
        // Safety: we checked above that at least one match exists in text
        let captures = DEC_NONNEG_INT_RE.captures_iter(text).next().unwrap();

        let digits = &captures[1];
        let number = digits.parse::<P>()?;

        let size_mult = captures.get(2);
        (number, size_mult)
    } else {
        return Err(IpxactParseError::InvalidNonnegInt(text.to_owned()));
    };

    let size_mult: Option<P> = size_mult_capture
        // Safety: we know from the regex that there is only one possible size mult char
        .map(|s| s.as_str().chars().next().unwrap())
        .map(|c| binary_size_mult_from_char(c))
        .transpose()?;

    Ok(match size_mult {
        Some(mult) => number_part * mult,
        None => number_part,
    })
}

/// Add text position information to an [IpxactParseError] converting it into a [PositionalError]
fn err_with_pos(
    e: impl Into<IpxactParseError>,
    node: &XmlNode,
) -> PositionalError<IpxactParseError> {
    e.into().with_byte_pos_range(node.range(), node.document())
}

fn find_number_with_tag(
    node: &XmlNode,
    tag: &str,
) -> Result<u64, PositionalError<IpxactParseError>> {
    let number_str = find_text_in_node_by_tag_name(node, tag)
        .map_err(PositionalError::<IpxactParseError>::from)?
        .0
        .to_owned()
        .replace("'h", "0x")
        .replace('_', "");
    parse_nonneg_int(&number_str).map_err(|err| err_with_pos(err, node))
}

fn maybe_find_number_with_tag(node: &XmlNode, tag: &str) -> Option<u64> {
    let number_str = maybe_find_text_in_node_by_tag_name(node, tag)?
        .0
        .replace("'h", "0x")
        .replace('_', "");
    parse_nonneg_int(&number_str).ok()
}

fn parse_field_node(field_node: &XmlNode) -> Result<Field<u64>, PositionalError<IpxactParseError>> {
    let name_field = find_text_in_node_by_tag_name(field_node, "name")?
        .0
        .to_owned();
    let offset_field = find_number_with_tag(field_node, "bitOffset")?;
    let value_reset = match field_node.children().find(|n| n.has_tag_name("resets")) {
        Some(resets) => {
            match resets.children().find(|n| n.has_tag_name("reset")) {
                Some(reset) => Some(find_number_with_tag(&reset, "value")?),
                // TODO
                None => None,
            }
        }
        // TODO
        None => None,
    };
    let width = find_number_with_tag(field_node, "bitWidth")?;
    let testable = match field_node.children().find(|n| n.has_tag_name("testable")) {
        Some(testable) => match testable.text().unwrap() {
            "true" => {
                let constraint = match testable.attribute("testConstraint") {
                    Some(constraint) => constraint.to_owned(),
                    None => "unconstrained".to_owned(),
                };
                FieldTestable::True(TestConstraint::from_str(&constraint))
            }
            "false" => FieldTestable::False,
            other => panic!("Invalid testable value: {other}."),
        },
        None => FieldTestable::default(),
    };
    let mut enumerated_values = Vec::new();
    if let Some(enumerated_values_node) = field_node
        .children()
        .find(|n| n.has_tag_name("enumeratedValues"))
    {
        for enumerated_value_node in enumerated_values_node
            .children()
            .filter(|n| n.has_tag_name("enumeratedValue"))
        {
            let enumerated_value_name =
                find_text_in_node_by_tag_name(&enumerated_value_node, "name")?
                    .0
                    .to_owned();
            let enumerated_value_value = find_number_with_tag(&enumerated_value_node, "value")?;
            let usage = match maybe_find_text_in_node_by_tag_name(&enumerated_value_node, "usage") {
                Some((usage, _)) => FieldValueUsage::from_str(usage),
                None => FieldValueUsage::default(),
            };
            let enumerated_value = FieldValue {
                name: enumerated_value_name,
                value: enumerated_value_value,
                usage,
            };
            enumerated_values.push(enumerated_value);
        }
    }
    Ok(Field {
        name: name_field,
        offset: offset_field,
        value_reset,
        width,
        testable,
        enumerated_values,
    })
}

fn parse_fields_from_register(
    register_node: &XmlNode,
) -> Result<Vec<Field<u64>>, PositionalError<IpxactParseError>> {
    let mut fields = Vec::new();
    for field_node in register_node.children().filter(|n| n.has_tag_name("field")) {
        let field = parse_field_node(&field_node)?;
        fields.push(field);
    }
    Ok(fields)
}

fn parse_register_node(
    register_node: &XmlNode,
) -> Result<IpxactRegister, PositionalError<IpxactParseError>> {
    let name_register = find_text_in_node_by_tag_name(register_node, "name")?
        .0
        .to_owned();
    let register_address_offset: u64 = find_number_with_tag(register_node, "addressOffset")?;
    let access = maybe_find_text_in_node_by_tag_name(register_node, "access")
        .map(|(access, _)| access)
        .unwrap_or_else(|| {
            info!("Register {name_register} does not have access value. Access is assumed to be 'read-write'.");
            "read-write"}).parse().map_err(|err| err_with_pos(err, register_node))?;
    let size = find_number_with_tag(register_node, "size")?;
    let address_offset_register = find_number_with_tag(register_node, "addressOffset")?;
    info!(" - - - {name_register} {register_address_offset}");
    let dimension = maybe_find_number_with_tag(register_node, "dim");
    let fields = parse_fields_from_register(register_node)?;
    let path = RegPathIpxact {
        // TODO: implement as option at first or maybe builder
        component: "PLACEHOLDER".to_owned(),
        address_space: None,
        local_memory_map: None,
        memory_map: None,
        address_block: None,
        register: name_register,
    };
    Ok(IpxactRegister {
        path,
        access,
        size,
        fields,
        address_offset_register,
        address_address_block: None,
        dimension,
    })
}

fn parse_registers_from_address_block(
    address_block_node: &XmlNode,
) -> Result<Vec<IpxactRegister>, PositionalError<IpxactParseError>> {
    let mut registers = Vec::new();
    for register_node in address_block_node
        .children()
        .filter(|n| n.has_tag_name("register"))
    {
        let register = parse_register_node(&register_node)?;
        registers.push(register);
    }
    Ok(registers)
}

fn parse_registers_from_local_memory_map(
    local_memory_map_node: &XmlNode,
) -> Result<Vec<IpxactRegister>, PositionalError<IpxactParseError>> {
    let mut registers = Vec::new();
    for address_block_node in local_memory_map_node
        .children()
        .filter(|n| n.has_tag_name("addressBlock"))
    {
        let name_address_block = find_text_in_node_by_tag_name(&address_block_node, "name")?
            .0
            .to_owned();
        let address_address_block = find_number_with_tag(&address_block_node, "baseAddress")?;
        info!(" - - {name_address_block} {address_address_block}");
        let mut address_block_registers = parse_registers_from_address_block(&address_block_node)?;
        for register in &mut address_block_registers {
            register.path.address_block = Some(name_address_block.clone());
            register.address_address_block = Some(address_address_block);
        }
        registers.extend(address_block_registers);
    }
    Ok(registers)
}

fn parse_registers_from_address_space(
    address_space_node: &XmlNode,
) -> Result<Vec<IpxactRegister>, PositionalError<IpxactParseError>> {
    let mut registers = Vec::new();
    for local_memory_map_node in address_space_node
        .children()
        .filter(|n| n.has_tag_name("localMemoryMap"))
    {
        let name_local_memory_map = find_text_in_node_by_tag_name(&local_memory_map_node, "name")?
            .0
            .to_owned();
        info!(" - {name_local_memory_map}");
        let mut local_memory_map_registers =
            parse_registers_from_local_memory_map(&local_memory_map_node)?;
        for register in &mut local_memory_map_registers {
            register.path.local_memory_map = Some(name_local_memory_map.clone());
        }
        registers.extend(local_memory_map_registers);
    }
    Ok(registers)
}

fn parse_registers_from_memory_map(
    memory_map_node: &XmlNode,
) -> Result<Vec<IpxactRegister>, PositionalError<IpxactParseError>> {
    let mut registers = Vec::new();
    for address_block_node in memory_map_node
        .children()
        .filter(|n| n.has_tag_name("addressBlock"))
    {
        let name_address_block = find_text_in_node_by_tag_name(&address_block_node, "name")?
            .0
            .to_owned();
        let address_address_block = find_number_with_tag(&address_block_node, "baseAddress")?;
        info!(" - - {name_address_block} {address_address_block}");
        let mut address_block_registers = parse_registers_from_address_block(&address_block_node)?;
        for register in &mut address_block_registers {
            register.path.address_block = Some(name_address_block.clone());
            register.address_address_block = Some(address_address_block);
        }
        registers.extend(address_block_registers);
    }
    Ok(registers)
}

fn parse_registers_from_component(
    component_node: &XmlNode,
) -> Result<Vec<IpxactRegister>, PositionalError<IpxactParseError>> {
    let mut registers = Vec::new();
    for address_space_node in component_node
        .children()
        .filter(|n| n.has_tag_name("addressSpace"))
    {
        let name_address_space = find_text_in_node_by_tag_name(&address_space_node, "name")?
            .0
            .to_owned();
        info!("{name_address_space}");
        let mut address_space_registers = parse_registers_from_address_space(&address_space_node)?;
        for register in &mut address_space_registers {
            register.path.address_space = Some(name_address_space.clone());
        }
        registers.extend(address_space_registers);
    }
    for memory_maps_node in component_node
        .children()
        .filter(|n| n.has_tag_name("memoryMaps"))
    {
        for memory_map_node in memory_maps_node
            .children()
            .filter(|n| n.has_tag_name("memoryMap"))
        {
            let name_memory_map = find_text_in_node_by_tag_name(&memory_map_node, "name")?
                .0
                .to_owned();
            info!(" - {name_memory_map}");
            let mut memory_map_registers = parse_registers_from_memory_map(&memory_map_node)?;
            for register in &mut memory_map_registers {
                register.path.memory_map = Some(name_memory_map.clone());
            }
            registers.extend(memory_map_registers);
        }
    }
    Ok(registers)
}

impl<P> From<Vec<IpxactRegister>> for Registers<P>
where
    P: ArchiPtr,
{
    fn from(value: Vec<IpxactRegister>) -> Self {
        let mut registers: Vec<Register<P>> = Vec::new();
        for ipxact in value {
            registers.push(ipxact.into());
        }
        Registers(registers)
    }
}

fn find_registers<P>(
    parsed: &XmlDocument,
) -> Result<Registers<P>, PositionalError<IpxactParseError>>
where
    P: ArchiPtr,
    IpxactParseError: From<<P as num::Num>::FromStrRadixErr>
        + From<<P as FromStr>::Err>
        + From<<P as TryFrom<u64>>::Error>,
{
    let mut registers: Vec<IpxactRegister> = Vec::new();
    for component_node in parsed
        .root()
        .children()
        .filter(|n| n.has_tag_name("component"))
    {
        let name_component = find_text_in_node_by_tag_name(&component_node, "name")?
            .0
            .to_owned();
        let version: u64 = find_number_with_tag(&component_node, "version")?;
        info!("{name_component} {version}");
        let mut component_registers = parse_registers_from_component(&component_node)?;
        for register in &mut component_registers {
            register.path.component = name_component.clone();
        }
        registers.extend(component_registers);
    }
    Ok(registers.into())
}

/// Parse IP-XACT-file.
pub(crate) fn parse<P>() -> Result<Registers<P>, Error>
where
    P: ArchiPtr,
    IpxactParseError: From<<P as num::Num>::FromStrRadixErr>
        + From<<P as FromStr>::Err>
        + From<<P as TryFrom<u64>>::Error>,
{
    // TODO: maybe error type for input file not found
    let content = read_file_from_env_or_panic("IPXACT_PATH")
        .unwrap_or_else(|| panic!("IPXACT_PATH must be set"));
    let parsed = XmlDocument::parse(&content)
        .unwrap_or_else(|error| panic!("Could not parse IP-XACT file: {error}"));
    let registers = match find_registers(&parsed) {
        Ok(result) => result,
        Err(error) => {
            // TODO: better error
            panic!("Could not find registers: {error}");
        }
    };
    if registers.is_empty() {
        return Err(Error::ZeroEntries);
    }
    info!("Found {} registers.", registers.len());
    Ok(registers)
}
