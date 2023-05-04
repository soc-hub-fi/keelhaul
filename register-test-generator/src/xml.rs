//! Tools for processing XML-format.

use crate::{CommonParseError, PositionalError};
use roxmltree::Node;

/// Find a child node with given tag name
///
/// # Arguments
///
/// * `node` - XML node
/// * `tag` - String slice with tag name
pub(crate) fn find_text_in_node_by_tag_name<'a>(
    node: &'a Node,
    tag: &str,
) -> Result<(&'a str, Node<'a, 'a>), PositionalError<CommonParseError>> {
    maybe_find_text_in_node_by_tag_name(node, tag).ok_or(
        CommonParseError::ExpectedTagInElement {
            elem_name: node.tag_name().name().to_owned(),
            tag: tag.to_owned(),
        }
        .with_byte_pos_range(node.range(), node.document()),
    )
}

/// Try to find a child node with given name
///
/// Unlike [find_text_in_node_by_tag_name], a node with given name might not exist
///
/// # Arguments
///
/// * `node` - XML node
/// * `tag` - String slice with tag name
pub(crate) fn maybe_find_text_in_node_by_tag_name<'a>(
    node: &'a Node,
    tag: &str,
) -> Option<(&'a str, Node<'a, 'a>)> {
    node.children()
        .find(|n| n.has_tag_name(tag))
        .map(|n| (n.text().expect("Node does not have text."), n))
}
