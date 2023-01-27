use crate::ui::{widget::UIWidget, UINode, UITag};

use anyhow::Result;
use fnv::FnvHashMap;
use tui::{backend::Backend, Terminal};

#[derive(Debug)]
pub struct UIManager<B: Backend> {
    terminal: Terminal<B>,
    elements: FnvHashMap<UITag, UINode>,
    root_node: Option<UITag>,
}

impl<B: Backend> UIManager<B> {
    pub fn new(terminal: Terminal<B>) -> Self {
        let mut elements = FnvHashMap::default();

        elements.insert(
            UITag(1),
            UINode::Text {
                text: "Hello".to_string(),
            },
        );

        elements.insert(
            UITag(2),
            UINode::Block {
                title: Some("test 1".to_string()),
                widget: UIWidget::Paragraph {
                    text: vec![UITag(1)],
                },
            },
        );

        elements.insert(
            UITag(3),
            UINode::Text {
                text: "World".to_string(),
            },
        );

        elements.insert(
            UITag(4),
            UINode::Block {
                title: Some("test 2".to_string()),
                widget: UIWidget::Paragraph {
                    text: vec![UITag(3)],
                },
            },
        );

        elements.insert(
            UITag(5),
            UINode::Layout {
                children: vec![UITag(2), UITag(4)],
            },
        );

        UIManager {
            terminal,
            elements,
            root_node: Some(UITag(5)),
        }
    }

    // pub fn create_view(&mut self, react_tag: UITag, root_tag: UITag, view_spec: UIWidget) {}

    // pub fn update_view(&mut self, react_tag: UITag, view_spec: UIWidget) {}

    pub fn render(&mut self) -> Result<()> {
        let UIManager {
            terminal,
            elements,
            root_node,
        } = self;

        terminal
            .draw(|f| {
                let size = f.size();

                if let Some(root_node) = root_node.as_ref() {
                    let root = elements.get(root_node).unwrap();

                    root.render(f, size, &elements);
                };
            })
            .map_err(|e| e.into())
    }
}
