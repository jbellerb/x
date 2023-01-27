use crate::ui::{widget::UIWidget, UITag};

use fnv::FnvHashMap;
use tui::{
    backend::Backend,
    layout::{Constraint, Layout, Rect},
    widgets::{Block, Borders},
    Frame,
};

#[derive(Debug)]
pub enum UINode {
    Layout {
        children: Vec<UITag>,
    },
    Block {
        title: Option<String>,
        widget: UIWidget,
    },
    Text {
        text: String,
    },
}

impl UINode {
    pub fn render<B: Backend>(
        &self,
        f: &mut Frame<B>,
        layout: Rect,
        elements: &FnvHashMap<UITag, UINode>,
    ) {
        match self {
            UINode::Layout { children } => {
                let chunks = Layout::default()
                    .constraints(
                        children
                            .iter()
                            .map(|_| Constraint::Ratio(1, children.len() as u32))
                            .collect::<Vec<Constraint>>(),
                    )
                    .split(layout);

                for (i, child) in children.iter().enumerate() {
                    let child = elements.get(child).unwrap();
                    child.render(f, chunks[i], elements);
                }
            }
            UINode::Block { title, widget } => {
                let mut block = Block::default().borders(Borders::ALL);

                if let Some(title) = title {
                    block = block.title(title.clone());
                }

                f.render_widget(widget.render(block, elements), layout);
            }
            _ => panic!("Text nodes can't be rendered individually!"),
        }
    }
}
