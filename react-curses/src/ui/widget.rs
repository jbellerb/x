use crate::ui::{node::UINode, UITag};

use fnv::FnvHashMap;
use tui::{
    text::{Span, Spans},
    widgets::{Block, Paragraph, Widget},
};

#[derive(Debug)]
pub enum UIWidget {
    Paragraph { text: Vec<UITag> },
}

impl UIWidget {
    pub fn render<'a>(
        &self,
        block: Block<'a>,
        elements: &'a FnvHashMap<UITag, UINode>,
    ) -> impl Widget + 'a {
        match self {
            UIWidget::Paragraph { text } => {
                let contents = text
                    .iter()
                    .map(|node| {
                        let node = elements.get(node).unwrap();
                        if let UINode::Text { text } = node {
                            return Span::raw(text);
                        }

                        panic!("Non-text nodes in paragraph");
                    })
                    .collect::<Vec<Span>>();

                Paragraph::new(Spans::from(contents)).block(block)
            }
        }
    }
}
