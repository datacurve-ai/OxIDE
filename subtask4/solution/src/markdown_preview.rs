// src/markdown_preview.rs

use askama::Template;
use pulldown_cmark::{html, Options, Parser};
use std::sync::{Arc, Mutex};
use warp::Filter;

/// Template for rendering the Markdown preview in the browser.
#[derive(Template)]
#[template(path = "preview.html", escape = "none")]
struct PreviewTemplate<'a> {
    content: &'a str,
}

/// Manages the Markdown preview functionality.
#[derive(Clone)]
pub struct MarkdownPreview {
    pub is_active: bool,
    html_content: Arc<Mutex<String>>,
}

impl MarkdownPreview {
    /// Creates a new MarkdownPreview instance.
    pub fn new() -> Self {
        MarkdownPreview {
            is_active: false,
            html_content: Arc::new(Mutex::new(String::new())),
        }
    }

    /// Updates the HTML content based on the provided Markdown text.
    pub fn update_content(&self, markdown_text: &str) {
        let mut options = Options::empty();
        options.insert(Options::ENABLE_TABLES | Options::ENABLE_FOOTNOTES);

        let parser = Parser::new_ext(markdown_text, options);
        let mut html_output = String::new();
        html::push_html(&mut html_output, parser);

        let mut content = self.html_content.lock().unwrap();
        *content = html_output;
    }

    /// Serves the Markdown preview via an HTTP server.
    pub async fn serve(self) -> Result<(), Box<dyn std::error::Error>> {
        let html_content = self.html_content.clone();
        let routes = warp::path::end().map(move || {
            let content = html_content.lock().unwrap();
            let template = PreviewTemplate { content: &content };
            warp::reply::html(template.render().unwrap())
        });

        // Serve static files (e.g., CSS)
        let static_files = warp::path("static").and(warp::fs::dir("static/"));

        let routes = routes.or(static_files);

        warp::serve(routes).run(([127, 0, 0, 1], 3030)).await;

        Ok(())
    }
}
