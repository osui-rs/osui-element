use osui_element::component;

pub type Element = i32;

pub trait Component: std::fmt::Debug {
    type Element;
    fn create_element(self) -> Self::Element;
}

#[component]
pub fn MyComponent<'a>(age: &'a str) {
    0
}

fn main() {}
