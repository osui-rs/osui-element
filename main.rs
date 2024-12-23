use osui_element::component;

pub type Element = i32;

pub trait Component {
    fn create_element(self) -> Element;
}

#[component]
pub fn MyComponent<'a>(age: &'a str) {
    0
}

fn main() {}
