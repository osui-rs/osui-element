use osui_element::component;

type Element = i32;

pub trait Component: std::fmt::Debug {
    fn create_element(&self) -> Element;
}

#[component]
fn testing() {
    20
}

fn main() {}
