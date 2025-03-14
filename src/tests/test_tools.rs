use crate::P;


pub fn into_oneliner(test_name: &str) -> String {
    let mut prefix = String::new();
    let mut test_name_ = String::new();
    let mut found_vertical: bool = false;
    for c in test_name.chars() {
        if found_vertical { test_name_ += c.to_string().as_str(); } else { prefix += c.to_string().as_str(); }
        if c == '|' {found_vertical = true;}
    }
    let test_name = test_name_;
    let test_name = " ".to_string() + test_name.trim_start();
    let line_count = test_name.lines().count();
    if line_count <= 1 { return prefix + test_name.as_str() };
    let test_name = test_name.lines().next().unwrap_or("").to_string() + "...";
    return prefix + test_name.as_str();
}