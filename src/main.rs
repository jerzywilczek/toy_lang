fn main() {
    let src = r#"
        module simple;

        fn test(i: i32) {
            return (-i + 1) * 2 + 3;
        }
    "#;

    let result = toy_parser::parse(src).unwrap();

    println!("{:#?}", result);
}
