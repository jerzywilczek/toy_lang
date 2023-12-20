fn main() {
    toy_lang::parser::parse(include_str!("../code_samples/simple_program.tl")).unwrap();
}
