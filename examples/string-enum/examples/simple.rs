use string_enum::make_enum;

make_enum! {MyEnum: "first", "second", "third", "fourth"}

fn main() {
    assert_eq!(MyEnum::First.as_str(), "first");
    assert_eq!(MyEnum::Second.as_str(), "second");
}
