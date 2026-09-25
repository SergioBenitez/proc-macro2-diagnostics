#[test]
fn main() {
    let nightly = version_check::is_feature_flaggable().unwrap_or(false);
    let suite = match (nightly, cfg!(feature = "nightly")) {
        (false, _) => "stable",
        (true, false) => "nightly-stable",
        (true, true) => "nightly",
    };

    let t = trybuild::TestCases::new();
    t.compile_fail(format!("tests/{suite}/fail/*.rs"));
    t.pass(format!("tests/{suite}/pass/*.rs"));
}
