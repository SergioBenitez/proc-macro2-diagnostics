use diagnostic_example::diagnostic_expr;

diagnostic_expr! { error: single }

diagnostic_expr! {
    error: first,
    note: second,
    _help: more detail,
    warning: third,
}

fn main() {
    let _: u8 = diagnostic_expr! {
        error: first,
        note: second,
        _help: more detail,
        warning: third,
    };
}
