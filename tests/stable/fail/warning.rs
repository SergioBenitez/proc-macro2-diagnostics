use diagnostic_example::{diagnostic_expr, diagnostic_item};

diagnostic_item! { warning: caution }

fn main() {
    let _: () = diagnostic_expr! { warning: caution };
}
