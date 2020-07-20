use diagnostic_example::diagnostic;

fn main() { }

diagnostic! {
    error: this is an error message,
}

diagnostic! {
    error: this is an error message,
    _note: but it has a note,
}

diagnostic! {
    error: hello,
    warning: just wanted to warn you,
    note: just jotting things down,
    _help: please help on that note,
    _note: notes on notes,
}

diagnostic! {
    warning: hello not found,
    _note: just a note to say hi,
    _help: its a good idea to say hello,
}

diagnostic! {
    hello: hello,
}

diagnostic! {
    help: this is standalone help,
    _note: but we add things to it,
    _help: like more help,
}
