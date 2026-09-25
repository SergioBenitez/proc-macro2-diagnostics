use std::fmt;

use crate::Level;

#[derive(Copy, Clone, PartialEq)]
enum Kind {
    New,
    Joined,
}

impl Kind {
    fn split(self) -> (&'static str, &'static str) {
        match self {
            Kind::New => ("[", "] "),
            Kind::Joined => ("= ", ": "),
        }
    }
}

pub struct Line<'a> {
    pub level: Level,
    pub msg: &'a str,
    kind: Kind
}

impl<'a> Line<'a> {
    pub fn new(level: Level, msg: &'a str) -> Line<'a> {
        Line { kind: Kind::New, level, msg }
    }

    pub fn joined(level: Level, msg: &'a str) -> Line<'a> {
        Line { kind: Kind::Joined, level, msg }
    }

    pub fn is_new(&self) -> bool {
        self.kind == Kind::New
    }

    fn parse_kind(kind: Kind, string: &str) -> Option<Line<'_>> {
        let string = string.trim_start();
        let (prefix, suffix) = kind.split();
        if !string.starts_with(prefix) {
            return None;
        }

        let end = string.find(suffix)?;
        let level: Level = string[prefix.len()..end].parse().ok()?;
        let msg = &string[end + suffix.len()..];
        Some(Line { level, msg, kind })
    }

    pub fn parse(string: &str) -> Option<Line<'_>> {
        Line::parse_kind(Kind::Joined, string)
            .or_else(|| Line::parse_kind(Kind::New, string))
    }
}

impl fmt::Display for Line<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (prefix, suffix) = self.kind.split();
        write!(f, "{}{}{}{}", prefix, self.level, suffix, self.msg)
    }
}
