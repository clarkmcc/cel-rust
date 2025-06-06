pub fn find_operator(input: &str) -> Option<&str> {
    for (op, operator) in OPERATORS {
        if op == input {
            return Some(operator);
        }
    }
    None
}

pub const CONDITIONAL: &'static str = "_?_:_";
pub const LOGICAL_AND: &'static str = "_&&_";
pub const LOGICAL_OR: &'static str = "_||_";
pub const LOGICAL_NOT: &'static str = "!_";
pub const SUBSTRACT: &'static str = "_-_";
pub const ADD: &'static str = "_+_";
pub const MULTIPLY: &'static str = "_*_";
pub const DIVIDE: &'static str = "_/_";
pub const MODULO: &'static str = "_%_";
pub const EQUALS: &'static str = "_==_";
pub const NOT_EQUALS: &'static str = "_!=_";
pub const GREATER_EQUALS: &'static str = "_>=_";
pub const LESS_EQUALS: &'static str = "_<=_";
pub const GREATER: &'static str = "_>_";
pub const LESS: &'static str = "_<_";
pub const NEGATE: &'static str = "-_";
pub const INDEX: &'static str = "_[_]";
pub const OPT_INDEX: &'static str = "_[?_]";
pub const OPT_SELECT: &'static str = "_?._";

pub const EXISTS_ONE: &'static str = "exists_one";
pub const HAS: &'static str = "has";
pub const ALL: &'static str = "all";
pub const EXISTS: &'static str = "exists";
pub const MAP: &'static str = "map";
pub const FILTER: &'static str = "filter";

pub const NOT_STRICTLY_FALSE: &'static str = "@not_strictly_false";
pub const IN: &'static str = "@in";

const OPERATORS: [(&str, &str); 12] = [
    ("-", SUBSTRACT),
    ("+", ADD),
    ("*", MULTIPLY),
    ("/", DIVIDE),
    ("%", MODULO),
    ("==", EQUALS),
    ("!=", NOT_EQUALS),
    (">=", GREATER_EQUALS),
    ("<=", LESS_EQUALS),
    (">", GREATER),
    ("<", LESS),
    ("in", IN),
];
