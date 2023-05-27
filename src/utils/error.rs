use chumsky::prelude::{extra, Rich, SimpleSpan};
use crate::utils::types::{PrimitiveType, Type};

pub type Span = SimpleSpan<usize>;
pub type ParserError<'a, T> = extra::Err<Rich<'a, T, Span>>;
pub type Spanned<T> = (T, Span);
pub type Error = Rich<'static, String, Span>;

pub fn get_code(reason_str: &String) -> String {
    let mut raw = reason_str.split('$');
    if let Some(code) = raw.next() {
        if let Ok(num) = u8::from_str_radix(code, 16) {
            match num {
                0x00..=0x3A => code.to_string(),
                _ => "XX".to_string()
            }
        } else {
            'X'.to_string()
        }
    }
    else {
        'X'.to_string()
    }
}

pub fn get_msg(reason_str: &String) -> String {
    let mut raw = reason_str.split('$');
    if let Some(code) = raw.next() {
        let default = format!("error code {} wrong: argument 1", code);
        let default2 = format!("error code {} wrong: argument 2", code);
        let default3 = format!("error code {} wrong: argument 3", code);
        match code {
            "00" => raw.next().map_or(default, |name| 
                    format!("undefined type {}", name)),
            "01" => raw.next().map_or(default, |ty|
                    format!("type of condition is {} but expected bool", ty)),
            "02" => raw.next().map_or(default, |ty_then| 
                    raw.next().map_or(default2, |ty_els|
                    format!("type of then branch is {} and the type of else branch is {}", ty_then, ty_els))),
            "03" => raw.next().map_or(default, |exp_ty|
                    raw.next().map_or(default2, |lit_ty|
                    format!("invalid pattern type, expected {}, got {}", exp_ty, lit_ty))),
            "04" => raw.next().map_or(default, |exp_ty|
                    format!("invalid pattern type, expected tuple, got {}", exp_ty)),
            "05" => raw.next().map_or(default, |tys_len|
                    raw.next().map_or(default2, |pats_len|
                    format!("invalid pattern type, expected tuple of size {}, got {}", tys_len, pats_len))),
            "06" => raw.next().map_or(default, |exp_ty|
                    raw.next().map_or(default2, |got_ty|
                    format!("invalid pattern type, expected {}, got {}", exp_ty, got_ty))),
            "07" => raw.next().map_or(default, |exp_ty|
                    raw.next().map_or(default2, |got_ty|
                    format!("invalid pattern type, expected {}, got {}", exp_ty, got_ty))),
            "08" => raw.next().map_or(default, |ty|
                    format!("enum {} not found", ty)),
            "09" => raw.next().map_or(default, |name|
                    format!("ctor {} not found in enum", name)),
            "0A" => raw.next().map_or(default, |exp_flen|
                    raw.next().map_or(default2, |got_flen|
                    format!("invalid number of fields, expected {}, got {}", exp_flen, got_flen))),
            "0B" => format!("expected nameless fields"),
            "0C" => raw.next().map_or(default, |exp_flen|
                    raw.next().map_or(default2, |got_flen|
                    format!("invalid number of fields, expected {}, got {}", exp_flen, got_flen))),
            "0D" => raw.next().map_or(default, |field|
                    format!("field {} not found in enum", field)),
            "0E" => format!("expected named fields"),
            "0F" => format!("expected no fields"),
            "10" => raw.next().map_or(default, |var| format!("undefined variable {}", var)),
            "11" => format!("empty array is not allowed"),
            "12" => format!("array elements must have the same type"),
            "13" => raw.next().map_or(default, |op|
                    raw.next().map_or(default2, |ty_lhs|
                    raw.next().map_or(default3, |ty_rhs|
                    format!("invalid types for binary operator {} between {} and {}", op, ty_lhs, ty_rhs)))),
            "14" => raw.next().map_or(default, |op|
                    raw.next().map_or(default2, |ty_rhs|
                    format!("invalid types for unary operator {} on {}", op, ty_rhs))),
            "15" => raw.next().map_or(default, |params_len|
                    raw.next().map_or(default2, |args_len|
                    format!("invalid number of arguments, expected {}, got {}", params_len, args_len))),
            "16" => raw.next().map_or(default, |param_ty|
                    raw.next().map_or(default2, |arg_ty|
                    format!("invalid argument type, expected {}, got {}", param_ty, arg_ty))),
            "17" => raw.next().map_or(default, |func_ty|
                    format!("invalid function type {}", func_ty)),
            "18" => raw.next().map_or(default, |index_ty|
                    format!("invalid index type {}", index_ty)),
            "19" => raw.next().map_or(default, |array_ty|
                    format!("invalid array type {}", array_ty)),
            "1A" => raw.next().map_or(default, |ty|
                    format!("type {} not found", ty)),
            "1B" => raw.next().map_or(default, |ctor|
                    format!("constructor {} not found", ctor)),
            "1C" => raw.next().map_or(default, |ctor_flen|
                    raw.next().map_or(default2, |got_flen|
                    format!("invalid number of fields, expected {}, got {}", ctor_flen, got_flen))),
            "1D" => raw.next().map_or(default, |field|
                    format!("field {} already set", field)),
            "1E" => raw.next().map_or(default, |exp_fty|
                    raw.next().map_or(default2, |got_fty|
                    format!("invalid field type, expected {}, got {}", exp_fty, got_fty))),
            "1F" => raw.next().map_or(default, |var|
                    format!("variable {} not found", var)),
            "20" => raw.next().map_or(default, |exp_fty|
                    raw.next().map_or(default2, |got_fty|
                    format!("invalid field type, expected {}, got {}", exp_fty, got_fty))),
            "21" => raw.next().map_or(default, |fname|
                    format!("invalid field name {}", fname)),
            "22" => format!("expected named fields"),
            "23" => raw.next().map_or(default, |ctor_flen|
                    raw.next().map_or(default2, |got_flen|
                    format!("invalid number of fields, expected {}, got {}", ctor_flen, got_flen))),
            "24" => raw.next().map_or(default, |exp_fty|
                    raw.next().map_or(default2, |got_fty|
                    format!("invalid field type, expected {}, got {}", exp_fty, got_fty))),
            "25" => format!("expected unnamed fields"),
            "26" => format!("expected no fields"),
            "27" => raw.next().map_or(default, |ty|
                    format!("can't match type {}", ty)),
            "28" => raw.next().map_or(default, |exp_ty|
                    raw.next().map_or(default2, |got_ty|
                    format!("invalid arm type, expected {}, got {}", exp_ty, got_ty))),
            "29" => format!("pattern not exhaustive"),
            "2A" => format!("expected at least one match arm"),
            "2B" => raw.next().map_or(default, |ret_ty|
                    raw.next().map_or(default2, |got_ty|
                    format!("invalid return type, expected {}, got {}", ret_ty, got_ty))),
            "2C" => raw.next().map_or(default, |exp_ty|
                    raw.next().map_or(default2, |got_ty|
                    format!("invalid rhs type, expected {}, got {}", exp_ty, got_ty))),
            "2D" => raw.next().map_or(default, |got_ty|
                    format!("invalid condition type, expected {}, got {}", Type::Primitive(PrimitiveType::Bool), got_ty)),
            "2E" => raw.next().map_or(default, |got_ty|
                    format!("invalid body type, expected no type, got {}", got_ty)),
            "2F" => raw.next().map_or(default, |got_ty|
                    format!("invalid start type, expected {}, got {}", Type::Primitive(PrimitiveType::Int), got_ty)),
            "30" => raw.next().map_or(default, |got_ty|
                    format!("invalid end type, expected {}, got {}", Type::Primitive(PrimitiveType::Int), got_ty)),
            "31" => raw.next().map_or(default, |got_ty|
                    format!("invalid body type, expected no type, got {}", got_ty)),
            "32" => raw.next().map_or(default, |ret_ty|
                    raw.next().map_or(default2, |got_ty|
                    format!("invalid return type, expected {}, got {}", ret_ty, got_ty))),
            "33" => raw.next().map_or(default, |var|
                    format!("undefined variable {}", var)),
            "34" => raw.next().map_or(default, |exp_ty|
                    raw.next().map_or(default2, |got_ty|
                    format!("invalid rhs type, expected {}, got {}", exp_ty, got_ty))),
            "35" => raw.next().map_or(default, |ret_ty|
                    raw.next().map_or(default2, |got_ty|
                    format!("invalid return type, expected {}, got {}", ret_ty, got_ty))),
            "36" => raw.next().map_or(default, |name|
                    format!("enum {} already defined", name)),
            "37" => raw.next().map_or(default, |name|
                    format!("cannot use a primitive type {}", name)),
            "38" => raw.next().map_or(default, |name|
                    format!("function {} already defined", name)),
            "39" => raw.next().map_or(default, |ctor_name|
                    raw.next().map_or(default2, |enum_name|
                    format!("ctor {} already defined in enum {}", ctor_name, enum_name))),
            "3A" => raw.next().map_or(default, |name|
                    format!("enum {} must have at least one ctor", name)),
            _ => format!("unknown error {}", reason_str).to_string(),
        }
    }
    else {
        "wrong error!".to_string()
    }
}