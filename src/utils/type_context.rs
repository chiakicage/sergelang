use crate::ast::ast::TypeStr;
use crate::utils::error::{Error, Span, Spanned};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use slotmap::{new_key_type, SlotMap};
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

new_key_type! {
    pub struct TypeRef;
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash, IntoPrimitive, TryFromPrimitive, EnumIter)]
#[repr(usize)]
pub enum PrimitiveType {
    Int = 0,
    Float = 1,
    Bool = 2,
    Char = 3,
    String = 4,
    Unit = 5,
}

#[derive(Debug, Clone)]
pub enum FieldsType {
    UnnamedFields(Vec<TypeRef>),
    NamedFields(HashMap<String, (TypeRef, usize)>),
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub ctors: HashMap<String, (Option<FieldsType>, usize)>,
}

impl PartialEq for Enum {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Enum {}
impl Hash for Enum {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Primitive(PrimitiveType),
    Enum(Enum),
    Opaque(String),
    Callable { params: Vec<TypeRef>, ret: TypeRef },
    // Reference(TypeReference),
    Tuple(Vec<TypeRef>),
    Array(TypeRef),
}

impl From<PrimitiveType> for Type {
    fn from(ty: PrimitiveType) -> Self {
        Type::Primitive(ty)
    }
}

impl From<Enum> for Type {
    fn from(ty: Enum) -> Self {
        Type::Enum(ty)
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::Int => write!(f, "i32"),
            PrimitiveType::Float => write!(f, "f64"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::Char => write!(f, "char"),
            PrimitiveType::String => write!(f, "str"),
            PrimitiveType::Unit => write!(f, "unit"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeContext {
    pub name_ref_map: HashMap<String, TypeRef>,
    pub type_ref_map: HashMap<Type, TypeRef>,
    pub prim_ref_map: [TypeRef; 6],
    pub types: SlotMap<TypeRef, Type>,
}

impl Default for TypeContext {
    fn default() -> Self {
        let mut ctx = Self {
            name_ref_map: Default::default(),
            type_ref_map: Default::default(),
            prim_ref_map: Default::default(),
            types: Default::default(),
        };
        ctx.add_primitives();
        ctx
    }
}

impl TypeContext {
    pub fn to_string(&self, ty: &Type) -> String {
        match ty {
            Type::Primitive(ty) => ty.to_string(),
            Type::Enum(ty) => ty.name.clone(),
            Type::Opaque(name) => name.clone(),
            Type::Callable { params, ret } => {
                let mut s = String::new();
                for (i, param) in params.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&self.typeref_to_string(*param));
                }
                s.push_str(") -> ");
                s.push_str(&self.typeref_to_string(*ret));
                s
            }
            Type::Tuple(types) => {
                let mut s = String::new();
                s.push_str("(");
                for (i, ty) in types.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&self.typeref_to_string(*ty));
                }
                s.push_str(")");
                s
            }
            Type::Array(ty) => {
                let mut s = String::new();
                s.push_str("[");
                s.push_str(&self.typeref_to_string(*ty));
                s.push_str("]");
                s
            }
        }
    }
    pub fn typeref_to_string(&self, ty: TypeRef) -> String {
        self.to_string(&self.types[ty])
    }

    pub fn add_primitives(&mut self) {
        for ty in PrimitiveType::iter() {
            let type_ref = self.types.insert(ty.into());
            self.type_ref_map.insert(ty.into(), type_ref);
            // self.type_ref_map
            //     .insert(TypeReference::Named(ty.to_string()).into(), type_ref);
            self.name_ref_map.insert(ty.to_string(), type_ref);
            self.prim_ref_map[ty as usize] = type_ref;
        }
    }
    pub fn get_primitive(&self, name: &'static str) -> TypeRef {
        match name {
            "i32" => self.prim_ref_map[PrimitiveType::Int as usize],
            "f64" => self.prim_ref_map[PrimitiveType::Float as usize],
            "bool" => self.prim_ref_map[PrimitiveType::Bool as usize],
            "char" => self.prim_ref_map[PrimitiveType::Char as usize],
            "str" => self.prim_ref_map[PrimitiveType::String as usize],
            "unit" => self.prim_ref_map[PrimitiveType::Unit as usize],
            _ => panic!("unknown primitive type"),
        }
    }

    pub fn get_i32(&self) -> TypeRef {
        self.get_primitive("i32")
    }
    pub fn get_f64(&self) -> TypeRef {
        self.get_primitive("f64")
    }
    pub fn get_bool(&self) -> TypeRef {
        self.get_primitive("bool")
    }
    pub fn get_char(&self) -> TypeRef {
        self.get_primitive("char")
    }
    pub fn get_str(&self) -> TypeRef {
        self.get_primitive("str")
    }
    pub fn get_unit(&self) -> TypeRef {
        self.get_primitive("unit")
    }
    pub fn enum_type(&mut self, ty: Enum) -> TypeRef {
        let type_ref = self.types.insert(ty.clone().into());
        // self.type_ref_map.insert(ty.clone().into(), type_ref);
        // self.name_ref_map.insert(ty.name.clone(), type_ref);
        type_ref
    }
    pub fn opaque_type(&mut self, name: String) -> TypeRef {
        let type_ref = self.types.insert(Type::Opaque(name.clone()));
        // self.type_ref_map
        //     .insert(TypeReference::Named(name.clone()).into(), type_ref);
        self.name_ref_map.insert(name, type_ref);
        type_ref
    }
    pub fn func_type(&mut self, params: Vec<TypeRef>, ret: TypeRef) -> TypeRef {
        // let type_ref = self.types.insert(Type::Callable { params, ret });
        let ty = Type::Callable { params, ret };
        self.get_or_insert_type(ty)
    }

    pub fn tuple_type(&mut self, types: Vec<TypeRef>) -> TypeRef {
        let ty = Type::Tuple(types);
        self.get_or_insert_type(ty)
    }
    pub fn array_type(&mut self, ty: TypeRef) -> TypeRef {
        let ty = Type::Array(ty);
        self.get_or_insert_type(ty)
    }

    pub fn is_array(&self, ty: TypeRef) -> bool {
        if let Type::Array(_) = &self.types[ty] {
            return true;
        }
        false
    }
    
    pub fn get_or_insert_type(&mut self, ty: Type) -> TypeRef {
        if let Some(ty_ref) = self.type_ref_map.get(&ty) {
            return *ty_ref;
        }
        let ty_ref = self.types.insert(ty.clone());
        self.type_ref_map.insert(ty, ty_ref);
        ty_ref
    }
    pub fn get_enum_by_typeref(&self, key: TypeRef) -> Option<&Enum> {
        if let Type::Enum(ty) = &self.types[key] {
            return Some(ty);
        } else {
            return None;
        }
    }
    pub fn get_typeref_by_type(&self, ty: Type) -> Option<TypeRef> {
        self.type_ref_map.get(&ty).copied()
    }
    pub fn get_typeref_by_name(&self, name: &str) -> Option<TypeRef> {
        self.name_ref_map.get(name).copied()
    }
    pub fn get_type_by_typeref(&self, key: TypeRef) -> Type {
        self.types[key].clone()
    }

    fn subsititute_all_type(&mut self, src: TypeRef, target: TypeRef) {
        for ty in self.name_ref_map.values_mut() {
            if *ty == src {
                *ty = target;
            }
        }
        let mut subsititute_tys = Vec::new();
        for ty in self.types.values_mut() {
            let src_ty = ty.clone();
            match ty {
                Type::Primitive(_) => {}
                Type::Enum(Enum { ctors, .. }) => {
                    for (name, (fields, _)) in ctors {
                        if let Some(fields) = fields {
                            match fields {
                                FieldsType::UnnamedFields(fields) => {
                                    for ty in fields {
                                        if *ty == src {
                                            *ty = target;
                                        }
                                    }
                                }
                                FieldsType::NamedFields(fields) => {
                                    for (ty, _) in fields.values_mut() {
                                        if *ty == src {
                                            *ty = target;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Type::Opaque(_) => {}
                Type::Callable { params, ret } => {
                    for ty in params {
                        if *ty == src {
                            *ty = target;
                        }
                    }
                    if *ret == src {
                        *ret = target;
                    }
                }
                Type::Tuple(types) => {
                    for ty in types {
                        if *ty == src {
                            *ty = target;
                        }
                    }
                }
                Type::Array(ty) => {
                    if *ty == src {
                        *ty = target;
                    }
                }
            }
            let target_ty = ty.clone();
            if src_ty != target_ty {
                subsititute_tys.push((src_ty, target_ty));
            }
        }
        for (src_ty, target_ty) in subsititute_tys.iter() {
            let tyref = self.type_ref_map.remove(src_ty).unwrap();
            self.type_ref_map.insert(target_ty.clone(), tyref);
        }
    }

    pub fn refine_all_type(&mut self) {
        let mut subsititute_refs = Vec::new();
        for (key, ty) in self.types.iter() {
            match ty {
                Type::Enum(Enum { name, .. }) => {
                    let opaque_key = self.get_typeref_by_name(name).unwrap();
                    subsititute_refs.push((opaque_key, key));
                    self.name_ref_map.insert(name.clone(), key);
                }
                _ => {}
            }
        }
        for (opaque_key, key) in subsititute_refs {
            self.subsititute_all_type(opaque_key, key);
            self.types.remove(opaque_key);
        }
    }

    pub fn convert_type_str(&mut self, ty: &TypeStr) -> Result<TypeRef, Error> {
        match ty {
            TypeStr::Named((s, span)) => {
                if *s == "unit" {
                    return Err(Error::custom(*span, "unit type is not allowed".to_string()));
                }
                self.get_typeref_by_name(s)
                    .ok_or(Error::custom(*span, format!("unknown type {}", s)))
            }
            TypeStr::Func(params, ret) => {
                let params = params
                    .iter()
                    .map(|(t, _)| self.convert_type_str(t))
                    .collect::<Result<Vec<_>, _>>()?;
                let ret = self.convert_type_str(&ret.0)?;
                let ty = Type::Callable { params, ret };
                Ok(self.get_or_insert_type(ty))
            }
            TypeStr::Tuple(tys) => {
                let tys = tys
                    .iter()
                    .map(|(t, _)| self.convert_type_str(t))
                    .collect::<Result<Vec<_>, _>>()?;
                let ty = Type::Tuple(tys);
                Ok(self.get_or_insert_type(ty))
            }
            TypeStr::Array(ty) => {
                let ty = self.convert_type_str(&ty.0)?;
                Ok(self.get_or_insert_type(Type::Array(ty)))
            }
        }
    }

    pub fn check_ty_can_be_matched(&self, ty: TypeRef) -> bool {
        match &self.types[ty] {
            Type::Primitive(PrimitiveType::Unit) => false,
            Type::Primitive(_) => true,
            Type::Enum(_) => true,
            Type::Callable { .. } => false,
            Type::Tuple(_) => true,
            Type::Array(_) => false,
            Type::Opaque(_) => unreachable!(),
        }
    }

}
