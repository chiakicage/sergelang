/// The basic design of middle level IR - MIR, follows the MIR of rust
use slotmap::{new_key_type, SlotMap};
use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::Write;
use std::{panic, vec};

use crate::midend::typed_ast::*;
use crate::utils::type_context::*;

use rpds::HashTrieMap;

type SymTable<K, V> = HashTrieMap<K, V>;

type BinOp = crate::ast::BinOp;
type UnOp = crate::ast::UnOp;

new_key_type! {
    pub struct VarRef;
    pub struct BlockRef;
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub typ: TypeRef,
    pub blocks: SlotMap<BlockRef, Block>,
    pub variables: SlotMap<VarRef, Var>,

    pub params: Vec<VarRef>,
    pub captured: Vec<VarRef>,

    // corresponds to MIR Locals
    pub locals: Vec<VarRef>,
    // Intermediate Rvalues
    pub temporaries: Vec<VarRef>,

    pub return_value: Option<VarRef>,

    pub entry: BlockRef,
    pub exit: BlockRef,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Var {
    pub name: String,
    pub typ: TypeRef,
}

// MIR Basic Block
#[derive(Debug, Clone)]
pub struct Block {
    pub name: String,
    pub stmts: Vec<Stmt>,
    pub terminator: Terminator,
}

#[derive(Debug, Default, Clone)]
pub struct Stmt {
    pub left: Option<VarRef>,
    pub right: Option<Rvalue>,
}

// MIR Terminator
#[derive(Debug, Clone)]
pub enum Terminator {
    Branch(Operand, BlockRef, BlockRef),
    Jump(BlockRef),
    Return,
}

#[derive(Debug, Clone)]
pub struct Operand {
    pub typ: TypeRef,
    pub val: Box<OperandEnum>,
}

#[derive(Debug, Clone)]
pub enum OperandEnum {
    Imm(i32),
    Literal(LiteralKind),
    Var(VarRef),
}

impl From<VarRef> for OperandEnum {
    fn from(var: VarRef) -> Self {
        Self::Var(var)
    }
}

#[derive(Debug, Clone)]
pub struct Rvalue {
    pub typ: TypeRef,
    pub val: Box<RvalueEnum>,
}

#[derive(Debug, Clone)]
pub enum RvalueEnum {
    BinaryOperator(BinOp, Operand, Operand),
    UnaryOperator(UnOp, Operand),
    Call(String, Vec<Operand>),
    Operand(Operand),
    UnboxedOperand(Operand),
    // Intrinsic(&'static str, Vec<Operand>),
    Index(Operand, Operand),
    MakeTuple(Vec<Operand>),
    Construct(usize, Vec<Operand>),
    ExtractTupleField(Operand, usize), // get the i-th field of a tuple operand
    ExtractEnumField(Operand, usize),  // get the i-th field of a enum operand
    ExtractEnumTag(Operand),
}

impl From<Operand> for Rvalue {
    fn from(op: Operand) -> Self {
        Self {
            typ: op.typ,
            val: Box::new(RvalueEnum::Operand(op)),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct MIR {
    pub ty_ctx: TypeContext,
    pub name_ctx: SymTable<String, TypeRef>,
    pub module: Vec<Func>,
}

struct BlockNamer {
    prefix: String,
    counter: usize,
}

fn mangling(name: &str) -> String {
    match name {
        "main" => "__serge_user_main".to_string(),
        "print" | "println" | "read_i32" => format!("__serge_{}", name),
        "len" => "__serge_array_length".to_string(),

        _ => name.to_string(),
    }
}

impl BlockNamer {
    fn new(prefix: &str) -> Self {
        Self {
            prefix: prefix.to_string(),
            counter: 0,
        }
    }

    fn get(&mut self, name: Option<&str>) -> String {
        let name = format!(
            "{}_{}_{}",
            self.prefix,
            name.unwrap_or("block"),
            self.counter
        );
        self.counter += 1;
        name
    }
}

struct FuncBuilder<'ctx> {
    ext_name_ctx: SymTable<String, TypeRef>,
    name_ctx: SymTable<String, TypeRef>,
    name_var_map: SymTable<String, VarRef>,
    ty_ctx: &'ctx TypeContext,
    func: Func,
    position: BlockRef,
    namer: BlockNamer,
    break_block: Option<BlockRef>,
    continue_block: Option<BlockRef>,
}

impl<'ctx> FuncBuilder<'ctx> {
    pub fn new(
        func: &TypedFunc,
        ext_name_ctx: SymTable<String, TypeRef>,
        name_var_map: SymTable<String, VarRef>,
        ty_ctx: &'ctx TypeContext,
        prefix: &str,
    ) -> Self {
        let mut blocks = SlotMap::with_key();
        let exit = blocks.insert(Block {
            name: format!("{}_exit", prefix),
            stmts: Vec::new(),
            terminator: Terminator::Return,
        });
        let entry = blocks.insert(Block {
            name: format!("{}_entry", prefix),
            stmts: Vec::new(),
            terminator: Terminator::Jump(exit),
        });
        let mut variables = SlotMap::with_key();
        let mut name_ctx = SymTable::new();
        let mut name_var_map = name_var_map.clone();
        let mut locals = Vec::new();
        let mut temporaries = Vec::new();
        let params = func
            .params
            .iter()
            .map(|param| {
                let var = Var {
                    name: param.0.clone(),
                    typ: param.1,
                };
                let var = variables.insert(var);
                name_ctx = name_ctx.insert(param.0.clone(), param.1);
                name_var_map = name_var_map.insert(param.0.clone(), var);
                locals.push(var);
                var
            })
            .collect::<Vec<_>>();

        let return_value = if func.return_ty == ty_ctx.get_unit() {
            None
        } else {
            let var = Var {
                name: "return".to_string(),
                typ: func.return_ty,
            };
            let var = variables.insert(var);
            temporaries.push(var);
            Some(var)
        };

        let mut func = Func {
            name: mangling(func.name.as_str()),
            typ: func.ty,
            blocks,
            variables,
            params,
            captured: Vec::new(),
            locals,
            temporaries,
            return_value,
            entry,
            exit,
        };

        let namer = BlockNamer::new(prefix);

        FuncBuilder {
            ext_name_ctx,
            name_ctx,
            name_var_map,
            ty_ctx,
            func,
            position: entry,
            namer,
            break_block: None,
            continue_block: None,
        }
    }
    fn create_variable(&mut self, name: Option<&str>, ty: TypeRef) -> VarRef {
        let var = Var {
            name: name.map_or("tmp".to_string(), |s| s.to_string()),
            typ: ty,
        };
        let var = self.func.variables.insert(var);
        if let Some(name) = name {
            self.name_ctx = self.name_ctx.insert(name.to_string(), ty);
            self.name_var_map = self.name_var_map.insert(name.to_string(), var);
            self.func.locals.push(var);
        } else {
            self.func.temporaries.push(var);
        }
        var
    }

    fn create_block(&mut self, name: Option<&str>) -> BlockRef {
        let name = self.namer.get(name);
        let block = Block {
            name,
            stmts: Vec::new(),
            terminator: Terminator::Jump(self.func.exit),
        };
        self.func.blocks.insert(block)
    }

    fn current_block(&mut self) -> &mut Block {
        &mut self.func.blocks[self.position]
    }

    fn add_stmt_to_current_block(&mut self, stmt: Stmt) {
        self.current_block().stmts.push(stmt);
    }
    fn terminate_current_block(&mut self, terminator: Terminator, new_block: bool) {
        self.current_block().terminator = terminator;
        if new_block {
            self.position = self.create_block(None);
        }
    }

    fn get_var_by_name(&self, name: &str) -> VarRef {
        self.name_var_map.get(name).copied().unwrap()
    }
    fn get_ty_by_name(&self, name: &str) -> TypeRef {
        println!("get_ty_by_name: {}", name);
        println!("name_ctx: {:?}", self.name_ctx.get(name));
        let ty = self.name_ctx.get(name).copied();
        if ty.is_none() {
            self.ext_name_ctx.get(name).copied().unwrap()
        } else {
            ty.unwrap()
        }
    }
    pub fn build_else(&mut self, els: &TypedElse) -> Option<VarRef> {
        match &els.kind {
            ElseKind::ElseIf(if_) => self.build_if(if_),
            ElseKind::Else(block) => self.build_block(block),
            ElseKind::None => None,
        }
    }
    pub fn build_if(&mut self, if_: &TypedIf) -> Option<VarRef> {
        let cond = self.build_expr(&if_.cond).unwrap();
        let if_value = if if_.ty == self.ty_ctx.get_unit() {
            None
        } else {
            Some(self.create_variable(None, if_.ty))
        };
        let then_block = self.create_block(Some("then"));
        let else_block = self.create_block(Some("else"));
        let end_block = self.create_block(Some("end"));
        self.terminate_current_block(Terminator::Branch(cond, then_block, else_block), false);
        self.position = then_block;
        let then = self.build_block(&if_.then);
        if if_value.is_some() {
            let stmt = Stmt {
                left: if_value,
                right: then.map(|v| {
                    Operand {
                        typ: if_.ty,
                        val: Box::new(v.into()),
                    }
                    .into()
                }),
            };
            self.add_stmt_to_current_block(stmt);
        }
        self.terminate_current_block(Terminator::Jump(end_block), false);
        self.position = else_block;
        let else_ = self.build_else(&if_.els);
        if if_value.is_some() {
            let stmt = Stmt {
                left: if_value,
                right: else_.map(|v| {
                    Operand {
                        typ: if_.ty,
                        val: Box::new(v.into()),
                    }
                    .into()
                }),
            };
            self.add_stmt_to_current_block(stmt);
        }
        self.terminate_current_block(Terminator::Jump(end_block), false);
        self.position = end_block;
        if_value
    }

    pub fn build_expr(&mut self, expr: &TypedExpr) -> Option<Operand> {
        let expr_value = match &expr.kind {
            ExprKind::Literal(lit) => Some(OperandEnum::Literal(lit.kind.clone())),
            ExprKind::Variable(var) => {
                let var = self.name_var_map.get(&var.name).copied().unwrap();
                Some(OperandEnum::Var(var))
            }
            ExprKind::Tuple(TypedTuple { elements, ty }) => {
                let elements = elements
                    .iter()
                    .map(|e| self.build_expr(e).unwrap())
                    .collect::<Vec<_>>();
                let tuple = self.create_variable(None, *ty);
                let value = Rvalue {
                    typ: *ty,
                    val: Box::new(RvalueEnum::MakeTuple(elements)),
                };
                let stmt = Stmt {
                    left: Some(tuple),
                    right: Some(value),
                };
                self.add_stmt_to_current_block(stmt);
                Some(OperandEnum::Var(tuple))
            }
            ExprKind::Array(TypedArray { elements, ty }) => {
                let elements = elements
                    .iter()
                    .map(|e| self.build_expr(e).unwrap())
                    .collect::<Vec<_>>();
                let array = self.create_variable(None, *ty);
                let value = Rvalue {
                    typ: *ty,
                    val: Box::new(RvalueEnum::Call("__serge_alloc_array".to_string(), vec![])),
                };
                let stmt = Stmt {
                    left: Some(array),
                    right: Some(value),
                };
                self.add_stmt_to_current_block(stmt);
                elements.iter().for_each(|e| {
                    let call = Rvalue {
                        typ: self.ty_ctx.get_unit(),
                        val: Box::new(RvalueEnum::Call(
                            "__serge_array_push_back".to_string(),
                            vec![
                                Operand {
                                    typ: self.ty_ctx.get_i32(),
                                    val: Box::new(OperandEnum::Var(array)),
                                },
                                e.clone(),
                            ],
                        )),
                    };
                    let stmt = Stmt {
                        left: None,
                        right: Some(call),
                    };
                    self.add_stmt_to_current_block(stmt);
                });
                Some(OperandEnum::Var(array))
            }
            ExprKind::Block(block) => {
                let var = self.build_block(block);
                var.map(|var| OperandEnum::Var(var))
            }
            ExprKind::BinOp(TypedBinOp { op, lhs, rhs, ty }) => {
                let lhs = self.build_expr(lhs.as_ref()).unwrap();
                let rhs = self.build_expr(rhs.as_ref()).unwrap();
                let var = self.create_variable(None, *ty);
                let value = Rvalue {
                    typ: *ty,
                    val: Box::new(RvalueEnum::BinaryOperator(op.clone(), lhs, rhs)),
                };
                let stmt = Stmt {
                    left: Some(var),
                    right: Some(value),
                };
                self.add_stmt_to_current_block(stmt);
                Some(OperandEnum::Var(var))
            }
            ExprKind::UnOp(TypedUnOp { op, rhs, ty }) => {
                let rhs = self.build_expr(rhs).unwrap();
                let var = self.create_variable(None, *ty);
                let value = Rvalue {
                    typ: *ty,
                    val: Box::new(RvalueEnum::UnaryOperator(op.clone(), rhs)),
                };
                let stmt = Stmt {
                    left: Some(var),
                    right: Some(value),
                };
                self.add_stmt_to_current_block(stmt);
                Some(OperandEnum::Var(var))
            }

            ExprKind::If(if_) => {
                let var = self.build_if(if_);
                var.map(|var| OperandEnum::Var(var))
            }
            ExprKind::Call(TypedCall { func, args, .. }) => {
                let args = args
                    .iter()
                    .map(|arg| self.build_expr(arg).unwrap())
                    .collect::<Vec<_>>();
                match &func.kind {
                    ExprKind::Variable(TypedVariable { name, ty }) => {
                        let func_ty = self.ty_ctx.get_type_by_typeref(*ty);
                        if let Type::Callable { ret, .. } = func_ty {
                            let value = Rvalue {
                                typ: ret,
                                val: Box::new(RvalueEnum::Call(mangling(name.as_str()), args)),
                            };
                            let var = if ret != self.ty_ctx.get_unit() {
                                Some(self.create_variable(None, ret))
                            } else {
                                None
                            };
                            let stmt = Stmt {
                                left: var,
                                right: Some(value),
                            };
                            self.add_stmt_to_current_block(stmt);
                            var.map(|var| OperandEnum::Var(var))
                        } else {
                            unreachable!()
                        }
                    }
                    _ => todo!(),
                }
            }
            ExprKind::Index(TypedIndex { array, index, ty }) => {
                let array = self.build_expr(array).unwrap();
                let index = self.build_expr(index).unwrap();
                let var = self.create_variable(None, *ty);
                let value = Rvalue {
                    typ: *ty,
                    val: Box::new(RvalueEnum::Index(array, index)),
                };
                let stmt = Stmt {
                    left: Some(var),
                    right: Some(value),
                };
                self.add_stmt_to_current_block(stmt);
                Some(OperandEnum::Var(var))
            }
            ExprKind::Ctor(TypedCtor {
                ty_name,
                name,
                fields,
                ty,
            }) => {
                let r#enum = self.ty_ctx.get_enum_by_typeref(*ty).unwrap();
                // let tag = r#enum.get_tag_by_ctor_name(name.as_str()).unwrap();
                let (ctor_fields, tag) = r#enum.ctors.get(name).unwrap();
                let exprs = if let Some(ctor_fields) = ctor_fields {
                    match ctor_fields {
                        FieldsType::UnnamedFields(tys) => {
                            if let Some(ExprFieldsKind::UnnamedFields(exprs)) = fields {
                                let exprs = exprs
                                    .iter()
                                    .map(|e| self.build_expr(e).unwrap())
                                    .collect::<Vec<_>>();
                                exprs
                            } else {
                                unreachable!()
                            }
                        }
                        FieldsType::NamedFields(fields_map) => {
                            if let Some(ExprFieldsKind::NamedFields(bindings)) = fields {
                                let mut exprs = Vec::new();
                                exprs.resize(fields_map.len(), None);
                                for (name, expr) in bindings {
                                    let (ty, index) = fields_map.get(name).unwrap();
                                    if let Some(expr) = expr {
                                        exprs[*index] = Some(self.build_expr(expr).unwrap());
                                    } else {
                                        exprs[*index] = Some(Operand {
                                            typ: *ty,
                                            val: Box::new(
                                                self.get_var_by_name(name.as_str()).into(),
                                            ),
                                        })
                                    }
                                }
                                let exprs =
                                    exprs.into_iter().map(|e| e.unwrap()).collect::<Vec<_>>();
                                exprs
                            } else {
                                unreachable!();
                            }
                        }
                    }
                } else {
                    Vec::new()
                };
                let var = self.create_variable(None, *ty);
                let value = Rvalue {
                    typ: *ty,
                    val: Box::new(RvalueEnum::Construct(tag.clone(), exprs)),
                };
                let stmt = Stmt {
                    left: Some(var),
                    right: Some(value),
                };
                self.add_stmt_to_current_block(stmt);
                Some(OperandEnum::Var(var))
            }
            ExprKind::Let(TypedLet { name, rhs, ty }) => {
                let var = self.create_variable(Some(name), *ty);
                let rhs = self.build_expr(rhs).unwrap();
                let value = Rvalue {
                    typ: *ty,
                    val: Box::new(RvalueEnum::Operand(rhs)),
                };
                let stmt = Stmt {
                    left: Some(var),
                    right: Some(value),
                };
                self.add_stmt_to_current_block(stmt);
                None
            }
            ExprKind::While(TypedWhile { cond, body }) => {
                let cond_block = self.create_block(Some("cond"));
                let body_block = self.create_block(Some("body"));
                let end_block = self.create_block(Some("end"));
                self.terminate_current_block(Terminator::Jump(cond_block), false);
                self.position = cond_block;
                let cond = self.build_expr(cond).unwrap();
                self.terminate_current_block(
                    Terminator::Branch(cond, body_block, end_block),
                    false,
                );
                let old_continue = self.continue_block;
                let old_break = self.break_block;
                self.continue_block = Some(cond_block);
                self.break_block = Some(end_block);
                self.position = body_block;
                self.build_block(body);
                self.continue_block = old_continue;
                self.break_block = old_break;
                self.terminate_current_block(Terminator::Jump(cond_block), false);
                self.position = end_block;
                None
            }
            ExprKind::For(TypedFor {
                var,
                start,
                end,
                body,
            }) => {
                let inc_block = self.create_block(Some("inc"));
                let cond_block = self.create_block(Some("cond"));
                let body_block = self.create_block(Some("body"));
                let end_block = self.create_block(Some("end"));
                // self.terminate_current_block(Terminator::Jump(start_block), false);
                // self.position = start_block;
                let iter_var = self.create_variable(Some(var), self.ty_ctx.get_i32());
                let start = self.build_expr(start).unwrap();
                let value = start.into();
                let stmt = Stmt {
                    left: Some(iter_var),
                    right: Some(value),
                };
                self.add_stmt_to_current_block(stmt);
                self.terminate_current_block(Terminator::Jump(cond_block), false);
                self.position = cond_block;

                let end = self.build_expr(end).unwrap();
                let cond_value = Rvalue {
                    typ: self.ty_ctx.get_bool(),
                    val: Box::new(RvalueEnum::BinaryOperator(
                        BinOp::Lt,
                        Operand {
                            typ: self.ty_ctx.get_i32(),
                            val: Box::new(iter_var.into()),
                        },
                        end,
                    )),
                };
                let cond_var = self.create_variable(None, self.ty_ctx.get_bool());
                let cond_stmt = Stmt {
                    left: Some(cond_var),
                    right: Some(cond_value),
                };
                self.add_stmt_to_current_block(cond_stmt);
                let cond = Operand {
                    typ: self.ty_ctx.get_bool(),
                    val: Box::new(cond_var.into()),
                };
                self.terminate_current_block(
                    Terminator::Branch(cond, body_block, end_block),
                    false,
                );
                let old_continue = self.continue_block;
                let old_break = self.break_block;
                let old_name_ctx = self.name_ctx.clone();
                let old_name_var_map = self.name_var_map.clone();
                self.name_ctx = self.name_ctx.insert(var.clone(), self.ty_ctx.get_i32());
                self.name_var_map = self.name_var_map.insert(var.clone(), iter_var);
                self.continue_block = Some(inc_block);
                self.break_block = Some(end_block);
                self.position = body_block;
                self.build_block(body);
                self.continue_block = old_continue;
                self.break_block = old_break;
                self.name_ctx = old_name_ctx;
                self.name_var_map = old_name_var_map;
                self.terminate_current_block(Terminator::Jump(inc_block), false);
                self.position = inc_block;
                let inc_value = Rvalue {
                    typ: self.ty_ctx.get_i32(),
                    val: Box::new(RvalueEnum::BinaryOperator(
                        BinOp::Add,
                        Operand {
                            typ: self.ty_ctx.get_i32(),
                            val: Box::new(iter_var.into()),
                        },
                        Operand {
                            typ: self.ty_ctx.get_i32(),
                            val: Box::new(OperandEnum::Imm(1)),
                        },
                    )),
                };
                let inc_stmt = Stmt {
                    left: Some(iter_var),
                    right: Some(inc_value),
                };
                self.add_stmt_to_current_block(inc_stmt);
                self.terminate_current_block(Terminator::Jump(cond_block), false);
                self.position = end_block;
                None
            }
            ExprKind::Return(TypedReturn { expr, .. }) => {
                let return_value = expr.as_ref().map(|e| self.build_expr(e).unwrap());
                if let Some(return_value) = return_value {
                    let value = Rvalue {
                        typ: return_value.typ,
                        val: Box::new(RvalueEnum::Operand(return_value)),
                    };
                    let stmt = Stmt {
                        left: self.func.return_value,
                        right: Some(value),
                    };
                    self.add_stmt_to_current_block(stmt);
                }
                let jmp = Terminator::Jump(self.func.exit);
                self.terminate_current_block(jmp, true);
                None
            }
            ExprKind::Break => {
                let jmp = Terminator::Jump(self.break_block.unwrap());
                self.terminate_current_block(jmp, true);
                None
            }
            ExprKind::Continue => {
                let jmp = Terminator::Jump(self.continue_block.unwrap());
                self.terminate_current_block(jmp, true);
                // self.position = self.create_block(Some("continue"));

                None
            }
            ExprKind::Assign(TypedAssign { name, rhs }) => {
                let value = self.build_expr(rhs).unwrap();
                match &name.kind {
                    ExprKind::Index(TypedIndex { array, index, ty }) => {
                        let array = self.build_expr(array).unwrap();
                        let index = self.build_expr(index).unwrap();
                        let write_array = Rvalue {
                            typ: self.ty_ctx.get_unit(),
                            val: Box::new(RvalueEnum::Call(
                                "__serge_array_write_index".to_string(),
                                vec![array, index, value],
                            )),
                        };
                        let stmt = Stmt {
                            left: None,
                            right: Some(write_array),
                        };
                        self.add_stmt_to_current_block(stmt);
                        None
                    }
                    ExprKind::Variable(TypedVariable { name, ty }) => {
                        let var = self.get_var_by_name(name);
                        let ty = self.get_ty_by_name(name);
                        let value = Rvalue {
                            typ: ty,
                            val: Box::new(RvalueEnum::Operand(value)),
                        };
                        let stmt = Stmt {
                            left: Some(var),
                            right: Some(value),
                        };
                        self.add_stmt_to_current_block(stmt);
                        None
                    }
                    _ => unreachable!(),
                }
            }
            _ => todo!(),
        };
        expr_value.map(|val| Operand {
            typ: expr.ty,
            val: Box::new(val),
        })
    }
    pub fn build_block(&mut self, block: &TypedBlock) -> Option<VarRef> {
        let name_ctx = self.name_ctx.clone();
        let name_var_map = self.name_var_map.clone();
        let mut last_value = None;
        for expr in &block.exprs {
            let value = self.build_expr(expr);
            if expr.ty != self.ty_ctx.get_unit() {
                last_value = value;
            } else {
                last_value = None;
            }
        }
        self.name_ctx = name_ctx;
        self.name_var_map = name_var_map;
        last_value.map(|val| {
            let var = self.create_variable(None, val.typ);
            let value = Rvalue {
                typ: val.typ,
                val: Box::new(RvalueEnum::Operand(val)),
            };
            let stmt = Stmt {
                left: Some(var),
                right: Some(value),
            };
            self.add_stmt_to_current_block(stmt);
            var
        })
    }
    pub fn build_func(&mut self, func: &TypedFunc) -> Func {
        let value = self.build_block(&func.body);
        value.map(|var| {
            let value = Operand {
                typ: func.return_ty,
                val: Box::new(OperandEnum::Var(var)),
            };
            let value = Rvalue {
                typ: value.typ,
                val: Box::new(RvalueEnum::Operand(value)),
            };
            let stmt = Stmt {
                left: self.func.return_value,
                right: Some(value),
            };
            self.add_stmt_to_current_block(stmt);
        });
        if !self.current_block().stmts.is_empty() {
            let jmp = Terminator::Jump(self.func.exit);
            self.terminate_current_block(jmp, false);
        }
        self.func.clone()
    }
}

impl MIR {
    pub fn create_from_typed_ast(typed_ast: &TypedModule) -> Self {
        let ty_ctx = typed_ast.ty_ctx.clone();
        let name_ctx = typed_ast.func_table.clone();
        let mut mir = MIR {
            ty_ctx,
            name_ctx,
            module: Vec::new(),
        };
        let mut name_var_map = SymTable::new();
        let mut global_func_vars = SlotMap::with_key();
        for func in &typed_ast.func_table {
            let func_var = global_func_vars.insert(Var {
                name: func.0.clone(),
                typ: *func.1,
            });
            name_var_map = name_var_map.insert(func.0.clone(), func_var);
        }
        for (i, func) in typed_ast.func_defs.iter().enumerate() {
            mir.module
                .push(mir.build_func(func, mir.name_ctx.clone(), name_var_map.clone(), i));
        }
        mir
    }
    fn build_func(
        &self,
        func: &TypedFunc,
        ext_name_ctx: SymTable<String, TypeRef>,
        name_var_map: SymTable<String, VarRef>,
        counter: usize,
    ) -> Func {
        let prefix = format!("func_{}_{}", counter, &func.name);
        let mut builder = FuncBuilder::new(func, ext_name_ctx, name_var_map, &self.ty_ctx, &prefix);
        let func = builder.build_func(func);
        func
    }
}
