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
    pub panic: BlockRef,
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
            name: func.name.clone(),
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
            panic,
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
    fn terminate_current_block(&mut self, terminator: Terminator) {
        self.current_block().terminator = terminator;
        self.position = self.create_block(None);
    }

    fn get_var_by_name(&self, name: &str) -> VarRef {
        self.name_var_map.get(name).copied().unwrap()
    }
    fn get_ty_by_name(&self, name: &str) -> TypeRef {
        self.name_ctx
            .get(name)
            .copied()
            .unwrap_or(self.ext_name_ctx.get(name).copied().unwrap())
    }

    pub fn build_expr(&mut self, expr: &TypedExpr) -> Option<Operand> {
        let expr_value = match &expr.kind {
            ExprKind::Literal(lit) => Some(match &lit.kind {
                LiteralKind::Int(i) => OperandEnum::Imm(*i),
                LiteralKind::Bool(b) => OperandEnum::Imm(if *b { 1 } else { 0 }),
                LiteralKind::Char(c) => OperandEnum::Imm(*c as i32),
                LiteralKind::Str(s) => todo!(),
                LiteralKind::Float(f) => todo!(),
            }),
            ExprKind::Variable(var) => {
                let var = self.name_var_map.get(&var.name).copied().unwrap();
                Some(OperandEnum::Var(var))
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
                    right: Some(value)
                };
                self.add_stmt_to_current_block(stmt);
                Some(OperandEnum::Var(var))
            }
            ExprKind::UnOp(TypedUnOp { op, rhs, ty }) => {
                let expr = self.build_expr(expr).unwrap();
                let var = self.create_variable(None, *ty);
                let value = Rvalue {
                    typ: *ty,
                    val: Box::new(RvalueEnum::UnaryOperator(op.clone(), expr)),
                };
                let stmt = Stmt {
                    left: Some(var),
                    right: Some(value)
                };
                self.add_stmt_to_current_block(stmt);
                Some(OperandEnum::Var(var))
            }

            ExprKind::If(TypedIf {
                cond, then, els, ..
            }) => todo!(),
            ExprKind::Call(TypedCall { func, args, .. }) => {
                let args = args
                    .iter()
                    .map(|arg| self.build_expr(arg).unwrap())
                    .collect::<Vec<_>>();
                match &func.kind {
                    ExprKind::Variable(TypedVariable { name, ty }) => {
                        let func_ty = self.ty_ctx.get_type_by_typeref(*ty);
                        if let Type::Callable { ret, .. } = func_ty {
                            let var = self.create_variable(None, ret);
                            let value = Rvalue {
                                typ: ret,
                                val: Box::new(RvalueEnum::Call(name.clone(), args)),
                            };
                            let stmt = Stmt {
                                left: Some(var),
                                right: Some(value),
                            };
                            self.add_stmt_to_current_block(stmt);
                            Some(OperandEnum::Var(var))
                        } else {
                            unreachable!()
                        }
                    }
                    _ => todo!(),
                }
            }
            ExprKind::Index(_) => todo!(),
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
            ExprKind::While(TypedWhile { cond, body }) => todo!(),
            ExprKind::For(TypedFor {
                var,
                start,
                end,
                body,
            }) => todo!(),
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
                self.terminate_current_block(jmp);
                None
            }
            ExprKind::Break => {
                let jmp = Terminator::Jump(self.break_block.unwrap());
                self.terminate_current_block(jmp);
                None
            }
            ExprKind::Continue => {
                let jmp = Terminator::Jump(self.continue_block.unwrap());
                self.terminate_current_block(jmp);
                None
            }
            ExprKind::Assign(TypedAssign { name, rhs }) => {
                let value = self.build_expr(rhs).unwrap();
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
            self.terminate_current_block(jmp);
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
