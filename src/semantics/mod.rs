/*
   Risk is a purely functional, strongly typed language.
   Copyright (C) 2024, Lokasku & NightProg

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

mod error;
mod tc;
mod warning;

use self::warning::SemanticWarningKind;
use crate::ast::{self, *};
use crate::semantics::error::*;
use crate::semantics::warning::SemanticWarning;
use polonius_the_crab::{polonius, polonius_return};
use std::collections::HashMap;
use std::hash::Hash;

const BUILTIN_TYPES: [&str; 7] = [
    "Integer", "Float", "Bool", "String", "Char", "True", "False",
];

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Symbol {
    span: Span,
    scope_id: u16,
    level: u16,
}

#[derive(Debug, PartialEq)]
pub struct SymbolData {
    arity: u8,
    used: bool,
}

#[derive(Debug)]
pub struct TypeData {
    arity: u8,
    used: bool,
}

#[derive(Debug)]
pub struct VariantData {
    arity: u8,
    used: bool,
}

type VariantName = Span;
type TypeName = Span;
type FuncName = String;

#[derive(Debug)]
pub struct AnalysisOutput {
    pub errors: Vec<SemanticError>,
    pub warnings: Vec<SemanticWarning>,
    pub symbols: HashMap<Symbol, SymbolData>,
    pub types: HashMap<TypeName, TypeData>,
    pub variants: HashMap<VariantName, VariantData>,
    pub signatures: Vec<FuncName>, // symbols to which a type has been assigned
    pub scope_id: u16,
    pub level: u16,
}

impl AnalysisOutput {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
            symbols: HashMap::new(),
            types: HashMap::new(),
            variants: HashMap::new(),
            signatures: Vec::new(),
            scope_id: 0,
            level: 0,
        }
    }

    fn find_identifier(&mut self, name: Span) -> Option<&mut SymbolData> {
        let mut sym = Symbol {
            span: name,
            scope_id: self.scope_id + 1,
            level: 0,
        };

        let mut symbols = &mut self.symbols;

        for level in (0..=self.level).rev() {
            sym.level = level;
            sym.scope_id = if sym.level == 0 { 0 } else { sym.scope_id - 1 };

            polonius!(|symbols| -> Option<&'polonius mut SymbolData> {
                if let Some(data) = symbols.get_mut(&sym) {
                    data.used = true;
                    polonius_return!(Some(data));
                }
            });
        }
        None
    }

    pub fn analyze_statement(&mut self, statement: Statement<ast::Span>) {
        match statement {
            Statement::Bind(Bind {
                name,
                args,
                expr,
                span,
            }) => {
                let sym = Symbol {
                    span: name.span.clone(),
                    scope_id: self.scope_id,
                    level: self.level,
                };

                let some_arguments = args.len() != 0;

                if !some_arguments {
                    if !self.symbols.contains_key(&sym) {
                        self.symbols.insert(
                            sym,
                            SymbolData {
                                arity: 0,
                                used: false,
                            },
                        );
                    } else {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::MultipleDeclarations {
                                name: name.name,
                                span: name.span,
                            },
                            span: span.clone(),
                        });
                    }
                } else if !self.symbols.contains_key(&sym) {
                    self.symbols.insert(
                        sym,
                        SymbolData {
                            arity: args.len() as u8,
                            used: false,
                        },
                    );
                }

                if some_arguments {
                    self.level += 1;
                    self.scope_id += 1;

                    for arg in args {
                        self.analyze_pattern(arg, span.clone());
                    }
                }

                self.analyze_expr(expr, span);

                if some_arguments {
                    self.level -= 1;
                }
            }
            Statement::TypeDecl(TypeDecl {
                name,
                variants,
                typevars,
                span,
            }) => {
                if !self.types.contains_key(&name.span) {
                    if !BUILTIN_TYPES.contains(&name.name.as_str()) {
                        self.types.insert(
                            // name.name,
                            span.clone(),
                            TypeData {
                                arity: typevars.len() as u8,
                                used: false,
                            },
                        );
                    } else {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::ReservedName {
                                name: name.name,
                                span: name.span,
                            },
                            span: span.clone(),
                        });
                    }
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::TypeAlreadyDefined {
                            type_name: name.name,
                        },
                        span: span.clone(),
                    });
                }

                for variant in variants {
                    if !self.variants.contains_key(&variant.id.span) {
                        if !BUILTIN_TYPES.contains(&variant.id.name.as_str()) {
                            self.variants.insert(
                                variant.id.span,
                                VariantData {
                                    arity: variant.types.len() as u8,
                                    used: false,
                                },
                            );
                        } else {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::ReservedName {
                                    name: variant.id.name,
                                    span: variant.id.span,
                                },
                                span: span.clone(),
                            });
                        }
                    } else {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::MultipleDeclarations {
                                name: variant.id.name,
                                span: variant.id.span,
                            },
                            span: span.clone(),
                        });
                    }

                    for ty in variant.types {
                        self.analyze_type(ty, span.clone());
                    }
                }
            }
            Statement::TypeAssign(TypeAssign { id, ty, span }) => {
                if !self.signatures.contains(&id.name) {
                    self.signatures.push(id.name);
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::AlreadyTypedSymbol {
                            symbol_name: id.name,
                        },
                        span: span.clone(),
                    });
                }
                self.analyze_type(ty, span);
            }
        }
    }

    pub fn analyze_expr(&mut self, expr: ParsedExpr, span_context: Span) {
        match expr {
            ParsedExpr::Identifier { id: Identifier { name, span } } => {
                if let Some(data) = self.find_identifier(span.clone()) {
                    let arity = data.arity;
                    if arity != 0 {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::WrongArity {
                                expected: arity as usize,
                                found: 0,
                                span,
                            },
                            span: span_context,
                        });
                    }
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedSymbol {
                            symbol_name: name,
                            span,
                        },
                        span: span_context,
                    });
                }
            }
            ParsedExpr::PCIdentifier { id: Identifier { name, span } } => {
                if let Some(data) = self.variants.get_mut(&span) {
                    let arity = data.arity;

                    if arity != 0 {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::WrongArity {
                                expected: arity as usize,
                                found: 0,
                                span,
                            },
                            span: span_context,
                        });
                    }
                    data.used = true;
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedConstructor {
                            constructor_name: name,
                            span,
                        },
                        span: span_context,
                    });
                }
            }
            ParsedExpr::App(App { ident, args, span }) => {
                if ident.name.chars().next().unwrap().is_lowercase() {
                    if let Some(data) = self.find_identifier(ident.span) {
                        let arity = data.arity;

                        if arity != args.len() as u8 {
                            self.errors.push(SemanticError {
                                kind: SemanticErrorKind::WrongArity {
                                    expected: arity as usize,
                                    found: args.len(),
                                    span,
                                },
                                span: span_context.clone(),
                            });
                        }
                    }
                } else if let Some(data) = self.variants.get_mut(&ident.span) {
                    data.used = true;

                    if data.arity != args.len() as u8 {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::WrongArity {
                                expected: data.arity as usize,
                                found: args.len(),
                                span,
                            },
                            span: span_context.clone(),
                        });
                    }
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::NotACallee {
                            name: ident.name,
                            span: span,
                        },
                        span: span_context.clone(),
                    });
                }

                for arg in args {
                    self.analyze_expr(arg, span_context.clone());
                }
            }
            ParsedExpr::Condition { cond, then, els, ann } => {
                self.analyze_expr(*cond, ann.clone());
                self.analyze_expr(*then, ann.clone());
                self.analyze_expr(*els, ann);
            }
            ParsedExpr::Let { binds, ret, ann } => {
                self.level += 1;
                self.scope_id += 1;

                for bind in binds {
                    self.analyze_statement(Statement::Bind(bind));
                }

                self.analyze_expr(*ret, ann);

                self.level -= 1;
            }
            ParsedExpr::Match { referral, cases, .. } => {
                self.analyze_expr(*referral, span_context.clone());

                for case in cases {
                    self.level += 1;
                    self.scope_id += 1;

                    self.analyze_pattern(case.0, span_context.clone());
                    self.analyze_expr(*case.1, span_context.clone());

                    self.level -= 1;
                }
            }
            ParsedExpr::BinOp { lhs, rhs, .. } => {
                self.analyze_expr(*lhs, span_context.clone());
                self.analyze_expr(*rhs, span_context);
            }
            ParsedExpr::Lambda { args, ret, .. } => {
                let some_arguments = args.len() != 0;

                if some_arguments {
                    self.level += 1;
                    self.scope_id += 1;

                    for arg in args {
                        self.analyze_pattern(arg, span_context.clone());
                    }
                }

                self.analyze_expr(*ret, span_context);

                if some_arguments {
                    self.level -= 1;
                }
            }
            ParsedExpr::Ann { expr, ann, .. } => {
                self.analyze_expr(*expr, span_context.clone());
                self.analyze_type(ann.1, span_context);
            }
            ParsedExpr::List { list, ..} | ParsedExpr::Tuple { list, .. } => {
                for item in list {
                    self.analyze_expr(item, span_context.clone());
                }
            }
            ParsedExpr::Literal(_) => {}
        }
    }

    pub fn analyze_pattern(&mut self, pattern: Pattern, span_context: Span) {
        match pattern {
            Pattern::Variable(id) => {
                let sym = Symbol {
                    span: id.span.clone(),
                    scope_id: self.scope_id,
                    level: self.level,
                };

                if !self.symbols.contains_key(&sym) {
                    self.symbols.insert(
                        sym,
                        SymbolData {
                            arity: 0,
                            used: false,
                        },
                    );
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::MultipleDeclarations {
                            name: id.name,
                            span: id.span,
                        },
                        span: span_context.clone(),
                    });
                }
            }
            Pattern::ListCons(lhs, rhs, _) => {
                self.analyze_pattern(*lhs, span_context.clone());
                self.analyze_pattern(*rhs, span_context);
            }
            Pattern::App(id, patterns, ..) => {
                if let Some(data) = self.variants.get_mut(&id.span) {
                    data.used = true;

                    if data.arity != patterns.len() as u8 {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::WrongArity {
                                expected: data.arity as usize,
                                found: patterns.len(),
                                span: id.span,
                            },
                            span: span_context.clone(),
                        });
                    }
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedConstructor {
                            constructor_name: id.name,
                            span: id.span,
                        },
                        span: span_context.clone(),
                    });
                }

                // for pattern in patterns {
                //    self.analyze_pattern(pattern);
                // }
            }
            Pattern::Id(id, ..) => {
                if let Some(data) = self.variants.get_mut(&id.span) {
                    let arity = data.arity;

                    if arity != 0 {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::WrongArity {
                                expected: arity as usize,
                                found: 0,
                                span: id.span,
                            },
                            span: span_context.clone(),
                        });
                    }
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedConstructor {
                            constructor_name: id.name,
                            span: id.span,
                        },
                        span: span_context,
                    });
                }
            }
            _ => {}
        }
    }

    pub fn analyze_type(&mut self, r#type: Type, span_context: Span) {
        match r#type {
            Type::Id(Identifier { name, span }) => {
                if BUILTIN_TYPES.contains(&name.as_str()) {
                } else if let Some(data) = self.types.get_mut(&span) {
                    data.used = true;

                    if data.arity != 0 {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::WrongArity {
                                expected: data.arity as usize,
                                found: 0,
                                span,
                            },
                            span: span_context,
                        })
                    }
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedType {
                            type_name: name,
                            span,
                        },
                        span: span_context,
                    })
                }
            }
            Type::App(id, types, span) => {
                if BUILTIN_TYPES.contains(&id.name.as_str()) {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::WrongArity {
                            expected: 0,
                            found: types.len(),
                            span,
                        },
                        span: span_context,
                    })
                } else if let Some(data) = self.types.get_mut(&id.span) {
                    data.used = true;

                    if types.len() as u8 != data.arity {
                        self.errors.push(SemanticError {
                            kind: SemanticErrorKind::WrongArity {
                                expected: data.arity as usize,
                                found: types.len(),
                                span: id.span,
                            },
                            span: span_context,
                        })
                    }
                } else {
                    self.errors.push(SemanticError {
                        kind: SemanticErrorKind::UndefinedType {
                            type_name: id.name,
                            span: id.span,
                        },
                        span: span_context,
                    })
                }
            }
            Type::Tuple(types, ..) => {
                for ty in types {
                    self.analyze_type(ty, span_context.clone());
                }
            }
            Type::Func(ret, args, ..) => {
                for arg in args {
                    self.analyze_type(arg, span_context.clone());
                }

                self.analyze_type(*ret, span_context);
            }
            _ => {}
        }
    }
}

pub fn analyze(ao: &mut AnalysisOutput, input: Program<ast::Span>) {
    for statement in input.statements {
        ao.analyze_statement(statement);
    }

    for (sym, data) in ao.symbols.iter() {
        if data.used == false {
            ao.warnings.push(SemanticWarning {
                kind: SemanticWarningKind::UnusedSymbol,
                span: sym.span.clone(),
            });
        }
    }

    for (sym, data) in ao.types.iter() {
        if data.used == false {
            ao.warnings.push(SemanticWarning {
                kind: SemanticWarningKind::UnusedType,
                span: sym.clone(),
            })
        }
    }

    for (sym, data) in ao.variants.iter() {
        if data.used == false {
            ao.warnings.push(SemanticWarning {
                kind: SemanticWarningKind::UnusedVariant,
                span: sym.clone(),
            })
        }
    }
}
