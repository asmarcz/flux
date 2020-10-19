pub use rustc_middle::mir::Local;

pub use crate::refinements::common::{BaseTy, BinOp, Literal, UnOp, IntSize};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Variable(Local);

#[derive(Debug)]
pub enum Predicate {
    Var(Variable),
    Lit(Literal),
    BinApp(BinOp, Box<Self>, Box<Self>),
    UnApp(UnOp, Box<Self>),
    Cond(Box<Self>, Box<Self>, Box<Self>),
    App(Variable, Vec<Self>),
}

impl Predicate {
    pub fn replace(&mut self, target: Variable, substitute: Variable) {
        match self {
            Self::Var(var) => {
                if *var == target {
                    *var = substitute;
                }
            }
            Self::Lit(_) => {}
            Self::BinApp(_, op1, op2) => {
                op1.replace(target, substitute);
                op2.replace(target, substitute);
            }
            Self::UnApp(_, op) => {
                op.replace(target, substitute);
            }
            Self::Cond(if_pred, then_pred, else_pred) => {
                if_pred.replace(target, substitute);
                then_pred.replace(target, substitute);
                else_pred.replace(target, substitute);
            }
            Self::App(func, args) => {
                if *func == target {
                    *func = substitute;
                }

                for arg in args {
                    arg.replace(target, substitute);
                }
            }
        }
    }
}

pub enum Constraint {
    Pred(Predicate),
    Or(Box<Self>, Box<Self>),
    ForAll(Variable, BaseTy, Predicate, Box<Self>),
}

#[derive(Debug)]
pub enum RefinedTy {
    RefBase(Variable, BaseTy, Predicate),
    RefFunc(Vec<(Variable, Self)>, Box<Self>),
}

impl RefinedTy {
    pub fn replace(&mut self, target: Variable, substitute: Variable) {
        match self {
            Self::RefBase(var, _, pred) => {
                if *var != target {
                    pred.replace(target, substitute);
                }
            }
            Self::RefFunc(params, ret_ty) => {
                for (param, param_ty) in params {
                    param_ty.replace(target, substitute);
                }
                ret_ty.replace(target, substitute);
            }
        }
    }
}