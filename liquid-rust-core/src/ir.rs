use std::fmt;

use liquid_rust_common::index::{Idx, IndexVec};
use rustc_hir::def_id::DefId;
pub use rustc_middle::mir::{BasicBlock, Local, SourceInfo, SwitchTargets, UnOp};
use rustc_middle::ty::IntTy;

use crate::ty::TypeLayout;

#[derive(Debug)]
pub struct Body {
    pub basic_blocks: IndexVec<BasicBlock, BasicBlockData>,
    pub arg_count: usize,
    pub local_decls: IndexVec<Local, LocalDecl>,
}

#[derive(Debug)]
pub struct LocalDecl {
    pub layout: TypeLayout,
}

#[derive(Debug)]
pub struct BasicBlockData {
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
}

#[derive(Debug)]
pub struct Terminator {
    pub kind: TerminatorKind,
    pub source_info: SourceInfo,
}

#[derive(Debug)]
pub enum TerminatorKind {
    Return,
    Call {
        func: DefId,
        args: Vec<Operand>,
        destination: Option<(Place, BasicBlock)>,
    },
    SwitchInt {
        discr: Operand,
        targets: SwitchTargets,
    },
    Goto {
        target: BasicBlock,
    },
}

pub struct Statement {
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Assign(Place, Rvalue),
    Nop,
}

pub enum Rvalue {
    Use(Operand),
    MutRef(Local),
    BinaryOp(BinOp, Operand, Operand),
    UnaryOp(UnOp, Operand),
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Gt,
    Lt,
}

pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(Constant),
}

pub struct Place {
    pub local: Local,
    pub projection: Vec<PlaceElem>,
}

#[derive(Debug)]
pub enum PlaceElem {
    Deref,
}

pub enum Constant {
    Int(i128, IntTy),
    Bool(bool),
}

impl Body {
    #[inline]
    pub fn args_iter(&self) -> impl Iterator<Item = Local> {
        (1..self.arg_count + 1).map(Local::new)
    }

    #[inline]
    pub fn vars_and_temps_iter(&self) -> impl Iterator<Item = Local> {
        (self.arg_count + 1..self.local_decls.len()).map(Local::new)
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            StatementKind::Assign(place, rvalue) => {
                write!(f, "{:?} = {:?}", place, rvalue)
            }
            StatementKind::Nop => write!(f, "nop"),
        }
    }
}

impl fmt::Debug for Place {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for elem in &self.projection {
            match elem {
                PlaceElem::Deref => write!(f, "*")?,
            }
        }
        write!(f, "{:?}", self.local)
    }
}

impl fmt::Debug for Rvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Use(op) => write!(f, "{:?}", op),
            Self::MutRef(local) => write!(f, "&mut {:?}", local),
            Self::BinaryOp(bin_op, op1, op2) => write!(f, "{:?}({:?}, {:?})", bin_op, op1, op2),
            Self::UnaryOp(un_up, op) => write!(f, "{:?}({:?})", un_up, op),
        }
    }
}

impl fmt::Debug for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Copy(place) => write!(f, "copy {:?}", place),
            Self::Move(place) => write!(f, "move {:?}", place),
            Self::Constant(c) => write!(f, "{:?}", c),
        }
    }
}

impl fmt::Debug for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n, int_ty) => write!(f, "{}{:?}", n, int_ty),
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
}