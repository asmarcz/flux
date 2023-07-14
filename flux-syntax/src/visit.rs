use rustc_span::symbol::Ident;

use crate::surface::{
    Arg, BaseSort, BaseTy, BaseTyKind, Expr, ExprKind, FnSig, Path, QPathExpr, RefineArg,
    RefineParam, Sort, Ty, TyKind,
};

pub trait Visitor: Sized {
    fn visit_fn_sig(&mut self, fn_sig: &FnSig) {
        walk_fn_sig(self, fn_sig);
    }

    fn visit_arg(&mut self, arg: &Arg) {
        walk_arg(self, arg);
    }

    fn visit_ty(&mut self, ty: &Ty) {
        walk_ty(self, ty);
    }

    fn visit_bty(&mut self, bty: &BaseTy) {
        walk_bty(self, bty);
    }

    fn visit_path(&mut self, path: &Path) {
        walk_path(self, path);
    }

    fn visit_sort(&mut self, sort: &Sort) {
        walk_sort(self, sort);
    }

    fn visit_base_sort(&mut self, sort: &BaseSort) {
        walk_base_sort(self, sort);
    }

    fn visit_refine_param(&mut self, param: &RefineParam) {
        walk_refine_param(self, param);
    }

    fn visit_refine_arg(&mut self, arg: &RefineArg) {
        walk_refine_arg(self, arg);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_ident(&mut self, _ident: Ident) {}
}

#[macro_export]
macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr $(, $($extra_args: expr),* )?) => {
        {
            #[allow(for_loops_over_fallibles)]
            for elem in $list {
                $visitor.$method(elem $(, $($extra_args,)* )?)
            }
        }
    }
}

pub fn walk_fn_sig<V: Visitor>(visitor: &mut V, fn_sig: &FnSig) {
    walk_list!(visitor, visit_refine_param, &fn_sig.params);
    if let Some(pred) = &fn_sig.requires {
        visitor.visit_expr(pred);
    }
    walk_list!(visitor, visit_arg, &fn_sig.args);
    if let Some(ret_ty) = &fn_sig.returns {
        visitor.visit_ty(ret_ty);
    }
    for (ident, ty) in &fn_sig.ensures {
        visitor.visit_ident(*ident);
        visitor.visit_ty(ty);
    }
}

pub fn walk_arg<V: Visitor>(visitor: &mut V, arg: &Arg) {
    match arg {
        Arg::Constr(ident, path, pred) => {
            visitor.visit_ident(*ident);
            visitor.visit_path(path);
            visitor.visit_expr(pred);
        }
        Arg::StrgRef(ident, ty) => {
            visitor.visit_ident(*ident);
            visitor.visit_ty(ty);
        }
        Arg::Ty(ident, ty) => {
            if let Some(ident) = ident {
                visitor.visit_ident(*ident);
            }
            visitor.visit_ty(ty);
        }
    }
}

pub fn walk_ty<V: Visitor>(visitor: &mut V, ty: &Ty) {
    match &ty.kind {
        TyKind::Base(bty) => {
            visitor.visit_bty(bty);
        }
        TyKind::Indexed { bty, indices } => {
            visitor.visit_bty(bty);
            walk_list!(visitor, visit_refine_arg, &indices.indices);
        }
        TyKind::Exists { bind, bty, pred } => {
            visitor.visit_ident(*bind);
            visitor.visit_bty(bty);
            visitor.visit_expr(pred);
        }
        TyKind::GeneralExists { params, ty, pred } => {
            walk_list!(visitor, visit_refine_param, params);
            visitor.visit_ty(ty);
            if let Some(pred) = pred {
                visitor.visit_expr(pred);
            }
        }
        TyKind::Ref(_, ty) => visitor.visit_ty(ty),
        TyKind::Constr(pred, ty) => {
            visitor.visit_expr(pred);
            visitor.visit_ty(ty);
        }
        TyKind::Tuple(fields) => {
            walk_list!(visitor, visit_ty, fields);
        }
        TyKind::Array(ty, _) => {
            visitor.visit_ty(ty);
        }
        TyKind::Hole => {}
    }
}

pub fn walk_bty<V: Visitor>(visitor: &mut V, bty: &BaseTy) {
    match &bty.kind {
        BaseTyKind::Path(path) => {
            visitor.visit_path(path);
        }
        BaseTyKind::Slice(ty) => {
            visitor.visit_ty(ty);
        }
    }
}

pub fn walk_path<V: Visitor>(visitor: &mut V, path: &Path) {
    for ident in &path.segments {
        visitor.visit_ident(*ident);
    }
    walk_list!(visitor, visit_ty, &path.generics);
    walk_list!(visitor, visit_refine_arg, &path.refine);
}

pub fn walk_sort<V: Visitor>(visitor: &mut V, sort: &Sort) {
    match sort {
        Sort::Base(bsort) => visitor.visit_base_sort(bsort),
        Sort::Func { inputs, output } => {
            walk_list!(visitor, visit_base_sort, inputs);
            visitor.visit_base_sort(output);
        }
        Sort::Infer => {}
    }
}

pub fn walk_base_sort<V: Visitor>(visitor: &mut V, sort: &BaseSort) {
    match sort {
        BaseSort::Ident(ident) => visitor.visit_ident(*ident),
        BaseSort::BitVec(_) => {}
        BaseSort::App(ct, args) => {
            visitor.visit_ident(*ct);
            walk_list!(visitor, visit_base_sort, args);
        }
    }
}

pub fn walk_refine_param<V: Visitor>(visitor: &mut V, param: &RefineParam) {
    visitor.visit_ident(param.name);
    visitor.visit_sort(&param.sort);
}

pub fn walk_refine_arg<V: Visitor>(visitor: &mut V, arg: &RefineArg) {
    match arg {
        RefineArg::Expr(expr) => visitor.visit_expr(expr),
        RefineArg::Bind(ident, ..) => visitor.visit_ident(*ident),
        RefineArg::Abs(_, body, _) => visitor.visit_expr(body),
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::QPath(QPathExpr { segments, .. }) => {
            for ident in segments {
                visitor.visit_ident(*ident);
            }
        }
        ExprKind::Dot(QPathExpr { segments, .. }, fld) => {
            for ident in segments {
                visitor.visit_ident(*ident);
            }
            visitor.visit_ident(*fld);
        }
        ExprKind::Literal(_) => {}
        ExprKind::BinaryOp(_, box [e1, e2]) => {
            visitor.visit_expr(e1);
            visitor.visit_expr(e2);
        }
        ExprKind::UnaryOp(_, e) => {
            visitor.visit_expr(e);
        }
        ExprKind::App(fun, args) => {
            visitor.visit_ident(*fun);
            walk_list!(visitor, visit_expr, args);
        }
        ExprKind::IfThenElse(box [e1, e2, e3]) => {
            visitor.visit_expr(e1);
            visitor.visit_expr(e2);
            visitor.visit_expr(e3);
        }
    }
}
