use super::{
    BaseTy, BaseTyKind, Constraint, Expr, ExprKind, FnOutput, FnSig, Func, FuncSort, GenericArg,
    Ident, Lifetime, MutTy, Path, RefineArg, RefineParam, Res, Sort, Ty, TyKind,
};

pub trait Visitor: Sized {
    fn visit_fn_sig(&mut self, fn_sig: &FnSig) {
        walk_fn_sig(self, fn_sig);
    }

    fn visit_refine_param(&mut self, param: &RefineParam) {
        walk_refine_param(self, param);
    }

    fn visit_sort(&mut self, sort: &Sort) {
        walk_sort(self, sort);
    }

    fn visit_func_sort(&mut self, fsort: &FuncSort) {
        walk_func_sort(self, fsort);
    }

    fn visit_constraint(&mut self, constraint: &Constraint) {
        walk_constraint(self, constraint);
    }

    fn visit_fn_output(&mut self, output: &FnOutput) {
        walk_fn_output(self, output);
    }

    fn visit_ty(&mut self, ty: &Ty) {
        walk_ty(self, ty);
    }

    fn visit_bty(&mut self, bty: &BaseTy) {
        walk_bty(self, bty);
    }

    fn visit_generic_arg(&mut self, arg: &GenericArg) {
        walk_generic_arg(self, arg)
    }

    fn visit_lifetime(&mut self, _lifetime: &Lifetime) {}

    fn visit_mut_ty(&mut self, mty: &MutTy) {
        walk_mut_ty(self, mty);
    }

    fn visit_path(&mut self, path: &Path) {
        walk_path(self, path);
    }

    fn visit_res(&mut self, _res: &Res) {}

    fn visit_refine_arg(&mut self, arg: &RefineArg) {
        walk_refine_arg(self, arg);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_func(&mut self, func: &Func) {
        walk_func(self, func);
    }

    fn visit_ident(&mut self, _ident: Ident) {}
}

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
    walk_list!(visitor, visit_constraint, &fn_sig.requires);
    walk_list!(visitor, visit_ty, &fn_sig.args);
    visitor.visit_fn_output(&fn_sig.output);
}

pub fn walk_refine_param<V: Visitor>(visitor: &mut V, param: &RefineParam) {
    visitor.visit_ident(param.ident);
    visitor.visit_sort(&param.sort);
}

pub fn walk_sort<V: Visitor>(visitor: &mut V, sort: &Sort) {
    match sort {
        Sort::App(_, args) => {
            walk_list!(visitor, visit_sort, args);
        }
        Sort::Func(fsort) => visitor.visit_func_sort(fsort),
        Sort::Int
        | Sort::Bool
        | Sort::Real
        | Sort::Loc
        | Sort::Unit
        | Sort::BitVec(_)
        | Sort::Record(_)
        | Sort::Param(_)
        | Sort::Wildcard
        | Sort::Infer(_) => {}
    }
}

pub fn walk_func_sort<V: Visitor>(visitor: &mut V, func_sort: &FuncSort) {
    walk_list!(visitor, visit_sort, &func_sort.inputs_and_output);
}

pub fn walk_constraint<V: Visitor>(visitor: &mut V, constraint: &Constraint) {
    match constraint {
        Constraint::Type(ident, ty) => {
            visitor.visit_ident(*ident);
            visitor.visit_ty(ty);
        }
        Constraint::Pred(expr) => visitor.visit_expr(expr),
    }
}

pub fn walk_fn_output<V: Visitor>(visitor: &mut V, output: &FnOutput) {
    walk_list!(visitor, visit_refine_param, &output.params);
    visitor.visit_ty(&output.ret);
    walk_list!(visitor, visit_constraint, &output.ensures);
}

pub fn walk_ty<V: Visitor>(visitor: &mut V, ty: &Ty) {
    match &ty.kind {
        TyKind::BaseTy(bty) => {
            visitor.visit_bty(bty);
        }
        TyKind::Indexed(bty, idx) => {
            visitor.visit_bty(bty);
            visitor.visit_refine_arg(idx);
        }
        TyKind::Exists(params, ty) => {
            walk_list!(visitor, visit_refine_param, params);
            visitor.visit_ty(ty);
        }
        TyKind::Constr(pred, ty) => {
            visitor.visit_expr(pred);
            visitor.visit_ty(ty);
        }
        TyKind::Ptr(lft, loc) => {
            visitor.visit_lifetime(lft);
            visitor.visit_ident(*loc);
        }
        TyKind::Ref(lft, mty) => {
            visitor.visit_lifetime(lft);
            visitor.visit_mut_ty(mty);
        }
        TyKind::Tuple(tys) => walk_list!(visitor, visit_ty, tys),
        TyKind::Array(ty, _) => visitor.visit_ty(ty),
        TyKind::RawPtr(ty, _) => visitor.visit_ty(ty),
        TyKind::Never | TyKind::Hole => {}
    }
}

pub fn walk_bty<V: Visitor>(visitor: &mut V, bty: &BaseTy) {
    match &bty.kind {
        BaseTyKind::Path(path) => visitor.visit_path(path),
        BaseTyKind::Slice(ty) => visitor.visit_ty(ty),
    }
}

pub fn walk_generic_arg<V: Visitor>(visitor: &mut V, arg: &GenericArg) {
    match arg {
        GenericArg::Lifetime(lft) => visitor.visit_lifetime(lft),
        GenericArg::Type(ty) => visitor.visit_ty(ty),
    }
}

pub fn walk_mut_ty<V: Visitor>(visitor: &mut V, mty: &MutTy) {
    visitor.visit_ty(&mty.ty);
}

pub fn walk_path<V: Visitor>(visitor: &mut V, path: &Path) {
    visitor.visit_res(&path.res);
    walk_list!(visitor, visit_generic_arg, &path.generics);
    walk_list!(visitor, visit_refine_arg, &path.refine);
}

pub fn walk_refine_arg<V: Visitor>(visitor: &mut V, arg: &RefineArg) {
    match arg {
        RefineArg::Expr { expr, is_binder: _ } => visitor.visit_expr(expr),
        RefineArg::Abs(params, body, _, _) => {
            walk_list!(visitor, visit_refine_param, params);
            visitor.visit_expr(body);
        }
        RefineArg::Record(_, args, _) => {
            walk_list!(visitor, visit_refine_arg, args);
        }
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Const(_, _) => todo!(),
        ExprKind::Var(_) => todo!(),
        ExprKind::Dot(base, _) => {
            visitor.visit_ident(*base);
        }
        ExprKind::Literal(_) => todo!(),
        ExprKind::BinaryOp(_, box [e1, e2]) => {
            visitor.visit_expr(e1);
            visitor.visit_expr(e2);
        }
        ExprKind::UnaryOp(_, e) => {
            visitor.visit_expr(e);
        }
        ExprKind::App(func, args) => {
            visitor.visit_func(func);
            walk_list!(visitor, visit_expr, args);
        }
        ExprKind::IfThenElse(box [e1, e2, e3]) => {
            visitor.visit_expr(e1);
            visitor.visit_expr(e2);
            visitor.visit_expr(e3);
        }
    }
}

pub fn walk_func<V: Visitor>(visitor: &mut V, func: &Func) {
    match func {
        Func::Var(ident, _) => visitor.visit_ident(*ident),
        Func::Global(_, _, _, _) => todo!(),
    }
}
