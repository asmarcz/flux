use std::fmt;

use crate::{
    constraint_builder::Cursor,
    inference,
    subst::Subst,
    ty::{Expr, ExprKind, KVid, Pred, RVid, Region, Ty, TyKind, Var},
};
use liquid_rust_common::{
    disjoint_sets::{DisjointSetsMap, Set},
    index::IndexGen,
};
use liquid_rust_core::ir::{self, Local};
use rustc_hash::FxHashMap;

#[derive(Clone)]
pub struct TyEnv {
    regions: DisjointSetsMap<RVid, RegionKind>,
}

#[derive(Clone)]
enum RegionKind {
    Concrete(Ty),
    Abstract { bound: Ty, ty: Ty },
}

impl RegionKind {
    fn ty(&self) -> Ty {
        match self {
            RegionKind::Concrete(ty) => ty.clone(),
            RegionKind::Abstract { ty, .. } => ty.clone(),
        }
    }

    fn ty_mut(&mut self) -> &mut Ty {
        match self {
            RegionKind::Concrete(ty) => ty,
            RegionKind::Abstract { ty, .. } => ty,
        }
    }
}

impl TyEnv {
    pub fn new() -> TyEnv {
        TyEnv {
            regions: DisjointSetsMap::new(),
        }
    }

    pub fn push_region(&mut self, ty: Ty) {
        self.regions.push(RegionKind::Concrete(ty));
    }

    pub fn lookup_region(&self, rvid: RVid) -> Ty {
        self.regions[rvid].ty()
    }

    pub fn lookup_place(&self, place: &ir::Place) -> Ty {
        let (_, ty) = self.walk_place(place);
        ty
    }

    pub fn update_region(&mut self, cursor: &mut Cursor, rvid: RVid, new_ty: Ty) {
        match &self.regions[rvid] {
            RegionKind::Concrete(_) => self.regions[rvid] = RegionKind::Concrete(new_ty),
            RegionKind::Abstract { bound, .. } => {
                cursor.subtyping(new_ty, bound.clone());
            }
        }
    }

    pub fn get_region(&self, place: &ir::Place) -> Region {
        let (local, _) = self.walk_place(place);
        self.regions.set(RVid::from(local)).collect()
    }

    pub fn move_place(&mut self, place: &ir::Place) -> Ty {
        let mut rvid = RVid::from(place.local);
        let mut ty = self.lookup_region(rvid);
        self.regions[RVid::from(place.local)] = RegionKind::Concrete(TyKind::Uninit.intern());
        for elem in &place.projection {
            match (elem, ty.kind()) {
                (ir::PlaceElem::Deref, TyKind::MutRef(region)) => {
                    self.regions[region[0]] = RegionKind::Concrete(TyKind::Uninit.intern());
                    ty = self.lookup_region(region[0]);
                }
                _ => {
                    unreachable!("unexpected type: {:?}", ty);
                }
            }
        }
        ty
    }

    pub fn write_place(&mut self, cursor: &mut Cursor, place: &ir::Place, new_ty: Ty) {
        let (local, ty) = self.walk_place(place);

        match ty.kind() {
            TyKind::Uninit | TyKind::Refine(..) => {
                self.update_region(cursor, local, new_ty);
            }
            TyKind::MutRef(_) => {
                todo!("implement update of mutable references")
            }
            TyKind::Exists(..) => unreachable!("unpacked existential: `{:?}`", ty),
        }
    }

    fn walk_place(&self, place: &ir::Place) -> (RVid, Ty) {
        let mut rvid = RVid::from(place.local);
        let mut ty = self.lookup_region(rvid);
        for elem in &place.projection {
            match (elem, ty.kind()) {
                (ir::PlaceElem::Deref, TyKind::MutRef(region)) => {
                    rvid = region[0];
                    ty = self.lookup_region(rvid);
                }
                _ => {
                    unreachable!("unexpected type: {:?}", ty);
                }
            }
        }
        (rvid, ty)
    }

    pub fn infer_bb_env(
        &self,
        cursor: &mut Cursor,
        shape: DisjointSetsMap<RVid, inference::Ty>,
        name_gen: &IndexGen<Var>,
    ) -> TyEnv {
        let regions = shape.map_values(|mut region, ty| {
            // We are assuming the region in self is a subset of the region in shape.
            let region_size = region.len();
            let rvid = region.next().unwrap();
            let ty = match &*ty {
                inference::TyS::Refine(_, _) => self.lookup_region(rvid),
                inference::TyS::Exists(bty) => {
                    let fresh = name_gen.fresh();
                    let pred = cursor.fresh_kvar(fresh, bty.sort());
                    TyKind::Exists(*bty, fresh, pred).intern()
                }
                inference::TyS::Uninit => TyKind::Uninit.intern(),
                inference::TyS::MutRef(region) => TyKind::MutRef(region.clone()).intern(),
            };
            if region_size > 1 {
                RegionKind::Abstract {
                    bound: ty.clone(),
                    ty,
                }
            } else {
                RegionKind::Concrete(ty)
            }
        });
        TyEnv { regions }
    }

    pub fn iter(&self) -> impl Iterator<Item = (Set<RVid>, Ty)> + '_ {
        self.regions
            .iter()
            .map(|(region, region_kind)| (region, region_kind.ty()))
    }

    pub fn unpack(&mut self, cursor: &mut Cursor, name_gen: &IndexGen<Var>) {
        for region_kind in self.regions.values_mut() {
            *region_kind.ty_mut() = cursor.unpack(name_gen, region_kind.ty());
        }
    }
}

impl fmt::Debug for TyEnv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.regions, f)
    }
}

impl fmt::Debug for RegionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegionKind::Concrete(ty) => write!(f, "{:?}", ty),
            RegionKind::Abstract { bound, ty } => {
                write!(f, "{:?} <: {:?}", ty, bound)
            }
        }
    }
}

impl From<DisjointSetsMap<Local, inference::Ty>> for TyEnv {
    fn from(map: DisjointSetsMap<Local, inference::Ty>) -> Self {
        todo!()
    }
}
