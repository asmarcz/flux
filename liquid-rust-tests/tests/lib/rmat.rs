#[lr::opaque]
#[lr::refined_by(rows: int, cols: int)]
pub struct RMat<T> {
    inner: Vec<Vec<T>>,
}

impl<T> RMat<T> {
    #[lr::assume]
    fn clone(n: usize, elem: T) -> Vec<T>
    where
        T: Copy,
    {
        let mut res = Vec::new();
        for _i in 0..n {
            res.push(elem);
        }
        res
    }

    #[lr::assume]
    #[lr::sig(fn(rows: usize, cols: usize, T) -> RMat<T>[rows, cols])]
    pub fn new(rows: usize, cols: usize, elem: T) -> RMat<T>
    where
        T: Copy,
    {
        let mut inner = Vec::new();
        for _i in 0..rows {
            let r = Self::clone(cols, elem);
            inner.push(r);
        }
        Self { inner }
    }

    #[lr::assume]
    #[lr::sig(fn(&RMat<T>[@m, @n], usize{v: 0 <= v && v < m}, usize{v:0 <= v && v < n}) -> &T)]
    pub fn get(&self, i: usize, j: usize) -> &T {
        &self.inner[i][j]
    }

    #[lr::assume]
    #[lr::sig(fn(&mut RMat<T>[@m, @n], usize{v: 0 <= v && v < m}, usize{v: 0 <= v && v < n}) -> &mut T)]
    pub fn get_mut(&mut self, i: usize, j: usize) -> &mut T {
        &mut self.inner[i][j]
    }
}

#[lr::assume]
#[lr::sig(fn() -> usize{v:false})]
pub fn die() -> usize {
    panic!("die")
}